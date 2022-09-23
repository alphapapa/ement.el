;;; ement-directory.el --- Public room directory support                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides support for viewing and searching public room directories on
;; Matrix homeservers.

;; To make rendering the list flexible and useful, we'll use `taxy-magit-section'.

;;; Code:

;;;; Requirements

(require 'ement)
(require 'ement-taxy)

(require 'taxy)
(require 'taxy-magit-section)

;;;; Variables

(defvar ement-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ement-directory-RET)
    (define-key map [mouse-1] #'ement-directory-mouse-1)
    map))

(defgroup ement-directory nil
  "Options for room directories."
  :group 'ement)

;;;; Mode

(define-derived-mode ement-directory-mode magit-section-mode "Ement-Directory"
  :global nil)

(defvar-local ement-directory-etc nil
  "Alist storing information in `ement-directory' buffers.")

;;;;; Keys

(eval-and-compile
  (taxy-define-key-definer ement-directory-define-key
    ement-directory-keys "ement-directory-key" "FIXME: Docstring."))

;; TODO: Other keys like guest_can_join, world_readable, etc.  (Last-updated time would be
;; nice, but the server doesn't include that in the results.)

(ement-directory-define-key joined-p ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) ement-directory-etc))
    (when (cl-find id (ement-session-rooms session)
                   :key #'ement-room-id :test #'equal)
      "Joined")))

(ement-directory-define-key size (&key < >)
  (pcase-let (((map ('num_joined_members size)) item))
    (cond ((and < (< size <))
           (format "< %s members" <))
          ((and > (> size >))
           (format "> %s members" >)))))

(ement-directory-define-key space-p ()
  "Groups rooms that are themselves spaces."
  (pcase-let (((map ('room_type type)) item))
    (when (equal "m.space" type)
      "Spaces")))

(defcustom ement-directory-default-keys
  '((joined-p)
    (space-p)
    ((size :> 10000))
    ((size :> 1000))
    ((size :> 100))
    ((size :> 10))
    ((size :< 11)))
  "Default keys."
  :type 'sexp)

;;;; Columns

(defvar-local ement-directory-room-avatar-cache (make-hash-table)
  ;; Use a buffer-local variable so that the cache is cleared when the buffer is closed.
  "Hash table caching room avatars for the `ement-directory' room list.")

(eval-and-compile
  (taxy-magit-section-define-column-definer "ement-directory"))

;; TODO: Fetch avatars (with queueing and async updating/insertion?).

(ement-directory-define-column #("✓" 0 1 (help-echo "Joined")) ()
  (pcase-let (((map ('room_id id)) item)
              ((map session) ement-directory-etc))
    (if (cl-find id (ement-session-rooms session)
                 :key #'ement-room-id :test #'equal)
        "✓"
      " ")))

(ement-directory-define-column "Name" (:max-width 25)
  (pcase-let* (((map name ('room_type type)) item)
               (face (pcase type
                       ("m.space" 'ement-room-list-space)
                       (_ 'ement-room-list-name))))
    (propertize (or name "[unnamed]")
                'face face)))

(ement-directory-define-column "Alias" (:max-width 25)
  (pcase-let (((map ('canonical_alias alias)) item))
    (or alias "")))

(ement-directory-define-column "Size" ()
  (pcase-let (((map ('num_joined_members size)) item))
    (number-to-string size)))

(ement-directory-define-column "Topic" (:max-width 50)
  (pcase-let (((map topic) item))
    (if topic
        (replace-regexp-in-string "\n" " | " topic nil t)
      "")))

(ement-directory-define-column "ID" ()
  (pcase-let (((map ('room_id id)) item))
    id))

(unless ement-directory-columns
  ;; TODO: Automate this or document it
  (setq-default ement-directory-columns
                '("Name" "Alias" "Size" "Topic" "ID")))

;;;; Commands

(cl-defun ement-directory (&key server session (limit 1000))
  "View the public room directory on SERVER with SESSION.
Show up to LIMIT rooms.  Interactively, with prefix, prompt for
server and LIMIT."
  (interactive (let* ((session (ement-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: " nil nil
                                               (ement-server-name (ement-session-server session)))
                                (ement-server-name (ement-session-server session))))
                      (args (list :server server :session session)))
                 (when current-prefix-arg
                   (cl-callf plist-put args
                     :limit (read-number "Limit number of rooms: " 1000)))
                 args))
  (pcase-let ((revert-function (lambda (&rest _ignore)
                                 (interactive)
                                 (ement-directory :server server :session session :limit limit)))
              (endpoint "publicRooms"))
    (ement-api session endpoint :params (list (list "limit" limit))
      :then (lambda (results)
              (ement-directory--view results
                                     :buffer-name (format "*Ement Directory: %s*" server)
                                     :root-section-name (format "Ement Directory: %s" server)
                                     :init-fn (lambda ()
                                                (setf (alist-get 'session ement-directory-etc) session)
                                                (setq-local revert-buffer-function revert-function)))))
    (ement-message "Listing %s rooms on %s..." limit server)))

(cl-defun ement-directory-search (query &key server session)
  "View public rooms on SERVER matching QUERY.
QUERY is a string used to filter results."
  (interactive (let* ((session (ement-complete-session :prompt "Search on session: "))
                      (server (if current-prefix-arg
                                  (read-string "Search on server: ")
                                (ement-server-name (ement-session-server session))))
                      (query (read-string (format "Search for rooms on %s: " server))))
                 (list query :server server :session session)))
  ;; TODO: Handle "include_all_networks" and "third_party_instance_id".  See § 10.5.4.
  (pcase-let* ((revert-function (lambda (&rest _ignore)
                                  (interactive)
                                  (ement-directory-search query :server server :session session)))
               (endpoint "publicRooms")
               (data (ement-alist "limit" 1000
                                  "filter" (ement-alist "generic_search_term" query))))
    (ement-api session endpoint :method 'post :data (json-encode data)
      :then (lambda (results)
              (ement-directory--view results
                                     :buffer-name (format "*Ement Directory: \"%s\" on %s*" query server)
                                     :root-section-name (format "Ement Directory: \"%s\" on %s" query server)
                                     :init-fn (lambda ()
                                                (setf (alist-get 'session ement-directory-etc) session)
                                                (setq-local revert-buffer-function revert-function)))))
    (ement-message "Searching for %S on %s..." query server)))

(defun ement-directory-mouse-1 (event)
  "Call `ement-directory-RET' at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'ement-directory-RET))

(defun ement-directory-RET ()
  "View or join room at point, or cycle section at point."
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (null nil)
    (list (pcase-let* (((map ('name name) ('room_id room-id)) (oref (magit-current-section) value))
                       ((map session) ement-directory-etc)
                       (room (cl-find room-id (ement-session-rooms session)
                                      :key #'ement-room-id :test #'equal)))
            (if room
                (ement-view-room room session)
              ;; Room not joined: prompt to join.  (Don't use the alias in the prompt,
              ;; because multiple rooms might have the same alias, e.g. when one is
              ;; upgraded or tombstoned.)
              (when (yes-or-no-p (format "Join room \"%s\" <%s>? " name room-id))
                (ement-join-room room-id session)))))
    (taxy-magit-section (call-interactively #'magit-section-cycle))))

;;;; Functions

(cl-defun ement-directory--view (results &key init-fn
                                         (buffer-name "*Ement Directory*")
                                         (root-section-name "Ement Directory")
                                         (keys ement-directory-default-keys)
                                         (display-buffer-action '(display-buffer-same-window)))
  "View RESULTS in an `ement-directory-mode' buffer.
Calls INIT-FN immediately after activating major mode.  Sets
BUFFER-NAME and ROOT-SECTION-NAME, and uses
DISPLAY-BUFFER-ACTION.  KEYS are a list of `taxy' keys.  To be
called by `ement-directory-search'."
  (let (format-table column-sizes window-start)
    (cl-labels ((format-item (item) (gethash item format-table))
                ;; NOTE: Since these functions take an "item" (which is a [room session]
                ;; vector), they're prefixed "item-" rather than "room-".
                (size
                 (item) (pcase-let (((map ('num_joined_members size)) item))
                          size))
                (t<nil (a b) (and a (not b)))
                (t>nil (a b) (and (not a) b))
                (make-fn (&rest args)
                         (apply #'make-taxy-magit-section
                                :make #'make-fn
                                :format-fn #'format-item
                                ;; FIXME: Should we reuse `ement-taxy-level-indent' here?
                                :level-indent ement-taxy-level-indent
                                ;; :visibility-fn #'visible-p
                                ;; :heading-indent 2
                                :item-indent 2
                                ;; :heading-face-fn #'heading-face
                                args)))
      (unless ement-sessions
        (error "Ement: Not connected.  Use `ement-connect' to connect"))
      (with-current-buffer (get-buffer-create buffer-name)
        (ement-directory-mode)
        (when init-fn
          (funcall init-fn))
        (pcase-let* (((map ('chunk rooms)) results)
                     (taxy (cl-macrolet ((first-item
                                          (pred) `(lambda (taxy)
                                                    (when (taxy-items taxy)
                                                      (,pred (car (taxy-items taxy)))))))
                             (thread-last
                               (make-fn
                                :name root-section-name
                                :take (taxy-make-take-function keys ement-directory-keys))
                               (taxy-fill (cl-coerce rooms 'list))
                               (taxy-sort #'> #'size)
                               (taxy-sort* #'string> #'taxy-name))))
                     (taxy-magit-section-insert-indent-items nil)
                     (inhibit-read-only t)
                     (format-cons (taxy-magit-section-format-items
                                   ement-directory-columns ement-directory-column-formatters taxy))
                     (pos (point))
                     (section-ident (when (magit-current-section)
                                      (magit-section-ident (magit-current-section)))))
          (setf format-table (car format-cons)
                column-sizes (cdr format-cons)
                header-line-format (taxy-magit-section-format-header
                                    column-sizes ement-directory-column-formatters)
                window-start (if (get-buffer-window buffer-name)
                                 (window-start (get-buffer-window buffer-name))
                               0))
          (delete-all-overlays)
          (erase-buffer)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first
              ;; :blank-between-depth bufler-taxy-blank-between-depth
              :initial-depth 0))
          (goto-char pos)
          (when (and section-ident (magit-get-section section-ident))
            (goto-char (oref (magit-get-section section-ident) start)))))
      (display-buffer buffer-name display-buffer-action)
      (when (get-buffer-window buffer-name)
        (set-window-start (get-buffer-window buffer-name) window-start))
      ;; NOTE: In order for `bookmark--jump-via' to work properly, the restored buffer
      ;; must be set as the current buffer, so we have to do this explicitly here.
      (set-buffer buffer-name))))

;;;; Footer

(provide 'ement-directory)
;;; ement-directory.el ends here
