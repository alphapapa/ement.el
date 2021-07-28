;;; ement-room-list.el --- Ement room list buffer    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm

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

;; This library implements a room list buffer.

;; NOTE: It doesn't appear that there is a way to get the number of
;; members in a room other than by retrieving the list of members and
;; counting them.  For a large room (e.g. the Spacemacs Gitter room or
;; #debian:matrix.org), that means thousands of users, none of the
;; details of which we care about.  So it seems impractical to know
;; the number of members when using lazy-loading.  So I guess we just
;; won't show the number of members.

;; TODO: (Or maybe there is, see m.joined_member_count).

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'tabulated-list)

;; FIXME: Depend on ts.
(require 'ts)

(require 'ement)

;;;; Variables

(defvar ement-room-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'tabulated-list-revert)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "S") #'tabulated-list-sort)))

(defvar ement-sessions)

;;;; Customization

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun ement-room-list-bookmark-make-record ()
  "Return a bookmark record for the `ement-room-list' buffer."
  (pcase-let* (((cl-struct ement-session user) ement-session)
               ((cl-struct ement-user (id session-id)) user))
    ;; MAYBE: Support bookmarking specific events in a room.
    (list (concat "Ement room list (" session-id ")")
          (cons 'session-id session-id)
          (cons 'handler #'ement-room-list-bookmark-handler))))

(defun ement-room-list-bookmark-handler (bookmark)
  "Show Ement room list buffer for BOOKMARK."
  (pcase-let* (((map session-id) bookmark))
    (unless (cl-loop for session in ement-sessions
                     thereis (equal session-id (ement-user-id (ement-session-user session))))
      ;; MAYBE: Automatically connect.
      (user-error "Session %s not connected: call `ement-connect' first" session-id))
    ;; FIXME: Support multiple sessions.
    (ement-room-list)))

;;;; Commands

;;;###autoload
(defun ement-room-list (&rest _ignore)
  "Show buffer listing joined rooms.
Calls `pop-to-buffer-same-window'.  Interactively, with prefix,
call `pop-to-buffer'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Ement Rooms*")
    (ement-room-list-mode)
    (setq-local bookmark-make-record-function #'ement-room-list-bookmark-make-record)
    ;; FIXME: There must be a better way to handle this.
    (funcall (if current-prefix-arg
                 #'pop-to-buffer #'pop-to-buffer-same-window)
             (current-buffer))))

;;;###autoload
(defalias 'ement-list-rooms 'ement-room-list)

(define-derived-mode ement-room-list-mode tabulated-list-mode
  "Ement room list"
  :group 'ement
  (setf tabulated-list-format (vector ;; '("U" 1 t) '("🐱" 4 t)
                               '("Name" 25 t) '("Topic" 35 t)
                               ;;  '("Members" 7 ement-room-list-members<)
                               '("Latest" 20 ement-room-list-latest<)
                               ;; '("D" 1 t) '("P" 1 t) '("Tags" 15 t)
                               '("Session" 15 t))
        tabulated-list-sort-key '("Latest" . t))
  (add-hook 'tabulated-list-revert-hook #'ement-room-list--set-entries nil 'local)
  (tabulated-list-init-header)
  (ement-room-list--set-entries)
  (tabulated-list-revert))

(defun ement-room-list-action (event)
  "Show buffer for room at EVENT or point."
  (interactive "e")
  (mouse-set-point event)
  (pcase-let* ((room (tabulated-list-get-id))
               (`[,_name ,_topic ,_latest ,user-id]
                (tabulated-list-get-entry))
               (session (cl-loop for session in ement-sessions
                                 when (equal user-id (ement-user-id (ement-session-user session)))
                                 return session)))
    (pop-to-buffer-same-window
     (ement-room--buffer session room (ement--room-buffer-name room)))))

;;;; Functions

(defun ement-room-list--set-entries ()
  "Set `tabulated-list-entries'."
  ;; Reset avatar size in case default font size has changed.
  ;; TODO: After implementing avatars.
  ;; (customize-set-variable 'ement-room-avatar-in-buffer-name-size ement-room-avatar-in-buffer-name-size)

  ;; NOTE: From Emacs docs:

  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.
  (setf tabulated-list-entries
        (cl-loop for session in ement-sessions
                 append (mapcar (apply-partially #'ement-room-list--entry session)
                                (ement-session-rooms session)))))

(defun ement-room-list--entry (session room)
  "Return entry for ROOM in SESSION for `tabulated-list-entries'."
  (pcase-let* (((cl-struct ement-room id canonical-alias display-name topic latest-ts) room)
               (e-alias (or canonical-alias
                            (setf (ement-room-canonical-alias room)
                                  (ement--room-alias room))
                            id))
               (topic (or topic
                          (setf (ement-room-topic room) (ement--room-topic room))))
               ;; FIXME: Figure out how to track unread status cleanly.
               ;;  (e-unread (if (buffer-modified-p buffer) "U" ""))
               ;;  (e-avatar (if avatar (ement-resize-avatar avatar) ""))
               (e-name (list (propertize (or display-name
                                             (setf (ement-room-display-name room)
                                                   (ement--room-display-name room)))
                                         'help-echo e-alias)
                             'action #'ement-room-list-action))
               (e-topic (if topic
                            ;; Remove newlines from topic.  Yes, this can happen.
                            (replace-regexp-in-string "\n" "" topic t t)
                          ""))
               (e-latest (propertize (ts-human-format-duration (- (ts-unix (ts-now)) (/ latest-ts 1000))
                                                               t)
                                     'value latest-ts))
               (e-session (propertize (ement-user-id (ement-session-user session))
                                      'value session))
               ;;  ((e-tags favorite-p low-priority-p) (ement-room-list--tags room))
               ;;  (e-direct-p (if (matrix-room-direct-p id session) "D" ""))
               ;; (e-priority (cond (favorite-p "F")
               ;;                   (low-priority-p "l")
               ;;                   ("N")))
               ;; (e-members (format "%s" (length members)))
               )
    (list room (vector                   ;; e-unread
                e-name e-topic ;; e-members
                ;; e-direct-p e-priority e-tags
                e-latest e-session
                ;; e-avatar
                ))))

;; TODO: Define sorters with a macro?  This gets repetitive and hard to update.

;; (defun ement-room-list-members< (a b)
;;   "Return non-nil if entry A has fewer members than room B.
;; A and B should be entries from `tabulated-list-mode'."
;;   (pcase-let* ((`(,_room [,_name-for-list ,_topic ,a-members ,_latest ,_session]) a)
;;                (`(,_room [,_name-for-list ,_topic ,b-members ,_latest ,_session]) b))
;;     (< (string-to-number a-members) (string-to-number b-members))))

(defun ement-room-list-latest< (a b)
  "Return non-nil if entry A has fewer members than room B.
A and B should be entries from `tabulated-list-mode'."
  (pcase-let* ((`(,_room [,_name-for-list ,_topic ,a-latest ,_session]) a)
               (`(,_room [,_name-for-list ,_topic ,b-latest ,_session]) b))
    (< (get-text-property 0 'value a-latest)
       (get-text-property 0 'value b-latest))))

;;;; Footer

(provide 'ement-room-list)

;;; ement-room-list.el ends here
