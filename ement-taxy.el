;;; ement-taxy.el --- List Ement rooms with Taxy     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: 

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

;; 

;;; Code:

(require 'rx)

(require 'taxy)
(require 'taxy-magit-section)

(require 'ement-room-list)

(defgroup ement-taxy nil
  "Group Ement rooms with Taxy."
  :group 'ement)

;;;; Variables

(defvar ement-taxy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ement-taxy-RET)
    (define-key map [mouse-1] #'ement-taxy-mouse-1)
    map))

;;;; Keys

;; Since some of these keys need access to the session, and room
;; structs don't include the session, we use a two-element vector in
;; which the session is the second element.

(eval-and-compile
  (taxy-define-key-definer ement-taxy-define-key
    ement-taxy-keys "ement-taxy" "FIXME: Docstring."))

(ement-taxy-define-key membership (&key name type)
  ;; FIXME: Docstring: type should be a symbol of either `invite', `join', `leave'.
  (cl-labels ((format-membership (membership)
                                 (pcase membership
                                   ('join "Joined")
                                   ('invite "Invited")
                                   ('leave "[Left]"))))
    (pcase-let ((`[,(cl-struct ement-room (type membership)) ,_session] item))
      (if type
          (when (equal type membership)
            (or name (format-membership membership)))
        (format-membership membership)))))

(ement-taxy-define-key alias (&key name regexp)
  (pcase-let ((`[,(cl-struct ement-room canonical-alias) ,_session] item))
    (when canonical-alias
      (when (string-match-p regexp canonical-alias)
        name))))

(ement-taxy-define-key buffer-p ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (when buffer
      "Buffer")))

(ement-taxy-define-key direct-p ()
  (pcase-let ((`[,room ,session] item))
    (when (ement-room--direct-p room session)
      "Direct")))

(ement-taxy-define-key people-p ()
  (pcase-let ((`[,room ,session] item))
    (when (ement-room--direct-p room session)
      "[People]")))

(ement-taxy-define-key name (&key name regexp)
  (pcase-let* ((`[,room ,_session] item)
               (display-name (ement-room--room-display-name room)))
    (when display-name
      (when (string-match-p regexp display-name)
        name))))

(ement-taxy-define-key session (&optional user-id)
  (pcase-let ((`[,_room ,(cl-struct ement-session
                                    (user (cl-struct ement-user id)))]
               item))
    (pcase user-id
      (`nil id)
      (_ (when (equal user-id id)
           user-id)))))

(ement-taxy-define-key topic (&key name regexp)
  (pcase-let ((`[,(cl-struct ement-room topic) ,_session] item))
    (when topic
      (when (string-match-p regexp topic)
        name))))

(ement-taxy-define-key unread-p ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (when (and buffer
               (buffer-modified-p buffer))
      "Unread")))

(defcustom ement-taxy-default-keys
  '((membership :type 'leave)
    (people-p (buffer-p unread-p))

    (unread-p)
    ((name :name "Matrix"
           :regexp (rx (or "matrix" "TWIM"))))
    ((name :name "Emacs"
           :regexp (rx (or "Emacs" "ement.el" "org-mode" "magit" "spacemacs" "systemcrafters"))))
    ((name :name "Lisp" :regexp (rx (or "lisp" "hy")))))
  "Default keys."
  :type 'sexp)

;;;; Columns

(taxy-magit-section-define-column-definer "ement-taxy")

(ement-taxy-define-column #("🐱" 0 1 (help-echo "Avatar")) (:align 'right)
  ;; FIXME: The images tend to break alignment; not sure why.
  (pcase-let ((`[,(cl-struct ement-room avatar (local (map room-list-avatar)))
                 ,_session]
               item))
    (if (and ement-room-list-avatars avatar)
        (or room-list-avatar
            (let ((new-avatar
                   (propertize " " 'display
                               (ement--resize-image (get-text-property 0 'display avatar)
                                                    (frame-char-width) nil))))
              ;; alist-get doesn't seem to return the new value when used with setf?
              (setf (alist-get 'room-list-avatar (ement-room-local room))
                    new-avatar)
              new-avatar))
      " ")))

(ement-taxy-define-column "Name" (:max-width 25)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-room (local (map buffer))) room)
               (display-name (ement-room--room-display-name room))
               (face))
    (or (when display-name
          ;; TODO: Use code from ement-room-list and put in a dedicated function.
          (setf face (cl-copy-list '(:inherit (ement-room-list-name))))
          (when (and buffer (buffer-modified-p buffer))
            ;; For some reason, `push' doesn't work with `map-elt'.
            (setf (map-elt face :inherit)
                  (cons 'ement-room-list-unread (map-elt face :inherit))))
          (when (ement-room--direct-p room session)
            (setf (map-elt face :inherit)
                  (cons 'ement-room-list-direct (map-elt face :inherit))))
          (pcase (ement-room-type room)
            ('invite
             (setf (map-elt face :inherit) (cons 'ement-room-list-invited
                                                 (map-elt face :inherit)))))
          (propertize display-name 'face face))
        "")))

(ement-taxy-define-column "Topic" (:max-width 35)
  (pcase-let ((`[,(cl-struct ement-room topic) ,_session] item))
    (or topic "")))

(ement-taxy-define-column "Members" (:align 'right)
  (pcase-let ((`[,(cl-struct ement-room
                             (summary (map ('m.joined_member_count member-count))))
                 ,_session]
               item))
    (if member-count
        (number-to-string member-count)
      "")))

(ement-taxy-define-column "Latest" ()
  (pcase-let ((`[,(cl-struct ement-room latest-ts) ,_session] item))
    (if latest-ts
        (let* ((difference-seconds (- (float-time) (/ latest-ts 1000)))
               (n (cl-typecase difference-seconds
                    ((number 0 86400) ;; 1 day
                     (truncate (/ difference-seconds 3600)))
                    (otherwise ;; Difference in weeks.
                     (min (/ (length ement-room-list-timestamp-colors) 2)
                          (+ 24 (truncate (/ difference-seconds 86400 7)))))))
               (face (list :foreground (elt ement-room-list-timestamp-colors n)))
               (formatted-ts (ts-human-format-duration difference-seconds 'abbreviate)))
          (propertize formatted-ts 'face face))
      "")))

(ement-taxy-define-column #("B" 0 1 (help-echo "Buffer exists for room")) ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (if buffer #("B" 0 1 (help-echo "Buffer exists for room")) " ")))

(ement-taxy-define-column "Session" ()
  (pcase-let ((`[,_room ,(cl-struct ement-session (user (cl-struct ement-user id)))] item))
    id))

(unless ement-taxy-columns
  ;; TODO: Automate this or document it
  (setq-default ement-taxy-columns
                (get 'ement-taxy-columns 'standard-value)))

;;;; Commands

(cl-defun ement-taxy (&key (buffer-name "*Ement Taxy*")
                           (keys ement-taxy-default-keys)
                           display-buffer-action visibility-fn)
  (interactive)
  (let (format-table column-sizes)
    (cl-labels (;; (heading-face
                ;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
                (format-item (item) (gethash item format-table))
                (latest-ts
                 (item) (ement-room-latest-ts (elt item 0)))
                (room-unread-p
                 (item) (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
                          (and (buffer-live-p buffer)
                               (buffer-modified-p buffer))))
                (room-left-p
                 (item) (pcase-let ((`[,(cl-struct ement-room type) ,_session] item))
                          (equal 'leave type)))
                (taxy-unread-p
                 (taxy) (or (cl-some #'room-unread-p (taxy-items taxy))
                            (cl-some #'taxy-unread-p (taxy-taxys taxy))))
                (visible-p
                 ;; This is very confusing and doesn't currently work.
                 (section) (let ((value (oref section value)))
                             (if (cl-typecase value
                                   (taxy-magit-section (taxy-unread-p value))
                                   (ement-room nil))
                                 'show
                               'hide)))
                (t<nil (a b) (and a (not b)))
                (make-fn (&rest args)
                         (apply #'make-taxy-magit-section
                                :make #'make-fn
                                :format-fn #'format-item
                                :level-indent ement-taxy-level-indent
                                ;; :visibility-fn #'visible-p
                                ;; :heading-indent 2
                                :item-indent 2
                                ;; :heading-face-fn #'heading-face
                                args)))
      ;; (when (get-buffer buffer-name)
      ;;   (kill-buffer buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
        (ement-taxy-mode)
        (let* ((room-session-vectors
                (cl-loop for (_id . session) in ement-sessions
                         append (cl-loop for room in (ement-session-rooms session)
                                         collect (vector room session))))
               (taxy (thread-last
                       (make-fn
                        :name "Ement Rooms"
                        :take (taxy-make-take-function keys ement-taxy-keys))
                       (taxy-fill room-session-vectors)
                       (taxy-sort #'> #'latest-ts)
                       (taxy-sort* #'string< #'taxy-name)
                       ;; (taxy-sort* #'> (lambda (taxy)
                       ;;                   (latest-ts (car (taxy-items taxy)))))
                       (taxy-sort #'t<nil #'room-unread-p)
                       (taxy-sort* #'t<nil (lambda (taxy)
                                             (room-unread-p (car (taxy-items taxy)))))
                       (taxy-sort* #'t<nil (lambda (taxy)
                                             (not (room-left-p (car (taxy-items taxy))))))))
               (taxy-magit-section-insert-indent-items nil)
               (inhibit-read-only t)
               (format-cons (taxy-magit-section-format-items
                             ement-taxy-columns ement-taxy-column-formatters taxy)))
          (setf format-table (car format-cons)
                column-sizes (cdr format-cons)
                header-line-format (taxy-magit-section-format-header
                                    column-sizes ement-taxy-column-formatters))
          (delete-all-overlays)
          (erase-buffer)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first
              ;; :blank-between-depth bufler-taxy-blank-between-depth
              :initial-depth 0))))
      (pop-to-buffer buffer-name display-buffer-action))))

(defun ement-taxy-revert (_ignore-auto _noconfirm)
  "Revert current Ement-Taxy buffer."
  (interactive)
  (ement-taxy :display-buffer-action '((display-buffer-same-window))))

(defun ement-taxy-mouse-1 (event)
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'ement-taxy-RET))

(defun ement-taxy-RET ()
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (vector (pcase-let ((`[,room ,session] (oref (magit-current-section) value)))
              (ement-view-room room session)))
    (taxy-magit-section (call-interactively #'magit-section-cycle))
    (null nil)))

(define-derived-mode ement-taxy-mode magit-section-mode "Ement-Taxy"
  :global nil
  (setq-local ;; bookmark-make-record-function #'ement-taxy--bookmark-make-record
   revert-buffer-function #'ement-taxy-revert))

;;;; Footer

(provide 'ement-taxy)

;;; ement-taxy.el ends here
