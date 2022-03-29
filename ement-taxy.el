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
    (define-key map (kbd "SPC") #'ement-taxy-next-unread)
    (define-key map [mouse-1] #'ement-taxy-mouse-1)
    map))

;;;; Customization

(defcustom ement-taxy-auto-update t
  "Automatically update the taxy-based room list buffer."
  :type 'boolean)

;;;;; Faces

(defface ement-room-list-space '((t (:inherit font-lock-keyword-face)))
  "Space rooms."
  :group 'ement-room-list)

;;;; Keys

;; Since some of these keys need access to the session, and room
;; structs don't include the session, we use a two-element vector in
;; which the session is the second element.

(eval-and-compile
  (taxy-define-key-definer ement-taxy-define-key
    ement-taxy-keys "ement-taxy" "FIXME: Docstring."))

(ement-taxy-define-key membership (&key name status)
  ;; FIXME: Docstring: status should be a symbol of either `invite', `join', `leave'.
  (cl-labels ((format-membership (membership)
                                 (pcase membership
                                   ('join "Joined")
                                   ('invite "Invited")
                                   ('leave "[Left]"))))
    (pcase-let ((`[,(cl-struct ement-room (status membership)) ,_session] item))
      (if status
          (when (equal status membership)
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

(ement-taxy-define-key space (&key name id)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-session rooms) session)
               ((cl-struct ement-room type (local (map parents))) room)
               (key))
    (cl-labels ((format-space
                 (id) (let* ((parent-room (cl-find id rooms :key #'ement-room-id :test #'equal))
                             (space-name (if parent-room
                                             (ement-room-display-name parent-room)
                                           id)))
                        (concat "[Space: " space-name  "]"))))
      (when-let ((key (if id
                          ;; ID specified.
                          (cond ((or (member id parents)
                                     (equal id (ement-room-id room)))
                                 ;; Room is in specified space.
                                 (or name (format-space id)))
                                ((and (equal type "m.space")
                                      (equal id (ement-room-id room)))
                                 ;; Room is a specified space.
                                 (or name (concat "[Space: " (ement-room-display-name room) "]"))
                                 ))
                        ;; ID not specified. 
                        (pcase (length parents)
                          (0 nil)
                          (1
                           ;; TODO: Make the rooms list a hash table to avoid this lookup.
                           (format-space (car parents)))
                          (_
                           ;; TODO: How to handle this better?  (though it should be very rare)
                           (string-join (mapcar #'format-space parents) ", "))))))
        (propertize key 'face 'ement-room-list-space)))))

(ement-taxy-define-key name (&key name regexp)
  (pcase-let* ((`[,room ,_session] item)
               (display-name (ement-room--room-display-name room)))
    (when display-name
      (when (string-match-p regexp display-name)
        (or name regexp)))))

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
  '((membership :status 'leave)
    (people-p)
    (space))
  "Default keys."
  :type 'sexp)

;;;; Columns

(defvar ement-taxy-room-avatar-cache (make-hash-table)
  "Hash table caching room avatars for the `ement-taxy' room list.")

(eval-and-compile
  (taxy-magit-section-define-column-definer "ement-taxy"))

(ement-taxy-define-column #("🐱" 0 1 (help-echo "Avatar")) (:align 'right)
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room avatar) room))
    (if (and ement-room-list-avatars avatar)
        ;; NOTE: We resize every avatar to be suitable for this buffer, rather than using
        ;; the one cached in the room's struct.  If the buffer's faces change height, this
        ;; will need refreshing, but it should be worth it to avoid resizing the images on
        ;; every update.
        (or (gethash room ement-taxy-room-avatar-cache)
            (let ((new-avatar
                   (propertize " " 'display
                               (ement--resize-image (get-text-property 0 'display avatar)
                                                    nil (frame-char-height)))))
              (puthash room new-avatar ement-taxy-room-avatar-cache)))
      " ")))

(ement-taxy-define-column "Name" (:max-width 25)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-room type) room)
               (display-name (ement-room--room-display-name room))
               (face))
    (or (when display-name
          ;; TODO: Use code from ement-room-list and put in a dedicated function.
          (setf face (cl-copy-list '(:inherit (ement-room-list-name))))
          ;; In concert with the "Unread" column, this is roughly equivalent to the
          ;; "red/gray/bold/idle" states listed in <https://github.com/matrix-org/matrix-react-sdk/blob/b0af163002e8252d99b6d7075c83aadd91866735/docs/room-list-store.md#list-ordering-algorithm-importance>.
          (when (ement--room-unread-p room session)
            ;; For some reason, `push' doesn't work with `map-elt'.
            (setf (map-elt face :inherit)
                  (cons 'ement-room-list-unread (map-elt face :inherit))))
          (when (equal "m.space" type)
            (setf (map-elt face :inherit)
                  (cons 'ement-room-list-space (map-elt face :inherit))))
          (when (ement-room--direct-p room session)
            (setf (map-elt face :inherit)
                  (cons 'ement-room-list-direct (map-elt face :inherit))))
          (pcase (ement-room-type room)
            ('invite
             (setf (map-elt face :inherit) (cons 'ement-room-list-invited
                                                 (map-elt face :inherit)))))
          (propertize (button-buttonize display-name #'ement-taxy-mouse-1)
                      'face face
                      'mouse-face 'highlight))
        "")))

(ement-taxy-define-column #("Unread" 0 6 (help-echo "Unread events (Notifications:Highlights")) (:align 'right)
  (pcase-let* ((`[,(cl-struct ement-room unread-notifications) ,_session] item)
               ((map notification_count highlight_count) unread-notifications))
    (if (or (not unread-notifications)
            (and (equal 0 notification_count)
                 (equal 0 highlight_count)))
        ""
      (concat (propertize (number-to-string notification_count)
                          'face 'highlight)
              ":"
              (propertize (number-to-string highlight_count)
                          'face (if (zerop highlight_count)
                                    'default
                                  'ement-room-mention))))))

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

(ement-taxy-define-column #("B" 0 1 (help-echo "Buffer exists for room")) ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (if buffer
        #("B" 0 1 (help-echo "Buffer exists for room"))
      " ")))

(ement-taxy-define-column "Session" ()
  (pcase-let ((`[,_room ,(cl-struct ement-session (user (cl-struct ement-user id)))] item))
    id))

(unless ement-taxy-columns
  ;; TODO: Automate this or document it
  (setq-default ement-taxy-columns
                (get 'ement-taxy-columns 'standard-value)))

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun ement-taxy-bookmark-make-record ()
  "Return a bookmark record for the `ement-taxy' buffer."
  (pcase-let* (((cl-struct ement-session user) ement-session)
               ((cl-struct ement-user (id session-id)) user))
    ;; MAYBE: Support bookmarking specific events in a room.
    (list (concat "Ement room list (Taxy) (" session-id ")")
          (cons 'session-id session-id)
          (cons 'handler #'ement-taxy-bookmark-handler))))

(defun ement-taxy-bookmark-handler (bookmark)
  "Show `ement-taxy' room list buffer for BOOKMARK."
  (pcase-let* (((map session-id) bookmark))
    (unless (alist-get session-id ement-sessions nil nil #'equal)
      ;; MAYBE: Automatically connect.
      (user-error "Session %s not connected: call `ement-connect' first" session-id))
    (ement-taxy-room-list)))

;;;; Commands

;;;###autoload
(cl-defun ement-taxy-room-list (&key (buffer-name "*Ement Taxy*")
                                     (keys ement-taxy-default-keys)
                                     (display-buffer-action '(display-buffer-same-window))
                                     ;; visibility-fn
                                     )
  "Show a buffer listing Ement rooms, grouped with Taxy."
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
                 (item) (pcase-let ((`[,(cl-struct ement-room status) ,_session] item))
                          (equal 'leave status)))
                (taxy-unread-p
                 (taxy) (or (cl-some #'room-unread-p (taxy-items taxy))
                            (cl-some #'taxy-unread-p (taxy-taxys taxy))))
                (room-space-p
                 (item) (pcase-let ((`[,(cl-struct ement-room type) ,_session] item))
                          (equal "m.space" type)))
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
      (unless ement-sessions
        (error "Ement: Not connected.  Use `ement-connect' to connect"))
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
                       (taxy-sort #'t<nil #'room-unread-p)
                       (taxy-sort* #'string< #'taxy-name)
                       (taxy-sort* #'t<nil (lambda (taxy)
                                             (room-unread-p (car (taxy-items taxy)))))
                       (taxy-sort* #'t<nil (lambda (taxy)
                                             (not (room-left-p (car (taxy-items taxy))))))
                       (taxy-sort #'t<nil #'room-space-p)))
               (taxy-magit-section-insert-indent-items nil)
               (inhibit-read-only t)
               (format-cons (taxy-magit-section-format-items
                             ement-taxy-columns ement-taxy-column-formatters taxy))
               (pos (point))
               (window-start (window-start (get-buffer-window))))
          (setf format-table (car format-cons)
                column-sizes (cdr format-cons)
                header-line-format (taxy-magit-section-format-header
                                    column-sizes ement-taxy-column-formatters))
          (delete-all-overlays)
          (erase-buffer)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first
              ;; :blank-between-depth bufler-taxy-blank-between-depth
              :initial-depth 0))
          (goto-char pos)
          (display-buffer buffer-name (if current-prefix-arg
                                          '((display-buffer-same-window))
                                        display-buffer-action))
          (when (get-buffer-window)
            (set-window-start (get-buffer-window) window-start)))))))

(defun ement-taxy-revert (_ignore-auto _noconfirm)
  "Revert current Ement-Taxy buffer."
  (interactive)
  (ement-taxy-room-list :display-buffer-action '(display-buffer-no-window (allow-no-window . t))))

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

(defun ement-taxy-next-unread ()
  "Show next unread room."
  (interactive)
  (unless (button-at (point))
    (call-interactively #'forward-button))
  (cl-labels ((room-unread-p
               () (pcase-let* ((`[,room ,_session] (oref (magit-current-section) value))
                               ((cl-struct ement-room unread-notifications local) room)
                               ((map notification_count highlight_count) unread-notifications)
                               ((map buffer) local))
                    ;; For now, we test both unread count and whether the buffer is
                    ;; modified; later, we might do only the former (still to be worked
                    ;; out how canonical notifications are handled).
                    (or (and (buffer-live-p buffer) (buffer-modified-p buffer))
                        (and unread-notifications
                             (or (not (zerop notification_count))
                                 (not (zerop highlight_count))))))))
    (unless (cl-loop with starting-line = (line-number-at-pos)
                     if (room-unread-p)
                     do (progn
                          (goto-char (button-end (button-at (point))))
                          (push-button (1- (point)))
                          (cl-return t))
                     else do (call-interactively #'forward-button)
                     while (> (line-number-at-pos) starting-line))
      ;; No more unread rooms.
      (message "No more unread rooms"))))

(define-derived-mode ement-taxy-mode magit-section-mode "Ement-Taxy"
  :global nil
  (setq-local bookmark-make-record-function #'ement-taxy-bookmark-make-record
              revert-buffer-function #'ement-taxy-revert))

;;;; Functions

;;;###autoload
(defun ement-taxy-auto-update (_session)
  "Automatically update the Taxy room list buffer.
+Does so when variable `ement-taxy-auto-update' is non-nil.
+To be called in `ement-sync-callback-hook'."
  (when (and ement-taxy-auto-update
             (buffer-live-p (get-buffer "*Ement Taxy*")))
    (with-current-buffer (get-buffer "*Ement Taxy*")
      (revert-buffer))))

;;;; Footer

(provide 'ement-taxy)

;;; ement-taxy.el ends here
