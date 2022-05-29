;;; ement-taxy.el --- List Ement rooms with Taxy     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>

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

(require 'button)
(require 'rx)

(require 'svg-lib)
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

(defface ement-room-list-space '((t (:inherit (font-lock-regexp-grouping-backslash ement-room-list-name))))
  "Space rooms."
  :group 'ement-room-list)

;;;; Keys

;; Since some of these keys need access to the session, and room
;; structs don't include the session, we use a two-element vector in
;; which the session is the second element.

(eval-and-compile
  (taxy-define-key-definer ement-taxy-define-key
    ement-taxy-keys "ement-taxy-key" "FIXME: Docstring."))

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

(ement-taxy-define-key buffer ()
  (pcase-let ((`[,(cl-struct ement-room (local (map buffer))) ,_session] item))
    (when buffer
      "Buffer")))

(ement-taxy-define-key direct ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-direct-p room session)
      "Direct")))

(ement-taxy-define-key people ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-direct-p room session)
      (propertize "People" 'face 'ement-room-list-direct))))

(ement-taxy-define-key space (&key name id)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-session rooms) session)
               ((cl-struct ement-room type (local (map parents))) room))
    (cl-labels ((format-space
                 (id) (let* ((parent-room (cl-find id rooms :key #'ement-room-id :test #'equal))
                             (space-name (if parent-room
                                             (ement-room-display-name parent-room)
                                           id)))
                        (concat "Space: " space-name))))
      (when-let ((key (if id
                          ;; ID specified.
                          (cond ((or (member id parents)
                                     (equal id (ement-room-id room)))
                                 ;; Room is in specified space.
                                 (or name (format-space id)))
                                ((and (equal type "m.space")
                                      (equal id (ement-room-id room)))
                                 ;; Room is a specified space.
                                 (or name (concat "Space: " (ement-room-display-name room)))
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

(ement-taxy-define-key space-p ()
  "Groups rooms that are themselves spaces."
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room type) room))
    (when (equal "m.space" type)
      "Spaces")))

(ement-taxy-define-key name (&key name regexp)
  (pcase-let* ((`[,room ,_session] item)
               (display-name (ement--room-display-name room)))
    (when display-name
      (when (string-match-p regexp display-name)
        (or name regexp)))))

(ement-taxy-define-key latest (&key name newer-than older-than)
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room latest-ts) room)
               (age))
    (when latest-ts
      (setf age (ts-diff (ts-now) (make-ts :unix (/ latest-ts 1000))))
      (cond (newer-than
             (when (<= age newer-than)
               (or name (format "Newer than %s seconds" newer-than))))
            (older-than
             (when (>= age older-than)
               (or name (format "Older than %s seconds" newer-than))))
            (t
             ;; Default to rooms with traffic in the last day.
             (if (<= age 86400)
                 "Last 24 hours"
               "Older than 24 hours"))))))

(ement-taxy-define-key freshness
  (&key (intervals '((86400 . "Past 24h")
                     (604800 . "Past week")
                     (2419200 . "Past month")
                     (31536000 . "Past year"))))
  (pcase-let* ((`[,room ,_session] item)
               ((cl-struct ement-room latest-ts) room)
               (age))
    (when latest-ts
      (setf age (- (ts-unix (ts-now)) (/ latest-ts 1000)))
      (or (alist-get age intervals nil nil #'>)
          "Older than a year"))))

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

(ement-taxy-define-key unread ()
  (pcase-let ((`[,room ,session] item))
    (when (ement--room-unread-p room session)
      "Unread")))

(ement-taxy-define-key favourite ()
  :then #'identity
  (pcase-let ((`[,room ,_session] item))
    (when (ement--room-favourite-p room)
      (propertize "Favourite" 'face 'ement-room-list-favourite))))

(ement-taxy-define-key low-priority ()
  :then #'identity
  (pcase-let ((`[,room ,_session] item))
    (when (ement--room-low-priority-p room)
      "Low-priority")))

(defcustom ement-taxy-default-keys
  '((space-p space)
    ((membership :status 'invite))
    (favourite)
    ((membership :status 'leave))
    (low-priority)
    (unread)
    ((latest :name "Last 24h" :newer-than 86400))
    (latest :name "Old" :older-than (* 86400 90))
    (people)
    freshness
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
               ((cl-struct ement-room avatar display-name) room))
    (if ement-room-list-avatars
        (or (gethash room ement-taxy-room-avatar-cache)
            (let ((new-avatar
                   (if avatar
                       ;; NOTE: We resize every avatar to be suitable for this buffer, rather than using
                       ;; the one cached in the room's struct.  If the buffer's faces change height, this
                       ;; will need refreshing, but it should be worth it to avoid resizing the images on
                       ;; every update.
                       (propertize " " 'display
                                   (ement--resize-image (get-text-property 0 'display avatar)
                                                        nil (frame-char-height)))
                     ;; Room has no avatar: make one.
                     (let* ((string (or display-name (ement--room-display-name room)))
                            (_ (when (string-match (rx bos (or "#" "!" "@")) string)
                                 (setf string (substring string 1))))
                            (color (ement-prism-color string)))
                       (propertize " " 'display (svg-lib-tag (substring string 0 1) nil
                                                             :background color :foreground "white"
                                                             :stroke 0))))))
              (puthash room new-avatar ement-taxy-room-avatar-cache)))
      ;; Avatars disabled: use a two-space string.
      " ")))

(ement-taxy-define-column "Name" (:max-width 25)
  (pcase-let* ((`[,room ,session] item)
               ((cl-struct ement-room type) room)
               (display-name (ement--room-display-name room))
               (face))
    (or (when display-name
          ;; TODO: Use code from ement-room-list and put in a dedicated function.
          (setf face (cl-copy-list '(:inherit (ement-room-list-name))))
          ;; In concert with the "Unread" column, this is roughly equivalent to the
          ;; "red/gray/bold/idle" states listed in <https://github.com/matrix-org/matrix-react-sdk/blob/b0af163002e8252d99b6d7075c83aadd91866735/docs/room-list-store.md#list-ordering-algorithm-importance>.
          (when (ement--room-unread-p room session)
            ;; For some reason, `push' doesn't work with `map-elt'...or does it?
            (push 'ement-room-list-unread (map-elt face :inherit)))
          (when (equal "m.space" type)
            (push 'ement-room-list-space (map-elt face :inherit)))
          (when (ement--room-direct-p room session)
            (push 'ement-room-list-direct (map-elt face :inherit)))
          (when (ement--room-favourite-p room)
            (push 'ement-room-list-favourite (map-elt face :inherit)))
          (when (ement--room-low-priority-p room)
            (push 'ement-room-list-low-priority (map-elt face :inherit)))
          (pcase (ement-room-status room)
            ('invite
             (push 'ement-room-list-invited (map-elt face :inherit)))
            ('leave
             (push 'ement-room-list-left (map-elt face :inherit))))
          (propertize (ement--button-buttonize display-name #'ement-taxy-mouse-1)
                      'face face
                      'mouse-face 'highlight))
        "")))

(ement-taxy-define-column #("Unread" 0 6 (help-echo "Unread events (Notifications:Highlights)")) (:align 'right)
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
                    ((number 0 3599) ;; <1 hour: 10-minute periods.
                     (truncate (/ difference-seconds 600)))
                    ((number 3600 86400) ;; 1 hour to 1 day: 24 1-hour periods.
                     (+ 6 (truncate (/ difference-seconds 3600))))
                    (otherwise ;; Difference in weeks.
                     (min (/ (length ement-room-list-timestamp-colors) 2)
                          (+ 24 (truncate (/ difference-seconds 86400 7)))))))
               (face (list :foreground (elt ement-room-list-timestamp-colors n)))
               (formatted-ts (ts-human-format-duration difference-seconds 'abbreviate)))
          (string-match (rx (1+ digit) (repeat 1 alpha)) formatted-ts)
          (propertize (match-string 0 formatted-ts) 'face face
                      'help-echo formatted-ts))
      "")))

(ement-taxy-define-column "Topic" (:max-width 35)
  (pcase-let ((`[,(cl-struct ement-room topic status) ,_session] item))
    ;; FIXME: Can the status and type unified, or is this inherent to the spec?
    (when topic
      (setf topic (replace-regexp-in-string "\n" " " topic 'fixedcase 'literal)))
    (pcase status
      ('invite (concat (propertize "[invited]"
                                   'face 'ement-room-list-invited)
                       " " topic))
      ('leave (concat (propertize "[left]"
                                  'face 'ement-room-list-left)
                      " " topic))
      (_ (or topic "")))))

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
  "Show a buffer listing Ement rooms, grouped with Taxy KEYS.
The buffer is named BUFFER-NAME and is shown with
DISPLAY-BUFFER-ACTION."
  (interactive)
  (let (format-table column-sizes)
    (cl-labels (;; (heading-face
                ;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
                (format-item (item) (gethash item format-table))
                ;; NOTE: Since these functions take an "item" (which is a [room session]
                ;; vector), they're prefixed "item-" rather than "room-".
                (item-latest-ts
                 (item) (or (ement-room-latest-ts (elt item 0))
                            ;; Room has no latest timestamp.  FIXME: This shouldn't
                            ;; happen, but it can, maybe due to oversights elsewhere.
                            0))
                (item-unread-p
                 (item) (pcase-let ((`[,room ,session] item))
                          (ement--room-unread-p room session)))
                (item-left-p
                 (item) (pcase-let ((`[,(cl-struct ement-room status) ,_session] item))
                          (equal 'leave status)))
                (taxy-unread-p
                 (taxy) (or (cl-some #'item-unread-p (taxy-items taxy))
                            (cl-some #'taxy-unread-p (taxy-taxys taxy))))
                (item-space-p
                 (item) (pcase-let ((`[,(cl-struct ement-room type) ,_session] item))
                          (equal "m.space" type)))
                (item-favourite-p
                 (item) (pcase-let ((`[,room ,_session] item))
                          (ement--room-favourite-p room)))
                (item-low-priority-p
                 (item) (pcase-let ((`[,room ,_session] item))
                          (ement--room-low-priority-p room)))
                (visible-p
                 ;; This is very confusing and doesn't currently work.
                 (section) (let ((value (oref section value)))
                             (if (cl-typecase value
                                   (taxy-magit-section (item-unread-p value))
                                   (ement-room nil))
                                 'show
                               'hide)))
                (item-invited-p
                 (item) (pcase-let ((`[,(cl-struct ement-room status) ,_session] item))
                          (equal 'invite status)))
                (t<nil (a b) (and a (not b)))
                (t>nil (a b) (and (not a) b))
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
               (taxy (cl-macrolet ((first-item
                                    (pred) `(lambda (taxy)
                                              (when (taxy-items taxy)
                                                (,pred (car (taxy-items taxy)))))))
                       (thread-last
                         (make-fn
                          :name "Ement Rooms"
                          :take (taxy-make-take-function keys ement-taxy-keys))
                         (taxy-fill room-session-vectors)
                         (taxy-sort #'> #'item-latest-ts)
                         (taxy-sort #'t<nil #'item-invited-p)
                         (taxy-sort #'t<nil #'item-favourite-p)
                         (taxy-sort #'t>nil #'item-low-priority-p)
                         (taxy-sort #'t<nil #'item-unread-p)
                         (taxy-sort #'t<nil #'item-space-p)
                         ;; Within each taxy, left rooms should be sorted last so that one
                         ;; can never be the first room in the taxy (unless it's the taxy
                         ;; of left rooms), which would cause the taxy to be incorrectly
                         ;; sorted last.
                         (taxy-sort #'t>nil #'item-left-p)
                         (taxy-sort* #'string< #'taxy-name)
                         (taxy-sort* #'> (lambda (taxy)
                                           (if (taxy-items taxy)
                                               (item-latest-ts (car (taxy-items taxy)))
                                             most-negative-fixnum)))
                         (taxy-sort* #'t<nil (first-item item-unread-p))
                         (taxy-sort* #'t<nil (first-item item-favourite-p))
                         (taxy-sort* #'t<nil (first-item item-invited-p))
                         (taxy-sort* #'t>nil (first-item item-low-priority-p))
                         (taxy-sort* #'t>nil (first-item item-left-p)))))
               (taxy-magit-section-insert-indent-items nil)
               (inhibit-read-only t)
               (format-cons (taxy-magit-section-format-items
                             ement-taxy-columns ement-taxy-column-formatters taxy))
               (pos (point))
               (window-start (if (get-buffer-window)
                                 (window-start (get-buffer-window))
                               0))
               (section-ident (when (magit-current-section)
                                (magit-section-ident (magit-current-section)))))
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
          (when (and section-ident (magit-get-section section-ident))
            (goto-char (oref (magit-get-section section-ident) start)))
          (display-buffer buffer-name display-buffer-action)
          (when (get-buffer-window)
            (set-window-start (get-buffer-window) window-start)))))))

(cl-defun ement-taxy-side-window (&key (side 'left))
  "Show room list in side window on SIDE.
Interactively, with prefix, show on right side; otherwise, on
left."
  (interactive (when current-prefix-arg
                 (list :side 'right)))
  (let ((display-buffer-mark-dedicated t))
    ;; Not sure if binding `display-buffer-mark-dedicated' is still necessary.
    (ement-taxy-room-list
     :display-buffer-action `(display-buffer-in-side-window
                              (dedicated . t)
                              (side . ,side)
                              (window-parameters
			       (no-delete-other-windows . t))))))

(defun ement-taxy-revert (_ignore-auto _noconfirm)
  "Revert current Ement-Taxy buffer."
  (interactive)
  (ement-taxy-room-list :display-buffer-action '(display-buffer-no-window (allow-no-window . t))))

(defun ement-taxy-mouse-1 (event)
  "Call `ement-taxy-RET' at point."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'ement-taxy-RET))

(defun ement-taxy-RET ()
  "View room at point, or cycle section at point."
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
      ;; FIXME: This seems to redisplay the buffer even when it's buried.  But it
      ;; shouldn't, because the revert function uses `display-buffer-no-window'.
      (revert-buffer))))

;;;; Footer

(provide 'ement-taxy)

;;; ement-taxy.el ends here
