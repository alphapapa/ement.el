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

;; NOTE: The tabulated-list API is awkward here.  When the
;; `tabulated-list-format' is changed, we have to make the change in 4
;; or 5 other places, and if one forgets to, bugs with non-obvious
;; causes happen.  I think library using EIEIO or structs would be
;; very helpful.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'tabulated-list)

(require 'ts)

(require 'ement)

;;;; Variables

(declare-function ement-notify-switch-to-mentions-buffer "ement-notify")
(declare-function ement-notify-switch-to-notifications-buffer "ement-notify")
(defvar ement-room-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "g") #'tabulated-list-revert)
    ;; (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "SPC") #'ement-room-list-next-unread)
    (define-key map (kbd "M-g M-m") #'ement-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'ement-notify-switch-to-notifications-buffer)
    ;; (define-key map (kbd "S") #'tabulated-list-sort)
    map))

(defvar ement-room-list-timestamp-colors nil
  "List of colors used for timestamps.
Set automatically when `ement-room-list-mode' is activated.")

(defvar ement-sessions)

;;;; Customization

(defgroup ement-room-list nil
  "Options for the room list buffer."
  :group 'ement)

(defcustom ement-room-list-auto-update t
  "Automatically update the room list buffer."
  :type 'boolean)

(defcustom ement-room-list-avatars t
  "Show room avatars in the room list."
  :type 'boolean)

(defcustom ement-room-list-simplify-timestamps t
  "Only show the largest unit of time in a timestamp.
For example, \"1h54m3s\" becomes \"1h\"."
  :type 'boolean)

;;;;; Faces

(defface ement-room-list-name
  '((t (:inherit font-lock-function-name-face button)))
  "Non-direct rooms.")

(defface ement-room-list-direct
  ;; In case `font-lock-constant-face' is bold, we set the weight to normal, so it can be
  ;; made bold for unread rooms only.
  '((t (:weight normal :inherit (font-lock-constant-face ement-room-list-name))))
  "Direct rooms.")

(defface ement-room-list-invited
  '((t (:inherit italic ement-room-list-name)))
  "Invited rooms.")

(defface ement-room-list-left
  '((t (:strike-through t :inherit ement-room-list-name)))
  "Left rooms.")

(defface ement-room-list-unread
  '((t (:inherit bold ement-room-list-name)))
  "Unread rooms.")

(defface ement-room-list-favourite '((t (:inherit (font-lock-doc-face ement-room-list-name))))
  "Favourite rooms.")

(defface ement-room-list-low-priority '((t (:inherit (font-lock-comment-face ement-room-list-name))))
  "Low-priority rooms.")

(defface ement-room-list-recent
  '((t (:inherit font-lock-warning-face)))
  "Latest timestamp of recently updated rooms.
The foreground color is used to generate a gradient of colors
from recent to non-recent for rooms updated in the past 24
hours.")

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
    (unless (alist-get session-id ement-sessions nil nil #'equal)
      ;; MAYBE: Automatically connect.
      (user-error "Session %s not connected: call `ement-connect' first" session-id))
    (ement-room-list)))

;;;; Commands

(defun ement-room-list-next-unread ()
  "Show next unread room."
  (interactive)
  (unless (button-at (point))
    (call-interactively #'forward-button))
  (unless (cl-loop with starting-line = (line-number-at-pos)
                   if (equal "U" (elt (tabulated-list-get-entry) 0))
                   do (progn
                        (goto-char (button-end (button-at (point))))
                        (push-button (1- (point)))
                        (cl-return t))
                   else do (call-interactively #'forward-button)
                   while (> (line-number-at-pos) starting-line))
    ;; No more unread rooms.
    (message "No more unread rooms")))

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
  "Ement-Room-List"
  :group 'ement
  (setf tabulated-list-format (vector
                               '("U" 1 t)
                               '(#("P" 0 1 (help-echo "Priority (favorite/low)")) 1 t)
                               '("B" 1 t)
                               ;; '("U" 1 t)
                               '("d" 1 t) ; Direct
                               (list (propertize "🐱"
                                                 'help-echo "Avatar")
                                     4 t) ; Avatar
                               '("Name" 25 t) '("Topic" 35 t)
                               (list "Latest"
                                     (if ement-room-list-simplify-timestamps
                                         6 20)
                                     #'ement-room-list-latest<
				     :right-align t)
                               '("Members" 7 ement-room-list-members<)
                               ;; '("P" 1 t) '("Tags" 15 t)
                               '("Session" 15 t))
        tabulated-list-sort-key '("Latest" . t)
        ement-room-list-timestamp-colors
        (cl-coerce
         (append (mapcar
                  ;; One face per hour, from "recent" to default.
                  (lambda (rgb)
                    (pcase-let ((`(,r ,g ,b) rgb))
                      (color-rgb-to-hex r g b 2)))
                  (color-gradient (color-name-to-rgb (face-foreground 'ement-room-list-recent
                                                                      nil 'default))
                                  (color-name-to-rgb (face-foreground 'default))
                                  24))
                 (mapcar
                  ;; One face per week for the last year (actually we
                  ;; generate colors for the past two years' worth so
                  ;; that the face for one-year-ago is halfway to
                  ;; invisible, and we don't use colors past that point).
                  (lambda (rgb)
                    (pcase-let ((`(,r ,g ,b) rgb))
                      (color-rgb-to-hex r g b 2)))
                  (color-gradient (color-name-to-rgb (face-foreground 'default))
                                  (color-name-to-rgb (face-background 'default))
                                  104)))
         'vector))
  (add-hook 'tabulated-list-revert-hook #'ement-room-list--set-entries nil 'local)
  (tabulated-list-init-header)
  (ement-room-list--set-entries)
  (tabulated-list-revert))

(defun ement-room-list-action (event)
  "Show buffer for room at EVENT or point."
  (interactive "e")
  (mouse-set-point event)
  (pcase-let* ((room (tabulated-list-get-id))
               (`[,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name ,_topic ,_latest ,_members ,user-id]
                (tabulated-list-get-entry))
               (session (alist-get user-id ement-sessions nil nil #'equal)))
    (ement-view-room room session)))

;;;; Functions

;;;###autoload
(defun ement-room-list-auto-update (_session)
  "Automatically update the room list buffer.
Does so when variable `ement-room-list-auto-update' is non-nil.
To be called in `ement-sync-callback-hook'."
  (when (and ement-room-list-auto-update
             (buffer-live-p (get-buffer "*Ement Rooms*")))
    (with-current-buffer (get-buffer "*Ement Rooms*")
      (revert-buffer))))

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
  (let ((entries (cl-loop for (_id . session) in ement-sessions
                          append (mapcar (lambda (room)
                                           (ement-room-list--entry session room))
                                         (ement-session-rooms session)))))
    (setf tabulated-list-entries
          ;; Pre-sort by latest event so that, when the list is sorted by other columns,
          ;; the rooms will be secondarily sorted by latest event.
          (cl-sort entries #'> :key (lambda (entry)
                                      ;; In case a room has no latest event (not sure if
                                      ;; this may obscure a bug, but this has happened, so
                                      ;; we need to handle it), we fall back to 0.
                                      (or (ement-room-latest-ts (car entry)) 0))))))

(defun ement-room-list--entry (session room)
  "Return entry for ROOM in SESSION for `tabulated-list-entries'."
  (pcase-let* (((cl-struct ement-room id canonical-alias display-name avatar topic latest-ts summary
                           (local (map buffer room-list-avatar)))
                room)
               ((map ('m.joined_member_count member-count)) summary)
               (e-alias (or canonical-alias
                            (setf (ement-room-canonical-alias room)
                                  (ement--room-alias room))
                            id))
               ;; FIXME: Figure out how to track unread status cleanly.
               (e-unread (if (and buffer (buffer-modified-p buffer))
                             (propertize "U" 'help-echo "Unread") ""))
               (e-buffer (if buffer (propertize "B" 'help-echo "Room has buffer") ""))
               (e-avatar (if (and ement-room-list-avatars avatar)
                             (or room-list-avatar
                                 (let ((new-avatar (propertize " " 'display
                                                               (ement--resize-image (get-text-property 0 'display avatar)
                                                                                    nil (frame-char-height)))))
                                   ;; alist-get doesn't seem to return the new value when used with setf?
                                   (setf (alist-get 'room-list-avatar (ement-room-local room))
                                         new-avatar)
                                   new-avatar))
                           ""))
               ;; We have to copy the list, otherwise using `setf' on it
               ;; later causes its value to be mutated for every entry.
               (name-face (cl-copy-list '(:inherit (ement-room-list-name))))
               (e-name (list (propertize (or display-name
                                             (ement-room--room-display-name room))
                                         ;; HACK: Apply face here, otherwise tabulated-list overrides it.
                                         'face name-face
                                         'help-echo e-alias)
                             'action #'ement-room-list-action))
               (e-topic (if topic
                            ;; Remove newlines from topic.  Yes, this can happen.
                            (replace-regexp-in-string "\n" "" topic t t)
                          ""))
               (formatted-timestamp (if latest-ts
                                        (ts-human-format-duration (- (ts-unix (ts-now)) (/ latest-ts 1000))
                                                                  t)
                                      ""))
               (latest-face (when latest-ts
                              (let* ((difference-seconds (- (float-time) (/ latest-ts 1000))  )
                                     (n (cl-typecase difference-seconds
                                          ((number 0 86400) ;; 1 day
                                           (truncate (/ difference-seconds 3600)))
                                          (otherwise ;; Difference in weeks.
                                           (min (/ (length ement-room-list-timestamp-colors) 2)
                                                (+ 24 (truncate (/ difference-seconds 86400 7))))))))
                                (list :foreground (elt ement-room-list-timestamp-colors n)))))
               (e-latest (or (when formatted-timestamp
                               (propertize formatted-timestamp
                                           'value latest-ts
                                           'face latest-face))
                             ;; Invited rooms don't have a latest-ts.
                             ""))
               (e-session (propertize (ement-user-id (ement-session-user session))
                                      'value session))
               ;;  ((e-tags favorite-p low-priority-p) (ement-room-list--tags room))
               (e-direct-p (if (ement-room--direct-p room session)
                               (propertize "d" 'help-echo "Direct room")
                             ""))
               (e-priority (cond ((ement--room-favourite-p room) "F")
                                 ((ement--room-low-priority-p room) "l")
                                 (" ")))
               (e-members (if member-count (number-to-string member-count) "")))
    (when ement-room-list-simplify-timestamps
      (setf e-latest (replace-regexp-in-string
                      (rx bos (1+ digit) (1+ alpha) (group (1+ (1+ digit) (1+ alpha))))
                      "" e-latest t t 1)))
    ;; Add face modifiers.
    (when (and buffer (buffer-modified-p buffer))
      ;; For some reason, `push' doesn't work with `map-elt'.
      (setf (map-elt name-face :inherit)
            (cons 'ement-room-list-unread (map-elt name-face :inherit))))
    (when (ement-room--direct-p room session)
      (setf (map-elt name-face :inherit)
            (cons 'ement-room-list-direct (map-elt name-face :inherit))))
    (when (ement--room-favourite-p room)
      (push 'ement-room-list-favourite (map-elt name-face :inherit)))
    (when (ement--room-low-priority-p room)
      (push 'ement-room-list-low-priority (map-elt name-face :inherit)))
    (pcase (ement-room-type room)
      ('invite
       (setf e-topic (concat (propertize "[invited]"
                                         'face 'ement-room-list-invited)
                             " " e-topic)
             (map-elt name-face :inherit) (cons 'ement-room-list-invited
                                                (map-elt name-face :inherit))))
      ('leave
       (setf e-topic (concat (propertize "[left]"
                                         'face 'ement-room-list-left)
                             " " e-topic)
             (map-elt name-face :inherit) (cons (map-elt name-face :inherit)
                                                'ement-room-list-left))))
    (list room (vector e-unread e-priority e-buffer e-direct-p
                       e-avatar e-name e-topic e-latest e-members
                       ;; e-tags
                       e-session
                       ;; e-avatar
                       ))))

;; TODO: Define sorters with a macro?  This gets repetitive and hard to update.

(defun ement-room-list-members< (a b)
  "Return non-nil if entry A has fewer members than room B.
A and B should be entries from `tabulated-list-mode'."
  (pcase-let* ((`(,_room [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,_latest ,a-members ,_session]) a)
               (`(,_room [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,_latest ,b-members ,_session]) b))
    (when (and a-members b-members)
      ;; Invited rooms may have no member count (I think).
      (< (string-to-number a-members) (string-to-number b-members)))))

(defun ement-room-list-latest< (a b)
  "Return non-nil if entry A has fewer members than room B.
A and B should be entries from `tabulated-list-mode'."
  (pcase-let* ((`(,_room-a [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,a-latest ,_a-members ,_session]) a)
               (`(,_room-b [,_unread ,_priority ,_buffer ,_direct ,_avatar ,_name-for-list ,_topic ,b-latest ,_b-members ,_session]) b)
               (a-latest (get-text-property 0 'value a-latest))
               (b-latest (get-text-property 0 'value b-latest)))
    (cond ((and a-latest b-latest)
           (< a-latest b-latest))
          (b-latest
           ;; Invited rooms have no latest timestamp, and we want to sort them first.
           nil)
          (t t))))

;;;; Footer

(provide 'ement-room-list)

;;; ement-room-list.el ends here
