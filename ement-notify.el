;;; ement-notify.el --- Notifications for Ement events  -*- lexical-binding: t; -*-

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

;; This library implements notifications for Ement events.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'notifications)

(require 'ement-lib)
(require 'ement-room)

(eval-when-compile
  (require 'ement-structs))

;;;; Variables

(declare-function ement-room-list "ement-room-list")
(defvar ement-notify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'ement-notify-reply)
    (define-key map (kbd "M-g M-l") #'ement-room-list)
    (define-key map (kbd "M-g M-m") #'ement-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'ement-notify-switch-to-notifications-buffer)
    (make-composed-keymap (list map button-buffer-map) 'view-mode-map))
  "Map for Ement notification buffers.")

(defvar ement-notify-dbus-p
  (and (featurep 'dbusbind)
       (require 'dbus nil :no-error)
       (dbus-ignore-errors (dbus-get-unique-name :session))
       ;; By default, emacs waits up to 25 seconds for a PONG.  Realistically, if there's
       ;; no pong after 2000ms, there's pretty sure no notification service connected or
       ;; the system's setup has issues.
       (dbus-ping :session "org.freedesktop.Notifications" 2000))
  "Whether D-Bus notifications are usable.")

;;;; Customization

(defgroup ement-notify nil
  "Notification options."
  :group 'ement)

(defcustom ement-notify-ignore-predicates
  '(ement-notify--event-not-message-p ement-notify--event-from-session-user-p)
  "Display notification if none of these return non-nil for an event.
Each predicate is called with three arguments: the event, the
room, and the session (each the respective struct)."
  :type '(repeat (choice (function-item ement-notify--event-not-message-p)
                         (function-item ement-notify--event-from-session-user-p)
                         (function :tag "Custom predicate"))))

(defcustom ement-notify-log-predicates
  '(ement-notify--event-mentions-session-user-p
    ement-notify--event-mentions-room-p
    ement-notify--room-buffer-live-p
    ement-notify--room-unread-p)
  "Predicates to determine whether to log an event to the notifications buffer.
If one of these returns non-nil for an event, the event is logged."
  :type 'hook
  :options '(ement-notify--event-mentions-session-user-p
             ement-notify--event-mentions-room-p
             ement-notify--room-buffer-live-p
             ement-notify--room-unread-p))

(defcustom ement-notify-mark-frame-urgent-predicates
  '(ement-notify--event-mentions-session-user-p
    ement-notify--event-mentions-room-p)
  "Predicates to determine whether to mark a frame as urgent.
If one of these returns non-nil for an event, the frame that most
recently showed the event's room's buffer is marked
urgent.  (Only works on X, not other GUI platforms.)"
  :type 'hook
  :options '(ement-notify--event-mentions-session-user-p
             ement-notify--event-mentions-room-p))

(defcustom ement-notify-mention-predicates
  '(ement-notify--event-mentions-session-user-p
    ement-notify--event-mentions-room-p)
  "Predicates to determine whether to log an event to the mentions buffer.
If one of these returns non-nil for an event, the event is logged."
  :type 'hook
  :options '(ement-notify--event-mentions-session-user-p
             ement-notify--event-mentions-room-p))

(defcustom ement-notify-notification-predicates
  '(ement-notify--event-mentions-session-user-p
    ement-notify--event-mentions-room-p
    ement-notify--room-buffer-live-p
    ement-notify--room-unread-p)
  "Predicates to determine whether to send a desktop notification.
If one of these returns non-nil for an event, the notification is sent."
  :type 'hook
  :options '(ement-notify--event-mentions-session-user-p
             ement-notify--event-mentions-room-p
             ement-notify--room-buffer-live-p
             ement-notify--room-unread-p))

(defcustom ement-notify-sound nil
  "Sound to play for notifications."
  :type '(choice (file :tag "Sound file")
                 (string :tag "XDG sound name")
                 (const :tag "Default XDG message sound" "message-new-instant")
                 (const :tag "Don't play a sound" nil)))

(defcustom ement-notify-limit-room-name-width nil
  "Limit the width of room display names in mentions and notifications buffers.
This prevents the margin from being made excessively wide."
  :type '(choice (integer :tag "Maximum width")
                 (const :tag "Unlimited width" nil)))

(defcustom ement-notify-prism-background nil
  "Add distinct background color by room to messages in notification buffers.
The color is specific to each room, generated automatically, and
can help distinguish messages by room."
  :type 'boolean)

(defcustom ement-notify-room-avatars t
  "Show room avatars in the notifications buffers.
This shows room avatars at the left of the window margin in
notification buffers.  It's not customizeable beyond that due to
limitations and complexities of displaying strings and images in
margins in Emacs.  But it's useful, anyway."
  :type 'boolean)

;;;; Commands

(declare-function ement-room-goto-event "ement-room")
(defun ement-notify-button-action (button)
  "Show BUTTON's event in its room buffer."
  ;; TODO: Is `interactive' necessary here?
  (interactive)
  (let* ((session (button-get button 'session))
         (room (button-get button 'room))
         (event (button-get button 'event)))
    (ement-view-room room session)
    (ement-room-goto-event event)))

(defun ement-notify-reply ()
  "Send a reply to event at point."
  (interactive)
  (save-window-excursion
    ;; Not sure why `call-interactively' doesn't work for `push-button' but oh well.
    (push-button)
    (call-interactively #'ement-room-write-reply)))

(defun ement-notify-switch-to-notifications-buffer ()
  "Switch to \"*Ement Notifications*\" buffer."
  (interactive)
  (switch-to-buffer (ement-notify--log-buffer "*Ement Notifications*")))

(defun ement-notify-switch-to-mentions-buffer ()
  "Switch to \"*Ement Mentions*\" buffer."
  (interactive)
  (switch-to-buffer (ement-notify--log-buffer "*Ement Mentions*")))

;;;; Functions

(defun ement-notify (event room session)
  "Send notifications for EVENT in ROOM on SESSION.
Sends if all of `ement-notify-ignore-predicates' return nil.
Does not do anything if session hasn't finished initial sync."
  (with-demoted-errors "ement-notify: Error: %S"
    (when (and (ement-session-has-synced-p session)
               (cl-loop for pred in ement-notify-ignore-predicates
                        never (funcall pred event room session)))
      (when (and ement-notify-dbus-p
                 (run-hook-with-args-until-success 'ement-notify-notification-predicates event room session))
        (ement-notify--notifications-notify event room session))
      (when (run-hook-with-args-until-success 'ement-notify-log-predicates event room session)
        (ement-notify--log-to-buffer event room session))
      (when (run-hook-with-args-until-success 'ement-notify-mention-predicates event room session)
        (ement-notify--log-to-buffer event room session :buffer-name "*Ement Mentions*"))
      (when (run-hook-with-args-until-success 'ement-notify-mark-frame-urgent-predicates event room session)
        (ement-notify--mark-frame-urgent event room session)))))

(defun ement-notify--mark-frame-urgent (_event room _session)
  "Mark frame showing ROOM's buffer as urgent.
If ROOM has no existing buffer, do nothing."
  (cl-labels ((mark-frame-urgent
               (frame) (let* ((prop "WM_HINTS")
                              (hints (cl-coerce
                                      (x-window-property prop frame prop nil nil t)
                                      'list)))
                         (setf (car hints) (logior (car hints) 256))
                         (x-change-window-property prop hints nil prop 32 t))))
    (when-let* ((buffer (alist-get 'buffer (ement-room-local room)))
                (frames (cl-loop for frame in (frame-list)
                                 when (eq 'x (framep frame))
                                 collect frame))
                (frame (pcase (length frames)
                         (1 (car frames))
                         (_
                          ;; Use the frame that most recently showed ROOM's buffer.
                          (car (sort frames
                                     (lambda (frame-a frame-b)
                                       (let ((a-pos (cl-position buffer (buffer-list frame-a)))
                                             (b-pos (cl-position buffer (buffer-list frame-b))))
                                         (cond ((and a-pos b-pos)
                                                (< a-pos b-pos))
                                               (a-pos)
                                               (b-pos))))))))))
      (mark-frame-urgent frame))))

(defun ement-notify--notifications-notify (event room _session)
  "Call `notifications-notify' for EVENT in ROOM on SESSION."
  (pcase-let* (((cl-struct ement-event sender content) event)
               ((cl-struct ement-room avatar (display-name room-displayname)) room)
               ((map body) content)
               (room-name (or room-displayname (ement--room-display-name room)))
               (sender-name (ement--user-displayname-in room sender))
               (title (format "%s in %s" sender-name room-name)))
    ;; TODO: Encode HTML entities.
    (when (stringp body)
      ;; If event has no body, it was probably redacted or something, so don't notify.
      (truncate-string-to-width body 60)
      (notifications-notify :title title :body body
                            :app-name "Ement.el"
                            :app-icon (when avatar
                                        (ement-notify--temp-file
                                         (plist-get (cdr (get-text-property 0 'display avatar)) :data)))
                            :category "im.received"
                            :timeout 5000
                            ;; FIXME: Using :sound-file seems to do nothing, ever.  Maybe a bug in notifications-notify?
                            :sound-file (when (and ement-notify-sound
                                                   (file-name-absolute-p ement-notify-sound))
                                          ement-notify-sound)
                            :sound-name (when (and ement-notify-sound
                                                   (not (file-name-absolute-p ement-notify-sound)))
                                          ement-notify-sound)
                            ;; TODO: Show when action used.
                            ;; :actions '("default" "Show")
                            ;; :on-action #'ement-notify-show
                            ))))

(cl-defun ement-notify--temp-file (content &key (timeout 5))
  "Return a filename holding CONTENT, and delete it after TIMEOUT seconds."
  (let ((filename (make-temp-file "ement-notify--temp-file-"))
        (coding-system-for-write 'no-conversion))
    (with-temp-file filename
      (insert content))
    (run-at-time timeout nil (lambda ()
                               (delete-file filename)))
    filename))

(define-derived-mode ement-notify-mode ement-room-mode "Ement Notify"
  (setf ement-room-sender-in-left-margin nil
        left-margin-width 0
        right-margin-width 8)
  (setq-local ement-room-message-format-spec "[%o%O] %S> %B%R%t"
              bookmark-make-record-function #'ement-notify-bookmark-make-record))

(cl-defun ement-notify--log-to-buffer (event room session &key (buffer-name "*Ement Notifications*"))
  "Log EVENT in ROOM on SESSION to \"*Ement Notifications*\" buffer."
  (with-demoted-errors "ement-notify--log-to-buffer: %S"
    ;; HACK: We only log "m.room.message" events for now.  This shouldn't be necessary
    ;; since we have `ement-notify--event-message-p' in `ement-notify-predicates', but
    ;; just to be safe...
    (when (equal "m.room.message" (ement-event-type event))
      (with-current-buffer (ement-notify--log-buffer buffer-name)
        (let* ((ement-session session)
               (ement-room room)
               (ement-room-sender-in-left-margin nil)
               (ement-room-message-format-spec "%o%O »%W %S> %B%R%t")
               (new-node (ement-room--insert-event event))
               (inhibit-read-only t)
               start end)
          (ewoc-goto-node ement-ewoc new-node)
          (setf start (point))
          (if-let (next-node (ewoc-next ement-ewoc new-node))
              (ewoc-goto-node ement-ewoc next-node)
            (goto-char (point-max)))
          (setf end (- (point) 2))
          (add-text-properties start end
                               (list 'button '(t)
                                     'category 'default-button
                                     'action #'ement-notify-button-action
                                     'session session
                                     'room room
                                     'event event))
          ;; Remove button face property.
          (alter-text-property start end 'face
                               (lambda (face)
                                 (pcase face
                                   ('button nil)
                                   ((pred listp) (remq 'button face))
                                   (_ face))))
          (when ement-notify-prism-background
            (add-face-text-property start end (list :background (ement-notify--room-background-color room)
                                                    :extend t))))))))

(defun ement-notify--log-buffer (name)
  "Return an Ement notifications buffer named NAME."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (ement-notify-mode)
        (current-buffer))))

(defun ement-notify--room-background-color (room)
  "Return a background color on which to display ROOM's messages."
  (or (alist-get 'notify-background-color (ement-room-local room))
      (setf (alist-get 'notify-background-color (ement-room-local room))
            (let ((color (color-desaturate-name
                          (ement--prism-color (ement-room-id room) :contrast-with (face-foreground 'default))
                          50)))
              (if (ement--color-dark-p (color-name-to-rgb (face-background 'default)))
                  (color-darken-name color 25)
                (color-lighten-name color 25))))))

;;;;; Predicates

(defun ement-notify--event-mentions-session-user-p (event room session)
  "Return non-nil if EVENT in ROOM mentions SESSION's user.
If EVENT's sender is SESSION's user, returns nil."
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-event sender) event))
    (unless (equal (ement-user-id user) (ement-user-id sender))
      (ement-room--event-mentions-user-p event user room))))

(defun ement-notify--room-buffer-live-p (_event room _session)
  "Return non-nil if ROOM has a live buffer."
  (buffer-live-p (alist-get 'buffer (ement-room-local room))))

(defun ement-notify--room-unread-p (_event room _session)
  "Return non-nil if ROOM has unread notifications.
According to the room's notification configuration on the server."
  (pcase-let* (((cl-struct ement-room unread-notifications) room)
               ((map notification_count highlight_count) unread-notifications))
    (not (and (equal 0 notification_count)
              (equal 0 highlight_count)))))

(defun ement-notify--event-message-p (event _room _session)
  "Return non-nil if EVENT is an \"m.room.message\" event."
  (equal "m.room.message" (ement-event-type event)))

(defun ement-notify--event-not-message-p (event _room _session)
  "Return non-nil if EVENT is not an \"m.room.message\" event."
  (not (equal "m.room.message" (ement-event-type event))))

(defun ement-notify--event-from-session-user-p (event _room session)
  "Return non-nil if EVENT is sent by SESSION's user."
  (equal (ement-user-id (ement-session-user session))
         (ement-user-id (ement-event-sender event))))

(defalias 'ement-notify--event-mentions-room-p #'ement--event-mentions-room-p)

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun ement-notify-bookmark-make-record ()
  "Return a bookmark record for the current `ement-notify' buffer."
  (list (buffer-name)
        ;; It seems silly to have to record the buffer name twice, but the
        ;; `bookmark-make-record' function seems to override the bookmark name sometimes,
        ;; which makes the result useless unless we save the buffer name separately.
        (cons 'buffer-name (buffer-name))
        (cons 'handler #'ement-notify-bookmark-handler)))

(defun ement-notify-bookmark-handler (bookmark)
  "Show Ement notifications buffer for BOOKMARK."
  (pcase-let ((`(,_bookmark-name . ,(map buffer-name)) bookmark))
    (switch-to-buffer (ement-notify--log-buffer buffer-name))))

;;;; Footer

(provide 'ement-notify)

;;; ement-notify.el ends here
