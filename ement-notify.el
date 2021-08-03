;;; ement-notify.el --- Notifications for Ement events  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

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

;; This library implements notifications for Ement events.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'notifications)

(require 'ement-room)

(eval-when-compile
  (require 'ement-structs))

;;;; Variables

(defvar ement-notify-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'ement-notify-reply)
    (make-composed-keymap (list map button-buffer-map) 'view-mode-map))
  "Map for Ement notification buffers.")

;;;; Customization

(defgroup ement-notify nil
  "Notification options."
  :group 'ement)

(defcustom ement-notify-predicates
  '(ement-notify--event-mentions-session-user-p ement-notify--room-buffer-live-p)
  "Display notification if any of these return non-nil for an event.
Each predicate is called with three arguments: the event, the
room, and the session (each the respective struct)."
  :type '(repeat (choice (function-item ement-notify--event-mentions-session-user-p)
                         (function-item ement-notify--room-buffer-live-p)
                         (function :tag "Custom predicate"))))

(defcustom ement-notify-filters
  '(ement-notify--event-message-p)
  "Display notification if all of these return non-nil for an event.
Each predicate is called with three arguments: the event, the
room, and the session (each the respective struct)."
  :type '(repeat (choice (function-item ement-notify--event-message-p)
                         (function :tag "Custom predicate"))))

(defcustom ement-notify-functions
  '(ement-notify--notify ement-notify--log-mention ement-notify--log-buffer)
  "Call these functions to send notifications for events.
These functions are called when the `ement-notify-predicates'
have already indicated that a notification should be sent.  Each
function is called with three arguments: the event, the room, and
the session (each the respective struct)."
  :type 'hook
  :options '(ement-notify--notify ement-notify--log-mention ement-notify--log-buffer))

(defcustom ement-notify-sound nil
  "Sound to play for notifications."
  :type '(choice (file :tag "Sound file")
                 (string :tag "XDG sound name")
                 (const :tag "Default XDG message sound" "message-new-instant")
                 (const :tag "Don't play a sound" nil)))

;;;; Commands

(declare-function ement-view-room "ement")
(defun ement-notify-button-action (button)
  "Show BUTTON's event in its room buffer."
  ;; TODO: Is `interactive' necessary here?
  (interactive)
  (let* ((session (button-get button 'session))
         (room (button-get button 'room))
         (event (button-get button 'event)))
    (ement-view-room session room)
    (ement-room-goto-event event)))

(defun ement-notify-reply ()
  "Send a reply to event at point."
  (interactive)
  (save-window-excursion
    ;; Not sure why `call-interactively' doesn't work for `push-button' but oh well.
    (push-button)
    (call-interactively #'ement-room-send-reply)))

;;;; Functions

(defun ement-notify (event room session)
  "Send notifications for EVENT in ROOM on SESSION.
Calls functions in `ement-notify-functions' if any of
`ement-notify-predicates' return non-nil.  Does not do anything
if session hasn't finished initial sync."
  (when (and (ement-session-has-synced-p session)
             (cl-loop for pred in ement-notify-predicates
                      thereis (funcall pred event room session))
             (cl-loop for pred in ement-notify-filters
                      always (funcall pred event room session)))
    (run-hook-with-args 'ement-notify-functions event room session)))

(defun ement-notify--notify (event room _session)
  "Call `notifications-notify' for EVENT in ROOM on SESSION."
  (pcase-let* (((cl-struct ement-event sender content) event)
               ((map body) content)
               (room-name (ement-room-display-name room))
               (sender-name (ement-room--user-display-name sender room))
               (title (format "%s in %s" sender-name room-name))
               ;; TODO: Encode HTML entities.
               (body (if (stringp body)
                         (truncate-string-to-width body 60)
                       (progn
                         (display-warning 'ement-notify--notify
                                          (format "Event has no body.  Please report this bug.  ID:%S  ROOM:%S  TYPE:%S"
                                                  (ement-event-id room)
                                                  room-name (ement-event-type event)))
                         ""))))
    (notifications-notify :title title :body body
                          :app-name "Ement.el"
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
                          )))

;; FIXME: We duplicate some of the tests in these two functions.  Should redesign this.

(defun ement-notify--log-mention (event room session)
  "Log EVENT in ROOM to \"*Ement Mentions*\" buffer if it mentions SESSION's user."
  (when (ement-notify--event-mentions-session-user-p event room session)
    (ement-notify--log-event event room session :buffer-name "*Ement Mentions*")))

(defun ement-notify--log-buffer (event room session)
  "Log EVENT in ROOM on SESSION to \"*Ement Notifications*\" buffer if ROOM has a buffer."
  (when (ement-notify--room-buffer-live-p event room session)
    (ement-notify--log-event event room session)))

(cl-defun ement-notify--log-event (event room session &key (buffer-name "*Ement Notifications*"))
  "Log EVENT in ROOM to \"*Ement Notifications*\" buffer."
  ;; HACK: We only log "m.room.message" events for now.  This shouldn't be necessary since we
  ;; have `ement-notify--event-message-p' in `ement-notify-predicates', but just to be safe...
  (when (equal "m.room.message" (ement-event-type event))
    ;; HACK: For now, we call `ement-room--format-message' in a buffer that pretends to be
    ;; the room's buffer.  We have to do this, because the room might not have a buffer yet.
    (with-temp-buffer
      ;; Set these buffer-local variables, which `ement-room--format-message' uses.
      (setf ement-session session
            ement-room room)
      (let* (;; Bind this to nil to prevent `ement-room--format-message' from padding sender name.
             (ement-room-sender-in-left-margin nil)
             (ement-room-message-format-spec "%O %S%L%B%R%t")
             (message (ement-room--format-event event))
             (buffer (or (get-buffer buffer-name)
                         (with-current-buffer (get-buffer-create buffer-name)
                           (view-mode)
                           (visual-line-mode)
                           (use-local-map ement-notify-map)
                           (setf left-margin-width ement-room-left-margin-width
                                 right-margin-width 8)
                           (current-buffer))))
             (new-left-margin-width
              (max (buffer-local-value 'left-margin-width buffer)
                   (+ (string-width (ement-room-display-name room))
                      (string-width (ement-room--user-display-name (ement-event-sender event) room))
                      2)))
             (inhibit-read-only t))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            ;; We make the button manually to avoid overriding the message faces.
            ;; TODO: Define our own button type?  Maybe integrating the hack below...
            (save-excursion
              (insert (propertize message
                                  'button '(t)
                                  'category 'default-button
                                  'action #'ement-notify-button-action
                                  'session session
                                  'room room
                                  'event event)
                      "\n"))
            ;; HACK: Limit room name width.
            (when ement-notify-limit-room-name-width
              (save-excursion
                (let ((room-name-width (- (next-single-property-change (point) 'face) (point))))
                  (when (> room-name-width ement-notify-limit-room-name-width)
                    (forward-char ement-notify-limit-room-name-width)
                    (delete-region (point) (next-single-property-change (point) 'face))))))
            ;; HACK: Try to remove `button' face property from new text.  (It works!)
            (cl-loop for next-face-change-pos = (next-single-property-change (point) 'face)
                     for face-at = (get-text-property (point) 'face)
                     when (pcase face-at
                            ('button t)
                            ((pred listp) (member 'button face-at)))
                     do (put-text-property (point) (or next-face-change-pos (point-max))
                                           'face (pcase face-at
                                                   ('button nil)
                                                   ((pred listp) (delete 'button face-at))))
                     while next-face-change-pos
                     do (goto-char next-face-change-pos)))
          (setf left-margin-width new-left-margin-width)
          (when-let (window (get-buffer-window buffer))
            (set-window-margins window new-left-margin-width right-margin-width)))))))

;;;;; Predicates

(defun ement-notify--event-mentions-session-user-p (event room session)
  "Return non-nil if EVENT in ROOM mentions SESSION's user.
If EVENT's sender is SESSION's user, returns nil."
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-event sender) event))
    (unless (equal (ement-user-id user) (ement-user-id sender))
      (ement-room--event-mentions-user event user room))))

(defun ement-notify--room-buffer-live-p (_event room _session)
  "Return non-nil if ROOM has a live buffer."
  (buffer-live-p (alist-get 'buffer (ement-room-local room))))

(defun ement-notify--event-message-p (event _room _session)
  "Return non-nil if EVENT is an \"m.room.message\" event."
  (equal "m.room.message" (ement-event-type event)))

;;;; Footer

(provide 'ement-notify)

;;; ement-notify.el ends here
