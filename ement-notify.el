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


;;;; Customization

(defgroup ement-notify nil
  "Notification options."
  :group 'ement)

(defcustom ement-notify-predicates
  '(ement-notify--event-mentions-session-user-p)
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
  '(ement-notify--notify ement-notify--log-mentions)
  "Call these functions to send notifications for events.
These functions are called when the `ement-notify-predicates'
have already indicated that a notification should be sent.  Each
function is called with three arguments: the event, the room, and
the session (each the respective struct)."
  :type 'hook
  :options '(ement-notify--notify ement-notify--log-mentions))

(defcustom ement-notify-sound nil
  "Sound to play for notifications."
  :type '(choice (file :tag "Sound file")
                 (string :tag "XDG sound name")
                 (const :tag "Default XDG message sound" "message-new-instant")
                 (const :tag "Don't play a sound" nil)))

;;;; Commands


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

(defun ement-notify--log-mentions (event room session)
  "Log EVENT in ROOM to \"*Ement Mentions*\" buffer if it mentions SESSION's user."
  (when (ement-notify--event-mentions-session-user-p event room session)
    ;; HACK: For now, we call `ement-room--format-message' in a buffer
    ;; that pretends to be the room's buffer.
    (with-temp-buffer
      ;; Set these buffer-local variables, which `ement-room--format-message' uses.
      (setf ement-session session
            ement-room room)
      (let* ((new-left-margin-width (+ (string-width (ement-room-display-name room))
                                       (string-width (ement-room--user-display-name (ement-event-sender event) room))
                                       2))
             ;; Bind this to nil to prevent `ement-room--format-message' from padding sender name.
             (ement-room-sender-in-left-margin nil)
             (message (ement-room--format-message event "%O %S%L%B%r%R%t"))
             (buffer (or (get-buffer "*Ement Mentions*")
                         (with-current-buffer (get-buffer-create "*Ement Mentions*")
                           (setf left-margin-width 30
                                 right-margin-width 8)
                           (current-buffer)))))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert message "\n"))
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
