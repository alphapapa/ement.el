;;; ement-notifications.el --- Notifications support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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

;; This library implements support for Matrix notifications.  It differs from
;; ement-notify.el, which implements a kind of bespoke notification system for events
;; received via sync requests rather than Matrix's own notifications endpoint.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'ement)

;;;; Structs

(cl-defstruct ement-notification
  "Represents a Matrix notification."
  room-id event readp)

(defun ement-notifications--make (notification)
  "Return an `ement-notification' struct for NOTIFICATION.
NOTIFICATION is an alist representing a notification returned
from the \"/notifications\" endpoint.  The notification's event
is passed through `ement--make-event'."
  (pcase-let (((map room_id _actions _ts event read) notification))
    (make-ement-notification :room-id room_id :readp read
                             :event (ement--make-event event))))

;;;; Variables

(defvar ement-notifications-hook '(ement-notifications-log-to-buffer)
  "Functions called for `ement-notifications' notifications.
Each function is called with two arguments, the session and the
`ement-notification' struct.")

(defvar-local ement-notifications-metadata nil
  "Metadata for `ement-notifications' buffers.")

;;;; Commands

;;;###autoload
(cl-defun ement-notifications (session &key from limit only)
  "Show the notifications buffer for SESSION.
FROM may be a \"next_token\" token from a previous request.
LIMIT may be a maximum number of events to return.  ONLY may be
the string \"highlight\" to only return notifications that have
the highlight tweak set."
  (interactive (list (ement-complete-session)
                     :only (when current-prefix-arg
                             "highlight")))
  (let ((endpoint "notifications")
        (params (remq nil
                      (list (when from
                              (list "from" from))
                            (when limit
                              (list "limit" (number-to-string limit)))
                            (when only
                              (list "only" only))))))
    (ement-api session endpoint :params params
      :then (lambda (data)
              (pcase-let (((map notifications next_token) data))
                (with-current-buffer (ement-notifications--log-buffer)
                  (setf (map-elt ement-notifications-metadata :next-token) next_token)
                  (cl-loop for notification across notifications
                           do (run-hook-with-args 'ement-notifications-hook
                                                  session (ement-notifications--make notification)))
                  (ement-room--insert-ts-headers)
                  (pop-to-buffer (current-buffer))))))))

;;;; Functions

;; FIXME: The buffer name is the same as used in `ement-notify--log-to-buffer', except capitalized.

(cl-defun ement-notifications-log-to-buffer (session notification &key (buffer-name "*Ement NOTIFICATIONS*"))
  "Log EVENT in ROOM on SESSION to \"*Ement NOTIFICATIONS*\" buffer."
  (with-demoted-errors "ement-notifications-log-to-buffer: %S"
    (with-current-buffer (ement-notifications--log-buffer :name buffer-name)
      (save-window-excursion
        (when-let ((buffer-window (get-buffer-window (current-buffer))))
          ;; Select the buffer's window to avoid EWOC bug.  (See #191.)
          (select-window buffer-window))
        ;; TODO: Use the :readp slot to mark unread events.
        (pcase-let* (((cl-struct ement-notification room-id event) notification)
                     (ement-session session)
                     (ement-room (cl-find room-id (ement-session-rooms session)
                                          :key #'ement-room-id :test #'equal))
                     (ement-room-sender-in-left-margin nil)
                     (ement-room-message-format-spec "%o%O »%W %S> %B%R%t")
                     (new-node (ement-room--insert-event event))
                     (inhibit-read-only t)
                     (start) (end))
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
                                     'room ement-room
                                     'event event))
          ;; Remove button face property.
          (alter-text-property start end 'face
                               (lambda (face)
                                 (pcase face
                                   ('button nil)
                                   ((pred listp) (remq 'button face))
                                   (_ face))))
          (when ement-notify-prism-background
            (add-face-text-property start end (list :background (ement-notify--room-background-color ement-room)
                                                    :extend t))))))))

(cl-defun ement-notifications--log-buffer (&key (name "*Ement NOTIFICATIONS*"))
  "Return an Ement notifications buffer named NAME."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (ement-notifications-mode)
        (current-buffer))))

;;;; Mode

(define-derived-mode ement-notifications-mode ement-room-mode "Ement Notifications"
  (setf ement-room-sender-in-left-margin nil
        left-margin-width 0
        right-margin-width 8)
  (setq-local ement-room-message-format-spec "[%o%O] %S> %B%R%t"
              bookmark-make-record-function #'ement-notifications-bookmark-make-record))

;;;; Bookmark support

(require 'bookmark)

(defun ement-notifications-bookmark-make-record ()
  "Return a bookmark record for the current `ement-notifications' buffer."
  (list (buffer-name)
        ;; It seems silly to have to record the buffer name twice, but the
        ;; `bookmark-make-record' function seems to override the bookmark name sometimes,
        ;; which makes the result useless unless we save the buffer name separately.
        (cons 'buffer-name (buffer-name))
        (cons 'handler #'ement-notifications-bookmark-handler)))

(defun ement-notifications-bookmark-handler (bookmark)
  "Show `ement-notifications' buffer for BOOKMARK."
  (pcase-let ((`(,_bookmark-name . ,(map buffer-name)) bookmark))
    (switch-to-buffer (ement-notifications--log-buffer buffer-name))))

;;; Footer

(provide 'ement-notifications)

;;; ement-notifications.el ends here
