;;; ement.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; URL: https://github.com/alphapapa/ement.el
;; Package-Requires: ((emacs "26.3") (plz "0.1-pre"))

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

;; Another Matrix client!  This one is written from scratch and is
;; intended to be more "Emacsy," more suitable for MELPA, etc.  Also
;; it has a shorter, perhaps catchier name, that is a mildly clever
;; play on the name of the official Matrix client and the Emacs Lisp
;; filename extension (oops, I explained the joke), which makes for
;; much shorter symbol names.

;;; Code:

;;;; Debugging

(eval-and-compile
  (require 'warnings)
  (setq-local warning-minimum-log-level nil)
  (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)
(require 'files)
(require 'map)

;; Third-party.

;; This package.
(require 'ement-api)
(require 'ement-macros)
(require 'ement-structs)
(require 'ement-room)

;;;; Variables

(defvar ement-sessions nil
  "List of active `ement-session' sessions.")

(defvar ement-syncs nil
  "Alist of outstanding sync processes for each session.")

(defvar ement-users (make-hash-table :test #'equal)
  ;; NOTE: When changing the ement-user struct, it's necessary to
  ;; reset this table to clear old-type structs.
  "Hash table storing user structs keyed on user ID.")

(defvar ement-progress-reporter nil
  "Used to report progress while processing sync events.")

(defvar ement-progress-value nil
  "Used to report progress while processing sync events.")

(defvar ement-login-hook '(ement--sync)
  "Hook run after successful login.
Run with one argument, the session logged into.")

(defvar ement-sync-callback-hook '(ement--update-room-buffers ement--auto-sync)
  "Hook run after `ement--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

;;;; Customization

(defgroup ement nil
  "Options for Ement, the Matrix client."
  :group 'comm)

(defcustom ement-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom ement-save-session-file "~/.cache/matrix-client.el.token"
  ;; FIXME: Uses matrix-client.el token.  This causes hair-pulling
  ;; bugs if a transaction ID is reused, causing sent messages to
  ;; appear to send but really the server says, nah, you already sent
  ;; one with that ID, but here's an event ID to make it look like
  ;; sending worked.
  "Save username and access token to this file."
  :type 'file)

(defcustom ement-auto-sync nil
  ;; FIXME: When ready, enable by default.
  "Automatically sync again after syncing."
  :type 'boolean)

;;;; Commands

;;;###autoload
(defun ement-connect (user-id password &optional token transaction-id)
  "Connect to Matrix.
Interactively, with prefix, ignore a saved session and log in again."
  (interactive (pcase-let* (((map username password token ('txn-id transaction-id))
                             (or (unless current-prefix-arg
				   (ement--load-session))
                                 (ement-alist 'username (read-string "User ID: ")
                                              'password (read-passwd "Password: ")))))
                 (list username password token transaction-id)))
  (pcase-let* ((hostname (if (string-match (rx ":" (group (1+ anything))) user-id)
                             (match-string 1 user-id)
                           "matrix.org"))
               ;; FIXME: Lookup hostname from user ID with DNS.
               ;; FIXME: Dynamic port.
               (server (make-ement-server :hostname hostname :port 443))
               (user (make-ement-user :id user-id))
               (transaction-id (or transaction-id (random 100000)))
               (session (make-ement-session :user user :server server :token token :transaction-id transaction-id)))
    (if token
        (progn
          ;; FIXME: Overwrites any current session.
          (setf ement-sessions (list session))
          (ement--sync (car ement-sessions)))
      ;; Log in.
      (pcase-let* (((cl-struct ement-session user token device-id initial-device-display-name) session)
                   ((cl-struct ement-user id) user)
                   (data (ement-alist "type" "m.login.password"
                                      "user" id
                                      "password" password
                                      "device_id" device-id
                                      "initial_device_display_name" initial-device-display-name)))
        ;; TODO: Clear password in callback.
        (ement-api server token "login" (apply-partially #'ement--login-callback session)
          :data (json-encode data) :method 'post)))))

(defun ement--login-callback (session data)
  "Finish logging in to SESSION and sync."
  (pcase-let* (((map ('access_token token) ('device_id device-id)) data))
    (setf ement-sessions (list session)
	  (ement-session-token session) token
          (ement-session-device-id session) device-id)
    (run-hook-with-args 'ement-login-hook session)))

(defun ement-view-room (session room)
  "Switch to a buffer showing ROOM on SESSION."
  (interactive (list (car ement-sessions) (ement-complete-room (car ement-sessions))))
  (let ((buffer-name (concat ement-room-buffer-prefix
                             (setf (ement-room-display-name room)
                                   (ement--room-display-name room))
                             ement-room-buffer-suffix)))
    (pop-to-buffer (ement-room--buffer session room buffer-name))))

;;;; Functions

(defun ement-complete-room (session)
  "Return a room selected from SESSION with completion."
  (pcase-let* (((cl-struct ement-session rooms) session)
               (name-to-room (cl-loop for room in rooms
                                      collect (cons (format "%s (%s)"
                                                            (setf (ement-room-display-name room)
                                                                  (ement--room-display-name room))
                                                            (ement--room-alias room))
                                                    room)))
               (names (mapcar #'car name-to-room))
               (selected-name (completing-read "Room: " names nil t)))
    (alist-get selected-name name-to-room nil nil #'string=)))

(cl-defun ement--sync (session)
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  (cl-assert (not (map-elt ement-syncs session)))
  (pcase-let* (((cl-struct ement-session server token next-batch) session)
               (params (remove nil (list (list "full_state" (if next-batch "false" "true"))
					 (when next-batch
					   (list "since" next-batch))
					 (when next-batch
					   (list "timeout" "30000")))))
               (sync-start-time (time-to-seconds))
               ;; FIXME: Auto-sync again in error handler.
               (process (ement-api server token "sync" (apply-partially #'ement--sync-callback session)
                          :params params
                          :else (lambda (plz-error)
                                  (setf (map-elt ement-syncs session) nil)
                                  (pcase (plz-error-curl-error plz-error)
                                    (`(28 . ,_) ; Timeout: sync again if enabled.
                                     (ement--auto-sync session))
                                    (_ (ement-api-error plz-error))))
                          :json-read-fn (lambda ()
                                          "Print a message, then call `json-read'."
                                          (message "Ement: Response arrived after %.2f seconds.  Reading %s JSON response..."
                                                   (- (time-to-seconds) sync-start-time)
                                                   (file-size-human-readable (buffer-size)))
                                          (let ((start-time (time-to-seconds)))
                                            (prog1 (json-read)
                                              (message "Ement: Reading JSON took %.2f seconds" (- (time-to-seconds) start-time))))))))
    (when process
      (setf (map-elt ement-syncs session) process)
      (message "Ement: Sync request sent, waiting for response..."))))

(defun ement--sync-callback (session data)
  "FIXME: Docstring."
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt ement-syncs session) nil)
  (pcase-let* (((map rooms ('next_batch next-batch)) data)
               ((map ('join joined-rooms)) rooms)
               ;; FIXME: Only counts events in joined-rooms list.
               (num-events (cl-loop for (_id . room) in joined-rooms
                                    sum (length (map-nested-elt room '(state events)))
                                    sum (length (map-nested-elt room '(timeline events)))))
               (ement-progress-reporter (make-progress-reporter "Ement: Reading events..." 0 num-events))
               (ement-progress-value 0))
    (mapc (apply-partially #'ement--push-joined-room-events session) joined-rooms)
    (setf (ement-session-next-batch session) next-batch)
    (run-hook-with-args 'ement-sync-callback-hook session)
    (message "Sync done")))

(defun ement--auto-sync (session)
  "If `ement-auto-sync' is non-nil, sync SESSION again."
  (when ement-auto-sync
    (ement--sync session)))

(defun ement--update-room-buffers (&rest _)
  "Add new events to Ement rooms which have buffers.
To be called in `ement-sync-callback-hook'."
  ;; For now, we primitively iterate over the buffer list to find ones
  ;; whose mode is `ement-room-mode'.
  (let* ((buffers (cl-loop for buffer being the buffers
                           when (eq 'ement-room-mode (buffer-local-value 'major-mode buffer))
                           collect buffer)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (cl-assert ement-room)
        (mapc #'ement-room--insert-event (ement-room-timeline* ement-room))
        ;; Move new events.
        (setf (ement-room-timeline ement-room)
              (append (ement-room-timeline* ement-room) (ement-room-timeline ement-room))
              (ement-room-timeline* ement-room) nil)))))

(defun ement--push-joined-room-events (session joined-room)
  "Push events for JOINED-ROOM into that room in SESSION."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id))    ; Really important that the ID is a STRING!
               (room (or (cl-find-if (lambda (room)
                                       (equal id (ement-room-id room)))
                                     (ement-session-rooms session))
                         (car (push (make-ement-room :id id) (ement-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('account_data account-data)
                     ('unread_notifications unread-notifications))
                event-types))
    (ignore account-data unread-notifications summary state ephemeral)
    ;; NOTE: The idea is that, assuming that events in the sync reponse are in chronological
    ;; order, we push them to the lists in the room slots in that order, leaving the head of
    ;; each list as the most recent event of that type.  That means that, e.g. the room
    ;; state events may be searched in order to find, e.g. the most recent room name event.

    ;; FIXME: Further mapping instead of alist-get.
    (cl-loop for event across (alist-get 'events state)
             do (push (ement--make-event event) (ement-room-state room))
             (progress-reporter-update ement-progress-reporter (cl-incf ement-progress-value)))
    (cl-loop for event across (alist-get 'events timeline)
             do (push (ement--make-event event) (ement-room-timeline* room))
             (progress-reporter-update ement-progress-reporter (cl-incf ement-progress-value)))
    (setf (ement-room-prev-batch room) (alist-get 'prev_batch timeline))))

(defun ement--make-event (event)
  "Return `ement-event' struct for raw EVENT list.
Adds sender to `ement-users' when necessary."
  (pcase-let* (((map content type unsigned
                     ('event_id id) ('origin_server_ts ts) ('sender sender-id) ('state_key _state-key))
                event)
               (sender (or (gethash sender-id ement-users)
                           (puthash sender-id (make-ement-user :id sender-id :room-display-names (make-hash-table))
                                    ement-users))))
    (make-ement-event :id id :sender sender :content content :origin-server-ts ts :type type :unsigned unsigned)))

(defun ement--room-display-name (room)
  "Return the displayname for ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id349>.
  ;; MAYBE: Optional "force" argument to make it update the room name/alias in the struct.
  (or (ement--room-name room)
      (ement--room-alias room)
      ;; FIXME: Steps 3, etc.
      (ement-room-id room)))

(defun ement--room-name (room)
  (cl-loop for event in (ement-room-state room)
           when (equal "m.room.name" (ement-event-type event))
           return (alist-get 'name (ement-event-content event))))

(defun ement--room-alias (room)
  (cl-loop for event in (ement-room-state room)
           when (equal "m.room.canonical_alias" (ement-event-type event))
           return (alist-get 'alias (ement-event-content event))))

(defun ement--load-session ()
  "Return saved session from file."
  (when (file-exists-p ement-save-session-file)
    (read (with-temp-buffer
            (insert-file-contents ement-save-session-file)
            (buffer-substring-no-properties (point-min) (point-max))))))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
