;;; ement.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; URL: https://github.com/alphapapa/ement.el
;; Package-Version: 0.1-pre
;; Package-Requires: ((emacs "26.3") (plz "0.1-pre") (ts "0.2"))

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
(require 'dns)
(require 'files)
(require 'map)

;; Third-party.

;; This package.
(require 'ement-api)
(require 'ement-macros)
(require 'ement-structs)
(require 'ement-room)
(require 'ement-notify)

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

(defvar ement-sync-callback-hook '(ement--update-room-buffers ement--auto-sync)
  "Hook run after `ement--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

(defvar ement-event-hook
  '(ement-notify ement--process-event)
  "Hook called for events.
Each function is called with three arguments: the event, the
room, and the session.")

(defvar ement-default-sync-filter
  '((room (state (lazy_load_members . t))
          (timeline (lazy_load_members . t))))
  "Default filter for sync requests.")

;;;; Customization

(defgroup ement nil
  "Options for Ement, the Matrix client."
  :group 'comm)

(defcustom ement-save-session nil
  "Save session to disk.
Writes the session file when Emacs is killed."
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (if value
             (add-hook 'kill-emacs-hook #'ement--kill-emacs-hook)
           (remove-hook 'kill-emacs-hook #'ement--kill-emacs-hook))))

(defcustom ement-session-file "~/.cache/ement.el"
  ;; FIXME: Expand correct XDG cache directory (new in Emacs 27).
  "Save username and access token to this file."
  :type 'file)

(defcustom ement-auto-sync t
  "Automatically sync again after syncing."
  :type 'boolean)

(defcustom ement-after-initial-sync-hook '(ement-list-rooms)
  "Hook run after initial sync.
Run with one argument, the session synced."
  :type 'hook)

;;;; Commands

;;;###autoload
(defun ement-connect (&optional user-id password)
  "Connect to Matrix with USER-ID and PASSWORD.
If USER-ID and PASSWORD are nil, and `ement-save-session' is
non-nil, and the `ement-session-file' is available, use the
saved session.  Interactively, with prefix, ignore a saved
session and log in again."
  (interactive (if (and ement-save-session
                        (file-readable-p ement-session-file))
                   ;; Session available: use it.
                   nil
                 ;; Read username and password.
                 (list (read-string "User ID: ")
                       (read-passwd "Password: "))))
  (if (not user-id)
      ;; Use saved session.
      (if-let ((saved-session (ement--read-session)))
          (progn
            ;; FIXME: Overwrites any current session.
            (setf ement-sessions (list saved-session))
            (ement--sync (car ement-sessions)))
        ;; Unable to read session: log in again.
        (let ((ement-save-session nil))
          (call-interactively #'ement-connect)))
    ;; Log in to new session.
    (unless (string-match (rx bos "@" (group (1+ (not (any ":")))) ; Username
                              ":" (group (optional (1+ (not (any blank)))))) ; Server name
                          user-id)
      (user-error "Invalid user ID format: use @USERNAME:SERVER"))
    (let* ((username (match-string 1 user-id))
           (server-name (match-string 2 user-id))
           ;; TODO: Also return port, and actually use that port elsewhere.
           (uri-prefix (ement--hostname-uri server-name))
           (user (make-ement-user :id user-id :username username :room-display-names (make-hash-table)))
           ;; FIXME: Dynamic port.
           (server (make-ement-server :name server-name :port 443 :uri-prefix uri-prefix))
           ;; A new session with a new token should be able to start over with a transaction ID of 0.
           (transaction-id 0)
           (session (make-ement-session :user user :server server :transaction-id transaction-id)))
      (cl-labels ((flows-callback
                   (data) (if (cl-loop for flow across (map-elt data 'flows)
                                       thereis (equal (map-elt flow 'type) "m.login.password"))
                              (progn
                                (message "Ement: Logging in with password...")
                                (password-login))
                            (error "Matrix server doesn't support m.login.password login flow.  Supported flows: %s"
                                   (cl-loop for flow in (map-elt data 'flows)
                                            collect (map-elt flow 'type)))))
                  (password-login
                   () (pcase-let* (((cl-struct ement-session user token device-id initial-device-display-name) session)
                                   ((cl-struct ement-user id) user)
                                   (data (ement-alist "type" "m.login.password"
                                                      "user" id
                                                      "password" password
                                                      "device_id" device-id
                                                      "initial_device_display_name" initial-device-display-name)))
                        ;; TODO: Clear password in callback (if we decide to hold on to it for retrying login timeouts).
                        (ement-api server token "login" (apply-partially #'ement--login-callback session)
                          :data (json-encode data) :method 'post))))
        ;; Verify that the m.login.password flow is supported.
        (when (ement-api server nil "login" #'flows-callback)
          (message "Ement: Checking server's login flows..."))))))

(defun ement-disconnect (session)
  "Disconnect from SESSION.
If `ement-auto-sync' is enabled, stop syncing, and clear the
session data.  When enabled, write the session to disk.  Any
existing room buffers are left alive and can be read, but other
commands in them won't work."
  (interactive (list (ement-complete-session)))
  (let ((id (ement-user-id (ement-session-user session))))
    (when-let ((process (map-elt ement-syncs session)))
      (ignore-errors
        (delete-process process)))
    (when ement-save-session
      ;; FIXME: Multiple sessions.
      (ement--write-session session))
    (setf ement-syncs (map-delete ement-syncs session)
          ement-sessions (map-delete ement-sessions session))
    (message "Disconnected %s" id)))

(defun ement--login-callback (session data)
  "Record DATA from logging in to SESSION and do initial sync."
  (pcase-let* (((map ('access_token token) ('device_id device-id)) data))
    (setf ement-sessions (list session)
	  (ement-session-token session) token
          (ement-session-device-id session) device-id))
  (ement--sync (car ement-sessions)))

;; FIXME: Make a room-buffer-name function or something.
(defvar ement-room-buffer-name-prefix)
(defvar ement-room-buffer-name-suffix)
(defun ement-view-room (session room)
  "Switch to a buffer showing ROOM on SESSION.
Calls `pop-to-buffer-same-window'.  Interactively, with prefix,
call `pop-to-buffer'."
  (interactive (list (car ement-sessions)
                     (ement-complete-room (car ement-sessions))))
  ;; FIXME: There must be a better way to handle this.
  (funcall (if current-prefix-arg
               #'pop-to-buffer #'pop-to-buffer-same-window)
           (ement-room--buffer session room (ement--room-buffer-name room)))
  (goto-char (point-max)))

;;;; Functions

(defun ement--sync-messages-p (session)
  "Return non-nil if sync-related messages should be shown for SESSION."
  ;; For now, this seems like the best way.
  (or (not (ement-session-has-synced-p session))
      (not ement-auto-sync)))

(defun ement--hostname-uri (hostname)
  "Return the \".well-known\" URI for server HOSTNAME.
If no URI is found, prompt the user for the hostname."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id178> ("4.1   Well-known URI")
  (cl-labels ((fail-prompt
               () (let ((input (read-string "Auto-discovery of server's well-known URI failed.  Input server hostname, or leave blank to use server name: ")))
                    (pcase input
                      ("" hostname)
                      (_ input))))
              (parse (string)
                     (if-let ((object (ignore-errors (json-read-from-string string))))
                         ;; Return extracted value.
                         (map-nested-elt object '(m.homeserver base_url))
                       ;; Parsing error: FAIL_PROMPT.
                       (fail-prompt))))
    (let ((response (condition-case err
                        (plz-get-sync (concat "https://" hostname "/.well-known/matrix/client")
                          :as 'response)
                      (plz-http-error (plz-error-response (cdr err))))))
      (pcase (plz-response-status response)
        (404 (fail-prompt))
        (200 (parse (plz-response-body response)))
        (_ (fail-prompt))))))

(defun ement--room-buffer-name (room)
  "Return name for ROOM's buffer."
  (concat ement-room-buffer-name-prefix
          (or (ement-room-display-name room)
              (setf (ement-room-display-name room)
                    (ement-room--room-display-name room)))
          ement-room-buffer-name-suffix))

(defun ement-complete-session ()
  "Return an Ement session selected with completion."
  (pcase-let* ((session-to-id
                (cl-loop for session in ement-sessions
                         collect (cons (ement-user-id (ement-session-user session))
                                       session)))
               (ids (mapcar #'car session-to-id))
               (selected-id (completing-read "Session: " ids nil t)))
    (alist-get selected-id session-to-id nil nil #'string=)))

(defun ement-complete-room (session)
  "Return a room selected from SESSION with completion."
  (pcase-let* ((name-to-room
                (cl-loop for room in (ement-session-rooms session)
                         collect (cons (format "%s (%s)"
                                               (or (ement-room-display-name room)
                                                   (setf (ement-room-display-name room)
                                                         (ement-room--room-display-name room)))
                                               (ement--room-alias room))
                                       room)))
               (names (mapcar #'car name-to-room))
               (selected-name (completing-read "Room: " names nil t)))
    (alist-get selected-name name-to-room nil nil #'string=)))

(cl-defun ement--sync (session &key (filter ement-default-sync-filter))
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used.

FILTER may be an alist representing a raw event filter (i.e. not
a filter ID).  When unspecified, the value of
`ement-default-sync-filter' is used.  The filter is encoded with
`json-encode'.  To use no filter, specify FILTER as nil."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Use a filter ID for default filter.
  ;; TODO: Optionally, automatically sync again when HTTP request fails.
  (when (map-elt ement-syncs session)
    (user-error "Ement: Already syncing this session"))
  (pcase-let* (((cl-struct ement-session server token next-batch) session)
               (params (remove
                        nil (list (list "full_state" (if next-batch "false" "true"))
                                  (when filter
				    ;; TODO: Document filter arg.
                                    (list "filter" (json-encode filter)))
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
                                     (display-warning 'ement "Sync timed out" :warning)
                                     (ement--auto-sync session))
                                    (_ (ement-api-error plz-error))))
                          :json-read-fn (lambda ()
                                          "Print a message, then call `json-read'."
                                          (when (ement--sync-messages-p session)
                                            (message "Ement: Response arrived after %.2f seconds.  Reading %s JSON response..."
                                                     (- (time-to-seconds) sync-start-time)
                                                     (file-size-human-readable (buffer-size))))
                                          (let ((start-time (time-to-seconds)))
                                            (prog1 (json-read)
                                              (when (ement--sync-messages-p session)
                                                (message "Ement: Reading JSON took %.2f seconds"
                                                         (- (time-to-seconds) start-time)))))))))
    (when process
      (setf (map-elt ement-syncs session) process)
      (when (ement--sync-messages-p session)
        (message "Ement: Sync request sent, waiting for response...")))))

(defun ement--sync-callback (session data)
  "Process sync DATA for SESSION.
Runs `ement-sync-callback-hook' with SESSION."
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt ement-syncs session) nil)
  (pcase-let* (((map rooms ('next_batch next-batch) ('account_data (map ('events account-data-events))))
                data)
               ((map ('join joined-rooms)) rooms)
               ;; FIXME: Only counts events in joined-rooms list.
               (num-events (cl-loop for (_id . room) in joined-rooms
                                    sum (length (map-nested-elt room '(state events)))
                                    sum (length (map-nested-elt room '(timeline events)))))
               (ement-progress-reporter (when (ement--sync-messages-p session)
                                          (make-progress-reporter "Ement: Reading events..." 0 num-events)))
               (ement-progress-value 0))
    (cl-callf2 append (cl-coerce account-data-events 'list) (ement-session-account-data session))
    (mapc (apply-partially #'ement--push-joined-room-events session) joined-rooms)
    ;; TODO: Process "left" rooms (remove room structs, etc).
    (setf (ement-session-next-batch session) next-batch)
    (run-hook-with-args 'ement-sync-callback-hook session)
    (when (ement--sync-messages-p session)
      (message (concat "Ement: Sync done."
                       (unless (ement-session-has-synced-p session)
                         (run-hook-with-args 'ement-after-initial-sync-hook session)
                         ;; Show tip after initial sync.
                         (setf (ement-session-has-synced-p session) t)
                         "  Use commands `ement-list-rooms' or `ement-view-room' to view a room."))))))

(defun ement--auto-sync (session)
  "If `ement-auto-sync' is non-nil, sync SESSION again."
  (when ement-auto-sync
    (ement--sync session)))

(defun ement--update-room-buffers (session)
  "Insert new events into SESSION's rooms which have buffers.
To be called in `ement-sync-callback-hook'."
  ;; TODO: Move this to ement-room.el, probably.
  ;; For now, we primitively iterate over the buffer list to find ones
  ;; whose mode is `ement-room-mode'.
  (let* ((buffers (cl-loop for room in (ement-session-rooms session)
                           when (map-elt (ement-room-local room) 'buffer)
                           collect it)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (cl-assert ement-room)
        (when (ement-room-ephemeral ement-room)
          (ement-room--process-events (ement-room-ephemeral ement-room))
          (setf (ement-room-ephemeral ement-room) nil))
        (when-let ((new-events (alist-get 'new-events (ement-room-local ement-room))))
          (ement-room--insert-events new-events)
          ;; For now, we also call `--process-events' for ones that are defined with `ement-room-defevent'.
          ;; FIXME: Unify this.
          ;; HACK: Process these events in reverse order, so that later events (like reactions)
          ;; which refer to earlier events can find them.  (Not sure if still necessary.)
          (ement-room--process-events (reverse new-events))
          ;; Clear new events slot.
          (setf (alist-get 'new-events (ement-room-local ement-room)) nil))))))

(defun ement--push-joined-room-events (session joined-room)
  "Push events for JOINED-ROOM into that room in SESSION."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id)) ; Really important that the ID is a STRING!
               (room (or (cl-find-if (lambda (room)
                                       (equal id (ement-room-id room)))
                                     (ement-session-rooms session))
                         (car (push (make-ement-room :id id) (ement-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('account_data account-data)
                     ('unread_notifications unread-notifications))
                event-types)
               (latest-timestamp))
    (ignore account-data unread-notifications summary state ephemeral)
    ;; NOTE: The idea is that, assuming that events in the sync reponse are in
    ;; chronological order, we push them to the lists in the room slots in that order,
    ;; leaving the head of each list as the most recent event of that type.  That means
    ;; that, e.g. the room state events may be searched in order to find, e.g. the most
    ;; recent room name event.  However, chronological order is not guaranteed, e.g. after
    ;; loading older messages (the "retro" function; this behavior is in development).

    ;; Save room summary.
    (dolist (parameter '(m.heroes m.joined_member_count m.invited_member_count))
      (when (alist-get parameter summary)
        ;; These fields are only included when they change.
        (setf (alist-get parameter (ement-room-summary room)) (alist-get parameter summary))))
    ;; Save state and timeline events.
    (cl-macrolet ((push-events
                   (type accessor)
                   ;; Push new events of TYPE to room's slot of ACCESSOR, and return the latest timestamp pushed.
		   `(let ((ts 0))
                      ;; NOTE: We replace each event in the vector with the
                      ;; struct, which is used when calling hooks later.
		      (cl-loop for event across-ref (alist-get 'events ,type)
                               do (setf event (ement--make-event event))
                               do (push event (,accessor room))
                               (when (ement--sync-messages-p session)
                                 (progress-reporter-update ement-progress-reporter (cl-incf ement-progress-value)))
                               (when (> (ement-event-origin-server-ts event) ts)
                                 (setf ts (ement-event-origin-server-ts event))))
		      ;; One would think that one should use `maximizing' here, but, completely
		      ;; inexplicably, it sometimes returns nil, even when every single value it's comparing
		      ;; is a number.  It's absolutely bizarre, but I have to do the equivalent manually.
		      ts)))
      (when (map-elt (ement-room-local room) 'buffer)
        ;; Only use ephemeral events if the room has a buffer, and don't use the
        ;; `push-events' macro because we don't use these events' timestamps.
        (cl-loop for event across (alist-get 'events ephemeral)
                 for event-struct = (ement--make-event event)
                 do (push event-struct (ement-room-ephemeral room))))
      ;; FIXME: This is a bit convoluted and hacky now.  Refactor it.
      (setf latest-timestamp
	    (max (push-events state ement-room-state)
		 (push-events timeline ement-room-timeline)))
      ;; NOTE: We also append the new events to the new-events list in the room's local
      ;; slot, which is used by `ement--update-room-buffers' to insert only new events.
      (cl-callf2 append (cl-coerce (alist-get 'events timeline) 'list)
                 (alist-get 'new-events (ement-room-local room)))
      ;; Update room's latest-timestamp slot.
      (when (> latest-timestamp (or (ement-room-latest-ts room) 0))
	(setf (ement-room-latest-ts room) latest-timestamp))
      (unless (ement-session-has-synced-p session)
        ;; Only set this token on initial sync, otherwise it would
        ;; overwrite earlier tokens from loading earlier messages.
        (setf (ement-room-prev-batch room) (alist-get 'prev_batch timeline))))
    ;; Run event hook for state and timeline events.
    (cl-loop for event across (alist-get 'events state)
             do (run-hook-with-args 'ement-event-hook event room session))
    (cl-loop for event across (alist-get 'events timeline)
             do (run-hook-with-args 'ement-event-hook event room session))))

(defun ement--make-event (event)
  "Return `ement-event' struct for raw EVENT list.
Adds sender to `ement-users' when necessary."
  (pcase-let* (((map content type unsigned
                     ('event_id id) ('origin_server_ts ts)
                     ('sender sender-id) ('state_key _state-key))
                event)
               (sender (or (gethash sender-id ement-users)
                           (puthash sender-id (make-ement-user
                                               :id sender-id :room-display-names (make-hash-table))
                                    ement-users))))
    (make-ement-event :id id :sender sender :type type :content content
                      :origin-server-ts ts :unsigned unsigned)))

;; FIXME: These functions probably need to compare timestamps to
;; ensure that older events that are inserted at the head of the
;; events lists aren't used instead of newer ones.

;; TODO: These two functions should be folded into event handlers.

(defun ement--room-alias (room)
  "Return latest m.room.canonical_alias event in ROOM."
  (or (cl-loop for event in (ement-room-timeline room)
               when (equal "m.room.canonical_alias" (ement-event-type event))
               return (alist-get 'alias (ement-event-content event)))
      (cl-loop for event in (ement-room-state room)
               when (equal "m.room.canonical_alias" (ement-event-type event))
               return (alist-get 'alias (ement-event-content event)))))

(defun ement--room-topic (room)
  "Return latest m.room.topic event in ROOM."
  (or (cl-loop for event in (ement-room-timeline room)
               when (equal "m.room.topic" (ement-event-type event))
               return (alist-get 'topic (ement-event-content event)))
      (cl-loop for event in (ement-room-state room)
               when (equal "m.room.topic" (ement-event-type event))
               return (alist-get 'topic (ement-event-content event)))))

(defun ement--read-session ()
  "Return saved session from file."
  (when (file-exists-p ement-session-file)
    (condition-case err
        ;; In case the saved session struct ever changes format, or something causes Emacs
        ;; to fail to read the saved structs, we handle any errors by logging in again.
        (let* ((read-circle t)
               (read-session (read (with-temp-buffer
                                     (insert-file-contents ement-session-file)
                                     (buffer-substring-no-properties (point-min) (point-max))))))
          (setf (ement-user-room-display-names (ement-session-user read-session)) (make-hash-table))
          (message "Ement: Read session.")
          read-session)
      (error (display-warning 'ement (format "Unable to read session from disk (%s).  Prompting to log in again.  Please report this error at https://github.com/alphapapa/ement.el"
                                             (error-message-string err)))))))

(defun ement--write-session (session)
  "Write SESSION to disk."
  ;; FIXME: This does not work with multiple sessions.
  ;; NOTE: If we ever persist more session data (like room data, so we
  ;; could avoid doing an initial sync next time), we should limit the
  ;; amount of session data saved (e.g. room history could grow
  ;; forever on-disk, which probably isn't what we want).
  (message "Ement: Writing session...")
  (with-temp-file ement-session-file
    (let* ((print-level nil)
           (print-length nil)
           ;; Very important to use `print-circle', although it doesn't
           ;; solve everything.  Writing/reading Lisp data can be tricky...
           (print-circle t)
           ;; We only want to write certain slots of the session to disk for now (because of problems
           ;; reading certain data back, the solving of which will probably require descending into
           ;; various rabbit holes), so we write a new session with only certain slots.
           (saved-session (make-ement-session :user (ement-session-user session)
                                              :server (ement-session-server session)
                                              :token (ement-session-token session)
                                              :transaction-id (ement-session-transaction-id session))))
      (prin1 saved-session (current-buffer))))
  ;; Ensure permissions are safe.
  (chmod ement-session-file #o600))

(defun ement--kill-emacs-hook ()
  "Function to be added to `kill-emacs-hook'.
Writes Ement session to disk when enabled."
  (ignore-errors
    ;; To avoid interfering with Emacs' exit, We must be careful that
    ;; this function handles errors, so just ignore any.
    (when (and ement-save-session
               (car ement-sessions))
      (ement--write-session (car ement-sessions)))))

(defun ement--mxc-to-url (uri session)
  "Return HTTPS URL for MXC URI accessed through SESSION."
  (pcase-let* (((cl-struct ement-session server) session)
               ((cl-struct ement-server uri-prefix) server)
               (server-name) (media-id))
    (string-match (rx "mxc://" (group (1+ (not (any "/"))))
                      "/" (group (1+ anything))) uri)
    (setf server-name (match-string 1 uri)
          media-id (match-string 2 uri))
    (format "%s/_matrix/media/r0/download/%s/%s"
            uri-prefix server-name media-id)))

;;;;; Event handlers

(defvar ement-event-handlers nil
  "Alist mapping event types to functions which process an event of each type.
Each function is called with three arguments: the event, the
room, and the session.  These handlers are run regardless of
whether a room has a live buffer.")

(defun ement--process-event (event room session)
  "Process EVENT for ROOM in SESSION.
Uses handlers defined in `ement-event-handlers'.  If no handler
is defined for EVENT's type, does nothing and returns nil."
  (when-let ((handler (alist-get (ement-event-type event) ement-event-handlers nil nil #'string=)))
    (funcall handler event room session)))

(defmacro ement-defevent (type &rest body)
  "Define an event handling function for events of TYPE, a string.
Around the BODY, the variable `event' is bound to the event being
processed, `room' to the room struct in which the event occurred,
and `session' to the session.  Adds function to
`ement-event-handlers', which see."
  (declare (indent defun))
  `(setf (alist-get ,type ement-event-handlers nil nil #'string=)
         (lambda (event room session)
           ,(concat "`ement-' handler function for " type " events.")
           ,@body)))

(ement-defevent "m.room.name"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map name))) event))
    (when name
      ;; Recalculate room name and cache in slot.
      (setf (ement-room-display-name room) (ement-room--room-display-name room)))))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
