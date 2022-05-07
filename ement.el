;;; ement.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; URL: https://github.com/alphapapa/ement.el
;; Package-Version: 0.1-pre
;; Package-Requires: ((emacs "27.1") (map "2.1") (plz "0.1-pre") (taxy "0.9") (taxy-magit-section "0.9") (svg-lib "0.2.5") (transient "0.3.7") (ts "0.2.1"))

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

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `ement-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (require 'warnings)
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)
(require 'dns)
(require 'files)
(require 'map)
(require 'xml)

;; Third-party.

(require 'magit-section)

;; This package.
(require 'ement-api)
(require 'ement-macros)
(require 'ement-structs)
(require 'ement-room)
(require 'ement-notify)

;;;;; Compilation

;; To avoid compilation warnings.

(eval-when-compile
  (require 'taxy-magit-section))

;;;; Variables

(defvar ement-sessions nil
  "Alist of active `ement-session' sessions, keyed by MXID.")

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

(defvar ement-sync-callback-hook
  '(ement--update-room-buffers ement--auto-sync ement-room-list-auto-update
                               ement-taxy-auto-update)
  "Hook run after `ement--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

(defvar ement-event-hook
  '(ement-notify ement--process-event ement--put-event)
  "Hook called for events.
Each function is called with three arguments: the event, the
room, and the session.  This hook isn't intended to be modified
by users; ones who do so should know what they're doing.")

(defvar ement-default-sync-filter
  '((room (state (lazy_load_members . t))
          (timeline (lazy_load_members . t))))
  "Default filter for sync requests.")

;; From other files.
(defvar ement-room-avatar-max-width)
(defvar ement-room-avatar-max-height)

;;;; Customization

(defgroup ement nil
  "Options for Ement, the Matrix client."
  :group 'comm)

(defcustom ement-save-sessions nil
  "Save session to disk.
Writes the session file when Emacs is killed."
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (if value
             (add-hook 'kill-emacs-hook #'ement--kill-emacs-hook)
           (remove-hook 'kill-emacs-hook #'ement--kill-emacs-hook))))

(defcustom ement-sessions-file "~/.cache/ement.el"
  ;; FIXME: Expand correct XDG cache directory (new in Emacs 27).
  "Save username and access token to this file."
  :type 'file)

(defcustom ement-auto-sync t
  "Automatically sync again after syncing."
  :type 'boolean)

(defcustom ement-after-initial-sync-hook
  '(ement-list-rooms ement-view-initial-rooms ement--link-children)
  "Hook run after initial sync.
Run with one argument, the session synced."
  :type 'hook)

(defcustom ement-initial-sync-timeout 40
  "Timeout in seconds for initial sync requests.
For accounts in many rooms, the Matrix server may take some time
to prepare the initial sync response, and increasing this timeout
might be necessary."
  :type 'integer)

(defcustom ement-auto-view-rooms nil
  "Rooms to view after initial sync.
Alist mapping user IDs to a list of room aliases/IDs to open buffers for."
  :type '(alist :key-type (string :tag "Local user ID")
                :value-type (repeat (string :tag "Room alias/ID"))))

;;;; Commands

;;;###autoload
(cl-defun ement-connect (&key user-id password uri-prefix session)
  "Connect to Matrix with USER-ID and PASSWORD, or using SESSION.
Interactively, with prefix, ignore a saved session and log in
again; otherwise, use a saved session if `ement-save-sessions' is
enabled and a saved session is available, or prompt to log in if
not enabled or available.

If USERID or PASSWORD are not specified, the user will be
prompted for them.

If URI-PREFIX is specified, it should be the prefix of the
server's API URI, including protocol, hostname, and optionally
the port, e.g.

  \"https://matrix-client.matrix.org\"
  \"http://localhost:8080\""
  (interactive (if current-prefix-arg
                   ;; Force new session.
                   (list :user-id (read-string "User ID: ")
                         :password (read-passwd "Password: "))
                 ;; Use known session.
                 (unless ement-sessions
                   ;; Read sessions from disk.
                   (condition-case err
                       (setf ement-sessions (ement--read-sessions))
                     (error (display-warning 'ement (format "Unable to read session data from disk (%s).  Prompting to log in again."
                                                            (error-message-string err))))))
                 (cl-case (length ement-sessions)
                   (0 (list :user-id (read-string "User ID: ")
                            :password (read-passwd "Password: ")))
                   (1 (list :session (cdar ement-sessions)))
                   (otherwise (list :session (ement-complete-session))))))
  (cl-labels ((new-session
               () (unless (string-match (rx bos "@" (group (1+ (not (any ":")))) ; Username
                                            ":" (group (optional (1+ (not (any blank)))))) ; Server name
                                        user-id)
                    (user-error "Invalid user ID format: use @USERNAME:SERVER"))
               (let* ((username (match-string 1 user-id))
                      (server-name (match-string 2 user-id))
                      (uri-prefix (or uri-prefix (ement--hostname-uri server-name)))
                      (user (make-ement-user :id user-id :username username :room-display-names (make-hash-table)))
                      (server (make-ement-server :name server-name :uri-prefix uri-prefix))
                      (transaction-id (ement--initial-transaction-id)))
                 (make-ement-session :user user :server server :transaction-id transaction-id
                                     :events (make-hash-table :test #'equal))))
              (password-login
               () (pcase-let* (((cl-struct ement-session user device-id initial-device-display-name) session)
                               ((cl-struct ement-user id) user)
                               (data (ement-alist "type" "m.login.password"
                                                  "user" id
                                                  "password" password
                                                  "device_id" device-id
                                                  "initial_device_display_name" initial-device-display-name)))
                    ;; TODO: Clear password in callback (if we decide to hold on to it for retrying login timeouts).
                    (ement-api session "login" :method 'post :data (json-encode data)
                      :then (apply-partially #'ement--login-callback session))))
              (flows-callback
               (data) (if (cl-loop for flow across (map-elt data 'flows)
                                   thereis (equal (map-elt flow 'type) "m.login.password"))
                          (progn
                            (message "Ement: Logging in with password...")
                            (password-login))
                        (error "Matrix server doesn't support m.login.password login flow.  Supported flows: %s"
                               (cl-loop for flow in (map-elt data 'flows)
                                        collect (map-elt flow 'type))))))
    (if session
        ;; Start syncing given session.
        (let ((user-id (ement-user-id (ement-session-user session))))
          ;; HACK: If session is already in ement-sessions, this replaces it.  I think that's okay...
          (setf (alist-get user-id ement-sessions nil nil #'equal) session)
          (ement--sync session))
      ;; Start password login flow.  Prompt for user ID and password
      ;; if not given (i.e. if not called interactively.)
      (unless user-id
        (setf user-id (read-string "User ID: ")))
      (unless password
        (setf password (read-passwd (format "Password for %s: " user-id))))
      (setf session (new-session))
      (when (ement-api session "login" :then #'flows-callback)
        (message "Ement: Checking server's login flows...")))))

(defun ement-disconnect (sessions)
  "Disconnect from SESSIONS.
Interactively, with prefix, disconnect from all sessions.  If
`ement-auto-sync' is enabled, stop syncing, and clear the session
data.  When enabled, write the session to disk.  Any existing
room buffers are left alive and can be read, but other commands
in them won't work."
  (interactive (list (if current-prefix-arg
                         (mapcar #'cdr ement-sessions)
                       (list (ement-complete-session)))))
  (when ement-save-sessions
    ;; Write sessions before we remove them from the variable.
    (ement--write-sessions ement-sessions))
  (dolist (session sessions)
    (let ((user-id (ement-user-id (ement-session-user session))))
      (when-let ((process (map-elt ement-syncs session)))
        (ignore-errors
          (delete-process process)))
      ;; NOTE: I'd like to use `map-elt' here, but not until
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47368> is fixed, I guess.
      (setf (alist-get session ement-syncs nil nil #'equal) nil
            (alist-get user-id ement-sessions nil 'remove #'equal) nil)))
  (unless ement-sessions
    ;; HACK: If no sessions remain, clear the users table.  It might be best
    ;; to store a per-session users table, but this is probably good enough.
    (clrhash ement-users))
  (message "Ement: Disconnected (%s)"
           (string-join (cl-loop for session in sessions
                                 collect (ement-user-id (ement-session-user session)))
                        ", ")))

(defun ement--login-callback (session data)
  "Record DATA from logging in to SESSION and do initial sync."
  (pcase-let* (((cl-struct ement-session (user (cl-struct ement-user (id user-id)))) session)
               ((map ('access_token token) ('device_id device-id)) data))
    (setf (ement-session-token session) token
          (ement-session-device-id session) device-id
          (alist-get user-id ement-sessions nil nil #'equal) session)
    (ement--sync session :timeout ement-initial-sync-timeout)))

;; FIXME: Make a room-buffer-name function or something.
(defvar ement-room-buffer-name-prefix)
(defvar ement-room-buffer-name-suffix)
(defun ement-view-room (room session)
  "Switch to a buffer showing ROOM on SESSION.
Calls `pop-to-buffer-same-window'.  Interactively, with prefix,
call `pop-to-buffer'."
  (interactive (ement-complete-room :session (ement-complete-session) :suggest nil))
  (pcase-let* (((cl-struct ement-room (local (map buffer))) room))
    (unless (buffer-live-p buffer)
      (setf buffer (ement-room--buffer session room
                                       (ement--room-buffer-name room))
            (alist-get 'buffer (ement-room-local room))  buffer))
    ;; FIXME: There must be a better way to handle this.
    (funcall (if current-prefix-arg
                 #'pop-to-buffer #'pop-to-buffer-same-window)
             buffer)))

(cl-defun ement-upload (session &key data filename then else
                                (content-type "application/octet-stream"))
  "Upload DATA with FILENAME to content repository on SESSION.
THEN and ELSE are passed to `ement-api', which see."
  (declare (indent defun))
  (ement-api session "upload" :method 'post :endpoint-category "media"
    ;; NOTE: Element currently uses "r0" not "v3", so so do we.
    :params (when filename
              (list (list "filename" filename)))
    :content-type content-type :data data :data-type 'binary
    :then then :else else))

(defun ement-complete-user-id ()
  "Return a user-id selected with completion.
Selects from seen users on all sessions.  If point is on an
event, suggests the event's sender as initial input.  Allows
unseen user IDs to be input as well."
  (cl-labels ((format-user (user)
                           (format "%s <%s>"
                                   (string-join
                                    (delete-dups
                                     (map-values
                                      (ement-user-room-display-names user)))
				    ", ")
				   (ement-user-id user))))
    (let* ((display-to-id
	    (cl-loop for key being the hash-keys of ement-users
		     using (hash-values value)
		     collect (cons (format-user value) key)))
           (user-at-point (when (equal major-mode 'ement-room-mode)
                            (when-let ((node (ewoc-locate ement-ewoc)))
                              (when (ement-event-p (ewoc-data node))
                                (format-user (ement-event-sender (ewoc-data node)))))))
	   (selected-user (completing-read "User: " (mapcar #'car display-to-id)
                                           nil nil user-at-point)))
      (or (alist-get selected-user display-to-id nil nil #'equal)
	  selected-user))))

(defun ement-send-direct-message (session user-id message)
  "Send a direct MESSAGE to USER-ID on SESSION.
Uses the latest existing direct room with the user, or creates a
new one automatically if necessary."
  ;; SPEC: 13.23.2.
  (interactive
   (let* ((session (ement-complete-session))
	  (user-id (ement-complete-user-id))
	  (message (read-string "Message: ")))
     (list session user-id message)))
  (if-let* ((seen-user (gethash user-id ement-users))
	    (existing-direct-room (ement--direct-room-for-user seen-user session)))
      (progn
        (ement-room-send-message existing-direct-room session :body message)
        (message "Message sent to %s <%s> in room %S <%s>."
                 (ement-room--user-display-name seen-user existing-direct-room)
                 user-id
                 (ement-room-display-name existing-direct-room) (ement-room-id existing-direct-room)))
    ;; No existing room for user: make new one.
    (message "Creating new room for user %s..." user-id)
    (ement-create-room session :direct-p t :invite (list user-id)
      :then (lambda (data)
              (let* ((room-id (alist-get 'room_id data))
	             (room (or (cl-find room-id (ement-session-rooms session)
                                        :key #'ement-room-id)
		               ;; New room hasn't synced yet: make a temporary struct.
		               (make-ement-room :id room-id)))
                     (direct-rooms-account-data-event-content
                      ;; FIXME: Make account-data a map.
                      (alist-get 'content (cl-find-if (lambda (event)
                                                        (equal "m.direct" (alist-get 'type event)))
                                                      (ement-session-account-data session)))))
                ;; Mark new room as direct: add the room to the account-data event, then
                ;; put the new account data to the server.  (See also:
                ;; <https://github.com/matrix-org/matrix-react-sdk/blob/919aab053e5b3bdb5a150fd90855ad406c19e4ab/src/Rooms.ts#L91>).
                (setf (map-elt direct-rooms-account-data-event-content user-id) (vector room-id))
                (ement-put-account-data session "m.direct" direct-rooms-account-data-event-content)
                ;; Send message to new room.
                (ement-room-send-message room session :body message)
                (message "Room \"%s\" created for user %s.  Sending message..."
	                 room-id user-id))))))

(cl-defun ement-create-room
    (session &key name alias topic invite direct-p
             (then (lambda (data)
                     (message "Created new room: %s" (alist-get 'room_id data))))
             (visibility 'private))
  "Create new room on SESSION with given arguments."
  ;; TODO: Document other arguments.
  ;; SPEC: 10.1.1.
  (declare (indent defun))
  (interactive (let ((session (ement-complete-session)))
                 (list session
                       :name (read-string "New room name: ")
                       :alias (read-string (concat "New room alias (\"foo\" for \"#foo:"
                                                   (replace-regexp-in-string "\\<https?://\\|/$" ""
                                                                             (ement-server-uri-prefix (ement-session-server session)))
                                                   "\"): "))
                       :topic (read-string "New room topic: ")
                       :visibility (completing-read "New room type: " '(private public)))))
  (cl-labels ((given-p
	       (var) (and var (not (string-empty-p var)))))
    (pcase-let* ((endpoint "createRoom")
		 (data (ement-aprog1
			   (ement-alist "visibility" visibility)
			 (when (given-p alias)
			   (push (cons "room_alias_name" alias) it))
			 (when (given-p name)
			   (push (cons "name" name) it))
			 (when (given-p topic)
			   (push (cons "topic" topic) it))
			 (when invite
			   (push (cons "invite" invite) it))
			 (when direct-p
			   (push (cons "is_direct" t) it)))))
      (ement-api session endpoint :method 'post :data (json-encode data)
        :then then))))

(defalias 'ement-room-forget #'ement-forget-room)
(defun ement-forget-room (room session)
  "Forget ROOM on SESSION."
  (interactive (ement-complete-room))
  (pcase-let* (((cl-struct ement-room id display-name) room)
               (endpoint (format "rooms/%s/forget" (url-hexify-string id))))
    (when (yes-or-no-p (format "Forget room \"%s\" (%s)? " display-name id))
      (ement-api session endpoint :method 'post :data ""
        :then (lambda (_data)
                ;; NOTE: The spec does not seem to indicate that the action of forgetting
                ;; a room is synced to other clients, so it seems that we need to remove
                ;; the room from the session here.
                (setf (ement-session-rooms session)
                      (cl-remove room (ement-session-rooms session)))
                ;; TODO: Indicate forgotten in footer in room buffer.
                (message "Room \"%s\" (%s) forgotten." display-name id))))))

(defun ement-invite (user-id room session)
  "Invite USER-ID to ROOM on SESSION."
  ;; SPEC: 10.4.2.1.
  (interactive
   (let* ((session (ement-complete-session))
          (user-id (ement-complete-user-id))
          (room (car (ement-complete-room :session session))))
     (list user-id room session)))
  (pcase-let* ((endpoint (format "rooms/%s/invite"
                                 (url-hexify-string (ement-room-id room))))
               (data (ement-alist "user_id" user-id) ))
    (ement-api session endpoint :method 'post :data (json-encode data)
      ;; TODO: Handle error codes.
      :then (lambda (_data)
              (message "User %s invited to room \"%s\" (%s)" user-id
                       (ement-room-display-name room)
                       (ement-room-id room))))))

(defun ement-redact (event room session &optional reason)
  "Redact EVENT in ROOM on SESSION, optionally for REASON."
  (pcase-let* (((cl-struct ement-event (id event-id)) event)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "rooms/%s/redact/%s/%s"
                                 room-id event-id (ement-room-update-transaction-id session)))
               (content (ement-alist "reason" reason)))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (lambda (_data)
              (message "Event %s redacted." event-id)))))

(defun ement-tag-room (tag room session &optional delete)
  "Add TAG to ROOM on SESSION.
If DELETE (interactively, with prefix), delete it."
  (interactive
   (pcase-let* ((`(,room ,session) (ement-complete-room))
                (prompt (if current-prefix-arg "Delete tag: " "Add tag: "))
                (default-tags (ement-alist "Favourite" "m.favourite"
                                           "Low-priority" "m.lowpriority"))
                (input (completing-read prompt default-tags))
                (tag (alist-get input default-tags (concat "u." input) nil #'string=)))
     (list tag room session current-prefix-arg)))
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-user (id user-id)) user)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "user/%s/rooms/%s/tags/%s"
                                 (url-hexify-string user-id) (url-hexify-string room-id) (url-hexify-string tag)))
               (method (if delete 'delete 'put)))
    ;; TODO: "order".
    (ement-api session endpoint :version "v3" :method method :data (unless delete "{}")
      :then (lambda (data)
              (ement-debug "Changed tag on room" method tag data room)))))

;;;; Functions

(defalias 'ement--button-buttonize
  (if (version< emacs-version "28.1")
      (lambda (string callback &optional data)
        "Make STRING into a button and return it.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument."
        (propertize string
                    'face 'button
                    'button t
                    'follow-link t
                    'category t
                    'button-data data
                    'keymap button-map
                    'action callback))
    #'button-buttonize))

(defun ement-view-initial-rooms (session)
  "View rooms for SESSION configured in `ement-auto-view-rooms'."
  (when-let (rooms (alist-get (ement-user-id (ement-session-user session))
			      ement-auto-view-rooms nil nil #'equal))
    (dolist (alias/id rooms)
      (when-let (room (cl-find-if (lambda (room)
				    (or (equal alias/id (ement-room-canonical-alias room))
					(equal alias/id (ement-room-id room))))
				  (ement-session-rooms session)))
        (ement-view-room room session)))))

(defun ement--initial-transaction-id ()
  "Return an initial  transaction ID for a new session."
  ;; We generate a somewhat-random initial transaction ID to avoid potential conflicts in
  ;; case, e.g. using Pantalaimon causes a transaction ID conflict.  See
  ;; <https://github.com/alphapapa/ement.el/issues/36>.
  (cl-parse-integer
   (secure-hash 'sha256 (prin1-to-string (list (current-time) (system-name))))
   :end 8 :radix 16))

(defsubst ement--sync-messages-p (session)
  "Return non-nil if sync-related messages should be shown for SESSION."
  ;; For now, this seems like the best way.
  (or (not (ement-session-has-synced-p session))
      (not ement-auto-sync)))

(defun ement--hostname-uri (hostname)
  "Return the \".well-known\" URI for server HOSTNAME.
If no URI is found, prompt the user for the hostname."
  ;; FIXME: When fail-prompting, a URI should be returned, not just a hostname.
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
    (condition-case err
        (let ((response (plz 'get (concat "https://" hostname "/.well-known/matrix/client")
                          :as 'response :then 'sync)))
          (if (plz-response-p response)
              (pcase (plz-response-status response)
                (200 (parse (plz-response-body response)))
                (404 (fail-prompt))
                (_ (warn "Ement: `plz' request for .well-known URI returned unexpected code: %s"
                         (plz-response-status response))
                   (fail-prompt)))
            (warn "Ement: `plz' request for .well-known URI did not return a `plz' response")
            (fail-prompt)))
      (error (warn "Ement: `plz' request for .well-known URI signaled an error: %S" err)
             (fail-prompt)))))

(defun ement--room-buffer-name (room)
  "Return name for ROOM's buffer."
  (concat ement-room-buffer-name-prefix
          (or (ement-room-display-name room)
              (setf (ement-room-display-name room)
                    (ement-room--room-display-name room)))
          ement-room-buffer-name-suffix))

(defun ement-complete-session ()
  "Return an Ement session selected with completion."
  (cl-etypecase (length ement-sessions)
    ((integer 1 1) (cdar ement-sessions))
    ((integer 2 *) (let* ((ids (mapcar #'car ement-sessions))
                          (selected-id (completing-read "Session: " ids nil t)))
                     (alist-get selected-id ement-sessions nil nil #'equal)))
    (otherwise (user-error "No active sessions.  Call `ement-connect' to log in"))))

(cl-defun ement-complete-room (&key session predicate
                                    (prompt "Room: ") (suggest t))
  "Return a (room session) list selected from SESSION with completion.
If SESSION is nil, select from rooms in all of `ement-sessions'.
When SUGGEST, suggest current buffer's room (or a room at point
in a room list buffer) as initial input (i.e. it should be set to
nil when switching from one room buffer to another).  PROMPT may
override the default prompt.  PREDICATE may be a function to
select which rooms are offered; it is also applied to the
suggested room."
  (declare (indent defun))
  (pcase-let* ((sessions (if session
                             (list session)
                           (mapcar #'cdr ement-sessions)))
               (name-to-room-session
                (cl-loop for session in sessions
                         append (cl-loop for room in (ement-session-rooms session)
                                         collect (cons (ement--format-room room)
                                                       (list room session)))))
               (names (mapcar #'car name-to-room-session))
               (selected-name (completing-read
                               prompt names nil t
                               (when suggest
                                 (when-let ((suggestion (ement--room-at-point)))
                                   (when (or (not predicate)
                                             (funcall predicate suggestion))
                                     suggestion))))))
    (alist-get selected-name name-to-room-session nil nil #'string=)))

(defun ement--room-at-point ()
  "Return room at point.
Works in major-modes `ement-room-mode', `ement-room-list-mode',
and `ement-taxy-mode'."
  (pcase major-mode
    ('ement-room-mode (ement--format-room ement-room))
    ('ement-room-list-mode (ement--format-room (tabulated-list-get-id)))
    ('ement-taxy-mode
     (cl-typecase (oref (magit-current-section) value)
       (taxy-magit-section nil)
       (t (pcase (oref (magit-current-section) value)
            (`[,room ,_session] (ement--format-room room))))))))

(defun ement--format-room (room)
  "Return ROOM formatted with name, alias, ID, and topic.
Suitable for use in completion, etc."
  (format "%s <%s> (<%s>): \"%s\""
          (or (ement-room-display-name room)
              (setf (ement-room-display-name room)
                    (ement-room--room-display-name room)))
          (ement-room-canonical-alias room)
          (ement-room-id room)
          (ement-room-topic room)))

(cl-defun ement--sync (session &key force quiet
                               (timeout 40) ;; Give the server an extra 10 seconds.
                               (filter ement-default-sync-filter))
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used.  If FORCE, first
delete any outstanding sync processes.  If QUIET, don't show a
message about syncing this time.

FILTER may be an alist representing a raw event filter (i.e. not
a filter ID).  When unspecified, the value of
`ement-default-sync-filter' is used.  The filter is encoded with
`json-encode'.  To use no filter, specify FILTER as nil."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Use a filter ID for default filter.
  ;; TODO: Optionally, automatically sync again when HTTP request fails.
  (when (map-elt ement-syncs session)
    (if force
        (condition-case err
            (delete-process (map-elt ement-syncs session))
          ;; Ensure the only error is the expected one from deleting the process.
          (ement-api-error (cl-assert (equal "curl process killed" (plz-error-message (cl-third err))))
                           (message "Ement: Forcing new sync")))
      (user-error "Ement: Already syncing this session")))
  (pcase-let* (((cl-struct ement-session next-batch) session)
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
               (process (ement-api session "sync" :params params
                          :timeout timeout
                          :then (apply-partially #'ement--sync-callback session)
                          :else (lambda (plz-error)
                                  (setf (map-elt ement-syncs session) nil)
                                  (pcase (plz-error-curl-error plz-error)
                                    (`(,(or 28 429) . ,_)
                                     ;; Timeout or "Too Many Requests": sync again if enabled.
                                     (if (not ement-auto-sync)
                                         (error (substitute-command-keys
                                                 "\\<ement-room-mode-map>Ement sync timed out (%s).  Press \\[ement-room-sync] in a room buffer to sync again")
                                                (ement-user-id (ement-session-user session)))
                                       (message "Ement: Sync timed out (%s).  Syncing again..." (ement-user-id (ement-session-user session)))
                                       ;; Set QUIET to allow the just-printed message to remain visible.
                                       (ement--sync session :quiet t)))
                                    (`(,code . ,message)
                                     (signal 'ement-api-error (list (format "Ement: Network error: %s: %s" code message) plz-error)))
                                    (_ (signal 'ement-api-error (list "Ement: Unrecognized network error" plz-error)))))
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
      (when (and (not quiet) (ement--sync-messages-p session))
        (message "Ement: Sync request sent, waiting for response...")))))

(defun ement--sync-callback (session data)
  "Process sync DATA for SESSION.
Runs `ement-sync-callback-hook' with SESSION."
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt ement-syncs session) nil)
  (pcase-let* (((map rooms ('next_batch next-batch) ('account_data (map ('events account-data-events))))
                data)
               ((map ('join joined-rooms) ('invite invited-rooms) ('leave left-rooms)) rooms)
               (num-events (+
                            ;; HACK: In `ement--push-joined-room-events', we do something
                            ;; with each event 3 times, so we multiply this by 3.
                            ;; FIXME: That calculation doesn't seem to be quite right, because
                            ;; the progress reporter never seems to hit 100% before it's done.
                            (* 3 (cl-loop for (_id . room) in joined-rooms
                                          sum (length (map-nested-elt room '(state events)))
                                          sum (length (map-nested-elt room '(timeline events)))))
                            (cl-loop for (_id . room) in invited-rooms
                                     sum (length (map-nested-elt room '(invite_state events)))))))
    ;; Append account data events.
    ;; TODO: Since only one event of each type is allowed in account data (the spec
    ;; doesn't seem to make this clear, but see
    ;; <https://github.com/matrix-org/matrix-js-sdk/blob/d0b964837f2820940bd93e718a2450b5f528bffc/src/store/memory.ts#L292>),
    ;; we should store account-data events in a hash table or alist rather than just a
    ;; list of events.
    (cl-callf2 append (cl-coerce account-data-events 'list) (ement-session-account-data session))
    ;; Process invited and joined rooms.
    (ement-with-progress-reporter (:when (ement--sync-messages-p session)
                                         :reporter ("Ement: Reading events..." 0 num-events))
      ;; Left rooms.
      (mapc (apply-partially #'ement--push-left-room-events session) left-rooms)
      ;; Invited rooms.
      (mapc (apply-partially #'ement--push-invite-room-events session) invited-rooms)
      ;; Joined rooms.
      (mapc (apply-partially #'ement--push-joined-room-events session) joined-rooms))
    ;; TODO: Process "left" rooms (remove room structs, etc).
    ;; NOTE: We update the next-batch token before updating any room buffers.  This means
    ;; that any errors in updating room buffers (like for unexpected event formats that
    ;; expose a bug) could cause events to not appear in the buffer, but the user could
    ;; still dismiss the error and start syncing again, and the client could remain
    ;; usable.  Updating the token after doing everything would be preferable in some
    ;; ways, but it would mean that an event that exposes a bug would be processed again
    ;; on every sync, causing the same error each time.  It would seem preferable to
    ;; maintain at least some usability rather than to keep repeating a broken behavior.
    (setf (ement-session-next-batch session) next-batch)
    ;; Run hooks which update buffers, etc.
    (run-hook-with-args 'ement-sync-callback-hook session)
    ;; Show sync message if appropriate, and run after-initial-sync-hook.
    (when (ement--sync-messages-p session)
      (message (concat "Ement: Sync done."
                       (unless (ement-session-has-synced-p session)
                         (run-hook-with-args 'ement-after-initial-sync-hook session)
                         ;; Show tip after initial sync.
                         (setf (ement-session-has-synced-p session) t)
                         "  Use commands `ement-list-rooms' or `ement-view-room' to view a room."))))))

(defun ement--push-invite-room-events (session invited-room)
  "Push events for INVITED-ROOM into that room in SESSION."
  ;; TODO: Make ement-session-rooms a hash-table.
  (ement--push-joined-room-events session invited-room 'invite))

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
                           for buffer = (map-elt (ement-room-local room) 'buffer)
                           when (buffer-live-p buffer)
                           collect buffer)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (save-window-excursion
          ;; NOTE: When the buffer has a window, it must be the selected one
          ;; while calling event-insertion functions.  I don't know if this is
          ;; due to a bug in EWOC or if I just misunderstand something, but
          ;; without doing this, events may be inserted at the wrong place.
          (when-let ((buffer-window (get-buffer-window buffer)))
            (select-window buffer-window))
          (cl-assert ement-room)
          (when (ement-room-ephemeral ement-room)
            ;; Ephemeral events.
            (ement-room--process-events (ement-room-ephemeral ement-room))
            (setf (ement-room-ephemeral ement-room) nil))
          (when-let ((new-events (alist-get 'new-events (ement-room-local ement-room))))
            ;; HACK: Process these events in reverse order, so that later events (like reactions)
            ;; which refer to earlier events can find them.  (Not sure if still necessary.)
            (ement-room--process-events (reverse new-events))
            (setf (alist-get 'new-events (ement-room-local ement-room)) nil))
          (when-let ((new-events (alist-get 'new-account-data-events (ement-room-local ement-room))))
            ;; Account data events.  Do this last so, e.g. read markers can refer to message events we've seen.
            (ement-room--process-events new-events)
            (setf (alist-get 'new-account-data-events (ement-room-local ement-room)) nil)))))))

(cl-defun ement--push-joined-room-events (session joined-room &optional (status 'join))
  "Push events for JOINED-ROOM into that room in SESSION.
Also used for left rooms, in which case STATUS should be set to
`leave'."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id)) ; Really important that the ID is a STRING!
               ;; TODO: Make ement-session-rooms a hash-table.
               (room (or (cl-find-if (lambda (room)
                                       (equal id (ement-room-id room)))
                                     (ement-session-rooms session))
                         (car (push (make-ement-room :id id) (ement-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('invite_state (map ('events invite-state-events)))
                     ('account_data (map ('events account-data-events)))
                     ('unread_notifications unread-notifications))
                event-types)
               (latest-timestamp))
    (setf (ement-room-status room) status
          (ement-room-unread-notifications room) unread-notifications)
    ;; NOTE: The idea is that, assuming that events in the sync reponse are in
    ;; chronological order, we push them to the lists in the room slots in that order,
    ;; leaving the head of each list as the most recent event of that type.  That means
    ;; that, e.g. the room state events may be searched in order to find, e.g. the most
    ;; recent room name event.  However, chronological order is not guaranteed, e.g. after
    ;; loading older messages (the "retro" function; this behavior is in development).

    ;; MAYBE: Use queue.el to store the events in a DLL, so they could
    ;; be accessed from either end.  Could be useful.

    ;; Push the StrippedState events to the room's invite-state.  (These events have no
    ;; timestamp data.)  We also run the event hook, because for invited rooms, the
    ;; invite-state events include room name, topic, etc.
    (cl-loop for event across-ref invite-state-events do
             (setf event (ement--make-event event))
             (push event (ement-room-invite-state room))
             (run-hook-with-args 'ement-event-hook event room session))

    ;; Save room summary.
    (dolist (parameter '(m.heroes m.joined_member_count m.invited_member_count))
      (when (alist-get parameter summary)
        ;; These fields are only included when they change.
        (setf (alist-get parameter (ement-room-summary room)) (alist-get parameter summary))))

    ;; Update account data.  According to the spec, only one of each event type is
    ;; supposed to be present in a room's account data, so we store them as an alist keyed
    ;; on their type.  (NOTE: We don't currently make them into event structs, but maybe
    ;; we should in the future.)
    (cl-loop for event across account-data-events
             for type = (alist-get 'type event)
             do (setf (alist-get type (ement-room-account-data room) nil nil #'equal) event))
    ;; But we also need to track just the new events so we can process those in a room
    ;; buffer (and for some reason, we do make them into structs here, but I don't
    ;; remember why).  FIXME: Unify this.
    (cl-callf2 append (mapcar #'ement--make-event account-data-events)
               (alist-get 'new-account-data-events (ement-room-local room)))

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
                                 (ement-progress-update))
                               (when (> (ement-event-origin-server-ts event) ts)
                                 (setf ts (ement-event-origin-server-ts event))))
                      ;; One would think that one should use `maximizing' here, but, completely
                      ;; inexplicably, it sometimes returns nil, even when every single value it's comparing
                      ;; is a number.  It's absolutely bizarre, but I have to do the equivalent manually.
                      ts)))
      ;; FIXME: This is a bit convoluted and hacky now.  Refactor it.
      (setf latest-timestamp
            (max (push-events state ement-room-state)
                 (push-events timeline ement-room-timeline)))
      ;; NOTE: We also append the new events to the new-events list in the room's local
      ;; slot, which is used by `ement--update-room-buffers' to insert only new events.
      ;; FIXME: Does this also need to be done for invite-state events?
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
             do (run-hook-with-args 'ement-event-hook event room session)
             (when (ement--sync-messages-p session)
               (ement-progress-update)))
    (cl-loop for event across (alist-get 'events timeline)
             do (run-hook-with-args 'ement-event-hook event room session)
             (when (ement--sync-messages-p session)
               (ement-progress-update)))
    ;; Ephemeral events (do this after state and timeline hooks, so those events will be
    ;; in the hash tables).
    (cl-loop for event across (alist-get 'events ephemeral)
             for event-struct = (ement--make-event event)
             do (ement--process-event event-struct room session))
    (when (ement-session-has-synced-p session)
      ;; NOTE: We don't fill gaps in "limited" requests on initial
      ;; sync, only in subsequent syncs, e.g. after the system has
      ;; slept and awakened.
      ;; NOTE: When not limited, the read value is `:json-false', so
      ;; we must explicitly compare to t.
      (when (eq t (alist-get 'limited timeline))
	;; Timeline was limited: start filling gap.  We start the
	;; gap-filling, retrieving up to the session's current
	;; next-batch token (this function is not called when retrieving
	;; older messages, so the session's next-batch token is only
	;; evaluated once, when this chain begins, and then that token
	;; is passed to repeated calls to `ement-room-retro-to-token'
	;; until the gap is filled).
	(ement-room-retro-to-token room session (alist-get 'prev_batch timeline)
				   (ement-session-next-batch session))))))

(defun ement--push-left-room-events (session left-room)
  "Push events for LEFT-ROOM into that room in SESSION."
  (ement--push-joined-room-events session left-room 'leave))

(defun ement--make-event (event)
  "Return `ement-event' struct for raw EVENT list.
Adds sender to `ement-users' when necessary."
  (pcase-let* (((map content type unsigned redacts
                     ('event_id id) ('origin_server_ts ts)
                     ('sender sender-id) ('state_key state-key))
                event)
               (sender (or (gethash sender-id ement-users)
                           (puthash sender-id (make-ement-user
                                               :id sender-id :room-display-names (make-hash-table))
                                    ement-users))))
    ;; MAYBE: Handle other keys in the event, such as "room_id" in "invite" events.
    (make-ement-event :id id :sender sender :type type :content content :state-key state-key
                      :origin-server-ts ts :unsigned unsigned
                      ;; Since very few events will be redactions and have this key, we
                      ;; record it in the local slot alist rather than as another slot on
                      ;; the struct.
                      :local (when redacts
                               (ement-alist 'redacts redacts)))))

(defun ement--put-event (event _room session)
  "Put EVENT on SESSION's events table."
  (puthash (ement-event-id event) event (ement-session-events session)))

(defun ement--events-equal-p (a b)
  "Return non-nil if events A and B are essentially equal.
That is, A and B are either the same event (having the same event
ID), or one event replaces the other (in their m.relates_to and
m.replace metadata)."
  (or (equal (ement-event-id a) (ement-event-id b))
      (ement--event-replaces-p a b)
      (ement--event-replaces-p b a)))

(defun ement--event-replaces-p (a b)
  "Return non-nil if event A replaces event B.
That is, event A replaces B in their m.relates_to/m.relations and
m.replace metadata."
  (pcase-let* (((cl-struct ement-event (id a-id)
                           (content (map ('m.relates_to
                                          (map ('rel_type a-rel-type)
                                               ('event_id a-replaces-event-id))))))
                a)
               ((cl-struct ement-event (id b-id)
                           ;; Not sure why this ends up in the unsigned key, but it does.
                           (unsigned (map ('m.relations
                                           (map ('m.replace
                                                 (map ('event_id b-replaced-by-event-id))))))))
                b))
    (or (and (equal "m.replace" a-rel-type)
             (equal a-replaces-event-id b-id))
        (equal a-id b-replaced-by-event-id))))

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

(defun ement--remove-face-property (string value)
  "Remove VALUE from STRING's `face' properties.
Used to remove the `button' face from buttons, because that face
can cause undesirable underlining."
  (let ((pos 0))
    (cl-loop for next-face-change-pos = (next-single-property-change pos 'face string)
             for face-at = (get-text-property pos 'face string)
             when face-at
             do (put-text-property pos (or next-face-change-pos (length string))
                                   'face (cl-typecase face-at
                                           (atom (if (equal value face-at)
                                                     nil face-at))
                                           (list (remove value face-at)))
                                   string)
             while next-face-change-pos
             do (setf pos next-face-change-pos))))

(defun ement--resize-image (image max-width max-height)
  "Return a copy of IMAGE set to MAX-WIDTH and MAX-HEIGHT.
IMAGE should be one as created by, e.g. `create-image'."
  ;; It would be nice if the image library had some simple functions to do this sort of thing.
  (let ((new-image (cl-copy-list image)))
    (when (fboundp 'imagemagick-types)
      ;; Only do this when ImageMagick is supported.
      ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
      (setf (image-property new-image :type) 'imagemagick))
    (setf (image-property new-image :max-width) max-width
          (image-property new-image :max-height) max-height)
    new-image))

(defun ement--direct-room-for-user (user session)
  "Return last-modified direct room with USER on SESSION, if one exists."
  ;; Loosely modeled on the Element function findDMForUser in createRoom.ts.
  (cl-labels ((membership-event-for-p
               (event user) (and (equal "m.room.member" (ement-event-type event))
                                 (equal (ement-user-id user) (ement-event-state-key event))))
              (latest-membership-for
               (user room)
               (when-let ((latest-membership-event
                           (car
                            (cl-sort
                             ;; I guess we need to check both state and timeline events.
                             (append (cl-remove-if-not (lambda (event)
                                                         (membership-event-for-p event user))
                                                       (ement-room-state room))
                                     (cl-remove-if-not (lambda (event)
                                                         (membership-event-for-p event user))
                                                       (ement-room-timeline room)))
                             (lambda (a b)
                               ;; Sort latest first so we can use the car.
                               (> (ement-event-origin-server-ts a)
                                  (ement-event-origin-server-ts b)))))))
                 (alist-get 'membership (ement-event-content latest-membership-event))))
              (latest-event-in
               (room) (car
                       (cl-sort
                        (append (ement-room-state room)
                                (ement-room-timeline room))
                        (lambda (a b)
                          ;; Sort latest first so we can use the car.
                          (> (ement-event-origin-server-ts a)
                             (ement-event-origin-server-ts b)))))))
    (let* ((direct-rooms (cl-remove-if-not
                          (lambda (room)
                            (ement-room--direct-p room session))
                          (ement-session-rooms session)))
           (direct-joined-rooms
            ;; Ensure that the local user is still in each room.
            (cl-remove-if-not
             (lambda (room)
               (equal "join" (latest-membership-for (ement-session-user session) room)))
             direct-rooms))
           ;; Since we don't currently keep a member list for each room, we look in the room's
           ;; join events to see if the user has joined or been invited.
           (direct-rooms-with-user
            (cl-remove-if-not
             (lambda (room)
               (member (latest-membership-for user room) '("invite" "join")))
             direct-joined-rooms)))
      (car (cl-sort direct-rooms-with-user
                    (lambda (a b)
                      (> (latest-event-in a) (latest-event-in b))))))))

(cl-defun ement-put-account-data
    (session type data &key
             (then (lambda (received-data)
                     ;; Handle echoed-back account data event (the spec does not explain this,
                     ;; but see <https://github.com/matrix-org/matrix-react-sdk/blob/675b4271e9c6e33be354a93fcd7807253bd27fcd/src/settings/handlers/AccountSettingsHandler.ts#L150>).
                     ;; FIXME: Make session account-data a map instead of a list of events.
                     (push received-data (ement-session-account-data session))

                     ;; NOTE: Commenting out this ement-debug form because a bug in Emacs
                     ;; causes this long string to be interpreted as the function's
                     ;; docstring and cause a too-long-docstring warning.

                     ;; (ement-debug "Account data put and received back on session %s:  PUT(json-encoded):%S  RECEIVED:%S"
                     ;;              (ement-user-id (ement-session-user session)) (json-encode data) received-data)
                     )))
  "Put account data of TYPE with DATA on SESSION.
Also handle the echoed-back event."
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-session (user (cl-struct ement-user (id user-id)))))
               (endpoint (format "user/%s/account_data/%s" (url-hexify-string user-id) type)))
    (ement-api session endpoint :method 'put :data (json-encode data)
      :then then)))

(defun ement-ignore-user (user-id session &optional unignore-p)
  "Ignore USER-ID on SESSION.
If UNIGNORE-P (interactively, with prefix), un-ignore USER."
  (interactive (list (ement-complete-user-id)
                     (ement-complete-session)
                     current-prefix-arg))
  (pcase-let* (((cl-struct ement-session account-data) session)
               ;; TODO: Store session account-data events in an alist keyed on type.
               ((map ('content (map ('ignored_users ignored-users))))
                (cl-find "m.ignored_user_list" account-data
                         :key (lambda (event) (alist-get 'type event)) :test #'equal)))
    (if unignore-p
        ;; Being map keys, the user IDs have been interned by `json-read'.
        (setf ignored-users (map-delete ignored-users (intern user-id)))
      ;; Empty maps are used to list ignored users.
      (setf (map-elt ignored-users user-id) nil))
    (ement-put-account-data session "m.ignored_user_list" (ement-alist "ignored_users" ignored-users)
      :then (lambda (data)
              (ement-debug "PUT successful" data)
              (message "Ement: User %s %s." user-id (if unignore-p "unignored" "ignored"))))))

(defun ement--room-unread-p (room session)
  "Return non-nil if ROOM is considered unread for SESSION.
The room is unread if it has a modified, live buffer; if it has
non-zero unread notification acounts; or if its fully-read marker
is not at the latest known message event."
  ;; Roughly equivalent to the "red/gray/bold/idle" states listed in
  ;; <https://github.com/matrix-org/matrix-react-sdk/blob/b0af163002e8252d99b6d7075c83aadd91866735/docs/room-list-store.md#list-ordering-algorithm-importance>.
  (pcase-let* (((cl-struct ement-room timeline account-data unread-notifications receipts
                           (local (map buffer)))
                room)
               ((cl-struct ement-session user) session)
               ((cl-struct ement-user (id our-id)) user)
               ((map notification_count highlight_count) unread-notifications)
               (fully-read-event-id (map-nested-elt (alist-get "m.fully_read" account-data nil nil #'equal)
                                                    '(content event_id))))
    ;; MAYBE: Ignore whether the buffer is modified.  Since we have a better handle on how
    ;; Matrix does notifications/unreads/highlights, maybe that's not needed, and it would
    ;; be more consistent to ignore it.
    (or (and buffer (buffer-modified-p buffer))
        (and unread-notifications
             (or (not (zerop notification_count))
                 (not (zerop highlight_count))))
        ;; NOTE: This is *WAY* too complicated, but it seems roughly equivalent to doesRoomHaveUnreadMessages() from
        ;; <https://github.com/matrix-org/matrix-react-sdk/blob/7fa01ffb068f014506041bce5f02df4f17305f02/src/Unread.ts#L52>.
        (when timeline
          ;; A room should rarely, if ever, have a nil timeline, but in case it does
          ;; (which apparently can happen, given user reports), it should not be
          ;; considered unread.
          (cl-labels ((event-counts-toward-unread-p
                       (event) (not (member (ement-event-type event) '("m.room.member" "m.reaction")))))
            (let ((our-read-receipt-event-id (car (gethash our-id receipts)))
                  (first-counting-event (cl-find-if #'event-counts-toward-unread-p timeline)))
              (cond ((equal fully-read-event-id (ement-event-id (car timeline)))
                     ;; The fully-read marker is at the last known event: not unread.
                     nil)
                    ((and (not our-read-receipt-event-id)
                          (when first-counting-event
                            (and (not (equal fully-read-event-id (ement-event-id first-counting-event)))
                                 (not (equal our-id (ement-user-id (ement-event-sender first-counting-event)))))))
                     ;; A missing read-receipt failsafes to marking the
                     ;; room unread, unless the fully-read marker is at
                     ;; the latest counting event or we sent the latest
                     ;; counting event.
                     t)
                    ((not (equal our-id (ement-user-id (ement-event-sender (car timeline)))))
                     ;; If we sent the last event in the room, the room is not unread.
                     nil)
                    ((and first-counting-event
                          (equal our-id (ement-user-id (ement-event-sender first-counting-event))))
                     ;; If we sent the last counting event in the room,
                     ;; the room is not unread.
                     nil)
                    ((cl-loop for event in timeline
                              when (event-counts-toward-unread-p event)
                              return (and (not (equal our-read-receipt-event-id (ement-event-id event)))
                                          (not (equal fully-read-event-id (ement-event-id event)))))
                     t))))))))

(defun ement--room-favourite-p (room)
  "Return non-nil if ROOM is tagged as favourite."
  (ement--room-tagged-p "m.favourite" room))

(defun ement--room-low-priority-p (room)
  "Return non-nil if ROOM is tagged as low-priority."
  (ement--room-tagged-p "m.lowpriority" room))

(defun ement--room-tagged-p (tag room)
  "Return non-nil if ROOM has TAG."
  ;; TODO: Use `make-ement-event' on account-data events.
  (pcase-let* (((cl-struct ement-room account-data) room)
               (tag-event (alist-get "m.tag" account-data nil nil #'equal)))
    (when tag-event
      (pcase-let (((map ('content (map tags))) tag-event))
        (cl-typecase tag
          ;; Tags are symbols internally, because `json-read' converts map keys to them.
          (string (setf tag (intern tag))))
        (assoc tag tags)))))

(defun ement--xml-escape-string (string)
  "Return STRING having been escaped with `xml-escape-string'.
Before Emacs 28, ignores `xml-invalid-character' errors (and any
invalid characters cause STRING to remain unescaped).  After
Emacs 28, uses the NOERROR argument to `xml-escape-string'."
  (condition-case _
      (xml-escape-string string 'noerror)
    (wrong-number-of-arguments
     (condition-case _
         (xml-escape-string string)
       (xml-invalid-character
        ;; We still don't want to error on this, so just return the string.
        string)))))

;;;;; Reading/writing sessions

(defun ement--read-sessions ()
  "Return saved sessions alist read from disk.
Returns nil if unable to read `ement-sessions-file'."
  (cl-labels ((plist-to-session
               (plist) (pcase-let* (((map (:user user-data) (:server server-data)
                                          (:token token) (:transaction-id transaction-id))
                                     plist)
                                    (user (apply #'make-ement-user user-data))
                                    (server (apply #'make-ement-server server-data))
                                    (session (make-ement-session :user user :server server
                                                                 :token token :transaction-id transaction-id)))
                         (setf (ement-session-events session) (make-hash-table :test #'equal)
                               (ement-user-room-display-names (ement-session-user session)) (make-hash-table))
                         session)))
    (when (file-exists-p ement-sessions-file)
      (pcase-let* ((read-circle t)
                   (sessions (with-temp-buffer
                               (insert-file-contents ement-sessions-file)
                               (read (current-buffer)))))
        (prog1
            (cl-loop for (id . plist) in sessions
                     collect (cons id (plist-to-session plist)))
          (message "Ement: Read sessions."))))))

(defun ement--write-sessions (sessions-alist)
  "Write SESSIONS-ALIST to disk."
  ;; We only record the slots we need.  We record them as a plist
  ;; so that changes to the struct definition don't matter.
  ;; NOTE: If we ever persist more session data (like room data, so we
  ;; could avoid doing an initial sync next time), we should limit the
  ;; amount of session data saved (e.g. room history could grow
  ;; forever on-disk, which probably isn't what we want).

  ;; NOTE: This writes all current sessions, even if there are multiple active ones and only one
  ;; is being disconnected.  That's probably okay, but it might be something to keep in mind.
  (cl-labels ((session-plist
               (session) (pcase-let* (((cl-struct ement-session user server token transaction-id) session)
                                      ((cl-struct ement-user (id user-id) username) user)
                                      ((cl-struct ement-server (name server-name) uri-prefix) server))
                           (list :user (list :id user-id
                                             :username username)
                                 :server (list :name server-name
                                               :uri-prefix uri-prefix)
                                 :token token
                                 :transaction-id transaction-id))))
    (message "Ement: Writing sessions...")
    (with-temp-file ement-sessions-file
      (pcase-let* ((print-level nil)
                   (print-length nil)
                   ;; Very important to use `print-circle', although it doesn't
                   ;; solve everything.  Writing/reading Lisp data can be tricky...
                   (print-circle t)
                   (sessions-alist-plist (cl-loop for (id . session) in sessions-alist
                                                  collect (cons id (session-plist session)))))
        (prin1 sessions-alist-plist (current-buffer))))
    ;; Ensure permissions are safe.
    (chmod ement-sessions-file #o600)))

(defun ement--kill-emacs-hook ()
  "Function to be added to `kill-emacs-hook'.
Writes Ement session to disk when enabled."
  (ignore-errors
    ;; To avoid interfering with Emacs' exit, We must be careful that
    ;; this function handles errors, so just ignore any.
    (when (and ement-save-sessions
               ement-sessions)
      (ement--write-sessions ement-sessions))))

;;;;; Event handlers

(defvar ement-event-handlers nil
  "Alist mapping event types to functions which process an event of each type.
Each function is called with three arguments: the event, the
room, and the session.  These handlers are run regardless of
whether a room has a live buffer.")

(defun ement--process-event (event room session)
  "Process EVENT for ROOM in SESSION.
Uses handlers defined in `ement-event-handlers'.  If no handler
is defined for EVENT's type, does nothing and returns nil.  Any
errors signaled during processing are demoted in order to prevent
unexpected errors from arresting event processing and syncing."
  (when-let ((handler (alist-get (ement-event-type event) ement-event-handlers nil nil #'equal)))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.  See,
    ;; e.g. <https://github.com/alphapapa/ement.el/pull/61>.
    (with-demoted-errors "Ement (ement--process-event): Error processing event: %S"
      (funcall handler event room session))))

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

;; I love how Lisp macros make it so easy and concise to define these
;; event handlers!

(ement-defevent "m.room.avatar"
  (when ement-room-avatars
    ;; If room avatars are disabled, we don't download avatars at all.  This
    ;; means that, if a user has them disabled and then reenables them, they will
    ;; likely need to reconnect to cause them to be displayed in most rooms.
    (if-let ((url (alist-get 'url (ement-event-content event))))
        (plz 'get (ement--mxc-to-url url session) :as 'binary :noquery t
          :then (lambda (data)
                  (when ement-room-avatars
                    ;; MAYBE: Store the raw image data instead of using create-image here.
                    (let ((image (create-image data nil 'data-p
                                               :ascent 'center
                                               :max-width ement-room-avatar-max-width
                                               :max-height ement-room-avatar-max-height)))
                      (when (fboundp 'imagemagick-types)
                        ;; Only do this when ImageMagick is supported.
                        ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                        (setf (image-property image :type) 'imagemagick))
                      ;; We set the room-avatar slot to a propertized string that displays
                      ;; as the image.  This seems the most convenient thing to do.
                      (setf (ement-room-avatar room) (propertize " " 'display image))))))
      ;; Unset avatar.
      (setf (ement-room-avatar room) nil
            (alist-get 'room-list-avatar (ement-room-local room)) nil))))

(ement-defevent "m.room.create"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map type))) event))
    (when type
      (setf (ement-room-type room) type))))

(ement-defevent "m.room.name"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map name))) event))
    (when name
      ;; Recalculate room name and cache in slot.
      (setf (ement-room-display-name room) (ement-room--room-display-name room)))))

(ement-defevent "m.room.topic"
  (ignore session)
  (pcase-let* (((cl-struct ement-event (content (map topic))) event))
    (when topic
      (setf (ement-room-topic room) topic))))

(ement-defevent "m.receipt"
  (ignore session)
  (pcase-let (((cl-struct ement-event content) event)
              ((cl-struct ement-room (receipts room-receipts)) room))
    (cl-loop for (event-id . receipts) in content
             do (cl-loop for (user-id . receipt) in (alist-get 'm.read receipts)
                         ;; Users may not have been "seen" yet, so although we'd
                         ;; prefer to key on the user struct, we key on the user ID.
                         ;; Same for events, unfortunately.
                         ;; NOTE: The JSON map keys are converted to symbols by `json-read'.
                         ;; MAYBE: (Should we keep them that way?  It would use less memory, I guess.)
                         do (puthash (symbol-name user-id)
                                     (cons (symbol-name event-id) (alist-get 'ts receipt))
                                     room-receipts)))))

(ement-defevent "m.space.child"
  ;; SPEC: v1.2/11.35.
  (pcase-let* ((space-room room)
               ((cl-struct ement-session rooms) session)
               ((cl-struct ement-room (id parent-room-id)) space-room)
               ((cl-struct ement-event (state-key child-room-id) (content (map via))) event)
               (child-room (cl-find child-room-id rooms :key #'ement-room-id :test #'equal)))
    (if via
        ;; Child being declared: add it.
        (progn
          (cl-pushnew child-room-id (alist-get 'children (ement-room-local space-room)) :test #'equal)
          (when child-room
            ;; The user is also in the child room: link the parent space-room in it.
            ;; FIXME: On initial sync, if the child room hasn't been processed yet, this will fail.
            (cl-pushnew parent-room-id (alist-get 'parents (ement-room-local child-room)) :test #'equal)))
      ;; Child being disowned: remove it.
      (setf (alist-get 'children (ement-room-local space-room))
            (delete child-room-id (alist-get 'children (ement-room-local space-room))))
      (when child-room
        ;; The user is also in the child room: unlink the parent space-room in it.
        (setf (alist-get 'parents (ement-room-local child-room))
              (delete parent-room-id (alist-get 'parents (ement-room-local child-room))))))))

(defun ement--link-children (session)
  "Link child rooms in SESSION.
To be called after initial sync."
  ;; On initial sync, when processing m.space.child events, the child rooms may not have
  ;; been processed yet, so we link them again here.
  (pcase-let (((cl-struct ement-session rooms) session))
    (dolist (room rooms)
      (pcase-let (((cl-struct ement-room (id parent-id) (local (map children))) room))
        (when children
          (dolist (child-id children)
            (when-let ((child-room (cl-find child-id rooms :key #'ement-room-id :test #'equal)))
              (cl-pushnew parent-id (alist-get 'parents (ement-room-local child-room)) :test #'equal))))))))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
