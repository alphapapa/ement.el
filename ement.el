;;; ement.el --- Matrix client                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
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

;; Another Matrix client!

;;; Code:

;;;; Debugging

(eval-and-compile
  (setq-local warning-minimum-log-level nil)
  (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)

;; Third-party.

;; This package.
(require 'ement-api)
(require 'ement-macros)

;;;; Structs

(cl-defstruct ement-user
  id displayname account-data)

(cl-defstruct ement-server
  hostname port)

(cl-defstruct ement-session
  user server token transaction-id rooms next-batch)

(cl-defstruct ement-room
  id summary state timeline timeline* ephemeral account-data unread-notifications)

;;;; Variables

(defvar ement-sessions nil
  "List of active `ement-session' sessions.")

;;;; Customization

(defgroup ement nil
  "Options for Ement, the Matrix client."
  :group 'comm)

(defcustom ement-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom ement-save-session-file "~/.cache/matrix-client.el.token"
  ;; FIXME: Uses matrix-client.el token.
  "Save username and access token to this file."
  :type 'file)

;;;; Commands

(defun ement-connect (user-id _password hostname token &optional transaction-id)
  ;; FIXME: Use password if given.
  "Connect to Matrix and sync once."
  (interactive (pcase-let* (((map username server token ('txn-id transaction-id))
                             (ement--load-session)))
                 (list username nil server token transaction-id))
               ;; (list (read-string "User ID: " (or (when (car ement-sessions)
               ;;                                      (ement-session-user (car ement-sessions)))
               ;;                                    ""))
               ;;       (read-passwd "Password: ")
               ;;       (read-string "Hostname (default: from user ID): ")
               ;;       (alist-get 'token (ement--load-session)))
               )
  ;; FIXME: Overwrites any current session.
  (pcase-let* ((hostname (if (not (string-empty-p hostname))
                             hostname
                           (if (string-match (rx ":" (group (1+ anything))) user-id)
                               (match-string 1 user-id)
                             "matrix.org")))
               ;; FIXME: Lookup hostname from user ID with DNS.
               ;; FIXME: Dynamic port.
               (server (make-ement-server :hostname hostname :port 443))
               (user (make-ement-user :id user-id))
               (transaction-id (or transaction-id (random 100000)))
               (session (make-ement-session :user user :server server :token token :transaction-id transaction-id)))
    (setf ement-sessions (list session)))
  (debug-warn (car ement-sessions))
  (ement--sync (car ement-sessions)))

;; (defun ement-view-room (room)
;;   "Switch to a buffer for ROOM."
;;   (interactive (list (ement-complete-room (car ement-sessions)))))

;;;; Functions

(defun ement-complete-room (session)
  "Return a room selected from SESSION with completion."
  (pcase-let* (((cl-struct ement-session rooms) session)
               (name-to-room (cl-loop for room in rooms
                                      collect (cons (format "%s (%s)"
                                                            (ement--room-display-name room)
                                                            (ement--room-alias room))
                                                    room)))
               (names (mapcar #'car name-to-room))
               (selected-name (completing-read "Room: " names nil t)))
    (alist-get selected-name name-to-room nil nil #'string=)))

(cl-defun ement--sync (session &key since)
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Timeout.
  (pcase-let* (((cl-struct ement-session server token transaction-id) session)
               ((cl-struct ement-server hostname port) server)
               (data (ement-alist 'since since
                                  'full_state (not since))))
    (debug-warn session data)
    (ement-api hostname port token transaction-id
      "sync" data (apply-partially #'ement--sync-callback session))))

(defun ement--sync-callback (session data)
  "SESSION.  DATA should be the parsed JSON response."
  (pcase-let* (((map rooms) data)
               ((map ('join joined-rooms)) rooms))
    (mapc (apply-partially #'ement--push-joined-room-events session) joined-rooms)
    (message "Sync done")))

(defun ement--push-joined-room-events (session joined-room)
  (pcase-let* ((`(,id . ,event-types) joined-room)
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
             do (push event (ement-room-state room)))
    (cl-loop for event across (alist-get 'events timeline)
             do (push event (ement-room-timeline* room)))))

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
           when (equal "m.room.name" (alist-get 'type event))
           return (map-nested-elt event '(content name))))

(defun ement--room-alias (room)
  (cl-loop for event in (ement-room-state room)
           when (equal "m.room.canonical_alias" (alist-get 'type event))
           return (map-nested-elt event '(content alias))))

(defun ement--load-session ()
  "Return saved session from file."
  (when (file-exists-p ement-save-session-file)
    (read (with-temp-buffer
            (insert-file-contents ement-save-session-file)
            (buffer-substring-no-properties (point-min) (point-max))))))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
