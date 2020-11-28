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

;;

;;; Code:

;;;; Requirements

;; Built in.
(require 'cl-lib)

;; Third-party.

;; This package.
(require 'ement-api)
(require 'ement-macros)

;;;; Debugging

(eval-and-compile
  (setq-local warning-minimum-log-level nil)
  (setq-local warning-minimum-log-level :debug))

(cl-defmacro debug-warn (&rest args)
  "Display a debug warning showing the runtime value of ARGS.
The warning automatically includes the name of the containing
function, and it is only displayed if `warning-minimum-log-level'
is `:debug' at expansion time (otherwise the macro expands to nil
and is eliminated by the byte-compiler).  When debugging, the
form also returns nil so, e.g. it may be used in a conditional in
place of nil.

Each of ARGS may be a string, which is displayed as-is, or a
symbol, the value of which is displayed prefixed by its name, or
a Lisp form, which is displayed prefixed by its first symbol.

Before the actual ARGS arguments, you can write keyword
arguments, i.e. alternating keywords and values.  The following
keywords are supported:

  :buffer BUFFER   Name of buffer to pass to `display-warning'.
  :level  LEVEL    Level passed to `display-warning', which see.
                   Default is :debug."
  (pcase-let* ((fn-name (with-current-buffer
                            (or byte-compile-current-buffer (current-buffer))
                          ;; This is a hack, but a nifty one.
                          (save-excursion
                            (beginning-of-defun)
                            (cl-second (read (current-buffer))))))
               (plist-args (cl-loop while (keywordp (car args))
                                    collect (pop args)
                                    collect (pop args)))
               ((map (:buffer buffer) (:level level)) plist-args)
               (level (or level :debug))
               (string (cl-loop for arg in args
                                concat (pcase arg
                                         ((pred stringp) "%S ")
                                         ((pred symbolp)
                                          (concat (upcase (symbol-name arg)) ":%S "))
                                         ((pred listp)
                                          (concat "(" (upcase (symbol-name (car arg)))
                                                  (pcase (length arg)
                                                    (1 ")")
                                                    (_ "...)"))
                                                  ":%S "))))))
    (when (eq :debug warning-minimum-log-level)
      `(progn
         (display-warning ',fn-name (format ,string ,@args) ,level ,buffer)
         nil))))

;;;; Structs

(cl-defstruct ement-user
  id displayname)

(cl-defstruct ement-server
  hostname port)

(cl-defstruct ement-session
  user server token transaction-id rooms)

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

(defcustom ement-save-token-file "~/.cache/matrix-client.el.token"
  ;; FIXME: Uses matrix-client.el token.
  "Save username and access token to this file."
  :type 'file)

;;;; Commands

(defun ement-connect (user-id _password hostname token)
  ;; FIXME: Use password if given.
  "Connect to Matrix and sync once."
  (interactive (list (read-string "User ID: " (or (when (car ement-sessions)
                                                    (ement-session-user (car ement-sessions)))
                                                  ""))
                     (read-passwd "Password: ")
                     (read-string "Hostname (default: from user ID): ")
                     (ement--load-token)))
  ;; FIXME: Overwrites any current session.
  (pcase-let* ((hostname (if (not (string-empty-p hostname))
                             hostname
                           (or (awhen (string-match (rx ":" (group (1+ anything))) user-id)
                                 (match-string 1 user-id))
                               "matrix.org")))
               ;; FIXME: Lookup hostname from user ID with DNS.
               ;; FIXME: Dynamic port.
               (server (make-ement-server :hostname hostname :port 443))
               (user (make-ement-user :id user-id))
               (transaction-id (random 100000))
               (session (make-ement-session :user user :server server :token token :transaction-id transaction-id)))
    (setf ement-sessions (list session)))
  (debug-warn (car ement-sessions))
  (ement--sync (car ement-sessions)))

(defun ement-view-room (room)
  "Switch to a buffer for ROOM."
  (interactive (list (ement-complete-room (car ement-sessions)))))

;;;; Functions

(defun ement-complete-room (session)
  "Return a room selected from SESSION with completion.")

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
  (debug-warn session data))

(defun ement--load-token ()
  "Return saved username and access token from file."
  (when (file-exists-p ement-save-token-file)
    (read (with-temp-buffer
            (insert-file-contents ement-save-token-file)
            (buffer-substring-no-properties (point-min) (point-max))))))

;;;; Footer

(provide 'ement)

;;; ement.el ends here
