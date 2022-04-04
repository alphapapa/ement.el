;;; ement-api.el --- Matrix API library              -*- lexical-binding: t; -*-

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

;;

;;; Code:

;;;; Debugging

(eval-and-compile
  (setq-local warning-minimum-log-level nil)
  (setq-local warning-minimum-log-level :debug))

;;;; Requirements

(require 'json)
(require 'url-parse)
(require 'url-util)

(require 'plz)

(require 'ement-macros)
(require 'ement-structs)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(cl-defun ement-api (session endpoint
                             &key then data params
                             (content-type "application/json")
                             (data-type 'text)
                             (else #'ement-api-error) (method 'get)
                             ;; FIXME: What's the right term for the URL part after "/_matrix/"?
                             (endpoint-category "client")
                             (json-read-fn #'ement-api--json-read-and-log)
                             ;; NOTE: Hard to say what the default timeouts
                             ;; should be.  Sometimes the matrix.org homeserver
                             ;; can get slow and respond a minute or two later.
                             (connect-timeout 10) (timeout 60))
  "FIXME: Docstring."
  ;; TODO: Remind users to json-encode data when needed.
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-session server token) session)
               ((cl-struct ement-server uri-prefix) server)
               ((cl-struct url type host portspec) (url-generic-parse-url uri-prefix))
               (path (format "/_matrix/%s/r0/%s" endpoint-category endpoint))
               (query (url-build-query-string params))
               (filename (concat path "?" query))
               (url (url-recreate-url
                     (url-parse-make-urlobj type nil nil host portspec filename nil data t)))
               (headers (ement-alist "Content-Type" content-type)))
    (when token
      ;; Almost every request will require a token (only a few, like checking login flows, don't),
      ;; so we simplify the API by using the token automatically when the session has one.
      (push (cons "Authorization" (concat "Bearer " token)) headers))
    ;; Omit `then' from debugging because if it's a partially applied
    ;; function on the session object, which may be very large, it
    ;; will take a very long time to print into the warnings buffer.
    ;;  (ement-debug (current-time) method url headers)
    (plz method url :headers headers :body data :body-type data-type
      :as json-read-fn :then then :else else
      :connect-timeout connect-timeout :timeout timeout :noquery t)))

(defun ement-api--json-read-and-log ()
  (let ((json-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create "*Ement API Log*")
      (save-excursion
        (goto-char (point-max))
        (let ((beg (point))
              end)
          (insert-buffer-substring json-buffer)
          (setf end (point))
          (insert "\n")
          (save-restriction
            (narrow-to-region beg end)
            (json-pretty-print-buffer))))))
  (json-read))

(define-error 'ement-api-error "Ement API error" 'error)

(defun ement-api-error (plz-error)
  "Signal an Ement API error for PLZ-ERROR."
  ;; This feels a little messy, but it seems to be reasonable.
  (pcase-let* (((cl-struct plz-error response
                           (message plz-message) (curl-error `(,curl-exit-code . ,curl-message)))
                plz-error)
               (status (when (plz-response-p response)
                         (plz-response-status response)))
               (body (when (plz-response-p response)
                       (plz-response-body response)))
               (json-object (when body
                              (ignore-errors
                                (json-read-from-string body))))
               (error-message (format "%S: %s"
                                      (or curl-exit-code status)
                                      (or (when json-object
                                            (alist-get 'error json-object))
                                          curl-message
                                          plz-message))))

    (signal 'ement-api-error (list error-message))))

;;;; Footer

(provide 'ement-api)

;;; ement-api.el ends here
