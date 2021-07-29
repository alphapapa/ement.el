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

(cl-defun ement-api (server token endpoint then
                            &key timeout data params
                            (content-type "application/json")
                            (data-type 'text)
                            (else #'ement-api-error) (method 'get)
                            ;; FIXME: What's the right term for the URL part after "/_matrix/"?
                            (endpoint-category "client")
                            (json-read-fn #'json-read))
  "FIXME: Docstring."
  (declare (indent defun))
  (pcase-let* (((cl-struct ement-server uri-prefix port) server)
               ((cl-struct url type host) (url-generic-parse-url uri-prefix))
               (path (format "/_matrix/%s/r0/%s" endpoint-category endpoint))
	       (query (url-build-query-string params))
	       (filename (concat path "?" query))
               (url (url-recreate-url
		     (url-parse-make-urlobj type nil nil host port filename nil data t)))
               (headers (ement-alist "Content-Type" content-type
                                     "Authorization" (concat "Bearer " token))))
    ;; Omit `then' from debugging because if it's a partially applied
    ;; function on the session object, which may be very large, it
    ;; will take a very long time to print into the warnings buffer.
    ;;  (ement-debug (current-time) method url headers)
    (plz method url :headers headers :body data :body-type data-type
      :as json-read-fn :then then :else else
      ;; FIXME: Timeout is not necessarily the same as connect-timeout, or shouldn't be.
      :connect-timeout timeout :noquery t)))

(define-error 'ement-api-error "Ement API error" 'error)

(defun ement-api-error (&rest args)
  "Signal an error about ARGS."
  (signal 'ement-api-error args))

;;;; Footer

(provide 'ement-api)

;;; ement-api.el ends here
