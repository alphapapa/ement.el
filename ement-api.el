;;; ement-api.el --- Matrix API library              -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

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

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(cl-defun ement-api (hostname port token _transaction-id endpoint _data then
                              &key _timeout _raw-data
                              (content-type "application/json")
                              (else #'ement-api-error) (method 'get))
  ;; FIXME: Use transaction-id or add it in calling functions.
  ;; FIXME: Use timeout.
  (declare (indent defun))
  (pcase-let* ((path (concat "/_matrix/client/r0/" endpoint))
               (url (url-recreate-url (url-parse-make-urlobj "https" nil nil hostname port path nil nil t)))
               (headers (ement-alist "Content-Type" content-type
                                     "Authorization" (concat "Bearer " token))))
    (debug-warn (current-time) method url headers then)
    (pcase method
      ('get (plz-get url :headers headers :as #'json-read :then then :else else)))))

(defun ement-api-error (&rest args)
  (debug-warn args)
  (error "ement-api-error: %S" args))

;;;; Footer

(provide 'ement-api)

;;; ement-api.el ends here
