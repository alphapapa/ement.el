;;; ement-taxy.el --- List Ement rooms with Taxy     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

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

(require 'taxy)
(require 'taxy-magit-section)

;;;; Keys

(taxy-define-key-definer ement-taxy-define-key
  ement-taxy-keys "ement-taxy" "FIXME: Docstring.")

(ement-taxy-define-key alias (_session regexp)
  (pcase-let* (((cl-struct ement-room canonical-alias) item))
    (string-match-p regexp canonical-alias)))

(ement-taxy-define-key buffer (_session)
  (pcase-let* (((cl-struct ement-room (local (map buffer))) item))
    buffer))

(ement-taxy-define-key direct (session)
  (ement-room--direct-p item session))

(ement-taxy-define-key name (_session regexp)
  (pcase-let* (((cl-struct ement-room display-name) item))
    (string-match-p regexp display-name)))

(ement-taxy-define-key session (session &optional user-id)
  (pcase user-id
    (`nil (ement-user-id (ement-session-user session)))
    (_ (when (equal user-id (ement-user-id (ement-session-user session)))
         user-id))))

(ement-taxy-define-key unread (_session)
  (pcase-let* (((cl-struct ement-room (local (map buffer))) item))
    (when buffer
      (buffer-modified-p buffer))))

;;;; Commands

(provide 'ement-taxy)

;;; ement-taxy.el ends here
