;;; ement-structs.el --- Ement structs               -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;;;; Structs

(cl-defstruct ement-user
  id displayname account-data room-display-names
  color)

(cl-defstruct ement-event
  id sender content origin-server-ts type unsigned)

(cl-defstruct ement-server
  name port uri-prefix)

(cl-defstruct ement-session
  user server token transaction-id rooms next-batch
  device-id initial-device-display-name has-synced-p)

(cl-defstruct ement-room
  id display-name prev-batch
  summary state timeline timeline* ephemeral account-data unread-notifications
  latest-ts topic canonical-alias)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions


;;;; Footer

(provide 'ement-structs)

;;; ement-structs.el ends here
