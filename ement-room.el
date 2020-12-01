;;; ement-room.el --- Ement room buffers             -*- lexical-binding: t; -*-

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

;;;; Requirements

(require 'ewoc)
(require 'subr-x)

(require 'ement-api)
(require 'ement-macros)
(require 'ement-structs)

;;;; Variables

(defvar-local ement-room-ewoc nil
  "EWOC for Ement room buffers.")

(defvar-local ement-room nil
  "Ement room for current buffer.")

(defvar-local ement-session nil
  "Ement session for current buffer.")

(defvar ement-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'ement-room-view-event)
    (define-key map (kbd "RET") #'ement-room-send-message)
    map)
  "Keymap for Ement room buffers.")

;;;; Customization

(defgroup ement-room nil
  "Options for room buffers."
  :group 'ement)

(defcustom ement-room-buffer-prefix "*Ement: "
  "Prefix for Ement room buffer names."
  :type 'string)

(defcustom ement-room-buffer-suffix "*"
  "Suffix for Ement room buffer names."
  :type 'string)

(defcustom ement-room-timestamp-format "%H:%M:%S"
  "Format string for event timestamps.
See function `format-time-string'."
  :type '(choice (const "%H:%M:%S")
                 (const "%Y-%m-%d %H:%M:%S")
                 string))

(defface ement-room-timestamp
  '((t (:inherit font-lock-variable-name-face)))
  "Event timestamps.")

(defface ement-room-user
  '((t (:inherit font-lock-keyword-face)))
  "Usernames.")

;;;; Commands

(defun ement-room-view-event (event)
  "Pop up buffer showing details of EVENT (interactively, the one at point)."
  (interactive (list (ewoc-data (ewoc-locate ement-room-ewoc))))
  (require 'pp)
  (let* ((buffer-name (format "*Ement event: %s*" (ement-event-id event)))
         (event (ement-alist :id (ement-event-id event)
                             :sender (ement-user-id (ement-event-sender event))
                             :content (ement-event-content event)
                             :origin-server-ts (ement-event-origin-server-ts event)
                             :type (ement-event-type event)
                             :unsigned (ement-event-unsigned event))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (pp event (current-buffer))
      (pop-to-buffer (current-buffer)))))

(defun ement-room-send-message ()
  "Send message in current buffer's room."
  (interactive)
  (let ((body (read-string "Send message: ")))
    (unless (string-empty-p body)
      (pcase-let* (((cl-struct ement-session server token transaction-id) ement-session)
                   ((cl-struct ement-server hostname port) server)
                   ((cl-struct ement-room id) ement-room)
                   (endpoint (format "rooms/%s/send/%s/%s" id "m.room.message"
				     (cl-incf transaction-id)))
		   (json-string (json-encode (ement-alist "msgtype" "m.text"
							  "body" body))))
        (ement-api hostname port token endpoint
          (lambda (&rest args)
            (message "SEND MESSAGE CALLBACK: %S" args))
	  :data json-string
          :method 'put)))))

;;;; Functions

(defun ement-room--buffer (session room name)
  "Return a buffer named NAME showing ROOM's events on SESSION."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (ement-room-mode)
        ;; FIXME: Move visual-line-mode to a hook.
        (visual-line-mode 1)
        (setf ement-session session
              ement-room room)
        (mapc #'ement-room--insert-event (ement-room-timeline room))
        (mapc #'ement-room--insert-event (ement-room-timeline* room))
        ;; Move new events to main list.
        (setf (ement-room-timeline room) (append (ement-room-timeline* room) (ement-room-timeline room))
              (ement-room-timeline* room) nil)
        (current-buffer))))             ; Return the buffer!

(define-derived-mode ement-room-mode fundamental-mode "Ement Room"
  "Major mode for Ement room buffers.
This mode initializes a buffer to be used for showing events in
an Ement room.  It kills all local variables, removes overlays,
and erases the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setf buffer-read-only t
        ement-room-ewoc (ewoc-create #'ement-room--pp-event "HEADER" "FOOTER")))

;;;;; EWOC

(defvar-local ement-room nil
  "The room displayed in the current buffer.")

(defun ement-room--pp-event (struct)
  "Pretty-print STRUCT.
To be used as the pretty-printer for `ewoc-create'."
  (cl-etypecase struct
    ;; FIXME: Null probably not needed anymore.
    ;;  (null (insert ""))
    (ement-event (insert "  " (ement-room--format-event struct)))
    (ement-user (insert (ement-room--format-user struct)))
    ;; FIXME: Function probably not needed anymore.
    ;; (function (insert ""))
    ))

(defface ement-room-membership
  '((t (:inherit font-lock-comment-face)))
  "Membership events (join/part).")

(defun ement-room--format-event (event)
  "Format `ement-event' EVENT."
  (pcase-let* (((cl-struct ement-event type content origin-server-ts) event)
               ((map body) content)
               (ts (/ origin-server-ts 1000)) ; Matrix timestamps are in milliseconds.
               (timestamp
                (propertize (format "[%s] " (format-time-string ement-room-timestamp-format ts))
                            'face 'ement-room-timestamp))
               (body-face (pcase type
                            ("m.room.member" 'ement-room-membership)
                            (_ 'default)))
               (string (propertize (pcase type
                                     ("m.room.message" body)
                                     ("m.room.member" (alist-get 'membership content))
                                     (_ (concat "EVENT-TYPE: " type)))
                                   'face body-face)))
    (concat timestamp string)))

(defun ement-room--format-user (user)
  "Format `ement-user' USER for current buffer's room."
  (propertize (or (gethash ement-room (ement-user-room-display-names user))
                  (puthash ement-room (ement-room--user-display-name user ement-room)
                           (ement-user-room-display-names user)))
              'face 'ement-room-user))

(defun ement-room--user-display-name (user room)
  "Return the displayname for USER in ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#calculating-the-display-name-for-a-user>.
  (if-let ((member-state-event (cl-loop for event in (ement-room-state room)
                                        when (and (equal "m.room.member" (ement-event-type event))
                                                  (equal user (ement-event-sender event)))
                                        return event)))
      (or (alist-get 'displayname (ement-event-content member-state-event))
          ;; FIXME: Add step 3 of the spec.  For now we skip to step 4.
          ;; No displayname given: use raw user ID.
          (ement-user-id user))
    ;; No membership state event: use pre-calculated displayname or ID.
    (or (ement-user-displayname user)
        (ement-user-id user))))

(defun ement-room--insert-event (event)
  "Insert EVENT into current buffer."
  (let* ((ewoc ement-room-ewoc)
         (event< (lambda (a b)
                   "Return non-nil if event A's timestamp is before B's."
                   (< (ement-event-origin-server-ts a)
                      (ement-event-origin-server-ts b))))
         (node-before (ement-room--ewoc-node-before ewoc event event< :pred #'ement-event-p))
         new-node)
    (setf new-node (if (not node-before)
                       (progn
                         (debug-warn "No event before it: add first.")
                         (if-let ((first-node (ewoc-nth ewoc 0)))
                             (progn
                               (debug-warn "EWOC not empty.")
                               (if (and (ement-user-p (ewoc-data first-node))
                                        (equal (ement-event-sender event)
                                               (ewoc-data first-node)))
                                   (progn
                                     (debug-warn "First node is header for this sender: insert after it, instead.")
                                     (setf node-before first-node)
                                     (ewoc-enter-after ewoc first-node event))
                                 (debug-warn "First node is not header for this sender: insert first.")
                                 (ewoc-enter-first ewoc event)))
                           (debug-warn "EWOC empty: add first.")
                           (ewoc-enter-first ewoc event)))
                     (debug-warn "Found event before new event: insert after it.")
                     (when-let ((next-node (ewoc-next ewoc node-before)))
                       (when (and (ement-user-p (ewoc-data next-node))
                                  (equal (ement-event-sender event)
                                         (ewoc-data next-node)))
                         (debug-warn "Next node is header for this sender: insert after it, instead.")
                         (setf node-before next-node)))
                     (ewoc-enter-after ewoc node-before event)))
    ;; Insert sender where necessary.
    (if (not node-before)
        (progn
          (debug-warn "No event before: Add sender before new node.")
          (ewoc-enter-before ewoc new-node (ement-event-sender event)))
      (debug-warn "Event before: compare sender.")
      (if (equal (ement-event-sender event)
                 (cl-typecase (ewoc-data node-before)
                   (ement-event (ement-event-sender (ewoc-data node-before)))
                   (ement-user (ewoc-data node-before))))
          (debug-warn "Same sender.")
        (debug-warn "Different sender: insert new sender node.")
        (ewoc-enter-before ewoc new-node (ement-event-sender event))
        (when-let* ((next-node (ewoc-next ewoc new-node)))
          (when (ement-event-p (ewoc-data next-node))
            (debug-warn "Event after from different sender: insert its sender before it.")
            (ewoc-enter-before ewoc next-node (ement-event-sender (ewoc-data next-node)))))))))

(cl-defun ement-room--ewoc-node-before (ewoc data <-fn
                                             &key (from 'last) (pred #'identity))
  "Return node in EWOC that matches PRED and belongs before DATA according to COMPARATOR."
  (cl-assert (member from '(first last)))
  (if (null (ewoc-nth ewoc 0))
      (debug-warn "EWOC is empty: returning nil.")
    (debug-warn "EWOC has data: add at appropriate place.")
    (cl-labels ((next-matching
                 (ewoc node next-fn pred) (cl-loop do (setf node (funcall next-fn ewoc node))
                                                   until (or (null node)
                                                             (funcall pred (ewoc-data node)))
                                                   finally return node)))
      (let* ((next-fn (pcase from ('first #'ewoc-next) ('last #'ewoc-prev)))
             (start-node (ewoc-nth ewoc (pcase from ('first 0) ('last -1)))))
        (unless (funcall pred (ewoc-data start-node))
          (setf start-node (next-matching ewoc start-node next-fn pred)))
        (if (funcall <-fn (ewoc-data start-node) data)
            (progn
              (debug-warn "New data goes before start node.")
              start-node)
          (debug-warn "New data goes after start node: find node before new data.")
          (let ((compare-node start-node))
            (cl-loop while (setf compare-node (next-matching ewoc compare-node next-fn pred))
                     until (funcall <-fn (ewoc-data compare-node) data)
                     finally return (if compare-node
                                        (progn
                                          (debug-warn "Found place: enter there.")
                                          compare-node)
                                      (debug-warn "Reached end of collection: insert there.")
                                      (pcase from
                                        ('first (ewoc-nth ewoc -1))
                                        ('last nil))))))))))

;;;; Footer

(provide 'ement-room)

;;; ement-room.el ends here
