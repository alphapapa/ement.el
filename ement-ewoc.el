;;; ement-ewoc.el --- EWOC testing                   -*- lexical-binding: t; -*-

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
(require 'map)
(require 'subr-x)

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

(defvar-local argh-counter 0)

;;;; Structs

(cl-defstruct ement-event
  id sender content origin-server-ts type unsigned)

(cl-defstruct ement-user
  id displayname)

;;;; Variables

(defvar ement-widget-users
  (list (make-ement-user :id "@alice:example.com" :displayname "Alice")
        (make-ement-user :id "@bob:example.com" :displayname "Bob")
        (make-ement-user :id "@charlie:example.com" :displayname "Charlie")
        ;; (make-ement-user :id "@dave:example.com" :displayname "Dave")
        ;; (make-ement-user :id "@edith:example.com" :displayname "Edith")
        ))

(defvar-local matrix-ewoc nil)

;;;; Customization

(defgroup ement nil
  "Options for Ement.el."
  :group 'matrix-client)

(defcustom ement-timestamp-format "%H:%M:%S"
  "Format string for event timestamps.
See function `format-time-string'."
  :type 'string)

(defface ement-timestamp
  '((t (:inherit matrix-client-metadata)))
  "Event timestamps.")

;;;; Commands

(defun ement-ewoc-groups-test ()
  "Start a new EWOC test."
  (interactive)
  (let* ((buffer (get-buffer-create "*EWOC Test")))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (setf buffer-read-only t
            argh-counter 0)
      (setq-local matrix-ewoc (ewoc-create #'ement-ewoc-pp "HEADER" "FOOTER" nil)))
    (pop-to-buffer buffer)
    (ement-ewoc-groups-test-add-random-event matrix-ewoc)))

(defun ement-ewoc-groups-test-add-random-event (&optional ewoc)
  "Add a randon event to the EWOC test buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*EWOC Test")
    (let* ((ewoc (or ewoc matrix-ewoc))
           (offset-seconds (if (zerop (random 2))
                               (- (random 60))
                             (random 60)))
           (user (seq-random-elt ement-widget-users))
           (event (make-ement-event :id "$deadbeef:example.com" :sender user
                                    :content (format "Hi, I'm %s (%s)" (ement-user-displayname user)
                                                     (cl-incf argh-counter))
                                    :origin-server-ts (ts-adjust 'second offset-seconds (ts-now))
                                    :type "m.message"))
           (_ (debug-warn user (ts-format "%H:%M:%S" (ement-event-origin-server-ts event))))
           (node-before (ement-ewoc-node-before ewoc event #'ement-event< :pred #'ement-event-p))
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
              (ewoc-enter-before ewoc next-node (ement-event-sender (ewoc-data next-node)))))))
      (pulse-momentary-highlight-one-line (ewoc-location new-node)))))


;;;; Functions

(defun ement-ewoc-pp (data)
  "Pretty-print DATA.
See function `ewoc-create'."
  (cl-etypecase data
    ;; FIXME: Null probably not needed anymore.
    (null (insert ""))
    (ement-event (insert "  " (ement-event-format data)))
    (ement-user (insert (ement-user-format data)))
    ;; FIXME: Function probably not needed anymore.
    (function (insert ""))))

(defun ement-event-format (event)
  "Format `ement-event' EVENT."
  (pcase-let* (((cl-struct ement-event content origin-server-ts) event))
    (concat (propertize (format "[%s] " (ts-format "%H:%M:%S" origin-server-ts))
                        'face 'matrix-client-date-header)
            content)))

(defun ement-user-format (user)
  "Format `ement-user' USER."
  (propertize (ement-user-displayname user)
              'face 'matrix-client-metadata))

(cl-defun ement-ewoc-node-before (ewoc data <-fn
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

(provide 'ement-ewoc)

;;; ement-ewoc.el ends here
