;;; ement-room.el --- Ement room buffers             -*- lexical-binding: t; -*-

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

;; EWOC is a great library.  If I had known about it and learned it
;; sooner, it would have saved me a lot of time in other projects.
;; I'm glad I decided to try it for this one.

;;; Code:

;;;; Debugging

(eval-and-compile
  (setq-local warning-minimum-log-level nil)
  (setq-local warning-minimum-log-level :debug))

;;;; Requirements

(require 'color)
(require 'ewoc)
(require 'mailcap)
(require 'shr)
(require 'subr-x)
(require 'mwheel)

(require 'ement-api)
(require 'ement-macros)
(require 'ement-structs)

;;;; Variables

(defvar-local ement-ewoc nil
  "EWOC for Ement room buffers.")

(defvar-local ement-room nil
  "Ement room for current buffer.")

(defvar-local ement-session nil
  "Ement session for current buffer.")

(defvar-local ement-room-retro-loading nil
  "Non-nil when earlier messages are being loaded.
Used to avoid overlapping requests.")

(defvar-local ement-room-replying-to-event nil
  "When non-nil, the user is replying to this event.
Used by `ement-room-send-message'.")

(defvar-local ement-room-replying-to-overlay nil
  "Used by ement-room-send-reply.")

(defvar ement-room-compose-hook nil
  "Hook run in compose buffers when created.
Used to, e.g. call `ement-room-compose-org'.")

(declare-function ement-view-room "ement.el")
(declare-function ement-room-list "ement-room-list.el")
(declare-function ement-notify-switch-to-mentions-buffer "ement-notify")
(declare-function ement-notify-switch-to-notifications-buffer "ement-notify")
(defvar ement-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'ement-room-send-reaction)
    (define-key map (kbd "e") #'ement-room-send-emote)
    (define-key map (kbd "g") #'ement-room-sync)
    (define-key map (kbd "r") #'ement-view-room)
    (define-key map (kbd "R") #'ement-room-list)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "v") #'ement-room-view-event)
    (define-key map (kbd "RET") #'ement-room-send-message)
    (define-key map (kbd "SPC") #'ement-room-scroll-up-mark-read)
    (define-key map (kbd "S-SPC") #'ement-room-scroll-down-command)
    (define-key map (kbd "M-SPC") #'ement-room-goto-fully-read-marker)
    (define-key map (kbd "S-<return>") #'ement-room-send-reply)
    (define-key map (kbd "<backtab>") #'ement-room-goto-prev)
    (define-key map (kbd "TAB") #'ement-room-goto-next)
    (define-key map (kbd "C-k") #'ement-room-delete-message)
    (define-key map [remap scroll-down-command] #'ement-room-scroll-down-command)
    (define-key map [remap mwheel-scroll] #'ement-room-mwheel-scroll)
    (define-key map (kbd "M-g M-l") #'ement-room-list)
    (define-key map (kbd "M-g M-m") #'ement-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'ement-notify-switch-to-notifications-buffer)
    map)
  "Keymap for Ement room buffers.")

(defvar ement-room-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c '") #'ement-room-compose-from-minibuffer)
    map)
  "Keymap used in `ement-room-read-string'.")

(defvar ement-room-sender-in-headers nil
  "Non-nil when sender is displayed in the left margin.
In that case, sender names are aligned to the margin edge.")

(defvar ement-room-messages-filter
  '((lazy_load_members . t))
  ;; NOTE: The confusing differences between what /sync and /messages
  ;; expect.  See <https://github.com/matrix-org/matrix-doc/issues/706>.
  "Default RoomEventFilter for /messages requests.")

(defvar ement-room-typing-timer nil
  "Timer used to send notifications while typing.")

;; Variables from other files.
(defvar ement-sessions)
(defvar ement-users)
(defvar ement-notify-limit-room-name-width)

;;;; Customization

(defgroup ement-room nil
  "Options for room buffers."
  :group 'ement)

;;;;; Faces

(defface ement-room-name
  '((t (:inherit font-lock-function-name-face)))
  "Room name shown in header line.")

(defface ement-room-membership
  '((t (:height 0.8 :inherit font-lock-comment-face)))
  "Membership events (join/part).")

(defface ement-room-reactions
  '((t (:inherit font-lock-comment-face :height 0.9)))
  "Reactions to messages (including the user count).")

(defface ement-room-reactions-key
  '((t (:inherit ement-room-reactions :height 1.5)))
  "Reactions to messages (the key, i.e. the emoji part).
Uses a separate face to allow the key to be shown at a different
size, because in some fonts, emojis are too small relative to
normal text.")

(defface ement-room-timestamp
  '((t (:inherit font-lock-comment-face)))
  "Event timestamps.")

(defface ement-room-user
  '((t (:inherit font-lock-function-name-face :weight bold :overline t)))
  "Usernames.")

(defface ement-room-self
  '((t (:inherit (font-lock-variable-name-face ement-room-user) :weight bold)))
  "Own username.")

(defface ement-room-message-text
  '((t (:inherit default)))
  "Text message bodies.")

(defface ement-room-message-emote
  '((t (:inherit italic)))
  "Emote message bodies.")

(defface ement-room-self-message
  '((t (:inherit (font-lock-variable-name-face))))
  "Oneself's message bodies.
Note that this does not need to inherit
`ement-room-message-text', because that face is combined with
this one automatically.")

(defface ement-room-timestamp-header
  '((t (:inherit header-line :weight bold :height 1.1)))
  "Timestamp headers.")

(defface ement-room-mention
  (if (version< emacs-version "27.1")
      '((t (:inverse-video t)))
    '((t (:inverse-video t :extend t))))
  "Messages that mention the local user.")

;;;;; Options

(defcustom ement-room-ellipsis "⋮"
  "String used when abbreviating certain strings."
  :type 'string)

(defcustom ement-room-avatars t
  "Show room avatars."
  :type 'boolean)

(defcustom ement-room-avatar-max-width 32
  "Maximum width in pixels of room avatars shown in header lines."
  :type 'integer)

(defcustom ement-room-avatar-max-height 32
  "Maximum height in pixels of room avatars shown in header lines."
  :type 'integer)

(defcustom ement-room-header-line-format
  ;; TODO: Show in new screenshots.
  '(:eval (concat (if ement-room-avatars
                      (or (ement-room-avatar ement-room)
                          "")
                    "")
                  " " (propertize (ement-room--escape-%
                                   (or (ement-room-display-name ement-room)
                                       "[no room name]"))
                                  'face 'ement-room-name)
                  ": " (propertize (ement-room--escape-%
                                    (or (ement-room-topic ement-room)
                                        "[no topic]"))
                                   ;; Also set help-echo in case the topic is too wide to fit.
                                   'help-echo (ement-room-topic ement-room))))
  "Header line format for room buffers.
See Info node `(elisp)Header lines'."
  :type 'sexp)
(put 'ement-room-header-line-format 'risky-local-variable t)

(defcustom ement-room-buffer-name-prefix "*Ement Room: "
  "Prefix for Ement room buffer names."
  :type 'string)

(defcustom ement-room-buffer-name-suffix "*"
  "Suffix for Ement room buffer names."
  :type 'string)

(defcustom ement-room-timestamp-format "%H:%M:%S"
  "Format string for event timestamps.
See function `format-time-string'."
  :type '(choice (const "%H:%M:%S")
                 (const "%Y-%m-%d %H:%M:%S")
                 string))

(defcustom ement-room-left-margin-width 0
  "Width of left margin in room buffers.
When using a non-graphical display, this should be set slightly
wider than when using a graphical display, to prevent sender
display names from colliding with event text."
  :type 'integer)

(defcustom ement-room-right-margin-width (length ement-room-timestamp-format)
  "Width of right margin in room buffers."
  :type 'integer)

(defcustom ement-room-sender-headers t
  "Show sender headers.
Automatically set by setting `ement-room-message-format-spec',
but may be overridden manually."
  :type 'boolean)

(defun ement-room-message-format-spec-setter (option value &optional local)
  "Set relevant options for `ement-room-message-format-spec', which see.
To be used as that option's setter.  OPTION and VALUE are
received from setting the customization option.  If LOCAL is
non-nil, set the variables buffer-locally (i.e. when called from
`ement-room-set-message-format'."
  (cl-macrolet ((set-vars (&rest pairs)
                          ;; Set variable-value pairs, locally if LOCAL is non-nil.
                          `(progn
                             ,@(cl-loop for (symbol value) on pairs by #'cddr
                                        collect `(if local
                                                     (set (make-local-variable ',symbol) ,value)
                                                   (set ',symbol ,value))))))
    (if local
        (set (make-local-variable option) value)
      (set-default option value))
    (pcase value
      ;; Try to set the margin widths smartly.
      ("%B%r%R%t" ;; "Elemental"
       (set-vars ement-room-left-margin-width 0
                 ement-room-right-margin-width 8
                 ement-room-sender-headers t
                 ement-room-sender-in-headers t))
      ("%S%L%B%r%R%t" ;; "IRC-style using margins"
       (set-vars ement-room-left-margin-width 12
                 ement-room-right-margin-width 8
                 ement-room-sender-headers nil
                 ement-room-sender-in-headers nil))
      ("[%t] %S> %B%r" ;; "IRC-style without margins"
       (set-vars ement-room-left-margin-width 0
                 ement-room-right-margin-width 0
                 ement-room-sender-headers nil
                 ement-room-sender-in-headers nil))
      (_ (set-vars ement-room-left-margin-width
                   (if (string-match-p "%L" value)
                       12 0)
                   ement-room-right-margin-width
                   (if (string-match-p "%R" value)
                       8 0)
                   ement-room-sender-headers
                   (if (string-match-p "%S" value)
                       nil t)
                   ement-room-sender-in-headers
                   (if (string-match-p (rx (1+ anything) "%S" (1+ anything) "%L") value)
                       t nil))
         (message "Ement: When using custom message format, setting margin widths may be necessary")))
    (unless ement-room-sender-in-headers
      ;; HACK: Disable overline on sender face.
      (require 'face-remap)
      (if local
          (progn
            (face-remap-reset-base 'ement-room-user)
            (face-remap-add-relative 'ement-room-user '(:overline nil)))
        (set-face-attribute 'ement-room-user nil :overline nil)))
    (unless local
      (when (and (bound-and-true-p ement-sessions) (car ement-sessions))
        ;; Only display when a session is connected (not sure why `bound-and-true-p'
        ;; is required to avoid compilation warnings).
        (message "Ement: Kill and reopen room buffers to display in new format")))))

(defcustom ement-room-message-format-spec "%S%L%B%r%R%t"
  "Format messages according to this spec.
It may contain these specifiers:

  %L  End of left margin
  %R  Start of right margin

  %b  Message body (plain-text)
  %B  Message body (formatted if available)
  %i  Event ID
  %O  Room display name (used for mentions buffer)
  %r  Reactions
  %s  Sender ID
  %S  Sender display name
  %t  Event timestamp, formatted according to
      `ement-room-timestamp-format'

Note that margin sizes must be set manually with
`ement-room-left-margin-width' and
`ement-room-right-margin-width'."
  :type '(choice (const :tag "IRC-style using margins" "%S%L%B%r%R%t")
                 (const :tag "IRC-style without margins" "[%t] %S> %B%r")
                 (const :tag "Elemental" "%B%r%R%t")
                 (string :tag "Custom format"))
  :set #'ement-room-message-format-spec-setter
  :set-after '(ement-room-left-margin-width ement-room-right-margin-width
                                            ement-room-sender-headers)
  ;; This file must be loaded before calling the setter to define the
  ;; `ement-room-user' face used in it.
  :require 'ement-room)

(defcustom ement-room-retro-messages-number 30
  "Number of messages to retrieve when loading earlier messages."
  :type 'integer)

(defcustom ement-room-timestamp-header-format " %H:%M "
  "Format string for timestamp headers where date is unchanged.
See function `format-time-string'.  If this string ends in a
newline, its background color will extend to the end of the
line."
  :type '(choice (const :tag "Time-only" " %H:%M ")
                 (const :tag "Always show date" " %Y-%m-%d %H:%M ")
                 string))

(defcustom ement-room-timestamp-header-with-date-format " %Y-%m-%d (%A) %H:%M\n"
  ;; FIXME: In Emacs 27+, maybe use :extend t instead of adding a newline.
  "Format string for timestamp headers where date changes.
See function `format-time-string'.  If this string ends in a
newline, its background color will extend to the end of the
line."
  :type '(choice (const " %Y-%m-%d (%A) %H:%M\n")
                 string))

(defcustom ement-room-replace-edited-messages t
  "Replace edited messages with their new content.
When nil, edited messages are displayed as new messages, leaving
the original messages visible."
  :type 'boolean)

(defcustom ement-room-prism 'name
  "Display users' names and messages in unique colors."
  :type '(choice (const :tag "Name only" name)
                 (const :tag "Name and message" both)
                 (const :tag "Neither" nil)))

(defcustom ement-room-prism-addressee t
  "Show addressees' names in their respective colors.
Applies to room member names at the beginning of messages,
preceded by a colon or comma.

Note that a limitation applies to the current implementation: if
a message from the addressee is not yet visible in a room at the
time the addressed message is formatted, the color may not be
applied."
  ;; FIXME: When we keep a hash table of members in a room, make this
  ;; smarter.
  :type 'boolean)

(defcustom ement-room-prism-color-adjustment 0
  "Number used to tweak computed username colors.
This may be used to adjust your favorite users' colors if you
don't like the default ones.  (The only way to do it is by
experimentation--there is no direct mapping available, nor a
per-user setting.)

The number is added to the hashed user ID before converting it to
a color.  Note that, since user ID hashes are ratioed against
`most-positive-fixnum', this number must be very large in order
to have any effect; it should be at least 1e13.

After changing this option, a room's buffer must be killed and
recreated to see the effect."
  :type 'number
  :set (lambda (option value)
         (unless (or (= 0 value) (>= value 1e13))
           (user-error "This option must be a very large number, at least 1e13"))
         (set-default option value)))

(defcustom ement-room-prism-minimum-contrast 6
  "Attempt to enforce this minimum contrast ratio for user faces.
This should be a reasonable number from, e.g. 0-7 or so."
  ;; Prot would almost approve of this default.  :) I would go all the way
  ;; to 7, but 6 already significantly dilutes the colors in some cases.
  :type 'number)

(defcustom ement-room-username-display-property '(raise -0.25)
  "Display property applied to username strings.
See Info node `(elisp)Other Display Specs'."
  :type '(choice (list :tag "Raise" (const raise :tag "Raise") (number :tag "Factor"))
                 (list :tag "Height" (const height)
                       (choice (list :tag "Larger" (const + :tag "Larger") (number :tag "Steps"))
                               (list :tag "Smaller" (const - :tag "Smaller") (number :tag "Steps"))
                               (number :tag "Factor")
                               (function :tag "Function")
                               (sexp :tag "Form"))) ))

(defcustom ement-room-event-separator-display-property '(space :ascent 50)
  "Display property applied to invisible space string after events.
Allows visual separation between events without, e.g. inserting
newlines.

See Info node `(elisp)Specified Space'."
  :type 'sexp)

(defcustom ement-room-timestamp-header-delta 600
  "Show timestamp header where events are at least this many seconds apart."
  :type 'integer)

(defcustom ement-room-send-message-filter nil
  "Function through which to pass message content before sending.
Used to, e.g. send an Org-formatted message by exporting it to
HTML first."
  :type '(choice (const :tag "Send messages as-is" nil)
                 (const :tag "Send messages in Org format" ement-room-send-org-filter)
                 (function :tag "Custom filter function"))
  :set (lambda (option value)
         (set-default option value)
         (pcase value
           ('ement-room-send-org-filter
            ;; Activate in compose buffer by default.
            (add-hook 'ement-room-compose-hook #'ement-room-compose-org))
           (_ (remove-hook 'ement-room-compose-hook #'ement-room-compose-org)))))

(defcustom ement-room-mark-rooms-read t
  "Mark rooms as read automatically.
Moves read and fully-read markers in rooms on the server when
`ement-room-scroll-up-mark-read' is called at the end of a
buffer.  When `send', also marks room as read when sending a
message in it.  When disabled, rooms may still be marked as read
manually by calling `ement-room-mark-read'.  Note that this is
not strictly the same as read receipts."
  :type '(choice (const :tag "When scrolling past end of buffer" t)
                 (const :tag "Also when sending" send)
                 (const :tag "Never" nil)))

(defcustom ement-room-send-typing t
  "Send typing notifications to the server while typing a message."
  :type 'boolean)

(defcustom ement-room-join-view-buffer t
  "View room buffer when joining a room."
  :type 'boolean)

(defcustom ement-room-leave-kill-buffer t
  "Kill room buffer when leaving a room.
When disabled, the room's buffer will remain open, but
Matrix-related commands in it will fail."
  :type 'boolean)

;;;; Macros

(defmacro ement-room-with-highlighted-event-at (position &rest body)
  "Highlight event at POSITION while evaluating BODY."
  ;; MAYBE: Accept a marker for POSITION.
  (declare (indent 1))
  `(let* ((node (ewoc-locate ement-ewoc ,position))
          (event (ewoc-data node))
          ement-room-replying-to-event ement-room-replying-to-overlay)
     (unless (and (ement-event-p event)
                  (ement-event-id event))
       (error "No event at point"))
     (unwind-protect
         (progn
           (setf ement-room-replying-to-event event
                 ement-room-replying-to-overlay
                 (make-overlay (ewoc-location node)
                               ;; NOTE: It doesn't seem possible to get the end position of
                               ;; a node, so if there is no next node, we use point-max.
                               ;; But this might break if we were to use an EWOC footer.
                               (if (ewoc-next ement-ewoc node)
                                   (ewoc-location (ewoc-next ement-ewoc node))
                                 (point-max))))
           (overlay-put ement-room-replying-to-overlay 'face 'highlight)
           ,@body)
       (when (overlayp ement-room-replying-to-overlay)
         (delete-overlay ement-room-replying-to-overlay))
       (setf ement-room-replying-to-event nil
             ement-room-replying-to-overlay nil))))

(defmacro ement-room-with-typing (&rest body)
  "Send typing notifications around BODY.
When `ement-room-send-typing' is enabled, typing notifications
are sent while BODY is executing.  BODY is wrapped in an
`unwind-protect' form that cancels `ement-room-typing-timer' and
sends a not-typing notification."
  (declare (indent defun))
  `(unwind-protect
       (progn
         (when ement-room-send-typing
           (when ement-room-typing-timer
             ;; In case there are any stray ones (e.g. a user typing in
             ;; more than room at once, which is possible but unlikely).
             (cancel-timer ement-room-typing-timer))
           (setf ement-room-typing-timer (run-at-time nil 15 #'ement-room--send-typing ement-session ement-room)))
         ,@body)
     (when ement-room-send-typing
       (when ement-room-typing-timer
         (cancel-timer ement-room-typing-timer)
         (setf ement-room-typing-timer nil))
       ;; Cancel typing notifications after sending a message.  (The
       ;; spec doesn't say whether this is needed, but it seems to be.)
       (ement-room--send-typing ement-session ement-room :typing nil))))

;;;;; Event formatting

;; NOTE: When adding specs, also add them to docstring
;; for `ement-room-message-format-spec'.

(defvar ement-room-event-formatters nil
  "Alist mapping characters to event-formatting functions.
Each function is called with three arguments: the event, the
room, and the session.  See macro
`ement-room-define-event-formatter'.")

(defvar ement-room--format-message-margin-p nil
  "Set by margin-related event formatters.")

(defmacro ement-room-define-event-formatter (char docstring &rest body)
  "Define an event formatter for CHAR with DOCSTRING and BODY.
BODY is wrapped in a lambda form that binds `event', `room', and
`session', and the lambda is added to the variable
`ement-room-event-formatters', which see."
  (declare (indent defun))
  `(setf (alist-get ,char ement-room-event-formatters nil nil #'equal)
         (lambda (event room session)
           ,docstring
           ,@body)))

(ement-room-define-event-formatter ?L
  "Text before this is shown in the left margin."
  (ignore event room session)
  (setf ement-room--format-message-margin-p t)
  (propertize " " 'left-margin-end t))

(ement-room-define-event-formatter ?R
  "Text after this is shown in the right margin."
  (ignore event room session)
  (setf ement-room--format-message-margin-p t)
  (propertize " " 'right-margin-start t))

(ement-room-define-event-formatter ?b
  "Plain-text body content."
  ;; NOTE: `save-match-data' is required around calls to `ement-room--format-message-body'.
  (let ((body (save-match-data
                (ement-room--format-message-body event :formatted-p nil)))
        (face (ement-room--event-body-face event room session)))
    (add-face-text-property 0 (length body) face 'append body)
    (when ement-room-prism-addressee
      (ement-room--add-member-face body room))
    body))

(ement-room-define-event-formatter ?B
  "Formatted body content (i.e. rendered HTML)."
  (let ((body (save-match-data
                (ement-room--format-message-body event)))
        (face (ement-room--event-body-face event room session)))
    (add-face-text-property 0 (length body) face 'append body)
    (when ement-room-prism-addressee
      (ement-room--add-member-face body room))
    body))

(ement-room-define-event-formatter ?i
  "Event ID."
  ;; Probably only useful for debugging, so might remove later.
  (ignore room session)
  (ement-event-id event))

(ement-room-define-event-formatter ?O
  "Room display name."
  (ignore event session)
  (let ((room-name (propertize (or (ement-room-display-name room)
                                   (ement-room--room-display-name room))
                               'face 'ement-room-name
                               'help-echo (or (ement-room-canonical-alias room)
                                              (ement-room-id room)))))
    ;; HACK: This will probably only be used in the notifications buffers, anyway.
    (when ement-notify-limit-room-name-width
      (setf room-name (truncate-string-to-width room-name ement-notify-limit-room-name-width
                                                nil nil ement-room-ellipsis)))
    room-name))

;; NOTE: In ?s and ?S, we add nearly-invisible ASCII unit-separator characters ("​")
;; to prevent, e.g. `dabbrev-expand' from expanding display names with body text.

(ement-room-define-event-formatter ?s
  "Sender MXID."
  (ignore room session)
  (concat (propertize (ement-user-id (ement-event-sender event))
                      'face 'ement-room-user)
          "​"))

(ement-room-define-event-formatter ?S
  "Sender display name."
  (ignore session)
  (let ((sender (ement-room--format-user (ement-event-sender event) room)))
    (when (and (not ement-room-sender-in-headers)
               (< (string-width sender) ement-room-left-margin-width))
      ;; Using :align-to or :width space display properties doesn't
      ;; seem to have any effect in the margin, so we make a string.
      (setf sender (concat (make-string (- ement-room-left-margin-width (string-width sender))
                                        ? )
                           sender)))
    ;; NOTE: I'd like to add a help-echo function to display the sender ID, but the Emacs
    ;; manual says that there is currently no way to make text in the margins mouse-sensitive.
    ;; So `ement-room--format-user' returns a string propertized with `help-echo' as a string.
    (concat sender "​")))

(ement-room-define-event-formatter ?r
  "Reactions."
  (ignore room session)
  (ement-room--format-reactions event))

(ement-room-define-event-formatter ?t
  "Timestamp."
  (ignore room session)
  (propertize (format-time-string ement-room-timestamp-format ;; Timestamps are in milliseconds.
                                  (/ (ement-event-origin-server-ts event) 1000))
              'face 'ement-room-timestamp
              'help-echo (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (/ (ement-event-origin-server-ts event) 1000))))

(defun ement-room--event-body-face (event room session)
  "Return face definition for EVENT in ROOM on SESSION."
  (ignore room)  ;; Unused for now, but keeping for consistency.
  ;; This used to be a macro in --format-message, which is probably better for
  ;; performance, but using a function is clearer, and avoids premature optimization.
  (pcase-let* (((cl-struct ement-event sender content) event)
               ((cl-struct ement-user (id sender-id)) sender)
               ((cl-struct ement-session user) session)
               ((cl-struct ement-user (id user-id)) user)
               (self-message-p (equal sender-id user-id))
               (type-face (pcase (alist-get 'msgtype content)
                            ("m.emote" 'ement-room-message-emote)
                            (_ 'ement-room-message-text)))
               (context-face (cond (self-message-p
                                    'ement-room-self-message)
                                   ((ement-room--event-mentions-user-p event user)
                                    'ement-room-mention)))
               (prism-color (unless self-message-p
                              (when (eq 'both ement-room-prism)
                                (or (ement-user-color sender)
                                    (setf (ement-user-color sender)
                                          (ement-room--user-color sender))))))
               (body-face (list :inherit (delq nil (list context-face type-face)))))
    (if prism-color
        (plist-put body-face :foreground prism-color)
      body-face)))

(defun ement-room--add-member-face (string room)
  "Add member faces in ROOM to STRING.
If STRING begins with the name of a member in ROOM followed by a
colon or comma (as if STRING is a message addressing that
member), apply that member's displayname color face to that part
of the string."
  ;; This only looks for a member name at the beginning of the string.  It would be neat to add
  ;; colors to every member mentioned in a message, but that would probably not perform well.
  (save-match-data
    ;; This function may be called from a chain of others that use the match data, so
    ;; rather than depending on all of them to save the match data, we do it here.
    ;; FIXME: Member names containing spaces aren't matched.  Can this even be fixed reasonably?
    (when (string-match (rx bos (group (1+ (not blank))) (or ":" ",") (1+ blank)) string)
      (when-let* ((member-name (match-string 1 string))
                  ;; HACK: Since we don't currently keep a list of all
                  ;; members in a room, we look to see if this displayname
                  ;; has any mentions in the room so far.
                  (user (save-match-data
                          (with-current-buffer (alist-get 'buffer (ement-room-local room))
                            (save-excursion
                              (goto-char (point-min))
                              (cl-labels ((found-sender-p
                                           (ewoc-data)
                                           (when (ement-event-p ewoc-data)
                                             (equal member-name
                                                    (gethash room (ement-user-room-display-names (ement-event-sender ewoc-data)))))))
                                (cl-loop with regexp = (regexp-quote member-name)
                                         while (re-search-forward regexp nil t)
                                         ;; NOTE: I don't know why, but sometimes the regexp
                                         ;; search ends on a non-event line, like a timestamp
                                         ;; header, so for now we just try to handle that case.
                                         for maybe-event = (ewoc-data (ewoc-locate ement-ewoc))
                                         when (found-sender-p maybe-event)
                                         return (ement-event-sender maybe-event)))))))
                  (prism-color (or (ement-user-color user)
                                   (setf (ement-user-color user)
                                         (ement-room--user-color user)))))
        (add-face-text-property (match-beginning 1) (match-end 1)
                                (list :foreground prism-color) nil string)))))

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun ement-room-bookmark-make-record ()
  "Return a bookmark record for the current `ement-room' buffer."
  (pcase-let* (((cl-struct ement-room (id room-id) canonical-alias display-name) ement-room)
               ((cl-struct ement-session user) ement-session)
               ((cl-struct ement-user (id session-id)) user))
    ;; MAYBE: Support bookmarking specific events in a room.
    (list (concat "Ement room: " display-name " (" canonical-alias ")")
          (cons 'session-id session-id)
          (cons 'room-id room-id)
          (cons 'handler #'ement-room-bookmark-handler))))

(defun ement-room-bookmark-handler (bookmark)
  "Show Ement room buffer for BOOKMARK."
  (pcase-let* ((`(,_name . ,(map session-id room-id)) bookmark)
               (session (ement-aprog1
                            (alist-get session-id ement-sessions nil nil #'equal)
                          (unless it
                            ;; MAYBE: Automatically connect.
                            (user-error "Session %s not connected: call `ement-connect' first" session-id))))
               (room (ement-aprog1
                         (ement-afirst (equal room-id (ement-room-id it))
                           (ement-session-rooms session))
                       (cl-assert it nil "Room %S not found on session %S" room-id session-id))))
    (ement-view-room room session)))

;;;; Commands

(defun ement-room-set-message-format (format-spec)
  "Set `ement-room-message-format-spec' in current buffer to FORMAT-SPEC.
Interactively, prompts for the spec using suggested values of the
option."
  (interactive (list (let* ((choices (thread-last (get 'ement-room-message-format-spec 'custom-type)
                                       cdr
                                       (seq-filter (lambda (it)
                                                     (eq (car it) 'const)))
                                       (mapcar (lambda (it)
                                                 (cons (nth 2 it) (nth 3 it))))))
                            (choice (completing-read "Format: " (mapcar #'car choices))))
                       (or (alist-get choice choices nil nil #'equal)
                           choice))))
  (cl-assert ement-ewoc)
  (ement-room-message-format-spec-setter 'ement-room-message-format-spec format-spec 'local)
  (set-window-margins nil ement-room-left-margin-width ement-room-right-margin-width)
  (if ement-room-sender-in-headers
      (ement-room--insert-sender-headers)
    (ewoc-filter ement-ewoc (lambda (node-data)
                              ;; Return non-nil for nodes that should stay.
                              (not (ement-user-p node-data)))))
  (ewoc-refresh ement-ewoc))

(defun ement-room-set-topic (session room topic)
  "Set ROOM's TOPIC on SESSION.
Interactively, set the current buffer's ROOM's TOPIC."
  (interactive (list ement-session ement-room
                     (read-string (format "New topic (%s): "
                                          (ement-room-display-name ement-room))
                                  nil nil (ement-room-topic ement-room) 'inherit-input-method)))
  (pcase-let* (((cl-struct ement-room (id room-id) display-name) room)
               (endpoint (format "rooms/%s/state/m.room.topic" (url-hexify-string room-id)))
               (data (ement-alist "topic" topic)))
    (ement-api session endpoint :method 'put :data (json-encode data)
      :then (lambda (_data)
              (message "Topic set (%s): %s" display-name topic)))))

(declare-function ement-upload "ement" t t)
(defun ement-room-send-image (file body room session)
  "Send image FILE to ROOM on SESSION, using message BODY."
  ;; TODO: Support URLs to remote files.
  (interactive (ement-room-with-typing
                 (let* ((file (read-file-name (format "Send image file (%s): " (ement-room-display-name ement-room))
                                              nil nil 'confirm))
                        (body (ement-room-read-string (format "Message body (%s): " (ement-room-display-name ement-room))
                                                      (file-name-nondirectory file) nil nil 'inherit-input-method)))
                   (list file body ement-room ement-session))))
  ;; NOTE: The typing notification won't be quite right, because it'll be canceled while waiting
  ;; for the file to upload.  It would be awkward to handle that, so this will do for now.
  (when (yes-or-no-p (format "Upload file %S to room %S? "
                             file (ement-room-display-name room)))
    (pcase-let* ((extension (file-name-extension file))
                 (mime-type (mailcap-extension-to-mime extension))
                 (data (with-temp-buffer
                         ;; NOTE: Using (set-buffer-multibyte nil) doesn't
                         ;; seem to be necessary, but I don't know why not.
                         (insert-file-contents file)
                         (buffer-string)))
                 (size (length data)))
      (ement-upload session :data data :content-type mime-type
        :then (lambda (data)
                (message "Uploaded file %S.  Sending message..." file)
                (pcase-let* (((map ('content_uri content-uri)) data)
                             ((cl-struct ement-room (id room-id)) room)
                             (endpoint (format "rooms/%s/send/%s/%s" (url-hexify-string room-id)
                                               "m.room.message" (ement-room-update-transaction-id session)))
                             ;; TODO: Image height/width (maybe not easy to get in Emacs).
                             (content (ement-alist "msgtype" "m.image"
                                                   "url" content-uri
                                                   "body" body
                                                   "info" (ement-alist "mimetype" mime-type
                                                                       "size" size))))
                  (ement-api session endpoint :method 'put :data (json-encode content)
                    :then (apply-partially #'ement-room-send-event-callback
                                           :room room :session session :content content :data))))))))

(defun ement-room-scroll-up-mark-read ()
  "Scroll buffer up, marking read and burying when at end."
  (interactive)
  (if (= (window-point) (point-max))
      (progn
        (when ement-room-mark-rooms-read
          (ement-room-mark-read ement-room ement-session
            :read-event (ewoc-data (ement-room--ewoc-last-matching ement-ewoc
                                     (lambda (data) (ement-event-p data))))
            :fully-read-event (ewoc-data (ement-room--ewoc-last-matching ement-ewoc
                                           (lambda (data) (ement-event-p data))))))
        (set-buffer-modified-p nil)
        (bury-buffer)
        (when (member major-mode '(ement-room-list-mode ement-taxy-room-list-mode))
          ;; Back in the room-list buffer: revert it.
          (revert-buffer)))
    (condition-case _err
        (scroll-up-command)
      (end-of-buffer (set-window-point nil (point-max))))))

(declare-function ement-complete-session "ement")
(defun ement-room-join (id-or-alias session)
  "Join room by ID-OR-ALIAS on SESSION."
  (interactive (list (read-string "Join room (ID or alias): ")
                     (or ement-session
                         (ement-complete-session))))
  (cl-assert id-or-alias) (cl-assert session)
  (unless (string-match-p
           ;; According to tulir in #matrix-dev:matrix.org, ": is not
           ;; allowed in the localpart, all other valid unicode is
           ;; allowed.  (user ids and room ids are the same over
           ;; federation).  it's mostly a lack of validation in
           ;; synapse (arbitrary unicode isn't intentionally allowed,
           ;; but it's not disallowed either)".  See
           ;; <https://matrix.to/#/!jxlRxnrZCsjpjDubDX:matrix.org/$Cnb53UQdYnGFizM49Aje_Xs0BxVdt-be7Dnm7_k-0ho>.
           (rx bos (or "#" "!") (1+ (not (any ":")))
               ":" (1+ (or alnum (any "-."))))
           id-or-alias)
    (user-error "Invalid room ID or alias (use, e.g. \"#ROOM-ALIAS:SERVER\")"))
  (let ((endpoint (format "join/%s" (url-hexify-string id-or-alias))))
    (ement-api session endpoint :method 'post :data ""
      :then (lambda (data)
              ;; NOTE: This generates a symbol and sets its function value to a lambda
              ;; which removes the symbol from the hook, removing itself from the hook.
              ;; TODO: When requiring Emacs 27, use `letrec'.
              (pcase-let* (((map ('room_id room-id)) data)
                           (join-fn-symbol (gensym (format "ement-join-%s" id-or-alias)))
                           (join-fn
                            (lambda (session)
                              (when-let ((room (cl-loop for room in (ement-session-rooms session)
                                                        when (equal room-id (ement-room-id room))
                                                        return room)))
                                ;; In case the join event is not in this next sync
                                ;; response, make sure the room is found before removing
                                ;; the function and joining the room.
                                (remove-hook 'ement-sync-callback-hook join-fn-symbol)
                                ;; FIXME: Probably need to unintern the symbol.
                                (ement-view-room room session)))))
                (setf (symbol-function join-fn-symbol) join-fn)
                (when ement-room-join-view-buffer
                  (add-hook 'ement-sync-callback-hook join-fn-symbol))
                (message "Joined room: %s" room-id)))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status body) response)
                           ((map error) (json-read-from-string body)))
                (pcase status
                  ((or 403 429) (error "Unable to join room %s: %s" id-or-alias error))
                  (_ (error "Unable to join room %s: %s %S" id-or-alias status plz-error))))))))
(defalias 'ement-join-room #'ement-room-join)

(declare-function ement-complete-room "ement")
(declare-function ement--format-room "ement")
(defun ement-room-leave (room session)
  "Leave ROOM on SESSION.
ROOM may be an `ement-room' struct, or a room ID or alias
string."
  (interactive (ement-complete-room (ement-complete-session)))
  (cl-assert room) (cl-assert session)
  (cl-etypecase room
    (ement-room)
    (string (setf room (ement-afirst (or (equal room (ement-room-canonical-alias it))
                                         (equal room (ement-room-id it)))
                         (ement-session-rooms session)))))
  (when (yes-or-no-p (format "Leave room %s? " (ement--format-room room)))
    (pcase-let* (((cl-struct ement-room id) room)
                 (endpoint (format "rooms/%s/leave" (url-hexify-string id))))
      (ement-api session endpoint :method 'post :data ""
        :then (lambda (_data)
                ;; NOTE: This generates a symbol and sets its function value to a lambda
                ;; which removes the symbol from the hook, removing itself from the hook.
                ;; TODO: When requiring Emacs 27, use `letrec'.
                (let* ((leave-fn-symbol (gensym (format "ement-leave-%s" room)))
                       (leave-fn
                        (lambda (_session)
                          (remove-hook 'ement-sync-callback-hook leave-fn-symbol)
                          ;; FIXME: Probably need to unintern the symbol.
                          (when-let ((buffer (map-elt (ement-room-local room) 'buffer)))
                            (when (buffer-live-p buffer)
                              (kill-buffer buffer))))))
                  (setf (symbol-function leave-fn-symbol) leave-fn)
                  (when ement-room-leave-kill-buffer
                    (add-hook 'ement-sync-callback-hook leave-fn-symbol))
                  (message "Left room: %s" (ement--format-room room))))
        :else (lambda (plz-error)
                (pcase-let* (((cl-struct plz-error response) plz-error)
                             ((cl-struct plz-response status body) response)
                             ((map error) (json-read-from-string body)))
                  (pcase status
                    (429 (error "Unable to leave room %s: %s" room error))
                    (_ (error "Unable to leave room %s: %s %S" room status plz-error)))))))))
(defalias 'ement-leave-room #'ement-room-leave)

(defun ement-room-goto-prev ()
  "Go to the previous message in buffer."
  (interactive)
  (ement-room-goto-next :next-fn #'ewoc-prev))

(cl-defun ement-room-goto-next (&key (next-fn #'ewoc-next))
  "Go to the next message in buffer.
NEXT-FN is passed to `ement-room--ewoc-next-matching', which
see."
  (interactive)
  (if-let (node (ement-room--ewoc-next-matching ement-ewoc
                  (ewoc-locate ement-ewoc) #'ement-event-p next-fn))
      (ewoc-goto-node ement-ewoc node)
    (user-error "End of events")))

(defun ement-room-scroll-down-command ()
  "Scroll down, and load NUMBER earlier messages when at top."
  (interactive)
  (condition-case _err
      (scroll-down nil)
    (beginning-of-buffer
     (when (call-interactively #'ement-room-retro)
       (message "Loading earlier messages...")))))

(defun ement-room-mwheel-scroll (event)
  "Scroll according to EVENT, loading earlier messages when at top."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (condition-case _err
        (mwheel-scroll event)
      (beginning-of-buffer
       (when (call-interactively #'ement-room-retro)
         (message "Loading earlier messages..."))))))

;; TODO: Unify these retro-loading functions.

(cl-defun ement-room-retro
    (room session number &key buffer
          (then (apply-partially #'ement-room-retro-callback room session)))
  ;; FIXME: Naming things is hard.
  "Retrieve NUMBER older messages in ROOM on SESSION."
  (interactive (list ement-room ement-session
                     (if current-prefix-arg
                         (read-number "Number of messages: ")
                       ement-room-retro-messages-number)
                     :buffer (current-buffer)))
  (unless ement-room-retro-loading
    (pcase-let* (((cl-struct ement-room id prev-batch) room)
                 (endpoint (format "rooms/%s/messages" (url-hexify-string id))))
      ;; We use a timeout of 30, because sometimes the server can take a while to
      ;; respond, especially if loading, e.g. hundreds or thousands of events.
      (ement-api session endpoint :timeout 30
        :params (list (list "from" prev-batch)
                      (list "dir" "b")
                      (list "limit" (number-to-string number))
                      (list "filter" (json-encode ement-room-messages-filter)))
        :then then
        :else (lambda (plz-error)
                (when buffer
                  (with-current-buffer buffer
                    (setf ement-room-retro-loading nil)))
                (signal 'ement-api-error (list "Loading earlier messages failed" plz-error))))
      (setf ement-room-retro-loading t))))

(cl-defun ement-room-retro-to (room session event-id &key then (batch-size 100) (limit 1000))
  "Retrieve messages in ROOM on SESSION back to EVENT-ID."
  (cl-assert
   ;; Ensure the event hasn't already been retrieved.
   (not (gethash event-id (ement-session-events session))))
  (let* ((total-retrieved 0)
         ;; TODO: Use letrec someday.
         (callback-symbol (gensym "ement-room-retro-to-callback-"))
         (callback (lambda (data)
                     (ement-room-retro-callback room session data)
                     (if (gethash event-id (ement-session-events session))
                         (progn
                           (message "Found event %S" event-id)
                           ;; FIXME: Probably need to unintern the symbol.
                           (when then
                             (funcall then)))
                       ;; FIXME: What if it hits the beginning of the timeline?
                       (if (>= (cl-incf total-retrieved batch-size) limit)
                           (message "%s older events retrieved without finding event %S"
                                    limit event-id)
                         (message "Looking back for event %S (%s/%s events retrieved)"
                                  event-id total-retrieved limit)
                         (ement-room-retro room session  batch-size
                                           :buffer (alist-get 'buffer (ement-room-local room))
                                           :then callback-symbol))))))
    (fset callback-symbol callback)
    (ement-room-retro room session batch-size
                      :buffer (alist-get 'buffer (ement-room-local room))
                      :then callback-symbol)))

(cl-defun ement-room-retro-to-token (room session from to
                                          &key (batch-size 100) (limit 1000))
  "Retrieve messages in ROOM on SESSION back from FROM to TO.
Retrieve batches of BATCH-SIZE up to total LIMIT.  FROM and TO
are sync batch tokens.  Used for, e.g. filling gaps in
\"limited\" sync responses."
  ;; NOTE: We don't set `ement-room-retro-loading' since the room may
  ;; not have a buffer.  This could theoretically allow a user to
  ;; overlap manual scrollback-induced loading of old messages with
  ;; this gap-filling loading, but that shouldn't matter, and probably
  ;; would be very rare, anyway.
  (pcase-let* (((cl-struct ement-room id) room)
               (endpoint (format "rooms/%s/messages" (url-hexify-string id)))
               (then
                (lambda (data)
                  (ement-room-retro-callback room session data
                                             :set-prev-batch nil)
                  (pcase-let* (((map end chunk) data))
		    ;; HACK: Comparing the END and TO tokens ought to
		    ;; work for determining whether we are done
		    ;; filling, but it isn't (maybe the server isn't
		    ;; returning the TO token as END when there are no
		    ;; more events), so instead we'll check the length
		    ;; of the chunk.
                    (unless (< (length chunk) batch-size)
                      ;; More pages remain to be loaded.
                      (let ((remaining-limit (- limit batch-size)))
                        (if (not (> remaining-limit 0))
                            ;; FIXME: This leaves a gap if it's larger than 1,000 events.
                            ;; Probably, the limit should be configurable, but it would be good
                            ;; to find some way to remember the gap and fill it if the user
                            ;; scrolls to it later (although that might be very awkward to do).
                            (display-warning 'ement-room-retro-to-token
                                             (format "Loaded events in %S (%S) without filling gap; not filling further"
                                                     (ement-room-display-name room)
                                                     (or (ement-room-canonical-alias room)
                                                         (ement-room-id room))))
			  ;; FIXME: Remove this message after further testing.
                          (message "Ement: Continuing to fill gap in %S (%S) (remaining limit: %s)"
                                   (ement-room-display-name room)
                                   (or (ement-room-canonical-alias room)
                                       (ement-room-id room))
                                   remaining-limit)
                          (ement-room-retro-to-token
                           room session end to :limit remaining-limit))))))))
    ;; FIXME: Remove this message after further testing.
    (message "Ement: Filling gap in %S (%S)"
	     (ement-room-display-name room)
             (or (ement-room-canonical-alias room)
                 (ement-room-id room)))
    (ement-api session endpoint :timeout 30
      :params (list (list "from" from)
                    (list "to" to)
                    (list "dir" "b")
                    (list "limit" (number-to-string batch-size))
                    (list "filter" (json-encode ement-room-messages-filter)))
      :then then
      :else (lambda (plz-error)
              (signal 'ement-api-error
                      (list (format "Filling gap in %S (%S) failed"
                                    (ement-room-display-name room)
                                    (or (ement-room-canonical-alias room)
                                        (ement-room-id room)))
                            plz-error))))))

;; NOTE: `declare-function' doesn't recognize cl-defun forms, so this declaration doesn't work.
(declare-function ement--sync "ement.el" t t)
(defun ement-room-sync (session &optional force)
  "Sync SESSION (interactively, current buffer's).
If FORCE (interactively, with prefix), cancel any outstanding
sync requests."
  (interactive (list ement-session current-prefix-arg))
  (ement--sync session :force force))

(defun ement-room-view-event (event)
  "Pop up buffer showing details of EVENT (interactively, the one at point)."
  (interactive (list (ewoc-data (ewoc-locate ement-ewoc))))
  (require 'pp)
  (let* ((buffer-name (format "*Ement event: %s*" (ement-event-id event)))
         (event (ement-alist :id (ement-event-id event)
                             :sender (ement-user-id (ement-event-sender event))
                             :content (ement-event-content event)
                             :origin-server-ts (ement-event-origin-server-ts event)
                             :type (ement-event-type event)
                             :state-key (ement-event-state-key event)
                             :unsigned (ement-event-unsigned event)
                             :receipts (ement-event-receipts event)))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (pp event (current-buffer))
      (view-mode)
      (pop-to-buffer (current-buffer)))))

(cl-defun ement-room-send-message (room session &key body formatted-body replying-to-event)
  "Send message to ROOM on SESSION with BODY and FORMATTED-BODY.
REPLYING-TO-EVENT may be an event the message is in reply to; the
message will reference it appropriately.

If `ement-room-send-message-filter' is non-nil, the message's
content alist is passed through it before sending.  This may be
used to, e.g. process the BODY into another format and add it to
the content. (e.g. see `ement-room-send-org-filter')."
  (interactive (progn
                 (cl-assert ement-room) (cl-assert ement-session)
                 (let* ((room ement-room)
                        (session ement-session)
                        (prompt (format "Send message (%s): " (ement-room-display-name room)))
                        (body (ement-room-with-typing
                                (ement-room-read-string prompt nil nil nil
                                                        'inherit-input-method))))
                   (list room session :body body))))
  (cl-assert (not (string-empty-p body)))
  (cl-assert (or (not formatted-body) (not (string-empty-p formatted-body))))
  (pcase-let* (((cl-struct ement-room (id room-id) (local (map buffer))) room)
               (window (when buffer (get-buffer-window buffer)))
               (endpoint (format "rooms/%s/send/m.room.message/%s" (url-hexify-string room-id)
                                 (ement-room-update-transaction-id session)))
               (content (ement-aprog1
                            (ement-alist "msgtype" "m.text"
                                         "body" body)
                          (when formatted-body
                            (push (cons "formatted_body" formatted-body) it)))))
    (when replying-to-event
      (setf content (ement-room--add-reply content replying-to-event)))
    (when ement-room-send-message-filter
      (setf content (funcall ement-room-send-message-filter content)))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (apply-partially #'ement-room-send-event-callback :room room :session session
                             :content content :data)) ;; Data is added when calling back.
    ;; NOTE: This assumes that the selected window is the buffer's window.  For now
    ;; this is almost surely the case, but in the future, we might let the function
    ;; send messages to other rooms more easily, so this assumption might not hold.
    (when window
      (with-selected-window window
        (when (>= (window-point) (ewoc-location (ewoc-nth ement-ewoc -1)))
          ;; Point is on last event: advance it to eob so that when the event is received
          ;; back, the window will scroll.  (This might not always be desirable, because
          ;; the user might have point on that event for a reason, but I think in most
          ;; cases, it will be what's expected and most helpful.)
          (setf (window-point) (point-max)))))))

(cl-defun ement-room-send-emote (room session &key body)
  "Send emote to ROOM on SESSION with BODY.

If `ement-room-send-message-filter' is non-nil, the message's
content alist is passed through it before sending.  This may be
used to, e.g. process the BODY into another format and add it to
the content. (e.g. see `ement-room-send-org-filter')."
  (interactive (progn
                 (cl-assert ement-room) (cl-assert ement-session)
                 (let* ((room ement-room)
                        (session ement-session)
                        (prompt (format "Send emote (%s): " (ement-room-display-name room)))
                        (body (ement-room-with-typing
                                (ement-room-read-string prompt nil nil nil
                                                        'inherit-input-method))))
                   (list room session :body body))))
  (cl-assert (not (string-empty-p body)))
  (pcase-let* (((cl-struct ement-room (id room-id) (local (map buffer))) room)
               (window (when buffer (get-buffer-window buffer)))
               (endpoint (format "rooms/%s/send/m.room.message/%s" (url-hexify-string room-id)
                                 (ement-room-update-transaction-id session)))
               (content (ement-aprog1
                            (ement-alist "msgtype" "m.emote"
                                         "body" body))))
    (when ement-room-send-message-filter
      (setf content (funcall ement-room-send-message-filter content)))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (apply-partially #'ement-room-send-event-callback :room room :session session
                             :content content :data)) ;; Data is added when calling back.
    ;; NOTE: This assumes that the selected window is the buffer's window.  For now
    ;; this is almost surely the case, but in the future, we might let the function
    ;; send messages to other rooms more easily, so this assumption might not hold.
    (when window
      (with-selected-window window
        (when (>= (window-point) (ewoc-location (ewoc-nth ement-ewoc -1)))
          ;; Point is on last event: advance it to eob so that when the event is received
          ;; back, the window will scroll.  (This might not always be desirable, because
          ;; the user might have point on that event for a reason, but I think in most
          ;; cases, it will be what's expected and most helpful.)
          (setf (window-point) (point-max)))))))

(cl-defun ement-room-send-event-callback (&key data room session content)
  "Callback for event-sending functions.
DATA is the parsed JSON object.  If DATA's event ID is already
present in SESSION's events table, show an appropriate warning
mentioning the ROOM and CONTENT."
  (pcase-let* (((map ('event_id event-id)) data))
    (if (gethash event-id (ement-session-events session))
        (let ((message (format "Event ID %S already seen in session %S.  This may indicate a reused transaction ID, which could mean that the event was not sent to the room (%S).  You may need to disconnect, delete the `ement-sessions-file', and connect again to start a new session.  Alternatively, this can happen if the event's sent-confirmation is received after the event itself is received in the next sync response, in which case no action is needed."
                               event-id (ement-user-id (ement-session-user session))
                               (ement-room-display-name room))))
          (when content
            (setf message (concat message (format " Event content: %S" content))))
          (display-warning 'ement-room-send-event-callback message)))
    (when (eq 'send ement-room-mark-rooms-read)
      ;; Move read markers.
      (when-let ((buffer (alist-get 'buffer (ement-room-local room))))
        (with-current-buffer buffer
          ;; NOTE: The new event may not exist in the buffer yet, so
          ;; we just have to use the last one.
          ;; FIXME: When we add local echo, this can be fixed.
          (save-excursion
            (goto-char (ewoc-location
                        (ement-room--ewoc-last-matching ement-ewoc #'ement-event-p)))
            (call-interactively #'ement-room-mark-read)))))))

(defun ement-room-edit-message (event room session body)
  "Edit EVENT in ROOM on SESSION to have new BODY.
The message must be one sent by the local user."
  (interactive (ement-room-with-highlighted-event-at (point)
                 (cl-assert ement-session) (cl-assert ement-room)
                 (pcase-let* ((event (ewoc-data (ewoc-locate ement-ewoc)))
                              ((cl-struct ement-session user) ement-session)
                              ((cl-struct ement-event sender
                                          (content (map body ('m.relates_to relates-to))))
                               event))
                   (unless (equal (ement-user-id sender) (ement-user-id user))
                     (user-error "You may only edit your own messages"))
                   (when relates-to
                     ;; FIXME: This isn't quite right.  When we show edits by replacing
                     ;; the original event, this will need to be changed.
                     (user-error "Only original messages may be edited, not the edit events themselves"))
                   ;; Remove any leading asterisk from the plain-text body.
                   (setf body (replace-regexp-in-string (rx bos "*" (1+ space)) "" body t t))
                   (ement-room-with-typing
                     (let* ((prompt (format "Edit message (%s): "
                                            (ement-room-display-name ement-room)))
                            (body (ement-room-read-string prompt body nil nil
                                                          'inherit-input-method)))
                       (when (string-empty-p body)
                         (user-error "To delete a message, use command `ement-room-delete-message'"))
                       (when (yes-or-no-p (format "Edit message to: %S? " body))
                         (list event ement-room ement-session body)))))))
  (let* ((endpoint (format "rooms/%s/send/%s/%s" (url-hexify-string (ement-room-id room))
                           "m.room.message" (ement-room-update-transaction-id session)))
         (new-content (ement-alist "body" body
                                   "msgtype" "m.text"))
         (_ (when ement-room-send-message-filter
              (setf new-content (funcall ement-room-send-message-filter new-content))))
         (content (ement-alist "msgtype" "m.text"
                               "body" body
                               "m.new_content" new-content
                               "m.relates_to" (ement-alist "rel_type" "m.replace"
                                                           "event_id" (ement-event-id event)))))
    ;; Prepend the asterisk after the filter may have modified the content.  Note that the
    ;; "m.new_content" body does not get the leading asterisk, only the "content" body,
    ;; which is intended as a fallback.
    (setf body (concat "* " body))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (apply-partially #'ement-room-send-event-callback :room room :session session
                             :content content :data))))

(defun ement-room-delete-message (event room session &optional reason)
  "Delete EVENT in ROOM on SESSION, optionally with REASON."
  (interactive (ement-room-with-highlighted-event-at (point)
                 (if (yes-or-no-p "Delete this event? ")
                     (list (ewoc-data (ewoc-locate ement-ewoc))
                           ement-room ement-session (read-string "Reason (optional): " nil nil nil 'inherit-input-method))
                   ;; HACK: This isn't really an error, but is there a cleaner way to cancel?
                   (user-error "Message not deleted"))))
  (pcase-let* (((cl-struct ement-event (id event-id)) event)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "rooms/%s/redact/%s/%s"
                                 (url-hexify-string room-id) (url-hexify-string event-id)
                                 (ement-room-update-transaction-id ement-session)))
               (content (if reason
                            (ement-alist "reason" reason)
                          ;; To get an empty JSON object, we use an empty hash table.
                          (make-hash-table))))
    (ement-api session endpoint :method 'put :data (json-encode content)
      :then (lambda (_data)
              (message "Event %S deleted." event-id)))))

(defun ement-room-send-reply ()
  "Send a reply to event at point."
  (interactive)
  (cl-assert ement-ewoc) (cl-assert ement-room) (cl-assert ement-session)
  (cl-assert (ement-event-p (ewoc-data (ewoc-locate ement-ewoc))))
  (ement-room-with-highlighted-event-at (point)
    (pcase-let* ((event (ewoc-data (ewoc-locate ement-ewoc)))
                 (room ement-room)
                 (session ement-session)
                 (prompt (format "Send reply (%s): " (ement-room-display-name room)))
                 (body (ement-room-with-typing
                         (ement-room-read-string prompt nil nil nil 'inherit-input-method))))
      (ement-room-send-message room session :body body :replying-to-event event))))

(defun ement-room-send-reaction (key position)
  "Send reaction of KEY to event at POSITION.
Interactively, send reaction to event at point.  KEY should be a
reaction string, e.g. \"👍\"."
  ;; SPEC: MSC2677 <https://github.com/matrix-org/matrix-doc/pull/2677>
  (interactive
   (cl-labels
       ((face-at-point-p
         (face) (let ((face-at-point (get-text-property (point) 'face)))
                  (or (eq face face-at-point)
                      (and (listp face-at-point)
                           (member face face-at-point)))))
        (buffer-substring-while
         (beg pred &key (forward-fn #'forward-char))
         "Return substring of current buffer from BEG while PRED is true."
         (save-excursion
           (goto-char beg)
           (cl-loop while (funcall pred)
                    do (funcall forward-fn)
                    finally return (buffer-substring-no-properties beg (point)))))
        (key-at
         (pos) (cond ((face-at-point-p 'ement-room-reactions-key)
                      (buffer-substring-while
                       pos (lambda () (face-at-point-p 'ement-room-reactions-key))))
                     ((face-at-point-p 'ement-room-reactions)
                      ;; Point is in a reaction button but after the key.
                      (buffer-substring-while
                       (button-start (button-at pos))
                       (lambda () (face-at-point-p 'ement-room-reactions-key)))))))
     (list
      (or (key-at (point))
          (char-to-string (read-char-by-name "Reaction (prepend \"*\" for substring search): ")))
      (point))))
  ;; HACK: We could simplify this by storing the key in a text property...
  (ement-room-with-highlighted-event-at position
    (pcase-let* ((event (or (ewoc-data (ewoc-locate ement-ewoc position))
                            (user-error "No event at point")))
                 ;; NOTE: Sadly, `face-at-point' doesn't work here because, e.g. if
                 ;; hl-line-mode is enabled, it only returns the hl-line face.
                 ((cl-struct ement-event (id event-id)) event)
                 ((cl-struct ement-room (id room-id)) ement-room)
                 (endpoint (format "rooms/%s/send/m.reaction/%s" (url-hexify-string room-id)
                                   (ement-room-update-transaction-id ement-session)))
                 (content (ement-alist "m.relates_to"
                                       (ement-alist "rel_type" "m.annotation"
                                                    "event_id" event-id
                                                    "key" key))))
      (ement-api ement-session endpoint :method 'put :data (json-encode content)
        :then (apply-partially #'ement-room-send-event-callback
                               :room ement-room :session ement-session :content content
                               :data)))))

(defun ement-room-reaction-button-action (button)
  "Push reaction BUTTON at point."
  ;; TODO: Toggle reactions off with redactions (not in spec yet, but Element does it).
  (save-excursion
    (goto-char (button-start button))
    (call-interactively #'ement-room-send-reaction)))

;;;; Functions

(defun ement-room-update-transaction-id (session)
  "Return SESSION's incremented transaction ID formatted for sending.
Increments ID and appends current timestamp to avoid reuse
problems."
  ;; TODO: Naming things is hard.
  ;; In the event that Emacs isn't killed cleanly and the session isn't saved to disk, the
  ;; transaction ID would get reused the next time the user connects.  To avoid that, we
  ;; append the current time to the ID.  (IDs are just strings, and Element does something
  ;; similar, so this seems reasonable.)
  (format "%s-%s"
          (cl-incf (ement-session-transaction-id session))
          (format-time-string "%s")))

(defun ement-room-goto-event (event)
  "Go to EVENT in current buffer."
  (if-let ((node (ement-room--ewoc-last-matching ement-ewoc
                   (lambda (data)
                     (equal (ement-event-id event) (ement-event-id data))))))
      (goto-char (ewoc-location node))
    (error "Event not found in buffer: %S" (ement-event-id event))))

(declare-function ement--make-event "ement.el")
(declare-function ement--put-event "ement.el")
(cl-defun ement-room-retro-callback (room session data
                                          &key (set-prev-batch t))
  "Push new DATA to ROOM on SESSION and add events to room buffer.
If SET-PREV-BATCH is nil, don't set ROOM's prev-batch slot to the
\"prev_batch\" token in response DATA (this should be set,
e.g. when filling timeline gaps as opposed to retrieving messages
before the earliest-seen message)."
  (pcase-let* (((cl-struct ement-room local) room)
	       ((map _start end chunk state) data)
               ((map buffer) local)
               (num-events (length chunk))
               ;; We do 3 things for chunk events, so we count them 3 times when
               ;; reporting progress.  (We also may receive some state events for
               ;; these chunk events, but we don't bother to include them in the
               ;; count, and we don't report progress for them, because they are
               ;; likely very few compared to the number of timeline events, which is
               ;; what the user is interested in (e.g. when loading 1000 earlier
               ;; messages in #emacs:matrix.org, only 31 state events were received).
               (progress-max-value (* 3 num-events)))
    ;; NOTE: Put the newly retrieved events at the end of the slots, because they should be
    ;; older events.  But reverse them first, because we're using "dir=b", which the
    ;; spec says causes the events to be returned in reverse-chronological order, and we
    ;; want to process them oldest-first (important because a membership event having a
    ;; user's displayname should be older than a message event sent by the user).
    ;; NOTE: The events in `chunk' and `state' are vectors, so we
    ;; convert them to a list before appending.
    (ement-debug num-events progress-max-value)
    (setf chunk (nreverse chunk)
          state (nreverse state))
    ;; FIXME: Like `ement--push-joined-room-events', this should probably run the `ement-event-hook' on the newly seen events.
    ;; Append state events.
    (cl-loop for event across-ref state
             do (setf event (ement--make-event event))
             finally do (setf (ement-room-state room)
                              (append (ement-room-state room) (append state nil))))
    (ement-with-progress-reporter (:reporter ("Ement: Processing earlier events..." 0 progress-max-value))
      ;; Append timeline events (in the "chunk").
      (cl-loop for event across-ref chunk
               do (setf event (ement--make-event event))
               ;; HACK: Put events on events table.  See FIXME above about using the event hook.
               (ement--put-event event nil session)
               (ement-progress-update)
               finally do (setf (ement-room-timeline room)
                                (append (ement-room-timeline room) (append chunk nil))))
      (when buffer
        ;; Insert events into the room's buffer.
        (with-current-buffer buffer
          (save-window-excursion
            ;; NOTE: See note in `ement--update-room-buffers'.
            (when-let ((buffer-window (get-buffer-window buffer)))
              (select-window buffer-window))
            ;; FIXME: Use retro-loading in event handlers, or in --handle-events, anyway.
            (ement-room--handle-events chunk)
            (when set-prev-batch
              ;; This feels a little hacky, but maybe not too bad.
              (setf (ement-room-prev-batch room) end))
            (setf ement-room-retro-loading nil)))))
    (message "Ement: Loaded %s earlier events." num-events)))

(defun ement-room--insert-events (events &optional retro)
  "Insert EVENTS into current buffer.
Calls `ement-room--insert-event' for each event and inserts
timestamp headers into appropriate places while maintaining
point's position.  If RETRO is non-nil, assume EVENTS are earlier
than any existing events, and only insert timestamp headers up to
the previously oldest event."
  (let (buffer-window point-node orig-first-node point-max-p)
    (when (get-buffer-window (current-buffer))
      ;; HACK: See below.
      (setf buffer-window (get-buffer-window (current-buffer))
            point-max-p (= (point) (point-max))))
    (when (and buffer-window retro)
      (setf point-node (ewoc-locate ement-ewoc (window-start buffer-window))
            orig-first-node (ewoc-nth ement-ewoc 0)))
    (save-window-excursion
      ;; NOTE: When inserting some events, seemingly only replies, if a different buffer's
      ;; window is selected, and this buffer's window-point is at the bottom, the formatted
      ;; events may be inserted into the wrong place in the buffer, even though they are
      ;; inserted into the EWOC at the right place.  We work around this by selecting the
      ;; buffer's window while inserting events, if it has one.  (I don't know if this is a bug
      ;; in EWOC or in this file somewhere.  But this has been particularly nasty to debug.)
      (when buffer-window
        (select-window buffer-window))
      (cl-loop for event being the elements of events
               do (ement-room--handle-event event)
               do (ement-progress-update)))
    ;; Since events can be received in any order, we have to check the whole buffer
    ;; for where to insert new timestamp headers.  (Avoiding that would require
    ;; getting a list of newly inserted nodes and checking each one instead of every
    ;; node in the buffer.  Doing that now would probably be premature optimization,
    ;; though it will likely be necessary if users keep buffers open for busy rooms
    ;; for a long time, as the time to do this in each buffer will increase with the
    ;; number of events.  At least we only do it once per batch of events.)
    (ement-room--insert-ts-headers nil (when retro orig-first-node))
    (when buffer-window
      (cond (retro (with-selected-window buffer-window
                     (set-window-start buffer-window (ewoc-location point-node))
                     ;; TODO: Experiment with this.
                     (forward-line -1)))
            (point-max-p (set-window-point buffer-window (point-max)))))))

(defun ement-room--add-reply (data replying-to-event)
  "Return DATA adding reply data for REPLYING-TO-EVENT in current buffer's room.
DATA is an unsent message event's data alist."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id351> "13.2.2.6.1  Rich replies"
  ;; FIXME: Rename DATA.
  (pcase-let* (((cl-struct ement-event (id replying-to-event-id)
                           content (sender replying-to-sender))
                replying-to-event)
               ((cl-struct ement-user (id replying-to-sender-id)) replying-to-sender)
               ((map ('body replying-to-body) ('formatted_body replying-to-formatted-body)) content)
               (replying-to-body (if (use-region-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                   replying-to-body))
               (replying-to-formatted-body (if (use-region-p)
                                               replying-to-body
                                             replying-to-formatted-body))
               (replying-to-sender-name (ement-room--user-display-name replying-to-sender ement-room))
               (quote-string (format "> <%s> %s\n\n" replying-to-sender-name replying-to-body))
               (reply-body (alist-get "body" data nil nil #'string=))
               (reply-body-with-quote (concat quote-string reply-body))
               (reply-formatted-body-with-quote
                (format "<mx-reply>
  <blockquote>
    <a href=\"https://matrix.to/#/%s/%s\">In reply to</a>
    <a href=\"https://matrix.to/#/%s\">%s</a>
    <br />
    %s
  </blockquote>
</mx-reply>
%s"
                        (ement-room-id ement-room) replying-to-event-id replying-to-sender-id replying-to-sender-name
                        ;; TODO: Encode HTML special characters.  Not as straightforward in Emacs as one
                        ;; might hope: there's `web-mode-html-entities' and `org-entities'.  See also
                        ;; <https://emacs.stackexchange.com/questions/8166/encode-non-html-characters-to-html-equivalent>.
                        (or replying-to-formatted-body replying-to-body)
                        reply-body)))
    (when (use-region-p)
      (deactivate-mark))
    ;; NOTE: map-elt doesn't work with string keys, so we use `alist-get'.
    (setf (alist-get "body" data nil nil #'string=) reply-body-with-quote
          (alist-get "formatted_body" data nil nil #'string=) reply-formatted-body-with-quote
          data (append (ement-alist "m.relates_to" (ement-alist "m.in_reply_to"
                                                                (ement-alist "event_id" replying-to-event-id))
                                    "format" "org.matrix.custom.html")
                       data))
    data))

(cl-defun ement-room--send-typing (session room &key (typing t))
  "Send a typing notification for ROOM on SESSION."
  (pcase-let* (((cl-struct ement-session user) session)
               ((cl-struct ement-user (id user-id)) user)
               ((cl-struct ement-room (id room-id)) room)
               (endpoint (format "rooms/%s/typing/%s"
                                 (url-hexify-string room-id) (url-hexify-string user-id)))
               (data (ement-alist "typing" typing "timeout" 20000)))
    (ement-api session endpoint :method 'put :data (json-encode data)
      ;; We don't really care about the response, I think.
      :then #'ignore)))

(defun ement-room--direct-p (room session)
  "Return non-nil if ROOM on SESSION is a direct chat."
  (cl-labels ((content-contains-room-id
               (content room-id) (cl-loop for (_user-id . room-ids) in content
                                          ;; NOTE: room-ids is a vector.
                                          thereis (seq-contains room-ids room-id))))
    (pcase-let* (((cl-struct ement-session account-data) session)
                 ((cl-struct ement-room id) room))
      (or (cl-loop for event in account-data
                   when (equal "m.direct" (alist-get 'type event))
                   thereis (content-contains-room-id (alist-get 'content event) id))
          (cl-loop
           ;; Invited rooms have no account-data yet, and their
           ;; directness flag is in invite-state events.
           for event in (ement-room-invite-state room)
           thereis (alist-get 'is_direct (ement-event-content event)))))))

(define-derived-mode ement-room-mode fundamental-mode
  `("Ement-Room"
    (:eval (unless (map-elt ement-syncs ement-session)
             (propertize ":Not-syncing"
                         'face 'font-lock-warning-face
                         'help-echo "Automatic syncing was interrupted; press \"g\" to resume"))))
  "Major mode for Ement room buffers.
This mode initializes a buffer to be used for showing events in
an Ement room.  It kills all local variables, removes overlays,
and erases the buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setf buffer-read-only t
        left-margin-width ement-room-left-margin-width
        right-margin-width ement-room-right-margin-width
        imenu-create-index-function #'ement-room--imenu-create-index-function
        ;; TODO: Use EWOC header/footer for, e.g. typing messages.
        ement-ewoc (ewoc-create #'ement-room--pp-thing))
  (setq-local completion-at-point-functions
              '(ement-room--complete-members-at-point ement-room--complete-rooms-at-point))
  (setq-local window-scroll-functions
              (cons 'ement-room-start-read-receipt-timer window-scroll-functions)))
(add-hook 'ement-room-mode-hook 'visual-line-mode)

(defun ement-room-read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Call `read-from-minibuffer', binding variables and keys for Ement.
Arguments PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, and
INHERIT-INPUT-METHOD are as those expected by `read-string',
which see."
  (let ((room ement-room)
        (session ement-session))
    (minibuffer-with-setup-hook
        (lambda ()
          "Bind keys and variables locally (to be called in minibuffer)."
          (setq-local ement-room room)
          (setq-local ement-session session)
          (setq-local completion-at-point-functions
                      '(ement-room--complete-members-at-point ement-room--complete-rooms-at-point))
          (visual-line-mode 1))
      (read-from-minibuffer prompt initial-input ement-room-minibuffer-map
                            nil history default-value inherit-input-method))))

(defun ement-room--buffer (session room name)
  "Return buffer named NAME showing ROOM's events on SESSION.
If ROOM has no buffer, one is made and stored in the room's local
data slot."
  (or (map-elt (ement-room-local room) 'buffer)
      (let ((new-buffer (generate-new-buffer name)))
        (with-current-buffer new-buffer
          (ement-room-mode)
          (setf header-line-format (when ement-room-header-line-format
                                     'ement-room-header-line-format)
                ement-session session
                ement-room room
                list-buffers-directory (or (ement-room-canonical-alias room)
                                           (ement-room-id room))
                ;; Track buffer in room's slot.
                (map-elt (ement-room-local room) 'buffer) (current-buffer))
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (setf (map-elt (ement-room-local room) 'buffer) nil))
                    nil 'local)
          (setq-local bookmark-make-record-function #'ement-room-bookmark-make-record)
          ;; Set initial header and footer.  (Do this before processing events, which
          ;; might cause the header/footer to be changed (e.g. a tombstone event).
          (let ((header (if (cl-find-if (apply-partially #'equal "m.room.encryption")
                                        (ement-room-invite-state ement-room)
                                        :key #'ement-event-type)
                            (propertize "This appears to be an encrypted room, which is not natively supported by Ement.el.  (See information about using Pantalaimon in Ement.el documentation.)"
                                        'face 'font-lock-warning-face)
                          ""))
                (footer (pcase (ement-room-type ement-room)
                          ;; Set header and footer for an invited room.
                          ('invite
                           (concat (propertize "You've been invited to this room.  "
                                               'face 'font-lock-warning-face)
                                   (propertize "[Join this room]"
                                               'button '(t)
                                               'category 'default-button
                                               'mouse-face 'highlight
                                               'follow-link t
                                               'action (lambda (_button)
                                                         ;; Kill the room buffer so it can be recreated after joining
                                                         ;; (which will cleanly update the room's name, footer, etc).
                                                         (let ((room ement-room)
                                                               (session ement-session))
                                                           (kill-buffer)
                                                           (message "Joining room... (buffer will be reopened after joining)")
                                                           (ement-room-join (ement-room-id room) session))))))
                          (_ ""))))
            (ewoc-set-hf ement-ewoc header footer))
          (setf
           ;; Clear new-events, because those only matter when a buffer is already open.
           (alist-get 'new-events (ement-room-local room)) nil
           ;; Set the new buffer in the room's local alist so that it
           ;; can be used by event-inserting functions before this
           ;; function returns, e.g. `ement-room--add-member-face'.
           (alist-get 'buffer (ement-room-local room)) new-buffer)
          ;; We don't use `ement-room--insert-events' to avoid extra
          ;; calls to `ement-room--insert-ts-headers'.
          (ement-room--handle-events (ement-room-state room))
          (ement-room--handle-events (ement-room-timeline room))
          (ement-room--insert-ts-headers)
          (ement-room-move-read-markers room
            :read-event (when-let ((event (alist-get "m.read" (ement-room-account-data room) nil nil #'equal)))
                          (map-nested-elt event '(content event_id)))
            :fully-read-event (when-let ((event (alist-get "m.fully_read" (ement-room-account-data room) nil nil #'equal)))
                                (map-nested-elt event '(content event_id)))))
        ;; Return the buffer!
        new-buffer)))

(defun ement-room--room-display-name (room)
  "Return the displayname for ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#calculating-the-display-name-for-a-room>.
  ;; NOTE: The spec seems incomplete, because the algorithm it recommends does not say how
  ;; or when to use "m.room.member" events for rooms without heroes (e.g. invited rooms).
  ;; TODO: Add SESSION argument and use it to remove local user from names.
  (cl-labels ((latest-event (type content-field)
                            (or (cl-loop for event in (ement-room-timeline room)
                                         when (and (equal type (ement-event-type event))
                                                   (not (string-empty-p (alist-get content-field (ement-event-content event)))))
                                         return (alist-get content-field (ement-event-content event)))
                                (cl-loop for event in (ement-room-state room)
                                         when (and (equal type (ement-event-type event))
                                                   (not (string-empty-p (alist-get content-field (ement-event-content event)))))
                                         return (alist-get content-field (ement-event-content event)))))
              (member-events-name
               () (when-let ((member-events (cl-loop for accessor in '(ement-room-timeline ement-room-state ement-room-invite-state)
                                                     append (cl-remove-if-not (apply-partially #'equal "m.room.member")
                                                                              (funcall accessor room)
                                                                              :key #'ement-event-type))))
                    (string-join (delete-dups
                                  (mapcar (lambda (event)
                                            (ement-room--user-display-name (ement-event-sender event) room))
                                          member-events))
                                 ", ")))
              (heroes-name
               () (pcase-let* (((cl-struct ement-room summary) room)
                               ((map ('m.heroes hero-ids) ('m.joined_member_count joined-count)
                                     ('m.invited_member_count invited-count))
                                summary))
                    ;; TODO: Disambiguate hero display names.
                    (when hero-ids
                      (cond ((<= (+ joined-count invited-count) 1)
                             ;; Empty room.
                             (empty-room hero-ids joined-count))
                            ((>= (length hero-ids) (1- (+ joined-count invited-count)))
                             ;; Members == heroes.
                             (hero-names hero-ids))
                            ((and (< (length hero-ids) (1- (+ joined-count invited-count)))
                                  (> (+ joined-count invited-count) 1))
                             ;; More members than heroes.
                             (heroes-and-others hero-ids joined-count))))))
              (hero-names
               (heroes) (string-join (mapcar #'hero-name heroes) ", "))
              (hero-name
               (id) (if-let ((user (gethash id ement-users)))
                        (ement-room--user-display-name user room)
                      id))
              (heroes-and-others
               (heroes joined)
               (format "%s, and %s others" (hero-names heroes)
                       (- joined (length heroes))))
              (empty-room
               (heroes joined) (cl-etypecase (length heroes)
                                 ((satisfies zerop) "Empty room")
                                 ((number 1 5) (format "Empty room (was %s)"
                                                       (hero-names heroes)))
                                 (t (format "Empty room (was %s)"
                                            (heroes-and-others heroes joined))))))
    (or (latest-event "m.room.name" 'name)
        (latest-event "m.room.canonical_alias" 'alias)
        (heroes-name)
        (member-events-name)
        (ement-room-id room))))

(defun ement-room--user-display-name (user room)
  "Return the displayname for USER in ROOM."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#calculating-the-display-name-for-a-user>.
  ;; FIXME: Add step 3 of the spec.  For now we skip to step 4.

  ;; NOTE: Both state and timeline events must be searched.  (A helpful user
  ;; in #matrix-dev:matrix.org, Michael (t3chguy), clarified this for me).
  (if-let ((cached-name (gethash room (ement-user-room-display-names user))))
      cached-name
    ;; Put timeline events before state events, because IIUC they should be more recent.
    (cl-labels ((join-displayname-event-p
                 (event) (and (equal "m.room.member" (ement-event-type event))
                              (equal "join" (alist-get 'membership (ement-event-content event)))
                              (equal user (ement-event-sender event))
                              (alist-get 'displayname (ement-event-content event)))))
      (if-let* ((displayname (or (cl-loop for event in (ement-room-timeline room)
                                          when (join-displayname-event-p event)
                                          return (alist-get 'displayname (ement-event-content event)))
                                 (cl-loop for event in (ement-room-state room)
                                          when (join-displayname-event-p event)
                                          return (alist-get 'displayname (ement-event-content event)))))
                (calculated-name displayname))
          (puthash room calculated-name (ement-user-room-display-names user))
        ;; No membership state event: use pre-calculated displayname or ID.
        (or (ement-user-displayname user)
            (ement-user-id user))))))

(defun ement-room--event-data (id)
  "Return event struct for event ID in current buffer."
  ;; Search from bottom, most likely to be faster.
  (cl-loop with node = (ewoc-nth ement-ewoc -1)
           while node
           for data = (ewoc-data node)
           when (and (ement-event-p data)
                     (equal id (ement-event-id data)))
           return data
           do (setf node (ewoc-prev ement-ewoc node))))

(defun ement-room--escape-% (string)
  "Return STRING with \"%\" escaped.
Needed to display things in the header line."
  (replace-regexp-in-string (rx "%") "%%" string t t))

;;;;; Imenu

(defun ement-room--imenu-create-index-function ()
  "Return Imenu index for the current buffer.
For use as `imenu-create-index-function'."
  (let ((timestamp-nodes (ement-room--ewoc-collect-nodes
                          ement-ewoc (lambda (node)
                                       (pcase (ewoc-data node)
                                         (`(ts . ,_) t)))))
        (ts-format (string-trim ement-room-timestamp-header-with-date-format)))
    (cl-loop for node in timestamp-nodes
             collect (pcase-let*
                         ((`(ts ,timestamp) (ewoc-data node))
                          (formatted (format-time-string ts-format timestamp)))
                       (cons formatted (ewoc-location node))))))

;;;;; Events

;; Functions to handle types of events.

;; NOTE: At the moment, this only handles "m.typing" ephemeral events.  Message
;; events are handled elsewhere.  A better framework should be designed...
;; TODO: Define other handlers this way.

;; MAYBE: Should we intern these functions?  That means every event
;; handled has to concat and intern.  Should we use lambdas in an
;; alist or hash-table instead?  For now let's use an alist.

(defvar ement-users)

(defvar ement-room-event-fns nil
  "Alist mapping event types to functions which process events in room buffers.")

;; NOTE: While transitioning to the defevent-based handler system, we
;; define both a handle-events and handle-event function that do the
;; same thing.

;; TODO: Tidy this up.

;; NOTE: --handle-events and --handle-event need to be called in the room
;; buffer's window, when it has one.  This is absolutely necessary,
;; otherwise the events may be inserted at the wrong place.  (I'm not
;; sure if this is a bug in EWOC or in my code, but doing this fixes it.)

(defun ement-room--handle-events (events)
  "Process EVENTS in current buffer.
Calls `ement-progress-update' for each event.  Calls
`ement-room--insert-ts-headers' when done.  Uses handlers defined
in `ement-room-event-fns'.  The current buffer should be a room's
buffer."
  ;; FIXME: Calling `ement-room--insert-ts-headers' is convenient, but it
  ;; may also be called in functions that call this function, which may
  ;; result in it being called multiple times for a single set of events.
  (cl-loop for event being the elements of events ;; EVENTS may be a list or array.
           for handler = (alist-get (ement-event-type event) ement-room-event-fns nil nil #'equal)
           when handler
           do (funcall handler event)
           do (ement-progress-update))
  (ement-room--insert-ts-headers))

(defun ement-room--handle-event (event)
  "Process EVENT in current buffer.
Uses handlers defined in `ement-room-event-fns'.  The current
buffer should be a room's buffer."
  (when-let ((handler (alist-get (ement-event-type event) ement-room-event-fns nil nil #'equal)))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.  See,
    ;; e.g. <https://github.com/alphapapa/ement.el/pull/61>.
    (with-demoted-errors "Ement (ement-room--handle-event): Error processing event: %S"
      (funcall handler event))))

;;;;;; Event handlers

(defmacro ement-room-defevent (type &rest body)
  "Define an event handling function for events of TYPE.
Around the BODY, the variable `event' is bound to the event being
processed.  The function is called in the room's buffer.  Adds
function to `ement-room-event-fns', which see."
  (declare (indent defun))
  `(setf (alist-get ,type ement-room-event-fns nil nil #'string=)
         (lambda (event)
           ,(concat "`ement-room' handler function for " type " events.")
           ,@body)))

(ement-room-defevent "m.reaction"
  (pcase-let* (((cl-struct ement-event content) event)
               ((map ('m.relates_to relates-to)) content)
               ((map ('event_id related-id) ('rel_type rel-type) _key) relates-to))
    ;; TODO: Handle other rel_types?
    (pcase rel-type
      ("m.annotation"
       ;; Look for related event in timeline.
       (if-let ((related-event (cl-loop for event in (ement-room-timeline ement-room)
                                        when (equal related-id (ement-event-id event))
                                        return event)))
           ;; Found related event: add reaction to local slot and invalidate node.
           (progn
             ;; Every time a room buffer is made, these reaction events are processed again, so we use pushnew to
             ;; avoid duplicates.  (In the future, as event-processing is refactored, this may not be necessary.)
             (cl-pushnew event (map-elt (ement-event-local related-event) 'reactions))
             (when-let ((nodes (ement-room--ewoc-last-matching ement-ewoc
                                 (lambda (data)
                                   (and (ement-event-p data)
                                        (equal related-id (ement-event-id data)))))))
               (ewoc-invalidate ement-ewoc nodes)))
         ;; No known related event: discard.
         ;; TODO: Is this the correct thing to do?
         nil)))))

(ement-room-defevent "m.typing"
  (pcase-let* (((cl-struct ement-session user) ement-session)
               ((cl-struct ement-user (id local-user-id)) user)
               ((cl-struct ement-event content) event)
               ((map ('user_ids user-ids)) content)
               (usernames) (footer))
    (setf user-ids (delete local-user-id user-ids))
    (if (zerop (length user-ids))
        (setf footer "")
      (setf usernames (cl-loop for id across user-ids
                               for user = (gethash id ement-users)
                               if user
                               collect (ement-room--user-display-name user ement-room)
                               else collect id)
            footer (propertize (concat "Typing: " (string-join usernames ", "))
                               'face 'font-lock-comment-face)))
    (with-silent-modifications
      (ewoc-set-hf ement-ewoc "" footer))))

(ement-room-defevent "m.room.avatar"
  (ement-room--insert-event event))

(ement-room-defevent "m.room.member"
  (with-silent-modifications
    (ement-room--insert-event event)))

(ement-room-defevent "m.room.message"
  (pcase-let* (((cl-struct ement-event content) event)
               ((map ('m.relates_to (map ('rel_type rel-type) ('event_id replaces-event-id)))) content))
    (if (and ement-room-replace-edited-messages
             replaces-event-id (equal "m.replace" rel-type))
        ;; Event replaces existing event: find and replace it in buffer if possible, otherwise insert it.
        (or (ement-room--replace-event replaces-event-id event)
            (progn
              (ement-debug "Unable to replace event ID: inserting instead." replaces-event-id)
              (ement-room--insert-event event)))
      ;; New event: insert it.
      (ement-room--insert-event event))))

(ement-room-defevent "m.room.tombstone"
  (pcase-let* (((cl-struct ement-event content) event)
               ((map body replacement_room) content)
               (footer (propertize (format "This room has been replaced.  Explanation:%S  Replacement room: <%s>" body replacement_room)
                                   'face 'font-lock-warning-face)))
    ;; NOTE: We assume that no more typing events will be received,
    ;; which would replace the footer.
    (ement-room--insert-event event)
    (ewoc-set-hf ement-ewoc "" footer)))

;;;;; Read markers

;; Marking rooms as read and showing lines where marks are.

(ement-room-defevent "m.read"
  (ement-room-move-read-markers ement-room
    :read-event (ement-event-id event)))

(ement-room-defevent "m.fully_read"
  (ement-room-move-read-markers ement-room
    :fully-read-event (ement-event-id event)))

(defvar-local ement-room-read-receipt-marker nil
  "EWOC node for the room's read-receipt marker.")

(defvar-local ement-room-read-receipt-timer nil
  "Timer that sets read receipt after scrolling.")

(defvar-local ement-room-fully-read-marker nil
  "EWOC node for the room's fully-read marker.")

(defface ement-room-read-receipt-marker
  '((t (:inherit show-paren-match)))
  "Read marker line in rooms.")

(defface ement-room-fully-read-marker
  '((t (:inherit isearch)))
  "Fully read marker line in rooms.")

(defcustom ement-room-send-read-receipts t
  "Whether to send read receipts.
Also controls whether the read-receipt marker in a room is moved
automatically."
  :type 'boolean)

(defun ement-room-start-read-receipt-timer (window _pos)
  "Start idle timer to set read-receipt to POS in WINDOW's room.
Read receipt is sent if `ement-room-send-read-receipts' is
non-nil, the read-receipt marker is between retrieved events, and
WINDOW's end is beyond the marker.  For use in
`window-scroll-functions'."
  (with-selected-window window
    (when (timerp ement-room-read-receipt-timer)
      (cancel-timer ement-room-read-receipt-timer))
    (when ement-room-send-read-receipts
      ;; This is highly suboptimal, because this function is called
      ;; from `window-scroll-functions', whose docstring says that
      ;; `window-end' is not valid when this function is called.  So
      ;; we have to call `window-end' from the idle timer, and the
      ;; window might not even be visible or on the same buffer by
      ;; that time; if that's the case, the receipt is not sent.

      ;; MAYBE: Reduce idle time so the receipt is less likely to not
      ;; get updated if the user only views a room's buffer for a
      ;; short time.
      (let ((room-buffer (window-buffer window)))
        (run-with-idle-timer
         3 nil (lambda ()
                 (when (and (windowp window)
                            (eq (window-buffer window) room-buffer))
                   (with-selected-window window
                     (when-let ((read-receipt-node (ement-room--ewoc-last-matching ement-ewoc
                                                     (lambda (node-data)
                                                       (eq 'ement-room-read-receipt-marker node-data)))))
                       ;; The read-receipt marker is visible (i.e. it's not between
                       ;; earlier events which we have not retrieved).
                       (when (> (window-end) (ewoc-location read-receipt-node))
                         ;; The window's end has been scrolled past the position of the receipt marker.
                         (when-let* ((window-end-node (or (ewoc-locate ement-ewoc (window-end))
                                                          (ewoc-nth ement-ewoc -1)))
                                     (event-node (cl-typecase (ewoc-data window-end-node)
                                                   (ement-event window-end-node)
                                                   (t (ement-room--ewoc-next-matching ement-ewoc window-end-node
                                                        #'ement-event-p #'ewoc-prev)))))
                           (ement-room-mark-read ement-room ement-session
                             :read-event (ewoc-data event-node)))))))))))))

(defun ement-room-goto-fully-read-marker ()
  "Move to the fully-read marker in the current room."
  (interactive)
  (let ((fully-read-pos (when ement-room-fully-read-marker
                          (ewoc-location ement-room-fully-read-marker))))
    (if fully-read-pos
        (setf (point) fully-read-pos (window-start) fully-read-pos)
      ;; Unlike the fully-read marker, there doesn't seem to be a
      ;; simple way to get the user's read-receipt marker.  So if
      ;; we haven't seen either marker in the retrieved events, we
      ;; go back to the fully-read marker.
      (if-let* ((fully-read-event (alist-get "m.fully_read" (ement-room-account-data ement-room) nil nil #'equal))
                (fully-read-event-id (map-nested-elt fully-read-event '(content event_id))))
          (let ((buffer (current-buffer)))
            (message "Searching for first unread event...")
            (ement-room-retro-to ement-room ement-session fully-read-event-id
                                 :then (lambda ()
                                         (with-current-buffer buffer
                                           ;; HACK: Should probably call this function elsewhere, in a hook or something.
                                           (ement-room-move-read-markers ement-room)
                                           (ement-room-goto-fully-read-marker)))))
        (error "Room has no fully-read event")))))

(cl-defun ement-room-mark-read (room session &key read-event fully-read-event)
  "Mark ROOM on SESSION as read on the server.
Set \"m.read\" to READ-EVENT and \"m.fully_read\" to
FULLY-READ-EVENT.

Interactively, mark both types as read up to event at point."
  (declare (indent defun))
  (interactive
   (progn
     (cl-assert (equal 'ement-room-mode major-mode) nil
                "This command is to be used in `ement-room-mode' buffers")
     (let* ((node (ewoc-locate ement-ewoc))
            (event (cl-typecase (ewoc-data node)
                     (ement-event (ewoc-data node))
                     (t (when-let ((prev-event-node (ement-room--ewoc-next-matching ement-ewoc node
                                                      #'ement-event-p #'ewoc-prev)))
                          (ewoc-data prev-event-node))))))
       (list ement-room ement-session
             :read-event event
             :fully-read-event event))))
  (cl-assert room) (cl-assert session) (cl-assert (or read-event fully-read-event))
  (if (not fully-read-event)
      ;; Sending only a read receipt, which uses a different endpoint
      ;; than when setting the fully-read marker or both.
      (ement-room-send-receipt room session read-event)
    ;; Setting the fully-read marker, and maybe the "m.read" one too.
    (pcase-let* (((cl-struct ement-room (id room-id)) room)
                 (endpoint (format "rooms/%s/read_markers" (url-hexify-string room-id)))
                 (data (ement-alist "m.fully_read" (ement-event-id fully-read-event))))
      (when read-event
        (push (cons "m.read" (ement-event-id read-event)) data))
      (ement-api session endpoint :method 'post :data (json-encode data)
        :then (lambda (_data)
                (ement-room-move-read-markers room
                  :read-event read-event :fully-read-event fully-read-event))))))

(cl-defun ement-room-send-receipt (room session event &key (type "m.read"))
  "Send receipt of TYPE for EVENT to ROOM on SESSION."
  (pcase-let* (((cl-struct ement-room (id room-id)) room)
               ((cl-struct ement-event (id event-id)) event)
               (endpoint (format "rooms/%s/receipt/%s/%s"
                                 (url-hexify-string room-id) type
                                 (url-hexify-string event-id))))
    (ement-api session endpoint :method 'post :data "{}"
      :then (pcase type
              ("m.read" (lambda (_data)
                          (ement-room-move-read-markers room
                            :read-event event)))
              ;; No other type is yet specified.
              (_ #'ignore)))))

(cl-defun ement-room-move-read-markers
    (room &key
          (read-event (when-let ((event (alist-get "m.read" (ement-room-account-data room) nil nil #'equal)))
                        (map-nested-elt event '(content event_id))))
          (fully-read-event (when-let ((event (alist-get "m.fully_read" (ement-room-account-data room) nil nil #'equal)))
                              (map-nested-elt event '(content event_id)))))
  "Move read markers in ROOM to given events.
Each event may be an `ement-event' struct or an event ID.  This
updates the markers in ROOM's buffer, not on the server; see
`ement-room-mark-read' for that."
  (declare (indent defun))
  (cl-labels ((update-marker (symbol to-event)
                             (let* ((old-node (symbol-value symbol))
                                    (new-event-id (cl-etypecase to-event
                                                    (ement-event (ement-event-id to-event))
                                                    (string to-event)))
                                    (event-node (ement-room--ewoc-last-matching ement-ewoc
                                                  (lambda (data)
                                                    (and (ement-event-p data)
                                                         (equal (ement-event-id data) new-event-id)))))
                                    (inhibit-read-only t))
                               (with-silent-modifications
                                 (when old-node
                                   (ewoc-delete ement-ewoc old-node))
                                 (set symbol (when event-node
                                               ;; If the event hasn't been inserted into the buffer yet,
                                               ;; this might be nil.  That shouldn't happen, but...
                                               (ewoc-enter-after ement-ewoc event-node symbol)))))))
    (when-let ((buffer (alist-get 'buffer (ement-room-local room))))
      ;; MAYBE: Error if no buffer?  Or does it matter?
      (with-current-buffer buffer
        (when read-event
          (update-marker 'ement-room-read-receipt-marker read-event))
        (when fully-read-event
          (update-marker 'ement-room-fully-read-marker fully-read-event))))
    ;; NOTE: Return nil so that, in the event this function is called manually with `eval-expression',
    ;; it does not cause an error due to the return value being an EWOC node, which is a structure too
    ;; big and/or circular to print.  (This was one of those bugs that only happens WHEN debugging.)
    nil))

;;;;; EWOC

(cl-defun ement-room--ewoc-next-matching (ewoc node pred &optional (move-fn #'ewoc-next))
  "Return the next node in EWOC after NODE that PRED is true of.
PRED is called with node's data.  Moves to next node by MOVE-FN."
  (declare (indent defun))
  (cl-loop do (setf node (funcall move-fn ewoc node))
           until (or (null node)
                     (funcall pred (ewoc-data node)))
           finally return node))

(defun ement-room--ewoc-last-matching (ewoc predicate)
  "Return the last node in EWOC matching PREDICATE.
PREDICATE is called with node's data.  Searches backward from
last node."
  (declare (indent defun))
  ;; Intended to be like `ewoc-collect', but returning as soon as a match is found.
  (cl-loop with node = (ewoc-nth ewoc -1)
           while node
           when (funcall predicate (ewoc-data node))
           return node
           do (setf node (ewoc-prev ewoc node))))

(defun ement-room--ewoc-collect-nodes (ewoc predicate)
  "Collect all nodes in EWOC matching PREDICATE.
PREDICATE is called with the full node."
  ;; Intended to be like `ewoc-collect', but working with the full node instead of just the node's data.
  (cl-loop with node = (ewoc-nth ewoc 0)
           do (setf node (ewoc-next ewoc node))
           while node
           when (funcall predicate node)
           collect node))

(defun ement-room--insert-ts-headers (&optional start-node end-node)
  "Insert timestamp headers into current buffer's `ement-ewoc'.
Inserts headers between START-NODE and END-NODE, which default to
the first and last nodes in the buffer, respectively."
  (let* ((type-predicate (lambda (node-data)
                           (and (ement-event-p node-data)
                                (not (equal "m.room.member" (ement-event-type node-data))))))
         (ewoc ement-ewoc)
         (end-node (or end-node
                       (ewoc-nth ewoc -1)))
         (end-pos (if end-node
                      (ewoc-location end-node)
                    ;; HACK: Trying to work around a bug in case the
                    ;; room doesn't seem to have any events yet.
                    (point-max)))
         (node-b (or start-node (ewoc-nth ewoc 0)))
         node-a)
    ;; On the first loop iteration, node-a is set to the first matching
    ;; node after node-b; then it's set to the first node after node-a.
    (while (and (setf node-a (ement-room--ewoc-next-matching ewoc (or node-a node-b) type-predicate)
                      node-b (when node-a
                               (ement-room--ewoc-next-matching ewoc node-a type-predicate)))
                (not (or (> (ewoc-location node-a) end-pos)
                         (when node-b
                           (> (ewoc-location node-b) end-pos)))))
      (cl-labels ((format-event
                   (event) (format "TS:%S (%s)  Sender:%s  Message:%S"
                                   (/ (ement-event-origin-server-ts (ewoc-data event)) 1000)
                                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                                       (/ (ement-event-origin-server-ts (ewoc-data event)) 1000))
                                   (ement-user-id (ement-event-sender (ewoc-data event)))
                                   (when (alist-get 'body (ement-event-content (ewoc-data event)))
                                     (substring-no-properties
                                      (truncate-string-to-width (alist-get 'body (ement-event-content (ewoc-data event))) 20))))))
        (ement-debug "Comparing event timestamps:"
                     (list 'A (format-event node-a))
                     (list 'B (format-event node-b))))
      ;; NOTE: Matrix timestamps are in milliseconds.
      (let* ((a-ts (/ (ement-event-origin-server-ts (ewoc-data node-a)) 1000))
             (b-ts (/ (ement-event-origin-server-ts (ewoc-data node-b)) 1000))
             (diff-seconds (- b-ts a-ts))
             (ement-room-timestamp-header-format ement-room-timestamp-header-format))
        (when (and (>= diff-seconds ement-room-timestamp-header-delta)
                   (not (when-let ((node-after-a (ewoc-next ewoc node-a)))
                          (pcase (ewoc-data node-after-a)
                            (`(ts . ,_) t)
                            ((or 'ement-room-read-receipt-marker 'ement-room-fully-read-marker) t)))))
          (unless (equal (time-to-days a-ts) (time-to-days b-ts))
            ;; Different date: bind format to print date.
            (setf ement-room-timestamp-header-format ement-room-timestamp-header-with-date-format))
          (with-silent-modifications
            ;; Avoid marking a buffer as modified just because we inserted a ts
            ;; header (this function may be called after other events which shouldn't
            ;; cause it to be marked modified, like moving the read markers).
            (ewoc-enter-after ewoc node-a (list 'ts b-ts))))))))

(defun ement-room--insert-sender-headers (&optional start-node end-node)
  ;; TODO: Use this in appropriate places.
  "Insert sender headers into current buffer's `ement-ewoc'.
Inserts headers between START-NODE and END-NODE, which default to
the first and last nodes in the buffer, respectively."
  (let* ((ewoc ement-ewoc)
         (end-pos (ewoc-location (or end-node
                                     (ewoc-nth ewoc -1))))
         (node-b (or start-node (ewoc-nth ewoc 0)))
         (type-predicate (lambda (node-data)
                           (pcase node-data
                             ((pred ement-event-p) t)
                             (`(ts . ,_) t))))
         node-a)
    ;; On the first loop iteration, node-a is set to the first matching
    ;; node after node-b; then it's set to the first node after node-a.
    (while (and (setf node-a (ement-room--ewoc-next-matching ewoc (or node-a node-b) type-predicate)
                      node-b (when node-a
                               (ement-room--ewoc-next-matching ewoc node-a type-predicate)))
                (not (or (> (ewoc-location node-a) end-pos)
                         (> (ewoc-location node-b) end-pos))))
      ;; This starts to get a little messy, accounting for the
      ;; different types of nodes.  EIEIO would probably help here.
      (let* ((a-data (ewoc-data node-a))
             (b-data (ewoc-data node-b))
             (a-sender (when (ement-event-p a-data)
                         (ement-event-sender a-data)))
             (b-sender (when (ement-event-p b-data)
                         (ement-event-sender b-data))))
        (unless (or (when (ement-event-p b-data)
                      ;; B is a membership event: don't insert sender header.
                      (equal "m.room.member" (ement-event-type b-data)))
                    (when-let ((node-after-a (ewoc-next ewoc node-a)))
                      ;; Node after A (regardless of type) is a sender header: don't insert another.
                      (ement-user-p (ewoc-data node-after-a))))
          (unless (equal a-sender b-sender)
            (when b-sender
              (ewoc-enter-before ewoc node-b b-sender))))))))

(defun ement-room--insert-event (event)
  "Insert EVENT into current buffer."
  (cl-labels ((format-event
               (event) (format "TS:%S (%s)  Sender:%s  Message:%S"
                               (/ (ement-event-origin-server-ts event) 1000)
                               (format-time-string "%Y-%m-%d %H:%M:%S"
                                                   (/ (ement-event-origin-server-ts event) 1000))
                               (ement-user-id (ement-event-sender event))
                               (when (alist-get 'body (ement-event-content event))
                                 (substring-no-properties
                                  (truncate-string-to-width (alist-get 'body (ement-event-content event)) 20))))))
    (ement-debug "INSERTING NEW EVENT: " (format-event event))
    (let* ((ewoc ement-ewoc)
           (event< (lambda (a b)
                     "Return non-nil if event A's timestamp is before B's."
                     (< (ement-event-origin-server-ts a)
                        (ement-event-origin-server-ts b))))
           (node-before (ement-room--ewoc-node-before ewoc event event< :pred #'ement-event-p))
           new-node)
      ;; HACK: Insert after any read markers.
      (cl-loop for node-after-node-before = (ewoc-next ewoc node-before)
               while node-after-node-before
               while (pcase (ewoc-data node-after-node-before)
                       ;; MAYBE: Invert this and test that it's *not* an event node.
                       ((or 'ement-room-read-receipt-marker 'ement-room-fully-read-marker) t))
               do (setf node-before node-after-node-before))
      (setf new-node (if (not node-before)
                         (progn
                           (ement-debug "No event before it: add first.")
                           (if-let ((first-node (ewoc-nth ewoc 0)))
                               (progn
                                 (ement-debug "EWOC not empty.")
                                 (if (and (ement-user-p (ewoc-data first-node))
                                          (equal (ement-event-sender event)
                                                 (ewoc-data first-node)))
                                     (progn
                                       (ement-debug "First node is header for this sender: insert after it, instead.")
                                       (setf node-before first-node)
                                       (ewoc-enter-after ewoc first-node event))
                                   (ement-debug "First node is not header for this sender: insert first.")
                                   (ewoc-enter-first ewoc event)))
                             (ement-debug "EWOC empty: add first.")
                             (ewoc-enter-first ewoc event)))
                       (ement-debug "Found event before new event: insert after it.")
                       (when-let ((next-node (ewoc-next ewoc node-before)))
                         (when (and (ement-user-p (ewoc-data next-node))
                                    (equal (ement-event-sender event)
                                           (ewoc-data next-node)))
                           (ement-debug "Next node is header for this sender: insert after it, instead.")
                           (setf node-before next-node)))
                       (ement-debug "Inserting after event"
                                    ;; NOTE: `format-event' is only for debugging, and it
                                    ;; doesn't handle user headers, so commenting it out or now.
                                    ;; (format-event (ewoc-data node-before))

                                    ;; NOTE: And it's *Very Bad* to pass the raw node data
                                    ;; to `ement-debug', because it makes event insertion
                                    ;; *Very Slow*.  So we just comment that out for now.
                                    ;; (ewoc-data node-before)
                                    )
                       (ewoc-enter-after ewoc node-before event)))
      ;; Insert sender where necessary.
      (when ement-room-sender-headers
        ;; TODO: Do this more flexibly.
        (if (not node-before)
            (progn
              (ement-debug "No event before: Add sender before new node.")
              (ewoc-enter-before ewoc new-node (ement-event-sender event)))
          (ement-debug "Event before: compare sender.")
          (if (equal (ement-event-sender event)
                     (pcase-exhaustive (ewoc-data node-before)
                       ((pred ement-event-p)
                        (ement-event-sender (ewoc-data node-before)))
                       ((pred ement-user-p)
                        (ewoc-data node-before))
                       (_
                        ;; Any other node, like timestamp header, read marker, etc.
                        (when-let ((node-before-ts (ewoc-prev ewoc node-before)))
                          ;; FIXME: Well this is ugly.  Make a filter predicate or something.
                          (pcase-exhaustive (ewoc-data node-before-ts)
                            ((pred ement-event-p)
                             (ement-event-sender (ewoc-data node-before)))
                            ((pred ement-user-p)
                             (ewoc-data node-before)))))))
              (ement-debug "Same sender.")
            (ement-debug "Different sender: insert new sender node.")
            (ewoc-enter-before ewoc new-node (ement-event-sender event))
            (when-let* ((next-node (ewoc-next ewoc new-node)))
              (when (ement-event-p (ewoc-data next-node))
                (ement-debug "Event after from different sender: insert its sender before it.")
                (ewoc-enter-before ewoc next-node (ement-event-sender (ewoc-data next-node)))))))))))

(defun ement-room--replace-event (old-event-id new-event)
  "Replace event having or replacing OLD-EVENT-ID with NEW-EVENT in current buffer.
If OLD-EVENT-ID is not found, return nil, otherwise non-nil."
  (let* ((ewoc ement-ewoc)
         (old-event-node (ement-room--ewoc-last-matching ewoc
                           (lambda (data)
                             (cl-typecase data
                               (ement-event (or (equal old-event-id (ement-event-id data))
                                                (pcase-let* (((cl-struct ement-event content) data)
                                                             ((map ('m.relates_to (map rel_type event_id))) content))
                                                  (and (equal "m.replace" rel_type)
                                                       (equal old-event-id event_id))))))))))
    (when old-event-node
      ;; TODO: Record old events in new event's local data, and make it accessible when inspecting the new event.
      (let ((node-before (ewoc-prev ewoc old-event-node))
            (inhibit-read-only t))
        (ewoc-delete ewoc old-event-node)
        (if node-before
            (ewoc-enter-after ewoc node-before new-event)
          (ewoc-enter-first ewoc new-event))))))

(cl-defun ement-room--ewoc-node-before (ewoc data <-fn
                                             &key (from 'last) (pred #'identity))
  "Return node in EWOC that matches PRED and goes before DATA by COMPARATOR."
  (cl-assert (member from '(first last)))
  (if (null (ewoc-nth ewoc 0))
      (ement-debug "EWOC is empty: returning nil.")
    (ement-debug "EWOC has data: add at appropriate place.")
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
              (ement-debug "New data goes before start node.")
              start-node)
          (ement-debug "New data goes after start node: find node before new data.")
          (let ((compare-node start-node))
            (cl-loop while (setf compare-node (next-matching ewoc compare-node next-fn pred))
                     until (funcall <-fn (ewoc-data compare-node) data)
                     finally return (if compare-node
                                        (progn
                                          (ement-debug "Found place: enter there.")
                                          compare-node)
                                      (ement-debug "Reached end of collection: insert there.")
                                      (pcase from
                                        ('first (ewoc-nth ewoc -1))
                                        ('last nil))))))))))

;;;;; Formatting

(defun ement-room--pp-thing (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'.  THING may be
an `ement-event' or `ement-user' struct, or a list like `(ts
TIMESTAMP)', where TIMESTAMP is a Unix timestamp number of
seconds."
  ;; TODO: Use handlers to insert so e.g. membership events can be inserted silently.
  (pcase-exhaustive thing
    ((pred ement-event-p)
     (insert "" (ement-room--format-event thing ement-room ement-session)))
    ((pred ement-user-p)
     (insert (propertize (ement-room--format-user thing)
                         'display ement-room-username-display-property)))
    (`(ts ,(and (pred numberp) ts)) ;; Insert a date header.
     (insert
      (if (equal ement-room-timestamp-header-format ement-room-timestamp-header-with-date-format)
          ;; HACK: Rather than using another variable, compare the format strings to
          ;; determine whether the date is changing: if so, add a newline before the header.
          "\n"
        "")
      (propertize (format-time-string ement-room-timestamp-header-format ts)
                  'face 'ement-room-timestamp-header)))
    ((or 'ement-room-read-receipt-marker 'ement-room-fully-read-marker)
     (insert (propertize " "
                         'display '(space :width text :height (1))
                         'face thing)))))

;; (defun ement-room--format-event (event)
;;   "Format `ement-event' EVENT."
;;   (pcase-let* (((cl-struct ement-event sender type content origin-server-ts) event)
;;                ((map body format ('formatted_body formatted-body)) content)
;;                (ts (/ origin-server-ts 1000)) ; Matrix timestamps are in milliseconds.
;;                (body (if (not formatted-body)
;;                          body
;;                        (pcase format
;;                          ("org.matrix.custom.html"
;;                           (ement-room--render-html formatted-body))
;;                          (_ (format "[unknown formatted-body format: %s] %s" format body)))))
;;                (timestamp (propertize
;;                            " " 'display `((margin left-margin)
;;                                           ,(propertize (format-time-string ement-room-timestamp-format ts)
;;                                                        'face 'ement-room-timestamp))))
;;                (body-face (pcase type
;;                             ("m.room.member" 'ement-room-membership)
;;                             (_ (if (equal (ement-user-id sender)
;;                                           (ement-user-id (ement-session-user ement-session)))
;;                                 'ement-room-self-message 'default))))
;;                (string (pcase type
;;                          ("m.room.message" body)
;;                          ("m.room.member" "")
;;                          (_ (format "[unknown event-type: %s] %s" type body)))))
;;     (add-face-text-property 0 (length body) body-face 'append body)
;;     (prog1 (concat timestamp string)
;;       ;; Hacky or elegant?  We return the string, but for certain event
;;       ;; types, we also insert a widget (this function is called by
;;       ;; EWOC with point at the insertion position).  Seems to work...
;;       (pcase type
;;         ("m.room.member"
;;          (widget-create 'ement-room-membership
;;                      :button-face 'ement-room-membership
;;                         :value (list (alist-get 'membership content))))))))

(defun ement-room--format-event (event room session)
  "Return EVENT in ROOM on SESSION formatted.
Formats according to `ement-room-message-format-spec', which see."
  (concat (pcase (ement-event-type event)
            ;; TODO: Define these with a macro, like the defevent and format-spec ones.
            ("m.room.message" (ement-room--format-message event room session))
            ("m.room.member"
             (widget-create 'ement-room-membership
                            :button-face 'ement-room-membership
                            :value event)
             "")
            ("m.reaction"
             ;; Handled by defevent-based handler.
             "")
            ("m.room.avatar"
             (propertize (format " %s changed the room's avatar"
                                 (propertize (ement-room--user-display-name (ement-event-sender event) room)
                                             'help-echo (ement-user-id (ement-event-sender event))))
                         'face 'ement-room-membership))
            (_ (propertize (format "[sender:%s type:%s]"
                                   (ement-user-id (ement-event-sender event))
                                   (ement-event-type event))
                           'help-echo (format "%S" (ement-event-content event)))))
          (propertize " "
                      'display ement-room-event-separator-display-property)))

(declare-function ement--remove-face-property "ement")
(defun ement-room--format-reactions (event)
  "Return formatted reactions to EVENT."
  ;; TODO: Like other events, pop to a buffer showing the raw reaction events when a key is pressed.
  (if-let ((reactions (map-elt (ement-event-local event) 'reactions)))
      (cl-labels ((format-reaction
                   (ks) (pcase-let* ((`(,key . ,senders) ks)
                                     (key (propertize key 'face 'ement-room-reactions-key))
                                     (count (propertize (format " (%s)" (length senders))
                                                        'face 'ement-room-reactions))
                                     (string
                                      (propertize (concat key count)
                                                  'button '(t)
                                                  'category 'default-button
                                                  'action #'ement-room-reaction-button-action
                                                  'follow-link t
                                                  'help-echo (lambda (_window buffer _pos)
                                                               (senders-names senders (buffer-local-value 'ement-room buffer))))))
                          (ement--remove-face-property string 'button)
                          string))
                  (senders-names
                   (senders room) (cl-loop for sender in senders
                                           collect (ement-room--user-display-name sender room)
                                           into names
                                           finally return (string-join names ", "))))
        (cl-loop with keys-senders
                 for reaction in reactions
                 for key = (map-nested-elt (ement-event-content reaction) '(m.relates_to key))
                 for sender = (ement-event-sender reaction)
                 do (push sender (alist-get key keys-senders nil nil #'string=))
                 finally do (setf keys-senders (cl-sort keys-senders #'> :key (lambda (pair) (length (cdr pair)))))
                 finally return (concat "\n  " (string-join (mapcar #'format-reaction keys-senders) "  "))))
    ""))

(cl-defun ement-room--format-message (event room session &optional (format ement-room-message-format-spec))
  "Return EVENT in ROOM on SESSION formatted according to FORMAT.
Format defaults to `ement-room-message-format-spec', which see."
  ;; Bind this locally so formatters can modify it for this call.
  (let ((ement-room--format-message-margin-p)
        (left-margin-width ement-room-left-margin-width)
        (right-margin-width ement-room-right-margin-width))
    ;; Copied from `format-spec'.
    (with-temp-buffer
      ;; Pretend this is a room buffer.
      (setf ement-session session
            ement-room room)
      ;; HACK: Setting these buffer-locally in a temp buffer is ugly.
      (setq-local ement-room-left-margin-width left-margin-width)
      (setq-local ement-room-right-margin-width right-margin-width)
      (insert format)
      (goto-char (point-min))
      (while (search-forward "%" nil t)
        (cond
         ;; Quoted percent sign.
         ((eq (char-after) ?%)
          (delete-char 1))
         ;; Valid format spec.
         ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
          (let* ((num (match-string 1))
                 (spec (string-to-char (match-string 2)))
                 (formatter (or (alist-get spec ement-room-event-formatters)
                                (error "Invalid format character: `%%%c'" spec)))
                 (val (or (funcall formatter event room session)
                          (let ((print-level 1))
                            (propertize (format "[Event has no value for spec \"?%s\"]" (char-to-string spec))
                                        'face 'font-lock-comment-face
                                        'help-echo (format "%S" event))))))
            ;; (setq val (cdr val))
            ;; Pad result to desired length.
            (let ((text (format (concat "%" num "s") val)))
              ;; Insert first, to preserve text properties.
              ;; (insert-and-inherit text)
              ;; ;;  Delete the specifier body.
              ;; (delete-region (+ (match-beginning 0) (string-width text))
              ;;                (+ (match-end 0) (string-width text)))
              ;; ;; Delete the percent sign.
              ;; (delete-region (1- (match-beginning 0)) (match-beginning 0))

              ;; NOTE: Actually, delete the specifier first, because it seems that if
              ;; `text' is multiline, the specifier body does not get deleted that way.
              ;; (Not sure if preserving the text properties is needed for this use case.
              ;; Leaving the old code commented in case there's a better solution.)
              (delete-region (1- (match-beginning 0)) (match-end 0))
              (insert text))))
         ;; Signal an error on bogus format strings.
         (t
          (error "Invalid format string"))))
      ;; Propertize margin text.
      (when ement-room--format-message-margin-p
        (when-let ((left-margin-end (next-single-property-change (point-min) 'left-margin-end)))
          (goto-char left-margin-end)
          (delete-char 1)
          (let ((left-margin-text-width (string-width (buffer-substring-no-properties (point-min) (point)))))
            ;; It would be preferable to not have to allocate a string to
            ;; calculate the display width, but I don't know of another way.
            (put-text-property (point-min) (point)
                               'display `((margin left-margin)
                                          ,(buffer-substring (point-min) (point))))
            (save-excursion
              (goto-char (point-min))
              ;; Insert a string with a display specification that causes it to be displayed in the
              ;; left margin as a space that displays with the width of the difference between the
              ;; left margin's width and the display width of the text in the left margin (whew).
              ;; This is complicated, but it seems to work (minus a possible Emacs/Gtk bug that
              ;; sometimes causes the space to have a little "junk" displayed in it at times, but
              ;; that's not our fault).  (And this is another example of how well-documented Emacs
              ;; is: this was only possible by carefully reading the Elisp manual.)
              (insert (propertize " " 'display `((margin left-margin)
                                                 (space :width (- left-margin ,left-margin-text-width))))))))
        (when-let ((right-margin-start (next-single-property-change (point-min) 'right-margin-start)))
          (goto-char right-margin-start)
          (delete-char 1)
          (let ((string (buffer-substring (point) (point-max))))
            ;; Relocate its text to the beginning so it won't be
            ;; displayed at the last line of wrapped messages.
            (delete-region (point) (point-max))
            (goto-char (point-min))
            (insert-and-inherit
             (propertize " "
                         'display `((margin right-margin) ,string))))))
      (buffer-string))))

(cl-defun ement-room--format-message-body (event &key (formatted-p t))
  "Return formatted body of \"m.room.message\" EVENT.
If FORMATTED-P, return the formatted body content, when available."
  (pcase-let* (((cl-struct ement-event content) event)
               ((map body msgtype ('format content-format) ('formatted_body formatted-body)
                     ('m.relates_to (map ('rel_type rel-type))))
                content)
               (body (if (or (not formatted-p) (not formatted-body))
                         ;; Copy the string so as not to add face properties to the one in the struct.
                         (copy-sequence body)
                       (pcase content-format
                         ("org.matrix.custom.html"
                          (save-match-data
                            (ement-room--render-html formatted-body)))
                         (_ (format "[unknown body format: %s] %s"
                                    content-format body)))))
               (appendix (pcase msgtype
                           ("m.image" (ement-room--format-m.image event))
                           (_ nil))))
    (when body
      ;; HACK: Once I got an error when body was nil, so let's avoid that.
      (setf body (ement-room--linkify-urls body)))
    ;; HACK: Ensure body isn't nil (e.g. redacted messages can have empty bodies).
    (unless body
      (setf body "[message has no body content]"))
    (when appendix
      (setf body (concat body " " appendix)))
    (when (equal "m.replace" rel-type)
      ;; Message is an edit.
      (setf body (concat body " " (propertize "[edited]" 'face 'font-lock-comment-face))))
    body))

(defun ement-room--render-html (string)
  "Return rendered version of HTML STRING.
HTML is rendered to Emacs text using `shr-insert-document'."
  (with-temp-buffer
    (insert string)
    (save-excursion
      ;; NOTE: We workaround `shr`'s not indenting the blockquote properly (it
      ;; doesn't seem to compensate for the margin).  I don't know exactly how
      ;; `shr-tag-blockquote' and `shr-mark-fill' and `shr-fill-line' and
      ;; `shr-indentation' work together, but through trial-and-error, this
      ;; seems to work.  It even seems to work properly when a window is
      ;; resized (i.e. the wrapping is adjusted automatically by redisplay
      ;; rather than requiring the message to be re-rendered to HTML).
      (let ((old-fn (symbol-function 'shr-tag-blockquote))) ;; Bind to a var to avoid unknown-function linting errors.
        (cl-letf (((symbol-function 'shr-fill-line) #'ignore)
                  ((symbol-function 'shr-tag-blockquote)
                   (lambda (dom)
                     (let ((beg (point-marker)))
                       (funcall old-fn dom)
                       (add-text-properties beg (point-max)
                                            '(wrap-prefix "    "
                                                          line-prefix "    "))))))
          (shr-insert-document
           (libxml-parse-html-region (point-min) (point-max))))))
    (string-trim (buffer-substring (point) (point-max)))))

(cl-defun ement-room--format-user (user &optional (room ement-room))
  "Format `ement-user' USER for ROOM.
ROOM defaults to the value of `ement-room'."
  (let ((face (cond ((equal (ement-user-id (ement-session-user ement-session))
                            (ement-user-id user))
                     'ement-room-self)
                    (ement-room-prism
                     `(:inherit ement-room-user :foreground ,(or (ement-user-color user)
                                                                 (setf (ement-user-color user)
                                                                       (ement-room--user-color user)))))
                    (t 'ement-room-user))))
    ;; FIXME: If a membership state event has not yet been received, this
    ;; sets the display name in the room to the user ID, and that prevents
    ;; the display name from being used if the state event arrives later.
    (propertize (ement-room--user-display-name user room)
                'face face
                'help-echo (ement-user-id user))))

(cl-defun ement-room--event-mentions-user-p (event user &optional (room ement-room))
  "Return non-nil if EVENT mentions USER."
  (pcase-let* (((cl-struct ement-event content) event)
               ((map body formatted_body) content)
               (body (or formatted_body body)))
    ;; FIXME: `ement-room--user-display-name' may not be returning the
    ;; right result for the local user, so test the displayname slot too.
    ;; HACK: So we use the username slot, which was created just for this, for now.
    (when body
      (cl-macrolet ((matches-body-p
                     (form) `(when-let ((string ,form))
                               (string-match-p (regexp-quote string) body))))
        (or (matches-body-p (ement-user-username user))
            (matches-body-p (ement-room--user-display-name user room))
            (matches-body-p (ement-user-id user)))))))

(defun ement-room--linkify-urls (string)
  "Return STRING with URLs in it made clickable."
  ;; Is there an existing Emacs function to do this?  I couldn't find one.
  ;; Yes, maybe: `goto-address-mode'.  TODO: Try goto-address-mode.
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space)))
                                      nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

;; NOTE: This function is not useful when displaynames are shown in the margin, because
;; margins are not mouse-interactive in Emacs, therefore the help-echo function is called
;; with the string and the position in the string, which leaves the buffer position
;; unknown.  So we have to set the help-echo to a string rather than a function.  But the
;; function may be useful in the future, so leaving it commented for now.

;; (defun ement-room--user-help-echo (window _object pos)
;;   "Return user ID string for POS in WINDOW.
;; For use as a `help-echo' function on `ement-user' headings."
;;   (let ((data (with-selected-window window
;;                 (ewoc-data (ewoc-locate ement-ewoc pos)))))
;;     (cl-typecase data
;;       (ement-event (ement-user-id (ement-event-sender data)))
;;       (ement-user (ement-user-id data)))))

(defun ement-room--user-color (user)
  "Return a color in which to display USER's messages."
  (cl-labels ((relative-luminance
               ;; Copy of `modus-themes-wcag-formula', an elegant
               ;; implementation by Protesilaos Stavrou.  Also see
               ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
               ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
               (rgb) (cl-loop for k in '(0.2126 0.7152 0.0722)
                              for x in rgb
                              sum (* k (if (<= x 0.03928)
                                           (/ x 12.92)
                                         (expt (/ (+ x 0.055) 1.055) 2.4)))))
              (contrast-ratio
               ;; Copy of `modus-themes-contrast'; see above.
               (a b) (let ((ct (/ (+ (relative-luminance a) 0.05)
                                  (+ (relative-luminance b) 0.05))))
                       (max ct (/ ct))))
              (increase-contrast
               (color against target toward)
               (let ((gradient (cdr (color-gradient color toward 20)))
                     new-color)
                 (cl-loop do (setf new-color (pop gradient))
                          while new-color
                          until (>= (contrast-ratio new-color against) target)
                          ;; Avoid infinite loop in case of weirdness
                          ;; by returning color as a fallback.
                          finally return (or new-color color)))))
    (let* ((id (ement-user-id user))
           (id-hash (float (+ (abs (sxhash id)) ement-room-prism-color-adjustment)))
           ;; TODO: Wrap-around the value to get the color I want.
           (ratio (/ id-hash (float most-positive-fixnum)))
           (color-num (round (* (* 255 255 255) ratio)))
           (color-rgb (list (/ (float (logand color-num 255)) 255)
                            (/ (float (lsh (logand color-num 65280) -8)) 255)
                            (/ (float (lsh (logand color-num 16711680) -16)) 255)))
           (background-rgb (color-name-to-rgb (face-background 'default))))
      (when (< (contrast-ratio color-rgb background-rgb) ement-room-prism-minimum-contrast)
        (setf color-rgb (increase-contrast color-rgb background-rgb ement-room-prism-minimum-contrast
                                           (color-name-to-rgb (face-foreground 'default)))))
      (apply #'color-rgb-to-hex (append color-rgb (list 2))))))

;;;;; Compose buffer

;; Compose messages in a separate buffer, like `org-edit-special'.

(defvar-local ement-room-compose-buffer nil
  "Non-nil in buffers that are composing a message to a room.")

(cl-defun ement-room-compose-message (room session &key body)
  "Compose a message to ROOM on SESSION.
Interactively, compose to the current buffer's room.  With BODY,
use it as the initial message contents."
  (interactive (progn
                 (cl-assert ement-room) (cl-assert ement-session)
                 (list ement-room ement-session)))
  (let* ((compose-buffer (generate-new-buffer (format "*Ement compose: %s*" (ement-room--room-display-name ement-room))))
         (send-message-filter ement-room-send-message-filter))
    (with-current-buffer compose-buffer
      (ement-room-init-compose-buffer room session)
      (setf ement-room-send-message-filter send-message-filter)
      ;; TODO: Make mode configurable.
      (when body
        (insert body))

      ;; FIXME: Inexplicably, this doesn't do anything, so we comment it out for now.
      ;; (add-function :override (local 'org-mode)
      ;;               ;; HACK: Since `org-mode' kills buffer-local variables we need, we add
      ;;               ;; buffer-local advice to prevent that from happening in case a user enables it.
      ;;               (lambda (&rest _ignore)
      ;;                 (message "Use `ement-room-compose-org' to activate Org in this buffer")))

      ;; NOTE: Surprisingly, we don't run this hook in `ement-room-init-compose-buffer',
      ;; because if a function in that hook calls the init function (like
      ;; `ement-room-compose-org' does), it makes `run-hooks' recursive.  As long as this
      ;; is the only function that makes the compose buffer, and as long as none of the
      ;; hooks do anything that activating `org-mode' nullifies, this should be okay...
      (run-hooks 'ement-room-compose-hook))
    (pop-to-buffer compose-buffer)))

(defun ement-room-compose-from-minibuffer ()
  "Edit the current message in a compose buffer.
To be called from a minibuffer opened from
`ement-room-read-string'."
  (interactive)
  (cl-assert (minibufferp)) (cl-assert ement-room) (cl-assert ement-session)
  ;; TODO: When requiring Emacs 27, use `letrec'.
  ;; HACK: I can't seem to find a better way to do this, to exit the minibuffer without exiting this command too.
  (let* ((body (minibuffer-contents))
         (compose-fn-symbol (gensym (format "ement-compose-%s" (or (ement-room-canonical-alias ement-room)
                                                                   (ement-room-id ement-room)))))
         (input-method current-input-method) ; Capture this value from the minibuffer.
         (send-message-filter ement-room-send-message-filter)
         (compose-fn (lambda ()
                       ;; HACK: Since exiting the minibuffer restores the previous window configuration,
                       ;; we have to do some magic to get the new compose buffer to appear.
                       ;; TODO: Use letrec with Emacs 27.
                       (remove-hook 'minibuffer-exit-hook compose-fn-symbol)
                       ;; FIXME: Probably need to unintern the symbol.
                       (ement-room-compose-message ement-room ement-session :body body)
		       ;; FIXME: This doesn't propagate the send-message-filter to the minibuffer.
                       (setf ement-room-send-message-filter send-message-filter)
                       (let* ((compose-buffer (current-buffer))
                              (show-buffer-fn-symbol (gensym "ement-show-compose-buffer"))
                              (show-buffer-fn (lambda ()
                                                (remove-hook 'window-configuration-change-hook show-buffer-fn-symbol)
                                                ;; FIXME: Probably need to unintern the symbol.
                                                (pop-to-buffer compose-buffer)
                                                (set-input-method input-method))))
                         (fset show-buffer-fn-symbol show-buffer-fn)
                         (add-hook 'window-configuration-change-hook show-buffer-fn-symbol)))))
    (fset compose-fn-symbol compose-fn)
    (add-hook 'minibuffer-exit-hook compose-fn-symbol)
    ;; Deactivate minibuffer's input method, otherwise subsequent
    ;; minibuffers will have it, too.
    (deactivate-input-method)
    (abort-recursive-edit)))

(defun ement-room-compose-send ()
  "Prompt to send the current compose buffer's contents.
To be called from an `ement-room-compose' buffer."
  (interactive)
  (cl-assert ement-room-compose-buffer)
  (cl-assert ement-room) (cl-assert ement-session)
  ;; Putting it in the kill ring seems like the best thing to do, to ensure
  ;; it doesn't get lost if the user exits the minibuffer before sending.
  (kill-new (string-trim (buffer-string)))
  (let ((room ement-room)
        (session ement-session)
        (input-method current-input-method)
        (send-message-filter ement-room-send-message-filter))
    (quit-restore-window nil 'kill)
    (ement-view-room room session)
    (let* ((prompt (format "Send message (%s): " (ement-room-display-name ement-room)))
           (current-input-method input-method) ; Bind around read-string call.
           (ement-room-send-message-filter send-message-filter)
           (body (ement-room-read-string prompt (car kill-ring) nil nil 'inherit-input-method)))
      (ement-room-send-message ement-room ement-session :body body))))

(defun ement-room-init-compose-buffer (room session)
  "Eval BODY, setting up the current buffer as a compose buffer.
Sets ROOM and SESSION buffer-locally, binds `save-buffer' in
a copy of the local keymap, and sets `header-line-format'."
  ;; Using a macro for this seems awkward but necessary.
  (setq-local ement-room room)
  (setq-local ement-session session)
  (setf ement-room-compose-buffer t)
  ;; FIXME: Compose with local map?
  (use-local-map (if (current-local-map)
                     (copy-keymap (current-local-map))
                   (make-sparse-keymap)))
  (local-set-key [remap save-buffer] #'ement-room-compose-send)
  (setq header-line-format (substitute-command-keys
                            (format " Press \\[save-buffer] to send message to room (%s)"
                                    (ement-room-display-name room)))))

;;;;; Widgets

(require 'widget)

(define-widget 'ement-room-membership 'item
  "Widget for membership events."
  ;; FIXME: This makes it hard to add a timestamp according to the buffer's message format spec.
  :format "%{ %v %}"
  :sample-face 'ement-room-membership
  :value-create (lambda (widget)
                  (pcase-let* ((event (widget-value widget)))
                    (insert (ement-room--format-member-event event)))))

(defun ement-room--format-member-event (event)
  "Return formatted string for \"m.room.member\" EVENT."
  ;; SPEC: Section 9.3.4: "m.room.member".
  (pcase-let* (((cl-struct ement-event sender state-key
                           (content (map reason ('membership new-membership) ('displayname new-displayname)))
                           (unsigned (map ('prev_content (map ('membership prev-membership)
                                                              ('displayname prev-displayname))))))
                event)
               (sender-name (ement-room--user-display-name sender ement-room)))
    (cl-macrolet ((sender-name-id-string
                   () `(propertize sender-name
                                   'help-echo (ement-user-id sender)))
                  (new-displayname-sender-name-state-key-string
                   () `(propertize (or new-displayname sender-name state-key)
                                   'help-echo state-key))
                  (sender-name-state-key-string
                   () `(propertize sender-name
                                   'help-echo state-key))
                  (prev-displayname-id-string
                   () `(propertize (or prev-displayname sender-name)
                                   'help-echo (ement-user-id sender))))
      (pcase-exhaustive new-membership
        ("invite"
         (pcase prev-membership
           ((or "leave" '())
            (format "%s invited %s"
                    (sender-name-id-string)
                    (new-displayname-sender-name-state-key-string)))
           (_ (format "%s sent unrecognized invite event for %s"
                      (sender-name-id-string)
                      (new-displayname-sender-name-state-key-string)))))
        ("join"
         (pcase prev-membership
           ("invite"
            (format "%s accepted invitation to join"
                    (sender-name-state-key-string)))
           ("join"
            (cond ((and new-displayname prev-displayname
                        (not (equal new-displayname prev-displayname)))
                   (propertize (format "%s changed name to %s"
                                       prev-displayname new-displayname)
                               'help-echo state-key))
                  (t (format "%s changed avatar"
                             (new-displayname-sender-name-state-key-string)))))
           ("leave"
            (format "%s rejoined"
                    (sender-name-state-key-string)))
           ('()
            (format "%s joined"
                    (new-displayname-sender-name-state-key-string)))
           (_ (format "%s sent unrecognized join event for %s"
                      (sender-name-id-string)
                      (new-displayname-sender-name-state-key-string)))))
        ("leave"
         (pcase prev-membership
           ("invite"
            (pcase state-key
              ((pred (equal (ement-user-id sender)))
               (format "%s rejected invitation"
                       (sender-name-id-string)))
              (_ (format "%s revoked %s's invitation"
                         (sender-name-id-string)
                         (new-displayname-sender-name-state-key-string)))))
           ("join"
            (pcase state-key
              ((pred (equal (ement-user-id sender)))
               (format "%s left%s"
                       (prev-displayname-id-string)
                       (if reason
                           (format " (%s)" reason)
                         "")))
              (_ (format "%s kicked %s%s"
                         (sender-name-id-string)
                         (propertize (or prev-displayname state-key)
                                     'help-echo state-key)
                         (if reason
                             (format " (%s)" reason)
                           "")))))
           ("ban"
            (format "%s unbanned %s"
                    (sender-name-id-string)
                    (new-displayname-sender-name-state-key-string)))
           (_ (format "%s left%s"
                      (prev-displayname-id-string)
                      (if reason
                          (format " (%s)" reason)
                        "")))))
        ("ban"
         (pcase prev-membership
           ((or "invite" "leave")
            (format "%s banned %s%s"
                    (sender-name-id-string)
                    (propertize (or prev-displayname state-key)
                                'help-echo state-key)
                    (if reason
                        (format " (%s)" reason)
                      "")))
           ("join"
            (format "%s kicked and banned %s%s"
                    (sender-name-id-string)
                    (propertize (or prev-displayname state-key)
                                'help-echo state-key)
                    (if reason
                        (format " (%s)" reason)
                      "")))
           (_ (format "%s sent unrecognized ban event for %s"
                      (sender-name-id-string)
                      (propertize (or prev-displayname state-key)
                                  'help-echo state-key)))))))))

;;;;; Images

;; Downloading and displaying images in messages, room/user avatars, etc.

(require 'image)

(defvar ement-room-image-keymap
  (let ((map (make-sparse-keymap)))
    ;; TODO: Make RET work for showing images too.
    ;; (define-key map (kbd "RET") #'ement-room-image-show)
    (define-key map [mouse-1] #'ement-room-image-scale-mouse)
    (define-key map [double-mouse-1] #'ement-room-image-show)
    map)
  "Keymap for images in room buffers.")

(defcustom ement-room-images t
  "Download and show images in messages, avatars, etc."
  :type 'boolean
  :set (lambda (option value)
         (if (or (fboundp 'imagemagick-types)
                 (when (fboundp 'image-transforms-p)
                   (image-transforms-p)))
             (set-default option value)
           (set-default option nil)
           (when value
             (display-warning 'ement "This Emacs was not built with ImageMagick support, nor does it support Cairo/XRender scaling, so images can't be displayed in Ement")))))

(defcustom ement-room-image-initial-height 0.2
  "Limit images' initial display height.
If a number, it should be no larger than 1 (because Emacs can't
display images larger than the window body height)."
  :type '(choice (const :tag "Use full window width" nil)
                 (number :tag "Limit to this multiple of the window body height")))

(defun ement-room-image-scale-mouse (event)
  "Toggle scale of image at mouse EVENT.
Scale image to fit within the window's body.  If image is already
fit to the window, reduce its max-height to 10% of the window's
height."
  (interactive "e")
  (pcase-let* ((`(,_type ,position ,_count) event)
               (window (posn-window position))
               (pos (event-start position)))
    (with-selected-window window
      (pcase-let* ((image (get-text-property pos 'display))
                   (window-width (window-body-width nil t))
                   (window-height (window-body-height nil t))
                   (new-height (if (= window-height (image-property image :max-height))
                                   (/ window-height 10)
                                 window-height)))
        (when (fboundp 'imagemagick-types)
          ;; Only do this when ImageMagick is supported.
          ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
          (setf (image-property image :type) 'imagemagick))
        (setf (image-property image :max-width) window-width
              (image-property image :max-height) new-height)))))

(defun ement-room-image-show (event)
  "Show image at mouse EVENT in a new buffer."
  (interactive "e")
  (pcase-let* ((`(,_type ,position ,_count) event)
               (window (posn-window position)))
    (with-current-buffer (window-buffer window)
      (pcase-let* ((pos (event-start position))
                   (image (copy-sequence (get-text-property pos 'display)))
                   (ement-event (ewoc-data (ewoc-locate ement-ewoc pos)))
                   ((cl-struct ement-event id) ement-event)
                   (buffer-name (format "*Ement image: %s*" id))
                   (new-buffer (get-buffer-create buffer-name)))
        (when (fboundp 'imagemagick-types)
          ;; Only do this when ImageMagick is supported.
          ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
          (setf (image-property image :type) 'imagemagick))
        (setf (image-property image :scale) 1.0
              (image-property image :max-width) nil
              (image-property image :max-height) nil)
        (with-current-buffer new-buffer
          (erase-buffer)
          (insert-image image))
        (pop-to-buffer new-buffer '((display-buffer-pop-up-frame)))
        (set-frame-parameter nil 'fullscreen 'maximized)))))

(declare-function ement--mxc-to-url "ement.el")
(defun ement-room--format-m.image (event)
  "Return \"m.image\" EVENT formatted as a string.
When `ement-room-images' is non-nil, also download it and then
show it in the buffer."
  (pcase-let* (((cl-struct ement-event content (local event-local)) event)
               ;; HACK: Get the room's buffer from the variable (the current buffer
               ;; will be a temp formatting buffer when this is called, but it still
               ;; inherits the `ement-room' variable from the room buffer, thankfully).
               ((cl-struct ement-room local) ement-room)
               ((map buffer) local)
               ;; TODO: Thumbnail support.
               ((map ('url mxc) info ;; ('thumbnail_url thumbnail-url)
                     ) content)
               ((map thumbnail_info) info)
               ((map ('h _thumbnail-height) ('w _thumbnail-width)) thumbnail_info)
               ((map image) event-local)
               (url (when mxc
                      (ement--mxc-to-url mxc ement-session)))
               ;; (thumbnail-url (ement--mxc-to-url thumbnail-url ement-session))
               )
    (if (and ement-room-images image)
        ;; Images enabled and image downloaded: create image and
        ;; return it in a string.
        (condition-case err
            (let ((image (create-image image nil 'data-p :ascent 'center))
                  (buffer-window (when buffer
                                   (get-buffer-window buffer)))
                  max-height max-width)
              ;; Calculate max image display size.
              (cond (ement-room-image-initial-height
                     ;; Use configured value.
                     (setf max-height (truncate
                                       ;; Emacs doesn't like floats as the max-height.
                                       (* (window-body-height buffer-window t)
                                          ement-room-image-initial-height))
                           max-width (window-body-width buffer-window t)))
                    (buffer-window
                     ;; Buffer displayed: use window size.
                     (setf max-height (window-body-height buffer-window t)
                           max-width (window-body-width buffer-window t)))
                    (t
                     ;; Buffer not displayed: use frame size.
                     (setf max-height (frame-pixel-height)
                           max-width (frame-pixel-width))))
              (when (fboundp 'imagemagick-types)
                ;; Only do this when ImageMagick is supported.
                ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                (setf (image-property image :type) 'imagemagick))
              (setf (image-property image :max-width) max-width
                    (image-property image :max-height) max-height)
              (concat "\n"
                      (propertize " " 'display image
                                  'keymap ement-room-image-keymap)))
          (error (format "\n [error inserting image: %s]" (error-message-string err))))
      ;; Image not downloaded: insert URL as button, and download if enabled.
      (prog1
          (with-temp-buffer
            (insert-text-button (or url "[no URL for image]")
                                'face 'link
                                'follow-link t)
            (buffer-string))
        (when (and ement-room-images url)
          ;; Images enabled and URL present: download it.
          (plz 'get url :as 'binary
            :then (apply-partially #'ement-room--m.image-callback event ement-room)
            :noquery t))))))

(defun ement-room--m.image-callback (event room data)
  "Add downloaded image from DATA to EVENT in ROOM.
Then invalidate EVENT's node to show the image."
  (pcase-let* (((cl-struct ement-room (local (map buffer))) room))
    (setf (map-elt (ement-event-local event) 'image) data)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ewoc-invalidate ement-ewoc (ement-room--ewoc-last-matching ement-ewoc
                                      (lambda (node-data)
                                        (eq node-data event))))))))

;;;;; Org format sending

;; Some of these declarations may need updating as Org changes.

(defvar org-export-with-toc)
(defvar org-export-with-broken-links)
(defvar org-export-with-section-numbers)
(defvar org-html-inline-images)

(declare-function org-element-property "org-element")
(declare-function org-export-data "ox")
(declare-function org-export-get-caption "ox")
(declare-function org-export-get-ordinal "ox")
(declare-function org-export-get-reference "ox")
(declare-function org-export-read-attribute "ox")
(declare-function org-html--has-caption-p "ox-html")
(declare-function org-html--textarea-block "ox-html")
(declare-function org-html--translate "ox-html")
(declare-function org-html-export-as-html "ox-html")
(declare-function org-html-format-code "ox-html")
(declare-function org-trim "org")

(defun ement-room-compose-org ()
  "Activate `org-mode' in current compose buffer.
Configures the buffer appropriately so that saving it will export
the Org buffer's contents."
  (interactive)
  (unless ement-room-compose-buffer
    (user-error "This command should be run in a compose buffer.  Use `ement-room-compose-message' first"))
  ;; Calling `org-mode' seems to wipe out local variables.
  (let ((room ement-room)
        (session ement-session))
    (org-mode)
    (ement-room-init-compose-buffer room session))
  (setq-local ement-room-send-message-filter #'ement-room-send-org-filter))

(defun ement-room-send-org-filter (content)
  "Return event CONTENT having processed its Org content.
The CONTENT's body is exported with
`org-html-export-as-html' (with some adjustments for
compatibility), and the result is added to the CONTENT as
\"formatted_body\"."
  (require 'ox-html)
  ;; The CONTENT alist has string keys before being sent.
  (pcase-let* ((body (alist-get "body" content nil nil #'equal))
               (formatted-body
                (save-window-excursion
                  (with-temp-buffer
                    (insert body)
                    (cl-letf (((symbol-function 'org-html-src-block)
                               (symbol-function 'ement-room--org-html-src-block)))
                      (let ((org-export-with-toc nil)
                            (org-export-with-broken-links t)
                            (org-export-with-section-numbers nil)
                            (org-html-inline-images nil))
                        (org-html-export-as-html nil nil nil 'body-only)))
                    (with-current-buffer "*Org HTML Export*"
                      (prog1 (string-trim (buffer-string))
                        (kill-buffer)))))))
    (push (cons "formatted_body" formatted-body) content)
    (push (cons "format" "org.matrix.custom.html") content)
    content))

(defun ement-room--org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is a copy of `org-html-src-block' that uses Riot
Web-compatible HTML output, using HTML like:

<pre><code class=\"language-python\">..."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (pcase (org-element-property :language src-block)
                  ;; Riot's syntax coloring doesn't support "elisp", but "lisp" works.
                  ("elisp" "lisp")
                  (else else)))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (and (org-element-property :name src-block)
				 (org-export-get-reference src-block info))))
		   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format "<div class=\"org-src-container\">\n%s%s\n</div>"
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (org-trim (org-export-data caption info))))))
		;; Contents.
		(format "<pre><code class=\"src language-%s\"%s>%s</code></pre>"
			lang label code))))))

;;;;; Completion

;; Completing member and room names.

(defun ement-room--complete-members-at-point ()
  "Complete member names and IDs at point.
Uses members in the current buffer's room.  For use in
`completion-at-point-functions'."
  (let ((beg (save-excursion
               (re-search-backward (rx (or bol bos blank)))
               (1+ (point))))
        (end (point))
        (collection-fn (completion-table-dynamic
                        ;; The manual seems to show the FUN ignoring any
                        ;; arguments, but the `completion-table-dynamic' docstring
                        ;; seems to say that it should use the argument.
                        (lambda (_ignore)
                          (ement-room--member-names-and-ids)))))
    (list beg end collection-fn :exclusive 'no)))

(defun ement-room--complete-rooms-at-point ()
  "Complete room aliases and IDs at point.
For use in `completion-at-point-functions'."
  (let ((beg (save-excursion
               (re-search-backward (rx (or bol bos blank)))
               (1+ (point))))
        (end (point))
        (collection-fn (completion-table-dynamic
                        ;; The manual seems to show the FUN ignoring any
                        ;; arguments, but the `completion-table-dynamic' docstring
                        ;; seems to say that it should use the argument.
                        (lambda (_ignore)
                          (ement-room--room-aliases-and-ids)))))
    (list beg end collection-fn :exclusive 'no)))

(defun ement-room--member-names-and-ids ()
  "Return a list of member names and IDs seen in current room.
For use in `completion-at-point-functions'."
  ;; For now, we just collect a list of members from events we've seen.
  ;; TODO: In the future, we may maintain a per-room table of members, which
  ;; would be more suitable for completing names according to the spec.
  (let* ((room (if (minibufferp)
                   (buffer-local-value
                    'ement-room (window-buffer (minibuffer-selected-window)))
                 ement-room))
         (ewoc (if (minibufferp)
                   (buffer-local-value
                    'ement-ewoc (window-buffer (minibuffer-selected-window)))
                 ement-ewoc))
         (members-seen (mapcar #'ement-event-sender
                               (ewoc-collect ewoc #'ement-event-p))))
    (delete-dups
     (cl-loop for member in members-seen
              collect (ement-user-id member)
              collect (ement-room--user-display-name member room)))))

(defun ement-room--room-aliases-and-ids ()
  "Return a list of room names and aliases seen in current session.
For use in `completion-at-point-functions'."
  (let* ((session (if (minibufferp)
                      (buffer-local-value
                       'ement-session (window-buffer (minibuffer-selected-window)))
                    ement-session)))
    (delete-dups
     (delq nil (cl-loop for room in (ement-session-rooms session)
                        collect (ement-room-id room)
                        collect (ement-room-canonical-alias room))))))

;;;; Footer

(provide 'ement-room)

;;; ement-room.el ends here
