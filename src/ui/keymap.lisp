(in-package #:clatter.ui.keymap)

(defun %set-current-buffer (app id)
  (setf (app-current-buffer-id app)
        (mod id (length (app-buffers app))))
  (mark-dirty app :chat :buflist :status :input))

(defun %buf-next (app)
  (%set-current-buffer app (1+ (app-current-buffer-id app))))

(defun %buf-prev (app)
  (%set-current-buffer app (1- (app-current-buffer-id app))))

(defun %scroll-up (app &optional (n 5))
  (let ((buf (current-buffer app)))
    (incf (buffer-scroll-offset buf) n)
    (mark-dirty app :chat)))

(defun %scroll-down (app &optional (n 5))
  (let ((buf (current-buffer app)))
    (setf (buffer-scroll-offset buf)
          (max 0 (- (buffer-scroll-offset buf) n)))
    (mark-dirty app :chat)))

(defun install-keybindings (screen app)
  ;; quit with F10 or Esc-q (use /quit command as primary method)
  (de.anvi.croatoan:bind screen :key-f10 #'de.anvi.croatoan:exit-event-loop)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\q) 'string) #'de.anvi.croatoan:exit-event-loop)

  ;; idle/tick: drain submit queue + redraw + check quit
  (de.anvi.croatoan:bind screen nil
    (lambda (obj event)
      (declare (ignore obj event))
      (de.anvi.croatoan:process)
      (when (app-quit-requested app)
        (de.anvi.croatoan:exit-event-loop screen))
      (when (dirty-p app)
        (render-frame app))))

  ;; basic editing - multiple backspace bindings for different terminals
  (de.anvi.croatoan:bind screen #\Backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen #\Rubout (lambda (o e) (declare (ignore o e)) (input-backspace app)))  ;; Rubout = DEL
  (de.anvi.croatoan:bind screen (code-char 127) (lambda (o e) (declare (ignore o e)) (input-backspace app)))  ;; DEL
  (de.anvi.croatoan:bind screen (code-char 8) (lambda (o e) (declare (ignore o e)) (input-backspace app)))    ;; Ctrl-H / BS
  (de.anvi.croatoan:bind screen :backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen :key-backspace (lambda (o e) (declare (ignore o e)) (input-backspace app)))
  (de.anvi.croatoan:bind screen :dc (lambda (o e) (declare (ignore o e)) (input-delete app)))
  (de.anvi.croatoan:bind screen :key-dc (lambda (o e) (declare (ignore o e)) (input-delete app)))  ;; Delete key
  (de.anvi.croatoan:bind screen :key-left (lambda (o e) (declare (ignore o e)) (input-move-left app)))
  (de.anvi.croatoan:bind screen :key-right (lambda (o e) (declare (ignore o e)) (input-move-right app)))
  (de.anvi.croatoan:bind screen :key-home (lambda (o e) (declare (ignore o e)) (input-move-home app)))
  (de.anvi.croatoan:bind screen :key-end (lambda (o e) (declare (ignore o e)) (input-move-end app)))
  (de.anvi.croatoan:bind screen :key-up (lambda (o e) (declare (ignore o e)) (input-history-prev app)))
  (de.anvi.croatoan:bind screen :key-down (lambda (o e) (declare (ignore o e)) (input-history-next app)))
  (de.anvi.croatoan:bind screen #\Newline (lambda (o e) (declare (ignore o e)) (input-submit-line app)))
  (de.anvi.croatoan:bind screen #\Return (lambda (o e) (declare (ignore o e)) (input-submit-line app)))
  (de.anvi.croatoan:bind screen #\Tab (lambda (o e) (declare (ignore o e)) (input-tab-complete app)))

  ;; buffer nav (WeeChat-ish fallbacks)
  (de.anvi.croatoan:bind screen :key-ppage (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))
  (de.anvi.croatoan:bind screen :key-npage (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))
  (de.anvi.croatoan:bind screen (code-char 16) (lambda (o e) (declare (ignore o e)) (%buf-prev app)))  ;; Ctrl-P
  (de.anvi.croatoan:bind screen (code-char 14) (lambda (o e) (declare (ignore o e)) (%buf-next app)))  ;; Ctrl-N

  ;; raw escape sequence binds (Ghostty/xterm)
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\A) 'string) (lambda (o e) (declare (ignore o e)) (input-history-prev app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\B) 'string) (lambda (o e) (declare (ignore o e)) (input-history-next app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\D) 'string) (lambda (o e) (declare (ignore o e)) (input-move-left app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\C) 'string) (lambda (o e) (declare (ignore o e)) (input-move-right app)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\5 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-up app 10)))
  (de.anvi.croatoan:bind screen (coerce '(#\Esc #\[ #\6 #\~) 'string) (lambda (o e) (declare (ignore o e)) (%scroll-down app 10)))

  ;; Default handler for all other events - handles printable characters
  ;; This catches any character the terminal sends, including Shift+letter -> uppercase
  (de.anvi.croatoan:bind screen t
    (lambda (o e)
      (declare (ignore o))
      (let ((key (de.anvi.croatoan:event-key e)))
        (when (and (characterp key)
                   (graphic-char-p key))
          (input-insert-char app key))))))
