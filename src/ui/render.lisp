(in-package #:clatter.ui.render)

;; Nick color palette - bright colors that work on dark backgrounds
(defparameter *nick-colors*
  '(:red :green :yellow :blue :magenta :cyan
    :lime :maroon :olive :navy :purple :teal))

(defun nick-color (nick)
  "Hash a nick to a consistent color from the palette."
  (let ((hash (reduce #'+ (map 'list #'char-code nick))))
    (nth (mod hash (length *nick-colors*)) *nick-colors*)))

(defun %ui (app) (app-ui app))

(defun %draw-line (win y x text)
  (setf (de.anvi.croatoan:cursor-position-y win) y)
  (setf (de.anvi.croatoan:cursor-position-x win) x)
  (format win "~a" text))

;; Unicode box-drawing characters for thin borders
(defparameter *box-h* #\─)      ;; horizontal
(defparameter *box-v* #\│)      ;; vertical
(defparameter *box-tl* #\┌)     ;; top-left
(defparameter *box-tr* #\┐)     ;; top-right
(defparameter *box-bl* #\└)     ;; bottom-left
(defparameter *box-br* #\┘)     ;; bottom-right

(defun %draw-box (win)
  "Draw a thin Unicode box border around the window content area."
  (let ((h (de.anvi.croatoan:height win))
        (w (de.anvi.croatoan:width win)))
    (when (and (> h 1) (> w 1))
      ;; top border
      (setf (de.anvi.croatoan:cursor-position-y win) 0)
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" *box-tl*)
      (loop repeat (- w 2) do (format win "~a" *box-h*))
      (format win "~a" *box-tr*)
      ;; bottom border
      (setf (de.anvi.croatoan:cursor-position-y win) (1- h))
      (setf (de.anvi.croatoan:cursor-position-x win) 0)
      (format win "~a" *box-bl*)
      (loop repeat (- w 2) do (format win "~a" *box-h*))
      (format win "~a" *box-br*)
      ;; side borders
      (loop for y from 1 below (1- h) do
        (setf (de.anvi.croatoan:cursor-position-y win) y)
        (setf (de.anvi.croatoan:cursor-position-x win) 0)
        (format win "~a" *box-v*)
        (setf (de.anvi.croatoan:cursor-position-x win) (1- w))
        (format win "~a" *box-v*)))))

(defun %clear-and-border (win &optional (title nil) (draw-border t))
  (de.anvi.croatoan:clear win)
  (when draw-border
    (%draw-box win))
  (when (and title draw-border)
    ;; Draw title in top border
    (let ((w (de.anvi.croatoan:width win))
          (tlen (length title)))
      (when (< (+ tlen 4) w)
        (setf (de.anvi.croatoan:cursor-position-y win) 0)
        (setf (de.anvi.croatoan:cursor-position-x win) 2)
        (format win "~a" title)))))

(defun render-frame (app)
  (unless (dirty-p app)
    (return-from render-frame app))

  (let* ((ui (%ui app))
         (wbuf (ui-win-buflist ui))
         (wchat (ui-win-chat ui))
         (wstatus (ui-win-status ui))
         (winput (ui-win-input ui)))

    ;; clear and draw borders
    (%clear-and-border wbuf " buffers ")
    (%clear-and-border wchat nil)  ;; no title, cleaner look
    (%clear-and-border wstatus nil nil)  ;; no border for status
    (%clear-and-border winput nil nil)   ;; no border for input

    ;; buffer list (inside border: start at y=1, x=1)
    (let ((content-w (- (de.anvi.croatoan:width wbuf) 2))
          (content-h (- (de.anvi.croatoan:height wbuf) 2)))
      (loop for i from 0 below (min (length (app-buffers app)) content-h)
            for buf = (aref (app-buffers app) i)
            for marker = (if (= i (app-current-buffer-id app)) "▶" " ")
            for unread = (buffer-unread-count buf)
            for hi = (buffer-highlight-count buf)
            for line = (format nil "~a~a~@[ (~d)~]~@[ !~d~]"
                               marker (buffer-title buf)
                               (and (> unread 0) unread)
                               (and (> hi 0) hi))
            do (%draw-line wbuf (1+ i) 1 (subseq line 0 (min (length line) content-w)))))

    ;; chat area: render last visible lines (inside border)
    (let* ((buf (current-buffer app))
           (msgs (ring->list (buffer-scrollback buf)))
           (h (de.anvi.croatoan:height wchat))
           (w (de.anvi.croatoan:width wchat))
           (content-h (- h 2))  ;; inside border
           (content-w (- w 2))
           (offset (buffer-scroll-offset buf))
           (end (max 0 (- (length msgs) offset)))
           (start (max 0 (- end content-h)))
           (y 1))  ;; start inside top border
      (loop for idx from start below end
            for m = (nth idx msgs)
            for nick = (or (clatter.core.model:message-nick m) "*")
            for text = (clatter.core.model:message-text m)
            for highlightp = (clatter.core.model:message-highlight m)
            for nick-display = (format nil "~a: " nick)
            for msg-display = (subseq text 0 (min (length text) (max 0 (- content-w (length nick-display)))))
            do (progn
                 ;; Position cursor
                 (setf (de.anvi.croatoan:cursor-position-y wchat) y)
                 (setf (de.anvi.croatoan:cursor-position-x wchat) 1)
                 ;; Draw nick with color (bold if highlighted)
                 (if highlightp
                     (de.anvi.croatoan:add-string wchat nick-display
                                                  :fgcolor :yellow :bgcolor :red
                                                  :attributes '(:bold))
                     (de.anvi.croatoan:add-string wchat nick-display :fgcolor (nick-color nick)))
                 ;; Draw message text (highlighted or default)
                 (if highlightp
                     (de.anvi.croatoan:add-string wchat msg-display
                                                  :fgcolor :white :bgcolor :red)
                     (de.anvi.croatoan:add-string wchat msg-display))
                 (incf y))))

    ;; status
    (let* ((buf (current-buffer app))
           (line (format nil " [~a]  unread:~d  hi:~d  (/quit to exit)"
                         (buffer-title buf)
                         (buffer-unread-count buf)
                         (buffer-highlight-count buf))))
      (%draw-line wstatus 0 0 (subseq line 0 (min (length line) (de.anvi.croatoan:width wstatus)))))

    ;; input
    (let* ((st (ui-input ui))
           (prompt "> ")
           (txt (input-text st))
           (cursor (input-cursor st))
           (full (concatenate 'string prompt txt)))
      (%draw-line winput 0 0 (subseq full 0 (min (length full) (de.anvi.croatoan:width winput))))
      ;; move cursor (roughly)
      (setf (de.anvi.croatoan:cursor-position-y winput) 0)
      (setf (de.anvi.croatoan:cursor-position-x winput) (min (de.anvi.croatoan:width winput) (+ (length prompt) cursor))))

    ;; batch refresh to reduce flicker
    (de.anvi.croatoan:mark-for-refresh wbuf)
    (de.anvi.croatoan:mark-for-refresh wchat)
    (de.anvi.croatoan:mark-for-refresh wstatus)
    (de.anvi.croatoan:mark-for-refresh winput)
    (de.anvi.croatoan:refresh-marked)

    ;; Final refresh of input window to position cursor there
    (de.anvi.croatoan:refresh winput)

    (clear-dirty app)
    app))
