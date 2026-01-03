(in-package #:clatter.ui.tui)

(defun run-tui (app)
  (de.anvi.croatoan:with-screen (scr
                                :input-buffering nil
                                :input-blocking nil
                                :enable-function-keys t
                                :cursor-visible t
                                :frame-rate 30)
    ;; store screen
    (setf (ui-screen (app-ui app)) scr)
    ;; Disable ncurses echo - we handle display ourselves
    (de.anvi.ncurses:noecho)

    ;; layout numbers - add 1 char gap between buffer list and chat
    (let* ((term-w (de.anvi.croatoan:width scr))
           (term-h (de.anvi.croatoan:height scr))
           (left-w (ui-buflist-w (app-ui app)))
           (gap 1)  ;; gap between panels
           (right-x (+ left-w gap))
           (right-w (- term-w right-x))
           (chat-h (- term-h 2)))
      (setf (ui-term-w (app-ui app)) term-w)
      (setf (ui-term-h (app-ui app)) term-h)

      (de.anvi.croatoan:with-windows
          ((wbuf   :x 0       :y 0       :width left-w  :height term-h :border nil)
           (wchat  :x right-x :y 0       :width right-w :height chat-h :border nil)
           (wstat  :x right-x :y chat-h  :width right-w :height 1      :border nil)
           (winp   :x right-x :y (1+ chat-h) :width right-w :height 1  :border nil))
        (setf (ui-win-buflist (app-ui app)) wbuf
              (ui-win-chat    (app-ui app)) wchat
              (ui-win-status  (app-ui app)) wstat
              (ui-win-input   (app-ui app)) winp)

        (install-keybindings scr app)

        (mark-dirty app :layout :chat :buflist :status :input)
        (render-frame app)

        (de.anvi.croatoan:run-event-loop scr)))))
