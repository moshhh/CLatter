(in-package #:clatter.net.client)

(defun start-demo-net-thread (app)
  (bordeaux-threads:make-thread
   (lambda ()
     (loop for n from 1 do
       (sleep 1)
       ;; Submit a UI-safe job: append message to #lisp buffer
       (de.anvi.croatoan:submit
         (let ((buf (find-buffer app 1)))
           (deliver-message
            app buf
            (clatter.core.model:make-message :level :chat :nick "demo-bot"
                                             :text (format nil "hello ~d from net thread" n))
            :highlightp (zerop (mod n 7)))))))
   :name "clatter-demo-net"))
