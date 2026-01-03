(in-package #:clatter.core.dispatch)

(defgeneric apply-event (app event))
(defgeneric deliver-message (app buffer message &key highlightp))

(defmethod deliver-message ((app app) (buf buffer) (msg message) &key (highlightp nil))
  (when highlightp
    (setf (clatter.core.model:message-highlight msg) t))
  (ring-push (buffer-scrollback buf) msg)
  (when (/= (clatter.core.model:buffer-id buf) (clatter.core.model:app-current-buffer-id app))
    (incf (buffer-unread-count buf))
    (when highlightp
      (incf (buffer-highlight-count buf))))
  (mark-dirty app :chat :buflist :status))

(defmethod apply-event ((app app) event)
  ;; default: ignore unknown events
  (declare (ignore event))
  app)
