(in-package #:clatter.core.events)

(defstruct (ev (:constructor ev (type &rest plist)))
  type
  plist)
