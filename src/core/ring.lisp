(in-package #:clatter.core.ring)

(defstruct (ring (:constructor %make-ring))
  (capacity 2000 :type fixnum)
  (vec (make-array 2000 :initial-element nil) :type simple-vector)
  (start 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-ring (&key (capacity 2000))
  (%make-ring :capacity capacity
              :vec (make-array capacity :initial-element nil)
              :start 0
              :count 0))

(defun ring-push (r item)
  (let* ((cap (ring-capacity r))
         (cnt (ring-count r))
         (start (ring-start r))
         (vec (ring-vec r)))
    (cond
      ((< cnt cap)
       (setf (svref vec (mod (+ start cnt) cap)) item)
       (incf (ring-count r)))
      (t
       ;; overwrite oldest
       (setf (svref vec start) item)
       (setf (ring-start r) (mod (1+ start) cap)))))
  r)

(defun ring->list (r)
  (let* ((cap (ring-capacity r))
         (cnt (ring-count r))
         (start (ring-start r))
         (vec (ring-vec r)))
    (loop for i from 0 below cnt
          collect (svref vec (mod (+ start i) cap)))))
