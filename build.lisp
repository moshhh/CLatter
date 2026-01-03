;;; build.lisp - Build script for CLatter executable
;;; Run with: sbcl --load build.lisp

(require :asdf)

;; Add project to ASDF registry
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load the system
(asdf:load-system :clatter)

;; Build the executable
(sb-ext:save-lisp-and-die "clatter"
                          :toplevel #'clatter:main
                          :executable t
                          :compression t)
