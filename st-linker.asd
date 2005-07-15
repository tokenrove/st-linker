;; -*- Lisp -*-

(defpackage #:linker-system (:use #:cl #:asdf))
(in-package #:linker-system)

(defsystem st-linker
  :depends-on (:anaphora :osicat)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "linker" :depends-on ("package" "utils"))
   (:file "prg" :depends-on ("package"))
   (:file "aout" :depends-on ("package"))
   (:file "raw-binary" :depends-on ("package"))))

