;; -*- Lisp -*-

(defpackage #:linker-system (:use #:cl #:asdf))
(in-package #:linker-system)

(defsystem st-linker
  :depends-on (:anaphora :osicat)
  :components
  ((:file "package")))
