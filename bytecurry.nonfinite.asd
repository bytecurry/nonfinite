(in-package :asdf-user)

(defsystem bytecurry.nonfinite
  :description "Portable library to deal with infinities and NaN."
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :version "0.1.0"
  :license "MIT"
  :depends-on (:alexandria)
  :components ((:file "nonfinite")))
