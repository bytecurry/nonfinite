(in-package :cl-user)

(defpackage bytecurry.nonfinite
  (:nicknames :nonfinite :ieee-special)
  (:use :cl)
  (:import-from :alexandria #:define-constant)
  (:export #:+nan+
           #:+infinity+
           #:+negative-infinity+
           #:nan-p
           #:infinity-p
           #:normal-number-p
           #:expanded-float
           #:expanded-real))

(in-package :bytecurry.nonfinite)

(define-constant +nan+ :nan
  :documentation "Not a number")
(define-constant +infinity+ :infinity
  :documentation "Positive Infinity")
(define-constant +negative-infinity+ :negative-infinity
  :documentation "Negative Infinity")

(define-constant +special-floats+ (list +nan+
                                    +infinity+
                                    +negative-infinity+)
  :documentation "List of special float values: NaN, +Inf, -Inf")

(deftype expanded-float () `(or float (member ,@+special-floats+)))
(deftype expanded-real () `(or real (member ,@+special-floats+)))



(defun nan-p (number)
  (declare (type expanded-real number))
  "Test if number is NaN"
  (eql number +nan+))

(defun infinity-p (number)
  (declare (type expanded-real number))
  "Test if number is infinite (positive or negative)"
  (or (eql number +infinity+)
      (eql number +negative-infinity+)))

(defun normal-number-p (number)
  (declare (type expanded-real number))
  "Test if NUMBER is a normal number. I.e. neither NaN or an infinity"
  (not (or (nan-p number) (infinity-p number))))
