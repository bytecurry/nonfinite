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
  :test 'equal
  :documentation "List of special float values: NaN, +Inf, -Inf")

(deftype expanded-float () `(or float (member ,@+special-floats+)))
(deftype expanded-real () `(or real (member ,@+special-floats+)))



(defun nan-p (number)
  (declare (type expanded-real number))
  "Test if number is NaN"
  (or (eql number +nan+)
      (and (floatp number)
           #+sbcl
           (sb-ext:float-nan-p number)
           #-sbcl
           (not (= number number)))))

(defun infinity-p (number)
  (declare (type expanded-real number))
  "Test if number is infinite (positive or negative)"
  (or (eql number +infinity+)
      (eql number +negative-infinity+)
      (and (floatp number)
           #+sbcl
           (sb-ext:float-infinity-p number)
           #-sbcl
           (or (> number most-positive-long-float)
               (< number most-negative-long-float)))))

(defun positive-infinity-p (number)
  (declare (type expanded-real number))
  "Test if a number is positive infinity."
  (or (eql number +infinity+)
      (and (floatp number) (> number most-positive-long-float))))

(defun negative-infinity-p (number)
  (declare (type expanded-real number))
  "Test if a number is negative infinity."
  (or (eql number +negative-infinity+)
      (and (floatp number) (< number most-negative-long-float))))

(defun normal-number-p (number)
  (declare (type expanded-real number))
  "Test if NUMBER is a normal number. I.e. neither NaN or an infinity"
  (not (or (nan-p number) (infinity-p number))))
