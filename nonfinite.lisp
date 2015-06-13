(in-package :cl-user)

(defpackage bytecurry.nonfinite
  (:nicknames :nonfinite :ieee-special)
  (:export #:+nan+
           #:+infinity+
           #:+negative-infinity+
           #:nan-p
           #:infinite-p
           #:normal-number-p))

(deftype expanded-float () `(or float (member ,+nan+ ,+infinity+ ,+negative-infinity+)))


(defconstant +nan+ :nan)
(defconstant +infinity+ :infinity)
(defconstant +negative-infinity+ :negative-infinity)

(defun nan-p (number)
  (declare (type expanded-float number))
  "Test if number is NaN"
  #+sbcl
  (or (eql number +nan+) (sb-ext:float-nan-p number))
  #-sbcl
  (eql num +nan+))

(defun infinite-p (number)
  (declare (type expanded-float number))
  "Test if number is infinite (positive or negative)"
  (or (eql number +infinity+)
      (eql number +negative-infinity+)
      #+sbcl
      (sb-ext:float-infinity-p number)))

(defun normal-number-p (number)
  (declare (type expanded-float number))
  "Test if NUMBER is a normal number. I.e. neither NaN or an infinity"
  (not (or (nan-p number) (infinite-p (number)))))
