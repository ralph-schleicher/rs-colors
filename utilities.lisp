;;; utilities.lisp --- utility definitions.

;; Copyright (C) 2014 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-colors)

;;;; Data and Control Flow

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like `defconstant' except that the initially set value
is reused when the `defconst' form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like `defun' except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

;;;; Conditions

(defun ensure-type (object type)
  "Signal a type error if OBJECT is not of the type TYPE.
Otherwise, return OBJECT."
  (if (typep object type)
      object
    (error (make-condition 'type-error :datum object :expected-type type))))

;;;; Exponential Functions

(defun cube (z)
  "Return Z cubed, that is Z raised to the power three.

Argument Z has to be a real number."
  (declare (type real z))
  (cond ((= z 0)
	 0)
	((= z 1)
	 1)
	(t
	 (* z z z))))

(defun cube-root (z)
  "Return the cube root of Z.

Argument Z has to be a real number.

If argument Z is zero, value is zero.  If argument Z is
a real number, value is the real cube root of Z.

The `cube-root' function attempts to propagate the type
of the argument Z to its value."
  (declare (type real z))
  (cond ((= z 0)
	 0)
	((= z 1)
	 1)
	(t
	 (let ((f (* (signum z) (expt (abs z) 1/3))))
	   (if (rationalp z)
	       (let ((r (rationalize f)))
		 (if (or (= r f) (= (cube r) z)) r f))
	     f)))))

;;;; Linear Algebra

(defun make-vector (&optional (x1 0) (x2 0) (x3 0))
  "Create a vector."
  (let ((x (make-array '(3) :element-type 'real :initial-element 0)))
    (setf (svref x 0) x1
	  (svref x 1) x2
	  (svref x 2) x3)
    x))

(defun copy-vector (x)
  "Return a copy of vector X."
  (make-vector (svref x 0)
	       (svref x 1)
	       (svref x 2)))

(defun make-matrix (&optional (a11 0) (a12 0) (a13 0)
			      (a21 0) (a22 0) (a23 0)
			      (a31 0) (a32 0) (a33 0))
  "Create a matrix."
  (let ((a (make-array '(3 3) :element-type 'real :initial-element 0)))
    (setf (aref a 0 0) a11 (aref a 0 1) a12 (aref a 0 2) a13
	  (aref a 1 0) a21 (aref a 1 1) a22 (aref a 1 2) a23
	  (aref a 2 0) a31 (aref a 2 1) a32 (aref a 2 2) a33)
    a))

(defun copy-matrix (a)
  "Return a copy of matrix A."
  (make-matrix (aref a 0 0) (aref a 0 1) (aref a 0 2)
	       (aref a 1 0) (aref a 1 1) (aref a 1 2)
	       (aref a 2 0) (aref a 2 1) (aref a 2 2)))

(defun det (a)
  "Return the determinant of matrix A."
  (+ (* (aref a 0 0) (- (* (aref a 1 1) (aref a 2 2))
			(* (aref a 1 2) (aref a 2 1))))
     (* (aref a 0 1) (- (* (aref a 1 2) (aref a 2 0))
			(* (aref a 1 0) (aref a 2 2))))
     (* (aref a 0 2) (- (* (aref a 1 0) (aref a 2 1))
			(* (aref a 1 1) (aref a 2 0))))))

(defun matrix-transpose (a)
  "Transpose matrix A in place."
  (rotatef (aref a 1 0) (aref a 0 1))
  (rotatef (aref a 2 0) (aref a 0 2))
  (rotatef (aref a 2 1) (aref a 1 2))
  a)

(defun matrix-cofactors (a)
  "Calculate matrix of cofactors of matrix A in place."
  (psetf (aref a 0 0) (- (* (aref a 1 1) (aref a 2 2))
			 (* (aref a 1 2) (aref a 2 1)))
	 (aref a 0 1) (- (* (aref a 1 2) (aref a 2 0))
			 (* (aref a 1 0) (aref a 2 2)))
	 (aref a 0 2) (- (* (aref a 1 0) (aref a 2 1))
			 (* (aref a 1 1) (aref a 2 0)))
	 (aref a 1 0) (- (* (aref a 0 2) (aref a 2 1))
			 (* (aref a 0 1) (aref a 2 2)))
	 (aref a 1 1) (- (* (aref a 0 0) (aref a 2 2))
			 (* (aref a 0 2) (aref a 2 0)))
	 (aref a 1 2) (- (* (aref a 0 1) (aref a 2 0))
			 (* (aref a 0 0) (aref a 2 1)))
	 (aref a 2 0) (- (* (aref a 0 1) (aref a 1 2))
			 (* (aref a 0 2) (aref a 1 1)))
	 (aref a 2 1) (- (* (aref a 0 2) (aref a 1 0))
			 (* (aref a 0 0) (aref a 1 2)))
	 (aref a 2 2) (- (* (aref a 0 0) (aref a 1 1))
			 (* (aref a 0 1) (aref a 1 0))))
  a)

(defun matrix-adjugate (a)
  "Calculate adjugate matrix of matrix A in place."
  (matrix-transpose (matrix-cofactors a)))

(defun matrix-inverse (a)
  "Calculate inverse matrix of matrix A in place."
  (let ((det (det a)))
    (when (zerop det)
      (error (make-condition 'division-by-zero :operation 'matrix-inverse :operands (list a))))
    (matrix-adjugate a)
    (setf (aref a 0 0) (/ (aref a 0 0) det)
	  (aref a 0 1) (/ (aref a 0 1) det)
	  (aref a 0 2) (/ (aref a 0 2) det)
	  (aref a 1 0) (/ (aref a 1 0) det)
	  (aref a 1 1) (/ (aref a 1 1) det)
	  (aref a 1 2) (/ (aref a 1 2) det)
	  (aref a 2 0) (/ (aref a 2 0) det)
	  (aref a 2 1) (/ (aref a 2 1) det)
	  (aref a 2 2) (/ (aref a 2 2) det)))
  a)

(defun gemv (a x &optional (y (make-vector)))
  "General matrix/vector multiplication."
  (psetf (svref y 0) (+ (* (aref a 0 0) (svref x 0))
			(* (aref a 0 1) (svref x 1))
			(* (aref a 0 2) (svref x 2)))
	 (svref y 1) (+ (* (aref a 1 0) (svref x 0))
			(* (aref a 1 1) (svref x 1))
			(* (aref a 1 2) (svref x 2)))
	 (svref y 2) (+ (* (aref a 2 0) (svref x 0))
			(* (aref a 2 1) (svref x 1))
			(* (aref a 2 2) (svref x 2))))
  y)

(defun gemm (a b &optional (c (make-matrix)))
  "General matrix/matrix multiplication."
  (psetf (aref c 0 0) (+ (* (aref a 0 0) (aref b 0 0))
			 (* (aref a 0 1) (aref b 1 0))
			 (* (aref a 0 2) (aref b 2 0)))
	 (aref c 0 1) (+ (* (aref a 0 0) (aref b 0 1))
			 (* (aref a 0 1) (aref b 1 1))
			 (* (aref a 0 2) (aref b 2 1)))
	 (aref c 0 2) (+ (* (aref a 0 0) (aref b 0 2))
			 (* (aref a 0 1) (aref b 1 2))
			 (* (aref a 0 2) (aref b 2 2)))
	 (aref c 1 0) (+ (* (aref a 1 0) (aref b 0 0))
			 (* (aref a 1 1) (aref b 1 0))
			 (* (aref a 1 2) (aref b 2 0)))
	 (aref c 1 1) (+ (* (aref a 1 0) (aref b 0 1))
			 (* (aref a 1 1) (aref b 1 1))
			 (* (aref a 1 2) (aref b 2 1)))
	 (aref c 1 2) (+ (* (aref a 1 0) (aref b 0 2))
			 (* (aref a 1 1) (aref b 1 2))
			 (* (aref a 1 2) (aref b 2 2)))
	 (aref c 2 0) (+ (* (aref a 2 0) (aref b 0 0))
			 (* (aref a 2 1) (aref b 1 0))
			 (* (aref a 2 2) (aref b 2 0)))
	 (aref c 2 1) (+ (* (aref a 2 0) (aref b 0 1))
			 (* (aref a 2 1) (aref b 1 1))
			 (* (aref a 2 2) (aref b 2 1)))
	 (aref c 2 2) (+ (* (aref a 2 0) (aref b 0 2))
			 (* (aref a 2 1) (aref b 1 2))
			 (* (aref a 2 2) (aref b 2 2))))
  c)

(defun float-array (a &optional (prototype 1F0))
  "Convert elements of a numeric array to floating-point numbers."
  (iter (for k :from 0 :below (array-total-size a))
    (setf (row-major-aref a k) (float (row-major-aref a k) prototype)))
  a)

;; GEMV with multiple values.
(defun linear-transformation (a x1 x2 x3)
  "Perform a linear transformation."
  (values (+ (* (aref a 0 0) x1)
	     (* (aref a 0 1) x2)
	     (* (aref a 0 2) x3))
	  (+ (* (aref a 1 0) x1)
	     (* (aref a 1 1) x2)
	     (* (aref a 1 2) x3))
	  (+ (* (aref a 2 0) x1)
	     (* (aref a 2 1) x2)
	     (* (aref a 2 2) x3))))

;;;; CIE Color Spaces

(defun cie-L*-from-Y/Yn (Y/Yn)
  "Map relative luminance Y/Yn to lightness L*."
  (if (> Y/Yn 216/24389)
      (- (* 116 (cube-root Y/Yn)) 16)
    (* 24389/27 Y/Yn)))

(defun cie-Y/Yn-from-L* (L*)
  "Map lightness L* to relative luminance Y/Yn."
  (if (> L* 8)
      (cube (/ (+ L* 16) 116))
    (* 27/24389 L*)))

;;;; RGB Color Spaces

(defun encode-triple (a b c &optional (byte-size 8))
  (let ((s (expt 2 byte-size)))
    (+ (* (+ (* a s) b) s) c)))

(defun decode-triple (value &optional (byte-size 8))
  (let ((s (expt 2 byte-size))
	(v value)
	(a 0)
	(b 0)
	(c 0))
    (multiple-value-setq (v c)
      (truncate v s))
    (multiple-value-setq (a b)
      (truncate v s))
    (values a b c)))

(defun rgb-transformation-matrices (red green blue white)
  "Return the transformation matrix for converting CIE XYZ color space
coordinates into RGB color space coordinates and vice versa.

Arguments RED, GREEN, BLUE, and WHITE are the chromaticity coordinates
of the red primary, the green primary, the blue primary, and the white
point respectively.

First value is the transformation matrix for converting CIE XYZ color
space coordinates into RGB color space coordinates.  Second value is
the inverse matrix."
  (labels ((xyz-from-xy (v)
	     (let ((x (elt v 0))
		   (y (elt v 1)))
	       (make-vector x y (- 1 x y)))))
    ;; See <http://docs-hoffmann.de/ciexyz29082000.pdf>, §11.4, page 15.
    (let* ((r (xyz-from-xy red))
	   (g (xyz-from-xy green))
	   (b (xyz-from-xy blue))
	   (w (xyz-from-xy white))
	   ;; Scale white point in advance.
	   (w* (let ((y (svref w 1)))
		 (make-vector (/ (svref w 0) y) 1 (/ (svref w 2) y))))
	   (p (make-matrix (svref r 0) (svref g 0) (svref b 0)
			   (svref r 1) (svref g 1) (svref b 1)
			   (svref r 2) (svref g 2) (svref b 2)))
	   (s (gemv (matrix-inverse (copy-matrix p)) w*))
	   (d (make-matrix (svref s 0) 0 0
			   0 (svref s 1) 0
			   0 0 (svref s 2)))
	   (c (gemm p d)))
      (values (matrix-inverse (copy-matrix c)) c))))

(defun make-rgb-color (color-type red green blue &optional byte-size)
  "Create a new color in an RGB color space."
  (let (r g b)
    (if (not byte-size)
	(setf r (ensure-type red '(real 0 1))
	      g (ensure-type green '(real 0 1))
	      b (ensure-type blue '(real 0 1)))
      (let ((s (1- (expt 2 (ensure-type byte-size '(integer 1))))))
	(setf r (/ (ensure-type red `(integer 0 ,s)) s)
	      g (/ (ensure-type green `(integer 0 ,s)) s)
	      b (/ (ensure-type blue `(integer 0 ,s)) s))))
    (make-instance color-type :red r :green g :blue b)))

(defun make-rgb-color-from-number (color-type value &optional (byte-size 8))
  "Create a new color in an RGB color space."
  (ensure-type value '(integer 0))
  (ensure-type byte-size '(integer 1))
  (multiple-value-bind (red green blue)
      (decode-triple value byte-size)
    (make-rgb-color color-type red green blue byte-size)))

;;; utilities.lisp ends here
