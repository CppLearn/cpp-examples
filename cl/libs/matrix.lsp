;; -------------------------------------------------------------------------
;;  File:    matrix.lsp
;;  Created: Thu May 12 23:47:00 2022
;;  Comment: Library of Matrix and Linear Algebra Routines.
;; -------------------------------------------------------------------------

(defpackage :matrix

  ( :use :common-lisp )

  ( :export 

    :display-dims
    :display-matrix
    :multiply

    ) )

(in-package matrix)

(defun display-dims(M name)
  (format t "~%~a dimensions: (~a, ~a)"
    name
    (first (array-dimensions M))
    (second (array-dimensions M))))

(defun display-matrix(a title)
  "Pretty print a matrix."
  (format t "~%~%   ------------- ~a ----------- ~%" title)
  (let ((a-rows (array-dimension a 0))
  (a-cols (array-dimension a 1)))
    (dotimes (r a-rows)
      (unless (= r 0)
  (format t "~%"))
      (dotimes (c a-cols)
  (format t "~8,2f " (aref a r c)))))
  (format t "~%"))

(defun multiply(a b)
  "General matrix multiply for a x b"
  (let* ((sum 0.0)
  (a-rows (array-dimension a 0))
  (a-cols (array-dimension a 1))
  (b-rows (array-dimension b 0))
  (b-cols (array-dimension b 1))
  (result-dims (list a-rows b-cols))
   (result (make-array result-dims :initial-element 0.0)))
    
    (dotimes (ar a-rows)
      (dotimes (bc b-cols)
  (setf sum 0.0)
  (dotimes (br b-rows)
    (incf sum (* (aref a ar br)
           (aref b br bc))))
  (setf (aref result ar bc) sum)))
    result))

