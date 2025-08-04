;; -------------------------------------------------------------------------
;;  File:    fizzle.lsp
;;  Created: Sat Apr 30 14:06:22 2016
;;  Comment: Library of Physics Constants and Functions.
;; -------------------------------------------------------------------------

(defpackage :fizzle

  ( :use :common-lisp :slip :moth )

  ( :export 
   ; constants
    :+speed-of-light+
    :+G+
    :+gravity+
    :+feet-in-meter+
   ; physics
   :time-dilation
   :free-fall
   :disp-free-fall
   :earth science
   :latlon-xyz
  ))

(in-package fizzle)

; constants

(defconstant +speed-of-light+ 299792458) ; speed of light m/s
(defconstant +gravity+ 9.8) ; m/s^2
(defconstant +G+ 6.67430e-11) ; (N*m^2)/kg^2
(defconstant +feet-in-meter+ 3.28084) ; ft in a meter

; testing with:
;
; 86% speed of light
; 3 hour trip
; ~6 hours elapse for people on earth

(defun time-dilation (pct_c t_hrs)
  "Computes the time in t_hrs that have elapsed on Earth if someone is 
   moving close to the speed of light (pct_c) for time in hours (t_hrs)."
  (let* ( (c_2 (expt +speed-of-light+ 2))
         (v (* pct_c +speed-of-light+))
          (v_2 (expt v 2)))
    (/ t_hrs (sqrt (- 1 (/ v_2 c_2))))))

(defun free-fall (s)
  "Computes free fall in meters after seconds (s)."
  (* 0.5 +gravity+ (expt s 2)))
  
(defun disp-free-fall (s)
  (format t "~% An object falls ~a meters after ~a seconds." (fizzle:free-fall s) s))

                                        ; Simplified version of converting lat|lon to xyz points.
                                        ; this is based on a Lat/Lon to ECEF coordinate system
                                        ; but it does not use the Earth's radius or a flattening
                                        ; factor like true ECEF coordinates.

(defun latlon-xyz (latitude longitude)
  (let* ((lat (moth:radians latitude))
         (lon (moth:radians longitude))
         (cos_lat (cos lat))
         (sin_lat (sin lat))
         (cos_lon (cos lon))
         (sin_lon (sin lon))
         (C (/ 1 (sqrt (+ (expt cos_lat 2) (expt sin_lat 2)))))
         (x (* C cos_lat cos_lon))
         (y (* C cos_lat sin_lon))
         (z (* C sin_lat)))
    (list x y z)))

