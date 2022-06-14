;; -------------------------------------------------------------------------
;;  File:    pixel.lsp
;;  Created: 02.26.21
;;  Comment: Library to support graphics with a Python/Pygame graphics server.
;; -------------------------------------------------------------------------

(defpackage :pixel

  ( :use :common-lisp )

  ( :export
		
    :init
    :shutdown
					;
					; -- methods --
					;
    :draw-pixel
    :draw-line
    :draw-circle
		:draw-text
					;
					; -- colors --

    :*black*
    :*white*
    :*red*
    :*green*
    :*blue*
    :*yellow*
    :*cyan*
    :*magenta*

    ))

(in-package pixel)

(defconstant *socket* 54321)
(defconstant *ip* "127.0.0.1")

(defconstant *black*    #(0 0 0))
(defconstant *white*    #(255 255 255))
(defconstant *red*      #(255 0 0))
(defconstant *green*    #(0 255 0))
(defconstant *blue*     #(0 0 255))
(defconstant *yellow*   #(255 255 0))
(defconstant *cyan*     #(0 255 255))
(defconstant *magenta*  #(255 0 255))


(defun launch-server (canvas)
  (progn
    (format t "~% [*] Launching Python/Pygame graphics server...")
		(format t "~% [*] canvas size: ~a x ~a"
						(first canvas)
						(second canvas))
		(format t "~%~%")
		(defvar args (loop for arg in canvas collect (write-to-string arg)))
		(append args '("&"))
		(ext:run-program "/home/rick/git/lisp/libs/launch-pixel-server.sh" :arguments args)
    (sleep 4)))

(defun init (canvas)
  (progn
    (launch-server canvas)
    (format t "~% [*] Connecting Lisp to Pixel Server with ip: ~a socket: ~a" *ip* *socket*)
    (defvar *pixel-stream* (ext:socket-connect *socket* *ip*))))

(defun shutdown ()
  (close *pixel-stream*))

(defun draw-pixel (x y color)
  (setf mesg (format nil "{ PIXEL ~a ~a ~a ~a ~a }"
		     x y
		     (svref color 0)
		     (svref color 1)
		     (svref color 2)
		     ))
  (print mesg *pixel-stream*))

(defun draw-line (x1 y1 x2 y2 color)
  (setf mesg (format nil "{ LINE ~a ~a ~a ~a ~a ~a ~a }"
		     x1 y1 x2 y2
		     (svref color 0)
		     (svref color 1)
		     (svref color 2)))	
  (print mesg *pixel-stream*))

(defun draw-circle (x y radius width color)
  (setf mesg (format nil "{ CIRCLE ~a ~a ~a ~a ~a ~a ~a }"
		     x y
		     radius
		     width
		     (svref color 0)
		     (svref color 1)
		     (svref color 2)))
  (print mesg *pixel-stream*))

(defun draw-text (text x y color)
	(setf mesg (format nil "{ TEXT ~a ~a ~a ~a ~a ~a }"
										 text										 
										 x y
										 (svref color 0)
										 (svref color 1)
										 (svref color 2)))
	(print mesg *pixel-stream*))



;; def world_to_canvas_x(self, x):
        
;;         dist_to_x_min = abs(self.world_x_min - x)
;;         dist_x_ratio = float(dist_to_x_min) / float(self.world_width)
;;         canvas_x = int(round(dist_x_ratio * self.canvas_width))

;;         return canvas_x

;;     def world_to_canvas_y(self, y):
        
;;         dist_to_y_min = abs(self.world_y_min - y)
;;         dist_y_ratio = float(dist_to_y_min) / float(self.world_height)
;;         canvas_y = int(round(dist_y_ratio * self.canvas_height))

;;         return canvas_y

