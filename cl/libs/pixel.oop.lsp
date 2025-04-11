;; -------------------------------------------------------------------------
;;  File:    pixel.lsp
;;  Created: 02.26.21
;;  Comment: Library to support graphics with a Python/Pygame graphics server.
;; -------------------------------------------------------------------------

(defpackage :pixel

  ( :use :common-lisp )

  ( :export   
    ;;
    ;; -- screen --
    :screen
    :screen-xmin
    :screen-xmax
    :screen-ymin 
    :screen-ymax 
    :screen-xrange 
    :screen-yrange
    :make-screen
    ;;
    ;; -- world --
    :world
    :world-xmin
    :world-xmax
    :world-ymin 
    :world-ymax 
    :world-xrange 
    :world-yrange
    :make-world
                                        ;
                                        ;
    :get-scale-x
    :get-scale-y
                                        ;
                                        ;
    :init
    :shutdown
    ;;
    ;; -- methods --
    ;;
    :draw-pixel
    :draw-line
    :draw-circle
    :draw-text
																				; with world scaling
		:wdraw-pixel
		:wdraw-line
		:wdraw-circle
		:wdraw-text
		
    ;;
    ;; -- colors --
    :colors   
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

(defconstant *black*    (vector 0 0 0))
(defconstant *white*    (vector 255 255 255))
(defconstant *red*      (vector 255 0 0))
(defconstant *green*    (vector 0 255 0))
(defconstant *blue*     (vector 0 0 255))
(defconstant *yellow*   (vector 255 255 0))
(defconstant *cyan*     (vector 0 255 255))
(defconstant *magenta*  (vector 255 0 255))

(defvar colors '(*black* *white* *red* *green* *blue* *yellow* *cyan* *magenta*))

(defun launch-server (canvas)
  (progn
    (format t "~% [*] Launching Lisp Graphics Server...")
    (format t "~% [*] canvas size: ~a x ~a"
            (first canvas)
            (second canvas))
    (format t "~%~%")
    (defvar args (loop for arg in canvas collect (write-to-string arg)))
    (append args '("&"))
																				; pygame
																				; (ext:run-program "/home/rick/git/lisp/libs/launch-pixel-server.sh" :arguments args)

    (ext:run-program "/home/rick/git/lisp/libs/launch-raylib-server.sh" :arguments args)
    (sleep 4)))

                                        ; -- screen class to handle
                                        ; -- physical screen resolution.
(defclass screen ()
  (
   (x-min   :initarg :xmin   :initform -1 :accessor screen-xmin )
   (x-max   :initarg :xmax   :initform -1 :accessor screen-xmax )
   (y-min   :initarg :ymin   :initform -1 :accessor screen-ymin )
   (y-max   :initarg :ymax   :initform -1 :accessor screen-ymax )
   (x-range :initarg :xrange :initform -1 :accessor screen-xrange )
   (y-range :initarg :yrange :initform -1 :accessor screen-yrange ))
  (:documentation "A screen class to handle screen properties."))


(defun make-screen (xwidth yheight)
  (defvar new-screen
    (make-instance 'screen :xmin 0 :xmax xwidth :ymin 0 :ymax yheight))
	
  (setf (screen-xrange new-screen) (- (screen-xmax new-screen)
                                      (screen-xmin new-screen)))
  (setf (screen-yrange new-screen) (- (screen-ymax new-screen)
                                      (screen-ymin new-screen)))
  new-screen)


                                        ; -- world class to handle
                                        ; -- world coordinates.
(defclass world ()
  (
   (x-min   :initarg :xmin   :initform -1  :accessor world-xmin )
   (x-max   :initarg :xmax   :initform -1  :accessor world-xmax )
   (y-min   :initarg :ymin   :initform -1  :accessor world-ymin )
   (y-max   :initarg :ymax   :initform -1  :accessor world-ymax )
   (x-range :initarg :xrange :initform -1  :accessor world-xrange )
   (y-range :initarg :yrange :initform -1  :accessor world-yrange )
	 (screen                   :initform nil :accessor world-screen))
  (:documentation "A world class to handle world properties."))

;; (defun dwarf (n &key weight gamma) 
       (format t "~% n = ~a, weight = ~a, gamma = ~a" n weight gamma))


(defun make-world (xmin xmax ymin ymax &key screen)
  (defvar new-world
    (make-instance 'world :xmin xmin
                          :xmax xmax
                          :ymin ymin
                          :ymax ymax
													:screen screen))

  (setf (world-xrange new-world) (- (world-xmax new-world)
                                    (world-xmin new-world)))
  (setf (world-yrange new-world) (- (world-ymax new-world)
                                    (world-ymin new-world)))
  new-world)

(defun get-scale-x (w)
  "Return function that computes world to screen transform: x coord"  
  (lambda (x)
    (let* ((dist-to-x-min (abs (- (world-xmin w)
                                  x)))
           (dist-x-ratio (float (/ dist-to-x-min
                                   (world-xrange w))))
           (canvas-x (round (* dist-x-ratio
                               (screen-xrange (world-screen w) )))))
      canvas-x)))

(defun get-scale-y (w)
  "Return function that computes world to screen transform: y coord"  
  (lambda (y)
    (let* ((dist-to-y-min (abs (- (world-ymin w)
                                  y)))
           (dist-y-ratio (float (/ dist-to-y-min
                                   (world-yrange w))))
           (canvas-y (round (* dist-y-ratio
                               (screen-yrange (world-screen w) )))))
      canvas-y)))

(defun init (s)
  "Initialize graphics with screen s"
  (progn
    (launch-server (list (screen-xmax s)
                         (screen-ymax s)))
    (format t "~% [*] Connecting Lisp to Pixel Server with ip: ~a socket: ~a" *ip* *socket*)
    (defvar *pixel-stream* (ext:socket-connect *socket* *ip*))))



(defvar world-screen-x (pixel:get-scale-x world screen))
(defvar world-screen-y (pixel:get-scale-y world screen))

(defun scale-x (x)
  (funcall world-screen-x x))
(defun scale-y (y)
  (funcall world-screen-y y))                                      







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




(defmacro wdraw-pixel (x y color)
	`(draw-pixel (cl-user:scale-x ,x) (cl-user:scale-y ,y) ,color))

(defmacro wdraw-line (x1 y1 x2 y2 color)
	`(draw-line (cl-user:scale-x ,x1) (cl-user:scale-y ,y1) (cl-user:scale-x ,x2) (cl-user:scale-y ,y2) ,color))

(defmacro wdraw-circle (x y radius width color)
	`(draw-circle (cl-user:scale-x ,x) (cl-user:scale-y ,y1) ,radius ,width ,color))

(defmacro wdraw-text (text x y color)
	`(draw-text (text (cl-user:scale-x ,x) (cl-user:scale-y ,y) ,color)))









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

