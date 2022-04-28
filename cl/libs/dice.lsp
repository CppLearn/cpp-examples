;; -------------------------------------------------------------------------
;;  File:    dice.lsp
;;  Created: Sun Sep 18 18:08:36 2016
;;  Comment: Library of Probability and Statistics Functions.
;; -------------------------------------------------------------------------

(defpackage :dice

  ( :use :common-lisp :slip )

  ( :export 
    :binomial-trial
    :poisson
    :repeats
    :entropy-hash
    :p-complement
    :bayes
    :mean-list
    :stddev-list
    :mean-dev-list
    :assign-probs
		:regression-xy ) )

(in-package dice)

(defun binomial-trial(n x p &optional (verbose nil))
  "Compute probability of x out of n trials succeeding 
   given probability p of success for each trial."
	(let ((probability
				 (* (moth:choose n x) 
						(expt p x) 
						(expt (- 1 p) (- n x)))) )
		(if verbose
				(format t "~% probability of ~a/~a successes is: ~$ %"
								x n (* 100 probability)))
		probability))
				
(defun poisson(lambda k)
  "Given the average rate (lambda) of a process that varies
  randomly around that rate, compute the Poisson probability
  of k occurrences (instead of lambda occurrences)."
  (/ (* (expt lambda k) (exp (- lambda))) 
     (moth:fact k) ))

(defun repeats(k n)
  "Compute expected number of repeats; where k = rounds and
   N is total # of items."
  (- k (* n (- 1
               (expt (- 1.0 (float (/ 1.0 n))) k)))))

;; (defvar p-stiff-neck (/ 1.0 20.0))
;; (defvar p-meningitis (/ 1.0 50000))
;; (defvar p-meningitis-stiff-neck 0.5)
;; (format t "~% ~,5f"
;; 				(dice:bayes p-stiff-neck p-meningitis p-meningitis-stiff-neck))
;; => 0.00020

(defun bayes (p-symptom p-disease p-s-given-d)
	"Returns the probability of a disease given a symptom."
	(/ (* p-s-given-d p-disease) p-symptom))

(defun p-complement (p)
    (- 1.0 p))

(defun mean-list (l)
  "Compute average of list of numbers."
  (if (listp l)
      (float (/ (reduce #'+ l) (length l)))))

(defun stddev-list (l)
  "Compute standard deviation of list of numbers."
  (if (listp l)
      (let* ((n (length l))
             (mean (dice:mean-list l))
             (devs-squared
              (loop for x in l collect (expt (- x mean) 2)))
             (sum-devs-squared (apply #'+ devs-squared)))

        (float (/ sum-devs-squared (- n 1))))))

(defun mean-dev-list (l)
  "Compute mean deviation of list."
  (if (listp l)
      (let* ((mean (dice:mean-list l))
            (mean-devs 
             (loop for x in l collect (abs (- x mean)))))
        (dice:mean-list mean-devs))))

(defun assign-probs (events)
  "Assign random probabilites to a list of events.
   Store probabilities in hash table with event name
   as key."
  (let ((budget 1.0) ; total of probabilities must sum to 1.
        (p 0.0)      ; temp probability for single event.
        (probs (make-hash-table)))

    (loop for e in events do 
          (slip:store-hash probs e 0.0))

    (loop while (> budget 0.0) do
      (setf p (random 0.1))
      (if (> p budget) 
          (setf p budget) )
      (setf choice (slip:random-choice events))    
      (slip:store-hash probs choice 
        (+ (gethash choice probs) p))
      (decf budget p) )
    probs))

(defun entropy-hash (h)
  "Compute the entropy of a hash table. 
   Keys are events/states, values are probabilities.
   (e.g. created by dice:assign-probs)" 
  (let ( (entropy-sum 0.0) 
         (p 0.0) )
    (loop for k being the hash-keys of h do
         (setf p (gethash k h))
         (incf entropy-sum (* p (log p))))
    entropy-sum))

(defun mean-nth (data n)
 	"Compute the mean of column n in a list of tuples.
 	e.g. (defvar data '( (x y)
 	                      ...
 	                     (x y)))"
 	
 	(let ( (sum (loop for row in data sum (nth n row))) )
 		(float (/ sum (length data)))))

(defun regression-xy (data)
	(let ( (a 0.0)
				 (b 0.0)
				 (mean-x (mean-nth data 0))
				 (mean-y (mean-nth data 1))
				 (numerator 0.0)
				 (denom 0.0) )
		
		(format t "~% compute-b: mean-x: ~a" mean-x)
		(format t "~% compute-b: mean-y: ~a" mean-y)
		
		(setf numerator (loop for xy in data sum
																				 (* (- (nth 0 xy) mean-x)
																						(- (nth 1 xy) mean-y))))
		(setf denom (loop for xy in data sum
																		 (expt (- (nth 0 xy) mean-x) 2)))
		
		(format t "~% compute-b: numerator: ~a" numerator)
		(format t "~% compute-b: denom: ~a" denom)
		(setf b (float (/ numerator denom)))
		(setf a (- mean-y (* b mean-x)))
		(format t "~% compute-b: a: ~a" a)
		(format t "~% compute-b: b: ~a" b)
		(values a b)))



