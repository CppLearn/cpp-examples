;; -------------------------------------------------------------------------
;;  File:    slip.lsp
;;  Created: Sun Sep 18 18:07:06 2016
;;  Comment: Common Lisp Library of General/Misc Functions.
;; -------------------------------------------------------------------------

(defpackage :slip
  
  ( :use :common-lisp ) 

  ( :export 
                                        ; general
   :hello
	 :blank-line
	 :bin-dir
   :puts    
   :dump
   :typewriter-string
   :dot-display
   :dot-display-type
                                        ; chars
   :char-upperp
   :rand-char
                                        ; strings
   :rtrim
   :starts-with
   :ends-with
   :join-strings
   :split-string
   :char-in-string
   :not-these-chars
   :word-in-string
   :last-word
                                        ; lists
   :filter
   :foreach-i
   :i
   :generate
	 :mapcard
		:sort-list
   :random-choice
   :random-elt
   :shuffle
   :one-of
   :list->array
                                        ; files
   :file-loop
   :process-file
   :file-to-list
   :cut-file
   :list-to-file
   :with-file-lines
   :file-matrix
   :matrix-to-file
                                        ; arrays
   :extract-column

                                        ; structs
   :magic-struct
                                        ; hash funcs
   :hash-key
   :store-hash
   :assoc-to-hash
   :list-to-hash
   :show-hash
   :show-hash-type
   :sum-hash
   :stats-hash
                                        ; bits
   :make-bit-array 
   :bit-set
   :bit-set?

   :save-to-file                        ; save/load funcs
   :open-file
   :read-objects

	 :emoji                               ; emojis
	 :emojis
	 :emoji-keys
	 :one-emoji
		:emoji-objects

																				; other data
		:populate-countries
	 
	 :creature-list
   :color
                                        ; colors
   :black
   :red
   :green
   :yellow
   :blue
   :magenta
   :cyan
   :white

   :black_u  
   :red_u    
   :green_u  
   :yellow_u 
   :blue_u   
   :magenta_u
   :cyan_u   
   :white_u  

   :black_b  
   :red_b    
   :green_b  
   :yellow_b 
   :blue_b   
   :magenta_b
   :cyan_b   
   :white_b

	 :reset

  ))

(in-package slip)

;;   [General Purpose Functions]

(defun hello ()
  (write-line "hello!  Package slip is available!"))

(defun blank-line ()
  (fresh-line)
  (terpri))

(defun bin-dir ()
	(concatenate 'string (format nil "~a" (user-homedir-pathname)) "bin/"))

(defun puts (var)
  (format t "~% :: ~a" var))

(defun dump (label &rest objs)
  (format t "~% [+] ~a:" label)
  (dolist (o objs) do
    (format t "~%     ~a" o)))

(defun typewriter-string (s n)
  (setf len (length s))
  (loop for i from 0 to (1- len) do
        (sleep n)
        (princ (aref s i))))

(defun dot-string (s n)
  (let* ( (dot-array (make-array 0
                                 :element-type 'character
                                 :fill-pointer 0
                                 :adjustable t))
          (len-s (length s))
          (num-dots (- n len-s)) )
    (loop for c across s do
          (vector-push-extend c dot-array))
    (dotimes (i num-dots)
      (vector-push-extend #\. dot-array))
    dot-array))

(defun dot-display (lab val n)
  (let ( (dotted (dot-string lab n)) )
    (format t "~% ~a~a" dotted val)))

(defun dot-display-type (lab val n)
  (let ( (dotted (dot-string lab n)) )
    (setf s (format nil "~% ~a~a" dotted val))
    (slip:typewriter-string s 0.02)))

;;   [Char Functions]

(defun char-upperp (c)
  (let ( (ci (char-int c)) )
    (if (and (>= ci 65)
             (<= ci 90))
        t
      nil)))

(defun rand-char ()
  (let ( (n (+ 65 (random (- 122 65)))) )
    (character n)))

;;   [String Functions]

(defun rtrim(s)
  "Remove spaces from right end of string."
  (let ( (i (- (length s) 1))
         (tr nil) )
    ; tr will be trimmed string.
    (progn
      ; travel from end of string until we hit
      ; first non-space character.
      (loop while (>= i 0) do
            (if (not (eql (aref s i) #\space))
                (return))
            (decf i) )

      ; build up the string again from end.
      (loop while (>= i 0) do
            (push (aref s i) tr)
            (decf i))

      ; convert back to string and return.
      (coerce tr 'string))))

(defun starts-with (sub str)
  "Test if a string (str) starts with the substring (sub)."
  (let ((end (length sub)))
    (if (string-equal sub (subseq str 0 end))
        t
        nil)))

(defun ends-with (sub str)
  "Test if a string (str) ends with the substring (sub)."
  (slip:starts-with (reverse sub) (reverse str)))

(defun join-strings (string-list)
  "Joins a list of strings into single string."
	(let ((one-string "")
				(last-string (1- (length string-list))))
		(loop for i from 0 to last-string do
			(setf one-string (concatenate 'string one-string (nth i string-list)))
			(if (< i last-string)
					(setf one-string (concatenate 'string one-string " "))))
		one-string))

(defun split-string(line delim)
  "Split a line of text separated by 'delim'."
  (let ( (token nil) 
         (tokens nil) 
         (delim-loc 0) )
    (loop while (not (null delim-loc)) do
           (setf delim-loc (position delim line))
           (if (null delim-loc)
               (progn
                 (setf tokens (append (list line) tokens))
                 (return-from split-string (reverse tokens)))
             (progn
               (setf token (subseq line 0 delim-loc))
               (setf tokens (append (list token) tokens))
               (setf line (subseq line (+ 1 delim-loc) (length line))))))))

(defun char-in-string (char string)
  (position char string))

(defun not-these-chars (char-list word)
  (let ((pos (loop for c in char-list collect (slip:char-in-string c word))))
    (every #'null pos)))

(defun word-in-string (word string)
  (loop for w in (slip:split-string string #\ ) do
       (if (string-equal w word) (return t))))

(defun last-word (string)
  (let* ((tokens (slip:split-string string #\ ))
        (num-tokens (length tokens))
        (last-word nil))
    (if (> num-tokens 0)
        (setf last-word (nth (1- num-tokens) tokens)))
    last-word))

;;   [List Functions]

(defun filter (f l)
  (remove-if (lambda (x)
               (not (funcall f x))) l))

(defmacro foreach-i(lst &body b)
  "example: slip:foreach-i animals (print slip:i))"
  `(loop for i in ,lst do
        ,@b))

(defun generate(start end &optional (increment 1) )
  (loop for i from start to end by increment collect i))

(defun mapcard (f l)
	"Destructively apply function f to each element of list l."
	(loop for cons on l do
		(setf (car cons) (funcall f (car cons))))
	l)

(defun sort-list (l)
  "Sort list of strings in alphabetical order (very common!)"
  (sort l #'string-lessp))

(defun random-choice (a)
  "Return random choice from list or array."
  (progn
    (cond ( (listp a)   (nth (random (length a)) a) )
          ( (vectorp a) (aref a (random (length a))) )
          ( t (princ "slip:random-choice: unknown type!") ) )))

                                        ; from Peter Norvig: PAIP

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun shuffle (l)
  "Return a randomized (shuffled) copy of a list."
  (let ( (shuffled nil)
         (used-indices nil)
         (len-l (length l))
         (new-rand nil) )

    (loop while (not (= (length shuffled) len-l)) do
      (progn
        ; get new random index.
        (setf new-rand (random len-l))
                                        ; loop until we get a random index of input list that
                                        ; we haven't used before.
        (loop while (member new-rand used-indices) do
          (setf new-rand (random len-l)))
                                        ; 
        (push new-rand used-indices)
        (push (nth new-rand l) shuffled)))
    
                                        ; return rearranged list.
    shuffled))

(defun one-of (set)
  "Pick on element of set, and amke a list of it."
  (list (random-elt set)))

(defun list->array (l)
  "Convert a list to an array."
  (make-array (length l) :initial-contents l))

;;   [File Functions]

(defun file-loop(filename func)
  "Call function on lines in a file."
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil 'stop)
       until (eq line 'stop)
       do (funcall func line))))

(defun process-file(filename func delim)
  "Process lines in a file with a function that uses delimeter."
  (let ( (new-lines nil) )
    (with-open-file (f filename :direction :input)
      (loop for line = (read-line f nil 'end)
            until (eq line 'end)
            do (progn
                 (push (funcall func line delim) new-lines)))
      (reverse new-lines))))

(defun file-to-list (filename)
  (let ( (lines nil) )
    (with-open-file (f filename :direction :input)
      (loop for line = (read-line f nil nil)
				while line do
        (push line lines)))
    (reverse lines)))

(defun cut-file (f delim col)
  "Cut nth column from file f delimited by 'delim' and specified by col (1-based)
   and return column as list."
  (let ((lyst nil) (split nil)
        (flines (slip:file-to-list f)))
    (print flines)
    (dolist (line flines)
      (setf split (slip:split-string line delim))
      (push (nth (1- col) split) lyst))
    (reverse lyst)))

(defun list-to-file (lst filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (loop for l in lst do
          (format f "~a~%" l))))

(defmacro with-file-lines (fname line &body b)
  `(with-open-file (f ,fname :direction :input)
      (loop for ,line = (read-line f nil 'end)
  until (eq ,line 'end)
    do (progn ,@b ))))

;;   [Array Functions]

(defun file-matrix (fname delim)
"parse delimited file (fname) with delimeter (delim) into a 2-d array."
  (let* ((row 0) (flist (slip:file-to-list fname))
         (nrows (length flist))
         (firstrow (nth 0 flist))
         (ncols (length (slip:split-string firstrow delim)))
         (farray (make-array (list nrows ncols) :initial-element nil)))
    (loop for line in flist do
         (progn
           (setf columns (slip:split-string line delim))
           (loop for col from 0 to (1- ncols) do
                (setf (aref farray row col) (nth col columns)))
           (incf row)))
    farray))

(defun matrix-to-file (array fname)
  (let* ((dims (array-dimensions array))
         (nrows (1- (first dims)))
         (ncols (1- (second dims))))
    
    (with-open-file (f fname :direction :output)
      (loop for r from 0 to nrows do
           (progn
             (format f "~%")
             (loop for c from 0 to ncols do
                  (format f "~a " (aref array r c))))))))

(defun extract-column (col-n matrix)
  "extract one column from a 2-d array and return it as a list."
  (let ((col-list nil)
        (nrows (first (array-dimensions matrix))))
    (loop for r from 0 to (1- nrows) do
         (push (aref matrix r col-n) col-list))
    (reverse col-list)))

;;   [Struct Functions]

(defun magic-struct (struct-name fields-list)
  (let ((struct-string))
    (setf struct-string (format nil "(defstruct ~a ~{~a ~})" struct-name fields-list))
    (eval (read-from-string struct-string))))

;;   [Hash Table Functions]

(defun hash-key (ht key)
  "Test if key is in hash table."
  (not (null (member key
       (loop for k being the hash-keys of ht collect k)))))

 (defun store-hash (h k v)
   "Store value (v) into hash table (h) using key (k)." 
   (setf (gethash k h) v)) 

(defun show-hash (h)
  "Show summary of hash table contents."
  (let ((keys nil))
    (setf keys (loop for k being the hash-keys of h collect k))
    (setf keys (reverse keys))
    (dolist (k keys)
      (slip:dot-display (string k) (gethash k h) 40))))

(defun show-hash-type (h)
  "Show summary of hash table contents."
  (let ((keys nil))
    (setf keys (loop for key being the hash-keys of h collect key))
    (dolist (key keys)
      (slip:dot-display-type (string key) (gethash key h) 30))))

(defun assoc-to-hash (l)
  "Convert an assoc list to a hash table."
  ;;
  ;; (defvar items '( (coffee . 1.50)
  ;;                (juice . 2.50)
  ;;                (muffin . .75)
  ;;                (crossaint . 3.50) ))
  ;;
  ;; (setf menu (slip:fill-hash items))

  (let ( (ht (make-hash-table)) )
  (dolist (item l)
    (slip:store-hash ht (car item) (cdr item)))
  ht))

(defun list-to-hash (lst)
  (let ( (hash (make-hash-table))
        (num-items (length lst)) )
    (loop for i from 0 to (- num-items 1) by 2 do
         (slip:store-hash hash (nth i lst) (nth (+ 1 i) lst)))
    hash))

(defun sum-hash (h)
  "Sum values across a hash table.  Assumes values are numeric."
  (loop for key being the hash-keys of h
     sum (gethash key h)))

(defun compute-sqr-deviation (x mean)
  (expt (- x mean) 2))

(defun z-score (x mean sigma)
  (/ (- x mean) sigma))

(defun stats-hash (h)
  "Display various statistics on the values on a hash h. (Assumes float values.)"
  (progn
    (slip:show-hash h)                    ; show hash again
    (let* ((n (hash-table-count h))
           (sum (slip:sum-hash h))
           (mean (/ sum n))
           (sum-deviations 0.0)
           (sigma 0.0)
           (s 0.0)
           (z-scores (make-hash-table)))
      
      (loop for k being the hash-keys of h do (incf sum-deviations (compute-sqr-deviation (gethash k h)
                                                                                          mean)))
      (setf sigma (sqrt (/ sum-deviations n)))
      (setf s (sqrt (/ sum-deviations (1- n))))

                                        ; create new hash table for z-scores.
      (loop for k being the hash-keys of h do
        (slip:store-hash z-scores k (z-score (gethash k h) mean s)))
           
      (format t "~% -----------------------------------------")
      (format t "~% n:                      ~,4f" n)
      (format t "~% sum:                    ~,4f" sum)
      ; (format t "~% sum squared deviations: ~,4f" sum-deviations)
      (format t "~% mean:                   ~,4f" mean)
      (format t "~% sigma (pop):            ~,4f" sigma)
      (format t "~% s (sample):             ~,4f" s)
      (format t "~% z-scores:")
      (slip:show-hash z-scores))))

;; bits

(defun make-bit-array (size)
  (make-array size :element-type 'bit))

(defun bit-set? (flags bit)
  (let ((size (1- (length flags))))
    (equal (aref flags (- size bit)) 1)))

(defun bit-set (flags bit)
  (let ((size (1- (length flags))))
    (setf (aref flags (- size bit)) 1)))

;;   [Load/Save Functions]

(defun save-to-file (obj fname)
  "Save a Lisp object to file."
  (let ( (f nil)
         (*print-readably* t))
    (if (probe-file fname)
        (setf f (open fname :direction :output :if-exists :append))
        (setf f (open fname :direction :output :if-exists :supersede)))
    (print obj f)
    (close f)))

(defun open-file (fname)
  "Open a file for reading."
  (let ( (f (open fname :direction :input)) )
    (format t "~% Now you can load lisp objects with:")
    (format t "~%   (defvar a (read f))")
    (format t "~%   (defvar b (read f))")
    (format t "~%   (close f)~%~%")
    f))

(defun read-objects(fname)
  "Read Lisp objects from file."
  (let ( (objects nil) )
    (with-open-file (f fname :direction :input)
      (loop for obj = (read f nil 'end)
            until (eq obj 'end)
            do (progn
                 (push obj objects)))
      (reverse objects))))

;;    [Test Helpers]

(defun creature-list ()
  "Return list of creatures to use as test data."
  (list 'dragon 'mermaid 'gorgon 'griffin 'minotaur 
        'kraken 'cyclops 'troll 'orc 'goblin 'unicorn
        'fawn 'siren 'vampire 'werewolf))

;; (defmacro with-timing (&body body)
;;   "A macro that wraps a body of code and prints the time taken to execute.
;;    Measures both real (wall clock) and run time."
;;   (let ((start-real-time (gensym 'real-time))
;;         (start-run-time (gensym 'run-time))
;;         (end-real-time (gensym 'real-time))
;;         (end-run-time (gensym 'run-time)))
;;     `(let ((,start-real-time (get-internal-real-time))
;;            (,start-run-time (get-internal-run-time)))
;;        (unwind-protect
;;             (progn 
;;               ,@body)
;;          (let ((,end-real-time (get-internal-real-time))
;;                (,end-run-time (get-internal-run-time)))
;;            (format t "~&Execution Time:~
;;                      ~%  Real Time: ~,3F seconds~
;;                      ~%  Run Time:  ~,3F seconds"
;;                    (/ (- ,end-real-time ,start-real-time) 
;;                       internal-time-units-per-second)
;;                    (/ (- ,end-run-time ,start-run-time) 
;;                       internal-time-units-per-second)))))))


(defvar black   "[1;30m")
(defvar red     "[1;31m")
(defvar green   "[1;32m")
(defvar yellow  "[1;33m")
(defvar blue    "[1;34m")
(defvar magenta "[1;35m")
(defvar cyan    "[1;36m")
(defvar white   "[1;37m")

(defvar black_u   "[4;30m")
(defvar red_u     "[4;31m")
(defvar green_u   "[4;32m")
(defvar yellow_u  "[4;33m")
(defvar blue_u    "[4;34m")
(defvar magenta_u "[4;35m")
(defvar cyan_u    "[4;36m")
(defvar white_u   "[4;37m")

(defvar black_b   "[40m")
(defvar red_b     "[41m")
(defvar green_b   "[42m")
(defvar yellow_b  "[43m")
(defvar blue_b    "[44m")
(defvar magenta_b "[45m")
(defvar cyan_b    "[46m")
(defvar white_b   "[47m")

(defvar reset   "[0m")
(defvar esc     #\ESC)

(defun color (mesg color)
  "Print out in color using format statement"
  (format t "~c~a" esc color)
  (format t "~A" mesg)
  (format t "~c~a" esc reset))

(defvar emojis (make-hash-table :test 'equal))

;; Smileys & Emotion
(setf (gethash "grinning face" emojis) 128512)
(setf (gethash "smiley" emojis) 128515)
(setf (gethash "smile" emojis) 128516)
(setf (gethash "grin" emojis) 128513)
(setf (gethash "laughing" emojis) 128518)
(setf (gethash "joy" emojis) 128514)
(setf (gethash "rofl" emojis) 129315)
(setf (gethash "relaxed" emojis) 9786)
(setf (gethash "blush" emojis) 128522)
(setf (gethash "innocent" emojis) 128519)
(setf (gethash "slight smile" emojis) 128578)
(setf (gethash "upside down" emojis) 128579)
(setf (gethash "wink" emojis) 128521)
(setf (gethash "relieved" emojis) 128524)
(setf (gethash "heart eyes" emojis) 128525)
(setf (gethash "kissing heart" emojis) 128536)
(setf (gethash "kissing" emojis) 128535)
(setf (gethash "kissing smiling eyes" emojis) 128537)
(setf (gethash "kissing closed eyes" emojis) 128538)
(setf (gethash "yum" emojis) 128523)
(setf (gethash "stuck out tongue" emojis) 128539)
(setf (gethash "stuck out tongue winking eye" emojis) 128540)
(setf (gethash "stuck out tongue closed eyes" emojis) 128541)
(setf (gethash "drooling" emojis) 129316)
(setf (gethash "thinking" emojis) 129300)
(setf (gethash "zipper mouth" emojis) 129296)
(setf (gethash "raised eyebrow" emojis) 129320)
(setf (gethash "neutral face" emojis) 128528)
(setf (gethash "expressionless" emojis) 128529)
(setf (gethash "no mouth" emojis) 128566)
(setf (gethash "smirk" emojis) 128527)
(setf (gethash "unamused" emojis) 128530)
(setf (gethash "rolling eyes" emojis) 128580)
(setf (gethash "grimacing" emojis) 128556)
(setf (gethash "lying face" emojis) 129317)
(setf (gethash "pensive" emojis) 128532)
(setf (gethash "worried" emojis) 128543)
(setf (gethash "confused" emojis) 128533)
(setf (gethash "slight frown" emojis) 128577)
(setf (gethash "frowning2" emojis) 9785)
(setf (gethash "persevere" emojis) 128547)
(setf (gethash "confounded" emojis) 128534)
(setf (gethash "tired face" emojis) 128553)
(setf (gethash "weary" emojis) 128552)
(setf (gethash "pleading face" emojis) 129402)
(setf (gethash "cry" emojis) 128546)
(setf (gethash "sob" emojis) 128557)
(setf (gethash "triumph" emojis) 128548)
(setf (gethash "angry" emojis) 128544)
(setf (gethash "rage" emojis) 128545)
(setf (gethash "exploding head" emojis) 129327)
(setf (gethash "flushed" emojis) 128563)
(setf (gethash "hot face" emojis) 129397)
(setf (gethash "cold face" emojis) 129398)
(setf (gethash "scream" emojis) 128561)
(setf (gethash "fearful" emojis) 128552)
(setf (gethash "cold sweat" emojis) 128560)
(setf (gethash "disappointed relieved" emojis) 128549)
(setf (gethash "sweat" emojis) 128531)
(setf (gethash "hugging face" emojis) 129303)
(setf (gethash "thinking face" emojis) 129300)
(setf (gethash "shushing face" emojis) 129323)
(setf (gethash "face with hand over mouth" emojis) 129325)
(setf (gethash "saluting face" emojis) 129761)

;; People & Body
(setf (gethash "waving hand" emojis) 128075)
(setf (gethash "raised back of hand" emojis) 129306)
(setf (gethash "raised hand" emojis) 9995)
(setf (gethash "vulcan salute" emojis) 128406)
(setf (gethash "ok hand" emojis) 128076)
(setf (gethash "pinched fingers" emojis) 129295)
(setf (gethash "pinching hand" emojis) 129295)
(setf (gethash "victory hand" emojis) 9996)
(setf (gethash "crossed fingers" emojis) 129310)
(setf (gethash "love you gesture" emojis) 129311)
(setf (gethash "metal" emojis) 129304)
(setf (gethash "call me hand" emojis) 129305)
(setf (gethash "point left" emojis) 128072)
(setf (gethash "point right" emojis) 128073)
(setf (gethash "point up 2" emojis) 128070)
(setf (gethash "middle finger" emojis) 128405)
(setf (gethash "point down" emojis) 128071)
(setf (gethash "thumbs up" emojis) 128077)
(setf (gethash "thumbs down" emojis) 128078)
(setf (gethash "raised fist" emojis) 9994)
(setf (gethash "oncoming fist" emojis) 128074)
(setf (gethash "left facing fist" emojis) 129307)
(setf (gethash "right facing fist" emojis) 129308)
(setf (gethash "clap" emojis) 128079)
(setf (gethash "raised hands" emojis) 128588)
(setf (gethash "heart hands" emojis) 129728)
(setf (gethash "open hands" emojis) 128080)
(setf (gethash "palms up together" emojis) 129330)
(setf (gethash "handshake" emojis) 129309)
(setf (gethash "pray" emojis) 128591)

;; Animals & Nature
(setf (gethash "monkey face" emojis) 128053)
(setf (gethash "monkey" emojis) 128018)
(setf (gethash "gorilla" emojis) 129421)
(setf (gethash "orangutan" emojis) 129447)
(setf (gethash "dog" emojis) 128054)
(setf (gethash "dog2" emojis) 128021)
(setf (gethash "guide dog" emojis) 129454)
(setf (gethash "cat" emojis) 128049)
(setf (gethash "cat2" emojis) 128008)
(setf (gethash "lion" emojis) 129409)
(setf (gethash "tiger" emojis) 128047)
(setf (gethash "tiger2" emojis) 128005)
(setf (gethash "horse" emojis) 128052)
(setf (gethash "horse2" emojis) 128014)
(setf (gethash "unicorn" emojis) 129412)
(setf (gethash "zebra" emojis) 129427)
(setf (gethash "deer" emojis) 129420)
(setf (gethash "cow" emojis) 128046)
(setf (gethash "ox" emojis) 128002)
(setf (gethash "water buffalo" emojis) 128003)
(setf (gethash "cow2" emojis) 128004)
(setf (gethash "pig" emojis) 128055)
(setf (gethash "pig2" emojis) 128022)
(setf (gethash "boar" emojis) 128023)
(setf (gethash "pig nose" emojis) 128061)
(setf (gethash "sheep" emojis) 128017)
(setf (gethash "goat" emojis) 128016)
(setf (gethash "camel" emojis) 128043)
(setf (gethash "giraffe" emojis) 129426)
(setf (gethash "elephant" emojis) 128024)
(setf (gethash "rhinoceros" emojis) 129430)
(setf (gethash "hippopotamus" emojis) 129435)

;; Food & Drink
(setf (gethash "grapes" emojis) 127815)
(setf (gethash "melon" emojis) 127816)
(setf (gethash "watermelon" emojis) 127817)
(setf (gethash "tangerine" emojis) 127818)
(setf (gethash "lemon" emojis) 127819)
(setf (gethash "banana" emojis) 127820)
(setf (gethash "pineapple" emojis) 127821)
(setf (gethash "apple" emojis) 127822)
(setf (gethash "green apple" emojis) 127823)
(setf (gethash "pear" emojis) 127824)
(setf (gethash "peach" emojis) 127825)
(setf (gethash "cherries" emojis) 127826)
(setf (gethash "strawberry" emojis) 127827)
(setf (gethash "pizza" emojis) 127829)
(setf (gethash "hamburger" emojis) 127828)
(setf (gethash "fries" emojis) 127839)
(setf (gethash "hotdog" emojis) 127789)
(setf (gethash "sandwich" emojis) 129386)
(setf (gethash "taco" emojis) 127790)
(setf (gethash "burrito" emojis) 127791)
(setf (gethash "cookie" emojis) 127850)
(setf (gethash "cake" emojis) 127856)
(setf (gethash "cupcake" emojis) 129473)
(setf (gethash "ice cream" emojis) 127846)
(setf (gethash "coffee" emojis) 9749)
(setf (gethash "tea" emojis) 127861)
(setf (gethash "sake" emojis) 127862)
(setf (gethash "beer" emojis) 127866)
(setf (gethash "wine glass" emojis) 127863)
(setf (gethash "tumbler glass" emojis) 129347)

;; Travel & Places
(setf (gethash "car" emojis) 128664)
(setf (gethash "bus" emojis) 128652)
(setf (gethash "train" emojis) 128646)
(setf (gethash "airplane" emojis) 9992)
(setf (gethash "rocket" emojis) 128640)
(setf (gethash "bicycle" emojis) 128690)
(setf (gethash "motorcycle" emojis) 127949)
(setf (gethash "house" emojis) 127968)
(setf (gethash "hotel" emojis) 127976)
(setf (gethash "stadium" emojis) 127967)
(setf (gethash "beach" emojis) 127958)
(setf (gethash "desert" emojis) 127964)
(setf (gethash "mountain" emojis) 9968)
(setf (gethash "volcano" emojis) 127755)
(setf (gethash "world map" emojis) 128506)
(setf (gethash "compass" emojis) 129517)

;; Activities
(setf (gethash "soccer" emojis) 9917)
(setf (gethash "basketball" emojis) 127936)
(setf (gethash "football" emojis) 127944)
(setf (gethash "baseball" emojis) 9918)
(setf (gethash "tennis" emojis) 127934)
(setf (gethash "volleyball" emojis) 127952)
(setf (gethash "trophy" emojis) 127942)
(setf (gethash "medal" emojis) 127941)
(setf (gethash "first place" emojis) 129351)
(setf (gethash "second place" emojis) 129352)
(setf (gethash "third place" emojis) 129353)

;; Objects
(setf (gethash "watch" emojis) 8986)
(setf (gethash "mobile phone" emojis) 128241)
(setf (gethash "laptop" emojis) 128187)
(setf (gethash "keyboard" emojis) 9000)
(setf (gethash "printer" emojis) 128424)
(setf (gethash "computer mouse" emojis) 128433)
(setf (gethash "joystick" emojis) 128377)
(setf (gethash "money bag" emojis) 128176)
(setf (gethash "dollar" emojis) 128181)
(setf (gethash "credit card" emojis) 128179)
(setf (gethash "gem" emojis) 128142)
(setf (gethash "tools" emojis) 128736)
(setf (gethash "hammer" emojis) 128296)
(setf (gethash "wrench" emojis) 128295)
(setf (gethash "gear" emojis) 9881)
(setf (gethash "lock" emojis) 128274)
(setf (gethash "unlock" emojis) 128275)
(setf (gethash "key" emojis) 128273)
(setf (gethash "bulb" emojis) 128161)
(setf (gethash "flashlight" emojis) 128294)
(setf (gethash "battery" emojis) 128267)
(setf (gethash "pill" emojis) 128138)
(setf (gethash "syringe" emojis) 128137)
(setf (gethash "test tube" emojis) 129514)
(setf (gethash "microscope" emojis) 128300)
(setf (gethash "telescope" emojis) 128301)

;; Symbols
(setf (gethash "heart" emojis) 10084)
(setf (gethash "orange heart" emojis) 129505)
(setf (gethash "yellow heart" emojis) 128155)
(setf (gethash "green heart" emojis) 128154)
(setf (gethash "blue heart" emojis) 128153)
(setf (gethash "purple heart" emojis) 128156)
(setf (gethash "black heart" emojis) 128420)
(setf (gethash "broken heart" emojis) 128148)
(setf (gethash "sparkling heart" emojis) 128150)
(setf (gethash "growing heart" emojis) 128151)
(setf (gethash "beating heart" emojis) 128147)
(setf (gethash "revolving hearts" emojis) 128158)
(setf (gethash "two hearts" emojis) 128149)
(setf (gethash "heart decoration" emojis) 128159)
(setf (gethash "star" emojis) 11088)
(setf (gethash "star2" emojis) 127775)
(setf (gethash "sparkles" emojis) 10024)
(setf (gethash "100" emojis) 128175)
(setf (gethash "anger" emojis) 128162)
(setf (gethash "boom" emojis) 128165)
(setf (gethash "dizzy" emojis) 128171)
(setf (gethash "sweat drops" emojis) 128166)
(setf (gethash "dash" emojis) 128168)
(setf (gethash "fire" emojis) 128293)
(setf (gethash "warning" emojis) 9888)
(setf (gethash "question" emojis) 10067)
(setf (gethash "exclamation" emojis) 10071)
(setf (gethash "ok" emojis) 127383)
(setf (gethash "new" emojis) 127381)
(setf (gethash "cool" emojis) 127378)
(setf (gethash "free" emojis) 127379)
(setf (gethash "zzz" emojis) 128164)

;; Faces & Fantasy Creatures
(setf (gethash "robot face" emojis) 129302)
(setf (gethash "alien" emojis) 128125)
(setf (gethash "alien monster" emojis) 128126)
(setf (gethash "ghost" emojis) 128123)
(setf (gethash "skull" emojis) 128128)
(setf (gethash "skull and crossbones" emojis) 9760)
(setf (gethash "imp" emojis) 128127)
(setf (gethash "smiling imp" emojis) 128520)
(setf (gethash "japanese ogre" emojis) 128121)
(setf (gethash "japanese goblin" emojis) 128122)
(setf (gethash "clown face" emojis) 129313)
(setf (gethash "poop" emojis) 128169)
(setf (gethash "jack o lantern" emojis) 127875)
(setf (gethash "space invader" emojis) 128126)
(setf (gethash "zombie" emojis) 129503)
(setf (gethash "vampire" emojis) 129499)
(setf (gethash "merperson" emojis) 129500)
(setf (gethash "fairy" emojis) 129498)
(setf (gethash "genie" emojis) 129502)
(setf (gethash "elf" emojis) 129497)
(setf (gethash "troll" emojis) 129497)
(setf (gethash "ninja" emojis) 129399)
(setf (gethash "supervillain" emojis) 129513)
(setf (gethash "superhero" emojis) 129464)
(setf (gethash "mage" emojis) 129497)

(defun emoji-keys ()
	(loop for k being the hash-keys of slip:emojis collect k))

(defun emoji (emoji-name)
	(code-char (gethash emoji-name slip:emojis)))

(defun one-emoji (emoji-name)
	(format t "~A" (slip:emoji emoji-name)))

(defconstant emoji-objects '("wine glass" 
														 "tumbler glass"
														 "bicycle"
														 "world map"
														 "compass"
														 "watch"
														 "mobile phone"
														 "laptop"
														 "keyboard"
														 "printer"
														 "computer mouse"
														 "joystick"
														 "money bag"
														 "dollar"
														 "credit card"
														 "gem"
														 "tools"
														 "hammer"
														 "wrench"
														 "gear"
														 "lock"
														 "unlock"
														 "key"
														 "bulb"
														 "flashlight"
														 "battery"
														 "pill"
														 "syringe"
														 "test tube"
														 "microscope"
														 "telescope"))

;; Hash table of the top 100 most populous countries
;; Key: country name (string),
;; Value: population in engineering notation (rounded to nearest million)

(defun populate-countries ()
	"This will create the hash-table, populate the countries at run-time and return 
   the hash-table that way we don't populate it while loading the package."

	(let ((countries (make-hash-table :test 'equal)))
	
		;; Top 100 countries by population (2024/2025 data)
		(setf (gethash "India" countries) 1.428e9)         ; 1,428 million
		(setf (gethash "China" countries) 1.413e9)         ; 1,413 million  
		(setf (gethash "United States" countries) 343e6)   ; 343 million
		(setf (gethash "Indonesia" countries) 278e6)       ; 278 million
		(setf (gethash "Pakistan" countries) 241e6)        ; 241 million
		(setf (gethash "Nigeria" countries) 230e6)         ; 230 million
		(setf (gethash "Brazil" countries) 217e6)          ; 217 million
		(setf (gethash "Bangladesh" countries) 173e6)      ; 173 million
		(setf (gethash "Russia" countries) 144e6)          ; 144 million
		(setf (gethash "Mexico" countries) 129e6)          ; 129 million
		(setf (gethash "Ethiopia" countries) 132e6)        ; 132 million
		(setf (gethash "Japan" countries) 123e6)           ; 123 million
		(setf (gethash "Philippines" countries) 118e6)     ; 118 million
		(setf (gethash "Egypt" countries) 113e6)           ; 113 million
		(setf (gethash "Vietnam" countries) 99e6)          ; 99 million
		(setf (gethash "Iran" countries) 87e6)             ; 87 million
		(setf (gethash "Turkey" countries) 86e6)           ; 86 million
		(setf (gethash "Germany" countries) 84e6)          ; 84 million
		(setf (gethash "Thailand" countries) 72e6)         ; 72 million
		(setf (gethash "United Kingdom" countries) 69e6)   ; 69 million
		(setf (gethash "France" countries) 69e6)           ; 69 million
		(setf (gethash "Tanzania" countries) 68e6)         ; 68 million
		(setf (gethash "South Africa" countries) 63e6)     ; 63 million
		(setf (gethash "Italy" countries) 59e6)            ; 59 million
		(setf (gethash "Kenya" countries) 56e6)            ; 56 million
		(setf (gethash "South Korea" countries) 52e6)      ; 52 million
		(setf (gethash "Colombia" countries) 52e6)         ; 52 million
		(setf (gethash "Myanmar" countries) 51e6)          ; 51 million
		(setf (gethash "Sudan" countries) 49e6)            ; 49 million
		(setf (gethash "Spain" countries) 48e6)            ; 48 million
		(setf (gethash "Uganda" countries) 48e6)           ; 48 million
		(setf (gethash "Argentina" countries) 46e6)        ; 46 million
		(setf (gethash "Algeria" countries) 46e6)          ; 46 million
		(setf (gethash "Iraq" countries) 46e6)             ; 46 million
		(setf (gethash "Afghanistan" countries) 42e6)      ; 42 million
		(setf (gethash "Canada" countries) 40e6)           ; 40 million
		(setf (gethash "Morocco" countries) 38e6)          ; 38 million
		(setf (gethash "Ukraine" countries) 37e6)          ; 37 million
		(setf (gethash "Poland" countries) 37e6)           ; 37 million
		(setf (gethash "Angola" countries) 37e6)           ; 37 million
		(setf (gethash "Saudi Arabia" countries) 36e6)     ; 36 million
		(setf (gethash "Uzbekistan" countries) 36e6)       ; 36 million
		(setf (gethash "Yemen" countries) 35e6)            ; 35 million
		(setf (gethash "Peru" countries) 34e6)             ; 34 million
		(setf (gethash "Malaysia" countries) 34e6)         ; 34 million
		(setf (gethash "Ghana" countries) 34e6)            ; 34 million
		(setf (gethash "Mozambique" countries) 33e6)       ; 33 million
		(setf (gethash "Madagascar" countries) 31e6)       ; 31 million
		(setf (gethash "Nepal" countries) 30e6)            ; 30 million
		(setf (gethash "Venezuela" countries) 29e6)        ; 29 million
		(setf (gethash "Ivory Coast" countries) 29e6)      ; 29 million
		(setf (gethash "Cameroon" countries) 28e6)         ; 28 million
		(setf (gethash "Niger" countries) 27e6)            ; 27 million
		(setf (gethash "Australia" countries) 26e6)        ; 26 million
		(setf (gethash "North Korea" countries) 26e6)      ; 26 million
		(setf (gethash "Taiwan" countries) 23e6)           ; 23 million
		(setf (gethash "Mali" countries) 23e6)             ; 23 million
		(setf (gethash "Burkina Faso" countries) 23e6)     ; 23 million
		(setf (gethash "Sri Lanka" countries) 22e6)        ; 22 million
		(setf (gethash "Malawi" countries) 21e6)           ; 21 million
		(setf (gethash "Kazakhstan" countries) 20e6)       ; 20 million
		(setf (gethash "Chile" countries) 20e6)            ; 20 million
		(setf (gethash "Zambia" countries) 20e6)           ; 20 million
		(setf (gethash "Romania" countries) 19e6)          ; 19 million
		(setf (gethash "Somalia" countries) 18e6)          ; 18 million
		(setf (gethash "Senegal" countries) 18e6)          ; 18 million
		(setf (gethash "Netherlands" countries) 18e6)      ; 18 million
		(setf (gethash "Chad" countries) 18e6)             ; 18 million
		(setf (gethash "Guatemala" countries) 18e6)        ; 18 million
		(setf (gethash "Zimbabwe" countries) 16e6)         ; 16 million
		(setf (gethash "Cambodia" countries) 17e6)         ; 17 million
		(setf (gethash "South Sudan" countries) 11e6)      ; 11 million
		(setf (gethash "Ecuador" countries) 18e6)          ; 18 million
		(setf (gethash "Guinea" countries) 14e6)           ; 14 million
		(setf (gethash "Rwanda" countries) 14e6)           ; 14 million
		(setf (gethash "Benin" countries) 13e6)            ; 13 million
		(setf (gethash "Burundi" countries) 13e6)          ; 13 million
		(setf (gethash "Tunisia" countries) 12e6)          ; 12 million
		(setf (gethash "Bolivia" countries) 12e6)          ; 12 million
		(setf (gethash "Belgium" countries) 12e6)          ; 12 million
		(setf (gethash "Haiti" countries) 12e6)            ; 12 million
		(setf (gethash "Cuba" countries) 11e6)             ; 11 million
		(setf (gethash "Dominican Republic" countries) 11e6) ; 11 million
		(setf (gethash "Czech Republic" countries) 11e6)   ; 11 million
		(setf (gethash "Greece" countries) 11e6)           ; 11 million
		(setf (gethash "Jordan" countries) 11e6)           ; 11 million
		(setf (gethash "Portugal" countries) 10e6)         ; 10 million
		(setf (gethash "Azerbaijan" countries) 10e6)       ; 10 million
		(setf (gethash "Sweden" countries) 10e6)           ; 10 million
		(setf (gethash "Honduras" countries) 10e6)         ; 10 million
		(setf (gethash "United Arab Emirates" countries) 10e6) ; 10 million
		(setf (gethash "Hungary" countries) 10e6)          ; 10 million
		(setf (gethash "Tajikistan" countries) 10e6)       ; 10 million
		(setf (gethash "Belarus" countries) 9e6)           ; 9 million
		(setf (gethash "Austria" countries) 9e6)           ; 9 million
		(setf (gethash "Papua New Guinea" countries) 9e6)  ; 9 million
		(setf (gethash "Serbia" countries) 7e6)            ; 7 million
		(setf (gethash "Israel" countries) 10e6)           ; 10 million
		(setf (gethash "Switzerland" countries) 9e6)       ; 9 million
		(setf (gethash "Togo" countries) 9e6)              ; 9 million
		(setf (gethash "Sierra Leone" countries) 8e6)      ; 8 million
		(setf (gethash "Hong Kong" countries) 7e6)         ; 7 million
		(setf (gethash "Laos" countries) 8e6)              ; 8 million
		(setf (gethash "Paraguay" countries) 6e6)          ; 6 million
		(setf (gethash "Bulgaria" countries) 7e6)          ; 7 million
		(setf (gethash "Libya" countries) 7e6)             ; 7 million
		(setf (gethash "Lebanon" countries) 5e6)           ; 5 million
		(setf (gethash "Nicaragua" countries) 7e6)         ; 7 million
		(setf (gethash "Kyrgyzstan" countries) 7e6)        ; 7 million
		(setf (gethash "El Salvador" countries) 6e6)       ; 6 million
		(setf (gethash "Turkmenistan" countries) 6e6)      ; 6 million
		(setf (gethash "Singapore" countries) 6e6)         ; 6 million
		(setf (gethash "Denmark" countries) 6e6)           ; 6 million
		(setf (gethash "Finland" countries) 6e6)           ; 6 million
		(setf (gethash "Congo" countries) 6e6)             ; 6 million
		(setf (gethash "Slovakia" countries) 6e6)          ; 6 million
		(setf (gethash "Norway" countries) 6e6)            ; 6 million
		(setf (gethash "Oman" countries) 5e6)              ; 5 million
		(setf (gethash "State of Palestine" countries) 5e6) ; 5 million
		(setf (gethash "Costa Rica" countries) 5e6)        ; 5 million
		(setf (gethash "Liberia" countries) 5e6)           ; 5 million
		(setf (gethash "Ireland" countries) 5e6)           ; 5 million
		(setf (gethash "Central African Republic" countries) 5e6) ; 5 million
		(setf (gethash "New Zealand" countries) 5e6)       ; 5 million
		(setf (gethash "Mauritania" countries) 5e6)        ; 5 million
		(setf (gethash "Panama" countries) 4e6)            ; 4 million
		(setf (gethash "Kuwait" countries) 5e6)            ; 5 million
		(setf (gethash "Croatia" countries) 4e6)           ; 4 million
		(setf (gethash "Moldova" countries) 3e6)           ; 3 million
		(setf (gethash "Georgia" countries) 4e6)           ; 4 million
		(setf (gethash "Eritrea" countries) 4e6)           ; 4 million
		(setf (gethash "Uruguay" countries) 3e6)           ; 3 million
		(setf (gethash "Bosnia and Herzegovina" countries) 3e6) ; 3 million
		(setf (gethash "Mongolia" countries) 3e6)          ; 3 million
		(setf (gethash "Armenia" countries) 3e6)           ; 3 million
		(setf (gethash "Jamaica" countries) 3e6)           ; 3 million
		(setf (gethash "Qatar" countries) 3e6)             ; 3 million
		(setf (gethash "Albania" countries) 3e6)           ; 3 million
		(setf (gethash "Puerto Rico" countries) 3e6)       ; 3 million
		(setf (gethash "Lithuania" countries) 3e6)         ; 3 million
		(setf (gethash "Namibia" countries) 3e6)           ; 3 million
		countries))




		

