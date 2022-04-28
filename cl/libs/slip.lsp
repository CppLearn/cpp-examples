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
   :report 
   :disp
   :typewriter-string
   :dot-display
   :dot-display-type
                                        ; chars
   :char-upperp
   :rand-char
                                        ; strings
   :rtrim
   :startswith
   :endswith
   :split-line   
   :rand-string
                                        ; lists
   :foreach-i
   :i
   :generate
   :sort-list
   :random-choice
   :random-elt
   :one-of
   :list->array
                                        ; files
   :file-loop
   :process-file
   :file-to-list
   :cut-file
   :list-to-file
   :file-matrix
   :matrix-to-file
                                        ; arrays
   :extract-column
                                        ; hash funcs
   :store-hash
   :assoc-to-hash
   :list-to-hash
   :show-hash
   :show-hash-type
   :sum-hash
   :creature-list
  ))

(in-package slip)

;;   [General Purpose Functions]

(defun hello ()
  (write-line "hello!  Package slip is available!"))

(defun blank-line ()
  (fresh-line)
  (terpri))

(defun report (label var)
  (format t "~% [+] ~a = ~a" label var))

(defun disp (label &rest objs)
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

(defun startswith (sub str)
  "Test if a string (str) starts with the substring (sub)."
  (let ((end (length sub)))
    (if (string-equal sub (subseq str 0 end))
        t
        nil)))

(defun endswith (sub str)
  "Test if a string (str) ends with the substring (sub)."
  (slip:startswith (reverse sub) (reverse str)))

(defun split-line(line delim)
  "Split a line of text separated by 'delim'."
  (let ( (token nil) 
         (tokens nil) 
         (delim-loc 0) )
    (loop while (not (null delim-loc)) do
           (setf delim-loc (position delim line))
           (if (null delim-loc)
               (progn
                 (setf tokens (append (list line) tokens))
                 (return-from split-line (reverse tokens)))
             (progn
               (setf token (subseq line 0 delim-loc))
               (setf tokens (append (list token) tokens))
               (setf line (subseq line (+ 1 delim-loc) (length line))))))))

(defun rand-string (len)
  (let ( (s "") )
    (loop for i from 1 to len do
          (setf s (concatenate 'string s (string (rand-char)))))
    s ))

;;   [List Functions]

(defmacro foreach-i(lst &body b)
  "example: slip:foreach-i animals (print slip:i))"
  `(loop for i in ,lst do
        ,@b))

(defun generate(start end &optional (increment 1) )
  (loop for i from start to end by increment collect i))

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
      (loop for object = (read-line f nil 'end)
            until (eq object 'end)
            do (push object lines)))
    (reverse lines)))

(defun cut-file (f delim col)
  "Cut nth column from file f delimited by 'delim' and specified by col (1-based)
   and return column as list."
  (let ((lyst nil) (split nil)
        (flines (slip:file-to-list f)))
    (print flines)
    (dolist (line flines)
      (setf split (slip:split-line line delim))
      (push (nth (1- col) split) lyst))
    (reverse lyst)))

(defun list-to-file (lst filename)
  (with-open-file (f filename :direction :output)
    (loop for l in lst do
          (format f "~%~a" l))))

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
         (ncols (length (slip:split-line firstrow delim)))
         (farray (make-array (list nrows ncols) :initial-element nil)))
    (loop for line in flist do
         (progn
           (setf columns (slip:split-line line delim))
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

;;   [Hash Table Functions]

 (defun store-hash (h k v)
   "Store value (v) into hash table (h) using key (k)." 
   (setf (gethash k h) v)) 

(defun show-hash (h)
  "Show summary of hash table contents."
  (loop for key being the hash-keys of h do
        (slip:dot-display (string key) (gethash key h) 40)))

(defun show-hash-type (h)
  "Show summary of hash table contents."
  (loop for key being the hash-keys of h do
        (slip:dot-display-type (string key) (gethash key h) 30)))

(defun assoc-to-hash (l)
  "Convert an assoc list to a hash table."
  ;; (defvar items '( (coffee 1.50)
  ;;                (juice 2.50)
  ;;                (muffin .75)
  ;;                (crossaint 3.50) ))
  ;; (setf menu (slip:fill-hash items))

  (let ( (ht (make-hash-table)) )
  (dolist (item l)
    (slip:store-hash ht (car item) (cadr item)))
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

;;    [Test Helpers]

(defun creature-list ()
  "Return list of creatures to use as test data."
  (list 'dragon 'mermaid 'gorgon 'griffin 'minotaur 
        'kraken 'cyclops 'troll 'orc 'goblin 'unicorn
        'fawn 'siren 'vampire 'werewolf))


