(defpackage extunk.word-env
  (:use :common-lisp :extunk.environment)
  (:import-from :common-utils read-file nlet a.when)
  (:nicknames wenv)
  (:export calc)) 
(in-package :extunk.word-env)

(defconstant +WORD-MIN-LENGTH+ 2)
(defconstant +CONTEXT-STRING-LENGTH-LIMIT+ 5)

;; NOTE: always return a fixnum value
(defun overlap-length (string start1 start2)
  (- (mismatch string string :start1 start1 :start2 start2) start1))

(defun char-invalid-p (ch) 
  (case ch ((#\Space #\Return #\Newline #\Tab #\。 #\、 #\　) t)))

(defun char-hiragana-p (ch)  
  (char<= #\ぁ ch #\ゖ))

(defun char-punctuation-p (ch)
  (case ch ((#\。 #\、) t)))

(defun right-context (text pos)
  (when (< pos (length text))
    (if (char-punctuation-p (char text pos))
	(subseq text pos (1+ pos))
      (let* ((end-limit (min (length text) (+ pos +CONTEXT-STRING-LENGTH-LIMIT+)))
	     (end (or (position-if-not #'char-hiragana-p text :start pos :end end-limit) end-limit)))
	(when (> end pos)
	  (subseq text pos end))))))

(defun left-context (text pos)
  (when (plusp pos)
    (if (char-punctuation-p (char text (1- pos)))
	(subseq text (1- pos) pos)
      (do ((i pos (1- i))
	   (border (max 0 (- pos +CONTEXT-STRING-LENGTH-LIMIT+))))
	  ((or (= i border)
	       (not (char-hiragana-p (char text (1- i)))))
	   (when (< i pos)
	     (subseq text i pos)))))))

(defparameter *freq-border* 10)

(defun add-to-env (env-set text from-len to-len indices &aux (head (car indices)))
  (when (< (length indices) *freq-border*)
    (return-from add-to-env))
  
  (loop FOR len FROM (max from-len +WORD-MIN-LENGTH+) TO to-len
	FOR word = (subseq text head (+ head len))
    DO
      (unless (some #'char-invalid-p word) 
	(let ((env (if #1=(gethash word env-set) #1# (setf #1# (make-env word)))))
	  (dolist (index indices)
	    (a.when (left-context text index)
	      (incf (gethash it (env-left env)  0)))
	    (a.when (right-context text (+ index len))
	      (incf (gethash it (env-right env) 0))))))))

;; まだバグがあるかも...
(defun calc (text &optional (env-set (make-hash-table :test #'equal)))
  (let ((indices (sort (loop FOR i FROM 0 BELOW (length text) COLLECT i)
		       (lambda (i j) (string> text text :start1 i :start2 j)))))

    (labels ((self (head-indices indices base-len prev-len)
               (if (<= prev-len base-len)
		   (values indices prev-len)
		 (destructuring-bind (1st &rest rest &aux (2nd (car rest))) indices
		   (let ((len (if 2nd (overlap-length text 1st 2nd) 0)))
		     (when (> len prev-len)
		       (setf (values rest len) (self indices rest prev-len len)))

		     (when (< len prev-len)
		       (add-to-env env-set text (1+ (max base-len len)) prev-len (ldiff head-indices rest)))
		     
		     (self head-indices rest base-len len)))))
	     
	     (toplevel (indices)
	       (destructuring-bind (1st &rest rest &aux (2nd (car rest))) indices
	         (let ((len (if 2nd (overlap-length text 1st 2nd) 0)))
		   (self indices rest 0 len)))))

      (loop WHILE (setf indices (toplevel indices)))))
  env-set)