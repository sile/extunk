(defpackage extunk.word-env
  (:use :common-lisp :extunk.environment)
  (:import-from :common-utils a.when)
  (:nicknames wenv)
  (:export calc
	   *freq-border*))
(in-package :extunk.word-env)

(defconstant +WORD-MIN-LENGTH+ 2)
(defvar *freq-border* 10)

(defun overlap-length (string start1 start2)
  (assert (/= start1 start2))
  (- (mismatch string string :start1 start1 :start2 start2) start1))

(defun valid-word-char-p (ch)
  (and (graphic-char-p ch)
       (not (find ch "。、 　"))))

(defun add-to-env (env-set text from-len to-len indices &aux (head (car indices)))
  (when (< (length indices) *freq-border*)
    (return-from add-to-env))
  
  (loop FOR len FROM (max from-len +WORD-MIN-LENGTH+) TO to-len
	FOR word = (subseq text head (+ head len))
    WHEN (every #'valid-word-char-p word)
    DO
    (let ((env (if #1=(gethash word env-set) #1# (setf #1# (make-env word)))))
      (dolist (index indices)
	(a.when (left-context text index)
	  (incf (gethash it (env-left env)  0)))
	(a.when (right-context text (+ index len))
	  (incf (gethash it (env-right env) 0)))))))

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