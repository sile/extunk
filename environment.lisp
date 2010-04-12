(defpackage extunk.environment
  (:use :common-lisp)
  (:export env
	   make-env
	   left
	   right
	   subject
	   env-left
	   env-right
	   env-subject
	   
	   left-context
	   right-context
	   ))
(in-package :extunk.environment)

(defstruct (env (:constructor 
		 make-env (subject &aux (left (make-hash-table :test #'equal))
				        (right(make-hash-table :test #'equal)))))
  subject
  left
  right)

(defconstant +CONTEXT-STRING-LENGTH-LIMIT+ 5)

(defun char-hiragana-p (ch) 
  (char<= #\ぁ ch #\ゖ))

(defun char-punctuation-p (ch) 
  (find ch "。、"))

(defun context-range (text start end from-end)
  '#1=(position-if-not #'char-hiragana-p text :start start :end end :from-end from-end)
  (if from-end
      (values (1+ (or #1# (1- start))) end)
    (values start (or #1# end))))

(defun right-context (text pos)
  (when (< pos (length text))
    (if (char-punctuation-p (char text pos))
	(subseq text pos (1+ pos))
      (let ((start pos)
	    (end-limit (min (length text) (+ pos +CONTEXT-STRING-LENGTH-LIMIT+))))
	(multiple-value-bind (start end) (context-range text start end-limit nil)
	  (when (< start end)
	    (subseq text start end)))))))

(defun left-context (text pos)
  (when (plusp pos)
    (if (char-punctuation-p (char text (1- pos)))
	(subseq text (1- pos) pos)
      (let ((start-limit (max 0 (- pos +CONTEXT-STRING-LENGTH-LIMIT+)))
	    (end pos))
        (multiple-value-bind (start end) (context-range text start-limit end t)
          (when (< start end)
	    (subseq text start end)))))))