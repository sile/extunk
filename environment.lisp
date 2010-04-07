(defpackage extunk.environment
  (:use :common-lisp)
  (:export env
	   make-env
	   left
	   right
	   subject
	   env-left
	   env-right
	   env-subject))
(in-package :extunk.environment)

(defstruct (env (:constructor 
		 make-env (subject &aux (left (make-hash-table :test #'equal))
				        (right(make-hash-table :test #'equal)))))
  subject
  left
  right)