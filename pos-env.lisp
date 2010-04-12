(defpackage extunk.pos-env
  (:use :common-lisp :extunk.environment)
  (:import-from :common-utils each-file-line a.when)
  (:nicknames penv)
  (:export init
	   calc))
(in-package :extunk.pos-env)

(defun ipadic-corpus-parser (feature)
  (declare (simple-string feature))
  (let ((unk? (= (count #\, feature) 6)))
    (if unk?
	:未知語
      (intern (subseq feature 0 (position #\, feature)) :keyword))))

(defvar *tagger*)

(defvar *target-pos* '(:形容詞 :連体詞 :動詞 :名詞))

(defun init (dic-dir)
  (setf *tagger* (igo:load-tagger dic-dir #'ipadic-corpus-parser)))

(defun find-env (pos envs)
  (find pos envs :key #'env-subject))

(defun calc (corpus-dir)
  (let ((envs (mapcar #'make-env *target-pos*)))
    (dolist (file (directory (merge-pathnames #P"*.txt" corpus-dir)))
      (each-file-line (l file)
        (loop FOR (surface pos start) IN (igo:parse l *tagger*) 
	      FOR env = (find-env pos envs)
          WHEN env
	  DO 
            (a.when (left-context l start)
              (incf (gethash it (env-left env) 0)))
	    (a.when (right-context l (+ start (length surface)))
	      (incf (gethash it (env-right env) 0))))))
    envs))

#+ignore
(defun pos-env (corpus-dir)
  (let ((envs '()))
    (dolist (file (directory (merge-pathnames #P"*.txt" corpus-dir)))
      (each-file-line (l file)
        
        (loop FOR (surface pos start) IN (igo:parse (trim l) *tagger*) DO
          (unless (find pos envs :key #'env-subject)
	    (push (make-env pos) envs))

	  (let ((env (find pos envs :key #'env-subject))
		(lft-w (get-rs l start))
		(rgt-w (get-s l (+ start (length surface)))))
	    (when (plusp (length lft-w))
	      (incf (gethash lft-w (env-left env) 0)))
	    (when (plusp (length rgt-w))
	      (incf (gethash rgt-w (env-right env) 0)))))))
    envs))

