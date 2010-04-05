(defpackage extunk.corpus
  (:use :common-lisp)
  (:export init
	   pos-env
	   word-env
	   generate))
(in-package :extunk.corpus)

(defvar *tagger*)

(defun ipadic-corpus-parser (feature)
  (declare (simple-string feature))
  (let ((unk? (= (count #\, feature) 6)))
    (if unk?
	:未知語
      (intern (subseq feature 0 (position #\, feature)) :keyword))))

(defun trim (s)
  (string-trim #.(format nil " 　~C~C" #\Tab #\Return) s))

(defun init (dic-dir)
  (setf *tagger* (igo:load-tagger dic-dir #'ipadic-corpus-parser)
	igo:*tagger* nil))

(defun generate (source-dir output-stream)
  (dolist (file (directory (merge-pathnames #P"*.txt" source-dir)))
    (common-utils:each-file-line (l file)
      (loop FOR (surface part-of-speech) IN (igo:parse (trim l) *tagger*) DO
        (format output-stream "~A~C~A~%" surface #\Tab part-of-speech))
      (terpri output-stream)))
  :done)

(defstruct (env (:constructor make-env (name &aux (left (make-hash-table :test #'equal))
					          (right(make-hash-table :test #'equal)))))
  name
  left
  right)

(defun char-kana-p (ch)
  (char<= #\ぁ ch #\HIRAGANA_LETTER_SMALL_KE))

(defun get-s (text start)
  (when (= (length text) start)
    (return-from get-s ""))

  (if (or (char= (char text start) #\。)
	  (char= (char text start) #\、))
      (subseq text start (1+ start))
    (subseq text start (position-if-not #'char-kana-p text :start start))))

(defun get-rs (text start)
  (when (zerop start)
    (return-from get-rs ""))

  (if (or (char= (char text (1- start)) #\。)
	  (char= (char text (1- start)) #\、))
      (subseq text (1- start) start)
    (subseq text 
	    (loop FOR i FROM (1- start) DOWNTO 0 
	      DO
	      (unless (char-kana-p (char text i))
		(return (1+ i)))
	      FINALLY (return 0))
	  start)))

(defun pos-env (corpus-dir)
  (let ((envs '()))
    (dolist (file (directory (merge-pathnames #P"*.txt" corpus-dir)))
      (common-utils:each-file-line (l file)
        (loop FOR (surface pos start) IN (igo:parse (trim l) *tagger*) DO
          (unless (find pos envs :key #'env-name)
	    (push (make-env pos) envs))

	  (let ((env (find pos envs :key #'env-name))
		(lft-w (get-rs l start))
		(rgt-w (get-s l (+ start (length surface)))))
	    (when (plusp (length lft-w))
	      (incf (gethash lft-w (env-left env) 0)))
	    (when (plusp (length rgt-w))
	      (incf (gethash rgt-w (env-right env) 0)))))))
    envs))
#|
(defun pos-env (corpus-file)
  (let ((envs '())
	(prev nil))
    (common-utils:each-file-line (l corpus-file)
      (if (zerop (length l))
	  (setf prev nil)
	(destructuring-bind (surface pos) 
			    (common-utils:split-by-chars #.(princ-to-string #\Tab) l)
	  (let ((pos (intern pos :keyword)))
	    (unless (find pos envs :key #'env-name)
	      (push (make-env pos) envs))
	    (when prev
	      (let ((cur-env (find pos envs           :key #'env-name))
		    (prv-env (find (second prev) envs :key #'env-name)))
		(incf (gethash (first prev) (env-left cur-env) 0))
		(incf (gethash surface (env-right prv-env) 0))))
	    (setf prev `(,surface ,pos))))))
    envs))
|#
(defun word-env (text-file &optional (freq-border 10))
  (declare (ignorable text-file freq-border))
  #|
  (let* ((envs (make-hash-table :test #'equal))
	 (text (common-utils:read-file text-file))
	 (prev-len 0)
	 (buf '())
	 (cnt 0)
	 (indices (loop FOR i FROM 0 BELOW (length text) COLLECT i)))
    (setf indices (sort indices (lambda (i j) (string> text text :start1 i :start2 j))))

    (loop FOR (cur next) ON indices DO
      (let ((len (mismatch text text :start1 cur :start2 (or next 0))))
	(cond ((= len prev-len))
	      ((< len prev-len)
	       (when (>= cnt freq-border)
		 (dolist (i (cons cur buf))
		   
		 ))
	      (t 
	       (when (>= cnt freq-border)
		 )
	       (setf buf '())
	       (setf cnt 0)))
	
	(incf cnt)
	(push cur buf)
	(setf prev-len len)))))
  |#
  :done)
    
     
#|
puts open(ARGV[0]).read.gsub(/《.*?》|※?［＃.*?］|｜/,'')
|#

(defun hash2list (hash &aux acc)
  (maphash (lambda (k v)
	     (push (cons k v) acc))
	   hash)
  acc)

(defun write-envs (envs file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (dolist (env envs :done)
      (with-slots (name left right) env
        (format out "===== ~A =====~%" name)
	(let ((lefts  (sort (hash2list left) #'> :key #'cdr))
	      (rights (sort (hash2list right) #'> :key #'cdr)))

	  (mapc 
	   (lambda (left right)
	     (format out "~A~20T~D~30T~A~10T~D~%" 
		     (car left) (cdr left)
		     (car right) (cdr right)))
	   lefts rights))
	(terpri out)))))