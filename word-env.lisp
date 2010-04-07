(defpackage extunk.word-env
  (:use :common-lisp :extunk.environment)
  (:import-from :common-utils read-file nlet a.when)
  (:nicknames wenv)
  (:export calc-env))
(in-package :extunk.word-env)

;; NOTE: In our usage, (mismatch ...) => always fixnum
;; TODO: -> overlapped
(defun overlap-length (string start1 start2)
  (- (mismatch string string :start1 start1 :start2 start2) start1))

(defun char-invalid-p (ch)
  (case ch ((#\Space #\Return #\Newline #\Tab #\。 #\、 #\　) t)))

(defun char-kana-p (ch)
  (char<= #\ぁ ch #\HIRAGANA_LETTER_SMALL_KE))

(defun get-right-word (text start)
  (when (< start (length text))
    (if (or (char= (char text start) #\。)
	    (char= (char text start) #\、))
	(subseq text start (1+ start))
      (let ((rlt (subseq text start (min (+ start 5) (position-if-not #'char-kana-p text :start start))))) ;; XXX: nil
	(when (plusp (length rlt))
	  rlt)))))

(defun get-left-word (text start)
  (unless (zerop start)
    (if (or (char= (char text (1- start)) #\。)
	    (char= (char text (1- start)) #\、))
	(subseq text (1- start) start)
      (let ((rlt (subseq text 
	      (loop FOR i FROM (1- start) DOWNTO 0 DO
	        (when (or (not (char-kana-p (char text i)))
			  (= i (- start 6)))
		  (return (1+ i)))
		FINALLY (return 0))
	      start)))
	(when (plusp (length rlt))
	  rlt)))))

(defun add-to-env (text env-set max-len list min-len low-freq-border &aux (head (car list)))
  (when (or (< (length list) low-freq-border)
	    (<= max-len 1))
    (return-from add-to-env))

  #+dp
  (progn 
  (format t "==========~%")
  (format t "~D - ~D, count: ~D~%" min-len max-len (length list))
  (dolist (cur list)
    (format t "~A@ @~A~%" 
	  (subseq text (max (- cur 10) 0) cur)
	  (subseq text cur (min (+ cur 10) (length text))))))
	  

  (loop FOR len  FROM min-len TO max-len 
	FOR word = (subseq text head (+ head len))
    WHEN
    (and (> len 1)
	 (not (some #'char-invalid-p word)))
    DO
    (let ((env (setf (gethash word env-set) (gethash word env-set (make-env word)))))
      (dolist (index list)
	(a.when (get-left-word text index)
	  (incf (gethash it (env-left env)  0)))
	(a.when (get-right-word text (+ index len))
	  (incf (gethash it (env-right env) 0)))))))

(defun calc-env (text &key (freq-border 10) (env-set (make-hash-table :test #'equal)))
  (let ((indices (sort (loop FOR i FROM 0 BELOW (length text) COLLECT i)
		       (lambda (i j) (string> text text :start1 i :start2 j)))))
    (labels ((self (head-indices indices base-len prev-len)
               (if (or (endp indices) (<= prev-len base-len))
		   (progn (add-to-env text env-set prev-len (ldiff head-indices indices) base-len freq-border)
			  (values indices prev-len))
		 (destructuring-bind (1st &rest rest &aux (2nd (car rest))) indices
		   (let ((len (if 2nd (overlap-length text 1st 2nd) 0)))
		     (cond ((> len prev-len)
			    (multiple-value-bind (rest prev-len) (self indices (cdr indices) prev-len len)
			      (self head-indices rest base-len prev-len)))
			   ((< len prev-len)
			    (add-to-env text env-set prev-len (ldiff head-indices indices) len freq-border)
			    (self head-indices rest base-len len))
			   (t 
			    (self head-indices rest base-len len)))))))
	     (toplevel (indices)
	       (destructuring-bind (1st &rest rest &aux (2nd (car rest))) indices
	         (let ((len (if 2nd (overlap-length text 1st 2nd) 0)))
		   (self indices rest 0 len)))))

      (loop WHILE (setf indices (toplevel indices)))))
  env-set)