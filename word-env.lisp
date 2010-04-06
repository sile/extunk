(defpackage extunk.word-env
  (:use :common-lisp :extunk.environment)
  (:import-from :common-utils read-file nlet a.when)
  (:nicknames wenv)
  (:export calc-env))
(in-package :extunk.word-env)

;; NOTE: In our usage, (mismatch ...) => always fixnum
(defun overlap-length (string start1 start2)
  (- (mismatch string string :start1 start1 :start2 start2) start1))

(defun char-invalid-p (ch)
  (case ch ((#\Space #\Return #\Newline #\Tab #\。 #\、) t)))

(defun char-kana-p (ch)
  (char<= #\ぁ ch #\HIRAGANA_LETTER_SMALL_KE))

(defun get-right-word (text start)
  (when (< start (length text))
    (if (or (char= (char text start) #\。)
	    (char= (char text start) #\、))
	(subseq text start (1+ start))
      (subseq text start (position-if-not #'char-kana-p text :start start)))))

(defun get-left-word (text start)
  (unless (zerop start)
    (if (or (char= (char text (1- start)) #\。)
	    (char= (char text (1- start)) #\、))
	(subseq text (1- start) start)
      (subseq text 
	      (loop FOR i FROM (1- start) DOWNTO 0  DO
	        (unless (char-kana-p (char text i))
		  (return (1+ i)))
		FINALLY (return 0))
	      start))))

(defun add-to-env (text env-set max-len list min-len low-freq-border &aux (head (car list)))
  (loop FOR len  FROM min-len TO max-len 
	FOR word = (subseq text head (+ head len))
    WHEN
    (and (> len 1)
	 (>= (length list) low-freq-border)
	 (not (some #'char-invalid-p word)))
    DO
    (let ((env (setf (gethash word env-set) (gethash word env-set (make-env word)))))
      (dolist (index list)
	(a.when (get-left-word  text index)
	  (incf (gethash it (env-left env)  0)))
	(a.when (get-right-word text (+ index (length word)))
	  (incf (gethash it (env-right env) 0)))))))

;; NOTE: オリジナルのものとは少し異なる
;; [例]  # TODO: 詳述
;;  - オリジナル: "大変感謝"にマッチしたら、"大変感"、"大変"、"大"にもマッチする
;;  - 下の関数では、全ての部分文字列が組み込まれることはない
;;    - 他のものと一つでも境界位置が一致しているもののみ考慮される => ex. "大変感謝"の場合、おそらく"大変感"は含まれない
;;    - 多分実用上問題ないのでは?
(defun calc-env (input-file &optional (low-freq-border 10))
  (let* ((env-set (make-hash-table :test #'equal))
	 (text (read-file input-file))
	 (indices (sort
		   (loop FOR i FROM 0 BELOW (length text) COLLECT i)
		   (lambda (i j) (string> text text :start1 i :start2 j)))))
    (nlet self ((prev (1- (length text)))
		(indices indices)
		(stack '())
		(cur-len  0))
      (if (endp indices)
	  (add-to-env text env-set cur-len (caar stack) 0 low-freq-border)
	(destructuring-bind (cur . rest) indices
	  (let ((len (overlap-length text prev cur)))
	    (cond ((zerop len)           
		   (loop FOR (start . len) IN stack DO
		     (add-to-env text env-set cur-len (ldiff start indices) len low-freq-border)
		                         (setf cur-len len))
		                         (self cur rest `((,indices . 0))                len))
		  ((= len cur-len)       (self cur rest stack                            len))
		  ((> len cur-len)       (self cur rest `((,indices . ,cur-len) ,@stack) len))
		  ((<= len (cdar stack)) 
		   (add-to-env text env-set cur-len (ldiff (caar stack) indices) len low-freq-border)
		                         (self cur rest (cdr stack)                      len))
		  (t                     
		   (add-to-env text env-set cur-len (ldiff (caar stack) indices) len low-freq-border)
		                         (self cur rest stack                            len)))))))
    env-set))
