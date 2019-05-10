;; Testing depth 4 vs random

(defun depth-4-vs-random ()
  (let ((count 0))
    (format t "TESTING DEPTH 4 ~%")
    (dotimes (i 100)
      (let ((g (make-sorry)))
	(compete-vs-random 4 g)
	(when (= (aref (get-score g) 0) *num-pieces*)
	  (incf count))))
    (format t "Red (AI) won ~A out of 100 times~%" count)
    (format t "~% ~% ~%")
    count))

;; Testing depth 6 vs random

(defun depth-6-vs-random ()
  (let ((count 0))
    (format t "TESTING DEPTH 6 ~%")
    ;; only 40 times because depth 6
    ;; is slower
    (dotimes (i 40)
      (let ((g (make-sorry)))
	(compete-vs-random 6 g)
	(when (= (aref (get-score g) 0) *num-pieces*)
	  (incf count))))
    (format t "Red (AI) won ~A out of 40 times~%" count)
    (format t "~% ~% ~%")
    count))


;; Testing depth 4 vs depth 6

(defun depth-4-vs-6 ()
  (let ((count 0)) 
    (format t "TESTING DEPTH 4 (RED) vs DEPTH 6 (GREEN)  ~%")
    (dotimes (i 40)
    (let ((g (make-sorry)))
      (compete 4 6 g)
      (when (= (aref (get-score g) 1) *num-pieces*)
	(incf count))))
    (format t "Green (Depth 6) won ~A out of 40 times~%" count)
    (format t "~% ~% ~%")
    count))

;; Testing the offensive vs default strategy

(defun offensive-vs-default ()
  (let ((count 0)) 
    (format t "TESTING OFFENSE (RED) vs DEFAULT (GREEN)  ~%")
    (dotimes (i 100)
    (let ((g (make-sorry)))
      (compete-diff-eval offensive #'default-eval-func g)
      (when (= (aref (get-score g) 1) *num-pieces*)
	(incf count))))
    (format t "Red (Offensive) won ~A out of 100 times~%" count)
    (format t "~% ~% ~%")
    count))

;; Testing the defensive vs default strategy

(defun defensive-vs-default ()
  (let ((count 0)) 
    (format t "TESTING DEFENSE (RED) vs DEFAULT (GREEN)  ~%")
    (dotimes (i 100)
    (let ((g (make-sorry)))
      (compete-diff-eval offensive #'default-eval-func g)
      (when (= (aref (get-score g) 1) *num-pieces*)
	(incf count))))
    (format t "Red (Defensive) won ~A out of 100 times~%" count)
    (format t "~% ~% ~%")
    count))

(defun runner-vs-default ()
  (let ((count 0)) 
    (format t "TESTING RUNNER (RED) vs DEFAULT (GREEN)  ~%")
    (dotimes (i 100)
    (let ((g (make-sorry)))
      (compete-diff-eval runner #'default-eval-func g)
      (when (= (aref (get-score g) 1) *num-pieces*)
	(incf count))))
    (format t "Red (Runner) won ~A out of 100 times~%" count)
    (format t "~% ~% ~%")
    count))

;; Testing the offesnive vs defensive strategies

(defun offensive-vs-defensive ()
  (let ((count 0)) 
    (format t "TESTING OFFENSE (RED) vs DEFENSE (GREEN)  ~%")
    (dotimes (i 100)
    (let ((g (make-sorry)))
      (compete-diff-eval offensive defensive g)
      (when (= (aref (get-score g) 1) *num-pieces*)
	(incf count))))
    (format t "Green (Defensive) won ~A out of 100 times~%" count)
    (format t "~% ~% ~%")
    count))