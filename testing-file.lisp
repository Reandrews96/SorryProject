;; Testing depth 4 vs random

(let ((count 0)
      (g nil))
  (format t "TESTING DEPTH 4 ~%")
  (dotimes (i 100)
    (let ((g (make-sorry)))
      (compete-vs-random 4 (make-sorry))
      (when (= (aref (get-score g) 0) *num-pieces*)
	(incf count))))
  (format t "Red (AI) won ~A out of 100 times~%" count)
  count)

;; Testing depth 6 vs random

(let ((count 0))
  (format t "TESTING DEPTH 6 ~%")
  ;; only 50 times because depth 6
  ;; is slower
  (dotimes (i 50)
    (let ((g (make-sorry)))
      (compete-vs-random 6 (make-sorry))
      (when (= (aref (get-score g) 0) *num-pieces*)
	(incf count))))
  (format t "Red (AI) won ~A out of 50 times~%" count)
  count)

;; Testing depth 4 vs depth 6

(let ((count 0)
      (g nil))
  (format t "TESTING DEPTH 4 (RED) vs DEPTH 6 (GREEN)  ~%")
  (dotimes (i 20)
    (let ((g (make-sorry)))
      (compete 4 6 (make-sorry))
      (when (= (aref (get-score g) 0) *num-pieces*)
	(incf count))))
  (format t "Red (AI) won ~A out of 20 times~%" count)
  count)