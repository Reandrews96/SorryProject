;;  COMPETE
;; --------------------------------------------------

(defun compete
    (red-depth green-depth g)
  (while (not (game-over g))
      (cond
       ((eq (sorry-whose-turn? g) *red*)
	(format t "Red ~%")
	(apply #'do-move! g nil (compute-move g red-depth #'eval-func)))
       (t
	(format t "Green ~%")
	(apply #'do-move! g nil (compute-move g green-depth #'eval-func)))))
    g)


;; COMPETE-VS-RANDOM
;; -------------------------------------------------

(defun compete-vs-random
    (depth g)
  (while (not (game-over g))
    (cond
      ((eq (sorry-whose-turn? g) *red*)
       (format t "Red (AI) ~%")
       (apply #'do-move! g nil (compute-move g depth #'eval-func)))
      (t
       (format t "Green (RANDOM) ~%")
       (setf (sorry-current-card g) (select-card g))
       (format t "CARD WAS ~A ~%" (sorry-current-card g))
       (let* ((moves (legal-card-moves g))
	      (n (random (length moves)))
	      (move (nth n moves)))
	 (format t "RANDOM MOVE ~A ~%" move)
	 (apply #'do-move! g nil move)))))
  g)

;; COMPETE-DIFF-EVAL
;; -----------------------------

(defun compete-diff-eval (g eval-red eval-green)
  'define)