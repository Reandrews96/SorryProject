;;  COMPETE
;; --------------------------------------------------

(defun compete
    (red-depth green-depth g)
    (while (not (game-over g))
      (cond
       ((eq (sorry-whose-turn? g) *red*)
	(format t "Red ~%")
	(apply #'do-move! g nil (compute-move g red-depth)))
       (t
	(format t "Green ~%")
	(apply #'do-move! g nil (compute-move g green-depth)))))
    g)

