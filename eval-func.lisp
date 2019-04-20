;; EVAL-FUNC
;; INPUTS: g, a SORRY struct
;; OUTPUT: a number representing an evaluation of how well
;; the current player is doing

(defun eval-func (g)
  (let* ((turn (sorry-whose-turn? g))
	 (reds (sorry-pieces-r g))
	 (greens (sorry-pieces-g g))
	 (amount 0))
    (cond
     ((= turn *red*)
      (let ((home (aref (sorry-eval-totals g) 0))
	    (on-board (- *num-pieces* (count *default-red-start* reds)))
	    (op-on-board (- *num-pieces* 
			    (count *default-green-start* greens))))
	(setf amount (+ amount home (* 2 on-board) (* -1 op-on-board)))))
      ((= turn *green*)
      (let ((home (aref (sorry-eval-totals g) 1))
	    (on-board (- *num-pieces* (count *default-green-start* greens)))
	    (op-on-board (- *num-pieces* 
			    (count *default-red-start* reds))))
	(setf amount (+ amount home (* 2 on-board) (* -1 op-on-board))))))
    amount))
     