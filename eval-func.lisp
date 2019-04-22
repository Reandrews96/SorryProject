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
     ;; When its red's turn
     ((= turn *red*)
      ;; Get the number already at home and the number of its pieces
      ;; And its oponents pieces on the board
      (let ((home (aref (sorry-eval-totals g) 0))
	    (on-board (- *num-pieces* (count *default-red-start* reds)))
	    (op-on-board (- *num-pieces* 
			    (count *default-green-start* greens))))
	;; Set amount to be amount constant*home + additional points for being
	;; on the board and minus points for oponent
	(setf amount (+ amount (* 5 home) (* 2 on-board) (* -1 op-on-board)))))
     ;; When its green's turn
     ((= turn *green*)
      ;; Get the number already at home and the number of its pieces
      ;; and its oponents pieces on the board
      (let ((home (aref (sorry-eval-totals g) 1))
	    (on-board (- *num-pieces* (count *default-green-start* greens)))
	    (op-on-board (- *num-pieces* 
			    (count *default-red-start* reds))))
	;; Set amount to be amount constant*home + additional points for being
	;; on the board and minus points for oponent
	(setf amount (+ amount (* 5 home) (* 2 on-board) (* -1 op-on-board))))))
    amount))
     