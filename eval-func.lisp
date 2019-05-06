;; DEFAULT-EVAL-FUNC (the default evaluation function)
;; ------------------------------------------------------
;; INPUTS: g, a SORRY struct
;; OUTPUT: a number representing an evaluation of how well
;; the current player is doing

(defun default-eval-func (g)
  (let* ((turn (if (= (sorry-whose-turn? g) *red*) *red* *green*))
	 (reds (sorry-pieces-r g))
	 (greens (sorry-pieces-g g))
	 (amount 0))
    (cond
     ;; When its red's turn
     ((= turn *red*)
      ;; Get the number already at home and the number of its pieces
      ;; And its oponents pieces on the board
      (let ((home (aref (get-score g) 0))
	    (on-board (- *num-pieces* (count *default-red-start* reds)))
	    (op-on-board (- *num-pieces* 
			    (count *default-green-start* greens)))
	    (my-home-close (calc-closeness-to-home reds *red*))
	    (my-op-close (calc-closeness-to-home greens *green*)))
	;; Set amount to be amount constant*home + additional points for being
	;; on the board and minus points for oponent
	(setf amount (+ amount (* 100 home) (* 20 on-board) (* -10 op-on-board)
			(* 30 my-home-close) (* -15 my-op-close)))))
     ;; When its green's turn
     ((= turn *green*)
      ;; Get the number already at home and the number of its pieces
      ;; and its oponents pieces on the board
      (let ((home (aref (get-score g) 1))
	    (on-board (- *num-pieces* (count *default-green-start* greens)))
	    (op-on-board (- *num-pieces* 
			    (count *default-red-start* reds)))
	    (my-home-close (calc-closeness-to-home greens *green*))
	    (my-op-close (calc-closeness-to-home reds *red*)))
	;; Set amount to be amount constant*home + additional points for being
	;; on the board and minus points for oponent and points for
	;; locations of pieces on board
	(setf amount (+ amount (* 100 home) (* 20 on-board) (* -10 op-on-board)
			(* 30 my-home-close) (* -15 my-op-close))))))
    amount))


;; MAKE-EVAL-FUNK
;; ---------------------------------------------------------------------------
;; INPUTS: HOME-PT, how much to value pieces at our home
;;         ON-BOARD-PT, how much to value having pieces on the board
;;         OP-ON-BOARD-PT, how much to penalize for oponents having pieces on the
;;          the board
;;         MY-HOME-CLOSE-PT, how much to value advancing pieces on the board
;;         MY-OP-CLOSE-PT, how much to subtract for oponents having pieces advancing
;;          on the board

(defun make-eval-funk (home-pt on-board-pt op-on-board-pt my-home-close-pt my-op-close-pt)
  ;; Return an evaluation function that weights each component of the game based on
  ;; the inputs
  #'(lambda (g)
      (let* ((turn (sorry-whose-turn? g))
	     (reds (sorry-pieces-r g))
	     (greens (sorry-pieces-g g))
	     (amount 0))
	(cond
	 ;; When its red's turn
	 ((= turn *red*)
	  ;; Get the number already at home and the number of its pieces
	  ;; And its oponents pieces on the board
	  (let ((home (aref (get-score g) 0))
		(on-board (- *num-pieces* (count *default-red-start* reds)))
		(op-on-board (- *num-pieces* 
				(count *default-green-start* greens)))
		(my-home-close (calc-closeness-to-home reds *red*))
		(my-op-close (calc-closeness-to-home greens *green*)))
	    ;; Set amount to be amount constant*home + additional points for being
	    ;; on the board and minus points for oponent
	    (setf amount (+ amount (* home-pt home) (* on-board-pt on-board) 
			    (* op-on-board-pt op-on-board)
			    (* my-home-close-pt my-home-close) (* my-op-close-pt my-op-close)))))
	 ;; When its green's turn
	 ((= turn *green*)
	  ;; Get the number already at home and the number of its pieces
	  ;; and its oponents pieces on the board	  
	  (let ((home (aref (get-score g) 1))
		(on-board (- *num-pieces* (count *default-green-start* greens)))
		(op-on-board (- *num-pieces* 
				(count *default-red-start* reds)))
		(my-home-close (calc-closeness-to-home greens *green*))
		(my-op-close (calc-closeness-to-home reds *red*)))
	    ;; Set amount to be amount constant*home + additional points for being
	    ;; on the board and minus points for oponent and points for
	    ;; locations of pieces on board
	    (setf amount (+ amount (* home-pt home) (* on-board-pt on-board) 
			    (* op-on-board-pt op-on-board)
			    (* my-home-close-pt my-home-close) (* my-op-close-pt my-op-close))))))
	amount)))



;; CALC-CLOSENESS-TO-HOME
;; --------------------------------------
;; INPUTS: PIECES, a vector of the current pieces
;;         TURN, a number representing the current turn
;; OUTPUTS: a number representing how close to the home
;;          the pieces are overall

(defun calc-closeness-to-home (pieces turn)
  (let ((sum 0))
    ;; For every piece
    (dotimes (i *num-pieces*)
      (let ((piece (aref pieces i)))
	(cond
	 ;; When its reds turn
	 ((= turn *red*)
	  (cond
	   ;; In the first strech (far right side
	   ;; of board)
	   ((>= 19 piece 11)
	    ;; Add 1
	    (incf sum 1))
	   ;; In the second strech
	   ((>= 27 piece 20)
	    ;; Add two
	    (incf sum 2))
	   ;; In the third 
	   ((>= 36 piece 28)
	    ;; Add 3
	    (incf sum 3))
	   ;; In the fourth
	   ((>= 10 piece 1)
	    ;; Add 4
	    (incf sum 4))
	   ;; In the safe zone
	   ((>= *default-red-home* piece *start-red-safe*)
	    ;; Add additional because can't be touched here
	    (incf sum 10))))
	 (t
	  ;; Repeat for green but start from its starting point
	   (cond
	   ((>= 36 piece 29)
	    (incf sum 1))
	   ((>= 10 piece 1)
	    (incf sum 2))
	   ((>= 19 piece 11)
	    (incf sum 3))
	   ((>= 28 piece 20)
	    (incf sum 4))
	   ((>= *default-green-home* piece *start-green-safe*)
	    (incf sum 10)))))))
    sum))
