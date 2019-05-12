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
      (let* ((score (get-score g))
	     (home (aref score 0))

	     (op-home (aref score 1))
	     (on-board (- *num-pieces* (count *default-red-start* reds)
			  (count *default-red-home* reds)))
	     (op-on-board (- *num-pieces* 
			     (count *default-green-start* greens)
			     (count *default-green-home* greens)))
	     (my-home-close (calc-closeness-to-home reds *red*))
	     (my-op-close (calc-closeness-to-home greens *green*)))
	;; Set amount to be amount + home + additional points for being
	;; on the board + points for having made progress on board
	;; and minus points for opponent at home and on board and having made
	;; progress on the board
	(setf amount (+ amount (* 400 home) (* -100 op-home) (* 50 on-board) 
			(* -20 op-on-board)
			(* 30 my-home-close) (* -15 my-op-close)))))
     ;; When its green's turn
     ((= turn *green*)
      ;; Get the number already at home and the number of its pieces
      ;; and its oponents pieces on the board
      (let* ((score (get-score g))
	     (home (aref score 1))
	     (op-home (aref score 0))
	     (on-board (- *num-pieces* (count *default-green-start* greens)
			  (count *default-green-home* greens)))
	     (op-on-board (- *num-pieces* 
			     (count *default-red-start* reds)
			     (count *default-red-home* reds)))
	     (my-home-close (calc-closeness-to-home greens *green*))
	     (my-op-close (calc-closeness-to-home reds *red*)))
        ;; Set amount to be amount + home + additional points for being
	;; on the board + points for having made progress on board
	;; and minus points for opponent at home and on board and having made
	;; progress on the board
	(setf amount (+ amount (* 700 home) (* -600 op-home) (* 50 on-board) 
			(* -20 op-on-board)
			(* 50 my-home-close) (* -50 my-op-close))))))
    amount))


;; MAKE-EVAL-FUNK
;; ---------------------------------------------------------------------------
;; INPUTS: HOME-PT, how much to value pieces at our home
;;         OP-HOME-PT, how much to penalize for opponents being at home
;;         ON-BOARD-PT, how much to value having pieces on the board
;;         OP-ON-BOARD-PT, how much to penalize for oponents having pieces on the
;;          the board
;;         MY-HOME-CLOSE-PT, how much to value advancing pieces on the board
;;         MY-OP-CLOSE-PT, how much to subtract for oponents having pieces advancing
;;          on the board
;; OUTPUT: a function that can be used an evaluation function for sorry

(defun make-eval-funk (home-pt op-home-pt on-board-pt op-on-board-pt my-home-close-pt my-op-close-pt)
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
	  (let* ((score (get-score g))
		 (home (aref score 0))
		 (op-home (aref score 1))
		 (on-board (- *num-pieces* (count *default-red-start* reds)
			      (count *default-red-home* reds)))
		 (op-on-board (- *num-pieces* 
				 (count *default-green-start* greens)
				 (count *default-green-home* greens)))
		 (my-home-close (calc-closeness-to-home reds *red*))
		 (my-op-close (calc-closeness-to-home greens *green*)))
	    ;; Set amount to be amount constant*home + additional points for being
	    ;; on the board and minus points for oponent
	    (setf amount (+ amount (* home-pt home) (* op-home-pt op-home)
			    (* on-board-pt on-board) 
			    (* op-on-board-pt op-on-board)
			    (* my-home-close-pt my-home-close) (* my-op-close-pt my-op-close)))))
	 ;; When its green's turn
	 ((= turn *green*)
	  ;; Get the number already at home and the number of its pieces
	  ;; and its oponents pieces on the board	  
	  (let* ((score (get-score g))
		 (home (aref score 1))
		 (op-home (aref score 0))
		 (on-board (- *num-pieces* (count *default-green-start* greens)
			      (count *default-green-home* greens)))
		 (op-on-board (- *num-pieces* 
				 (count *default-red-start* reds)
				 (count *default-red-home* reds)))
		 (my-home-close (calc-closeness-to-home greens *green*))
		 (my-op-close (calc-closeness-to-home reds *red*)))
	    ;; Set amount to be amount constant*home + additional points for being
	    ;; on the board and minus points for oponent and points for
	    ;; locations of pieces on board
	    (setf amount (+ amount (* home-pt home) (* op-home-pt op-home)
			    (* on-board-pt on-board) 
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
	   ((> *default-red-home* piece *start-red-safe*)
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
	   ((> *default-green-home* piece *start-green-safe*)
	    (incf sum 10)))))))
    sum))


;; EVAL-FUNCTIONS
;; -----------------------------------------------------------


;; Offensive strategy: highly value preventing your opponent
;; from getting their pieces on the board, and getting them around the board
;; even more than making your own progress
(defparameter offensive (make-eval-funk 700 -600 50 -200 50 -300))

;; Defensive strategy: highly value keeping your pieces on the board,
;; and getting them into home
(defparameter defensive (make-eval-funk 2000 -600 500 -20 50 -50))

;; Running strategy: highly value getting pieces close to the board and
;; getting them into home above all else
(defparameter runner (make-eval-funk 700 -600 50 -20 500 -50))
