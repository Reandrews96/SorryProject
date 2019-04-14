;;; =================================
;;;  Project: Sorry! Set Up
;;;  Rebecca Andrews and Yina Wang
;;; ================================

;; Compiler flags

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;; GLOBAL CONSTANTS

;; Num pieces per person
(defconstant *num-pieces* 3)

;; Players

(defconstant *red* 0)
(defconstant *green* 1)

(defconstant *red-symbol* "R")
(defconstant *green-symbol* "G")

(defconstant *default-red-start* -1)
(defconstant *default-green-start* -2)

(defconstant *default-red-home* -6)
(defconstant *default-green-home* -16)

;; CARDS

;; the sorry card
(defconstant *sorry* 0)

;; cards available
(defconstant *cards* (vector *sorry* 1 2 5 10))

;; starting number of each card, indexes correspond to above
(defconstant *num-each-card* (vector 4 4 4 4 4))

;; starting number of total cards
(defconstant *start-deck-num* 20)

;; constant for if the deck is infinite or not
(defconstant *infinite-deck* t)

;; WIN-LOSS VALUES

(defconstant *win-value* 4000)
(defconstant *loss-value* 4000)

;; NEGATIVE and POSITIVE INFINITY

(defconstant *neg-inf* -100000)
(defconstant *pos-inf* -100000)


;; SORRY struct
;; -----------------------------------
;; Fields:
;;   PIECES-R --- location of pieces for the red player
;;   PIECES-G --- location of pieces for the green player
;;   WHOSE-TURN? -- Either *red* or *green*
;;   DECK -- a vector representing the available cards
;;   EVAL-TOTALS -- a vector representing the number of pieces in the home (goal)
;;      of each player, goes (red, green)
;;   MOVE-HISTORY -- List of the moves that got us from the initial
;;      state to the current state

(defstruct (sorry (:print-function print-sorry))
  (pieces-r (make-array *num-pieces* :initial-element *default-red-start*))
  (pieces-g (make-array *num-pieces* :initial-element *default-green-start*))
  (whose-turn? *red*)
  (eval-totals (vector 0 0))
  (num-cards 20)
  (deck (copy-seq *num-each-card*))
  (current-card nil)
  move-history nil)


;; PRINT-SORRY
;; --------------------------------------------------
;; Print function for SORRY struct

(defun print-sorry (game str depth)
  ;; Get the current pieces and score, plus
  ;; get how many pieces still in each start
  (let ((red (sorry-pieces-r game))
	(green (sorry-pieces-g game))
	(card (sorry-current-card game))
	(turn (sorry-whose-turn? game))
	(score (sorry-eval-totals game)))
    (declare (ignore depth))
    (format str "Current game: ~% ~%")
    ;; Print out the board. The board starts at 1 in the upper left
    ;; corner and snakes around it get all the way back, ending at
    ;; 37 (each side is length 10)
    ;; The two safe areas have indices outside these bounds.
    ;; Red's home is from -10 to -6.
    ;; Green's home is from -20 to -16.
    (format str "               ")
    (dotimes (f-row 10)
      ;; Printing top row
      (format str "~A "
	      (cond
	       ((find (+ f-row 1) red) 
		(concatenate 'string
		  *red-symbol* 
		  (write-to-string (position (+ f-row 1) red))))
	       ((find (+ f-row 1) green) 
		(concatenate 'string
		  *green-symbol* 
		  (write-to-string (position (+ f-row 1) green))))
	       (t  "__"))))
    ;; Printing R's safe zone
    (dotimes (home-length-r 4)
      (format str "~A "
	      (cond
	       ((find (+ -10 home-length-r) red) 
		(concatenate 'string
		  *red-symbol* 
		  (write-to-string (position 
				    (+ -10 home-length-r) red))))
	       (t "__"))))
    (format str "RH~%")
    (dotimes (cols 8)
      ;; Printing each of the sides of the board
      (format str "               ~A"
	      (cond 
	       ((find (- 37 cols) red) (concatenate 'string 
					 *red-symbol*
					 (write-to-string (position
							   (- 37 cols) red))))
	       ((find (- 37 cols) green) (concatenate 'string 
					 *green-symbol*
					 (write-to-string (position
							   (- 37 cols) green))))
	       (t "__")))
      (format str "                         ")
      (format str "~A ~%"
	      (cond
	       ((find (+ 11 cols) red) (concatenate 'string 
					 *red-symbol*
					 (write-to-string (position
							   (+ 11 cols) red))))
	       ((find (+ 11 cols) green) (concatenate 'string 
					 *green-symbol*
					 (write-to-string (position
							   (+ 11 cols) green))))
	       (t "__"))))
    (format str "GH ")
    ;; Printing green's safe zone
    (dotimes (home-length-g 4)
      (format str "~A "
	      (cond
	       ((find (+ -20 home-length-g) green) 
		(concatenate 'string
		  *green-symbol*
		  (write-to-string (position (+ -20 home-length-g) green))))
	       (t "__"))))
    ;; Printing the bottom part of the board
    (dotimes (b-row 10)
      (format str "~A " 
	      (cond
	       ((find (- 29 b-row) red) (concatenate 'string 
					 *red-symbol*
					 (write-to-string (position
							   (- 29 b-row) red))))
	       ((find (- 29 b-row) green) (concatenate 'string 
					 *green-symbol*
					 (write-to-string (position
							   (- 29 b-row) green))))
	       (t "__"))))
    (format str "~%")
    
    ;; Print out the score and how many pieces still in start
    (format str "~%Red Team's Score: ~A Green Team's Score: ~A ~%"
	    (svref score *red*) (svref score *green*))
    (format str "Red has ~A still at start. Green has ~A still at start. ~%"
	    (count *default-red-start* red)
	    (count *default-green-start* green))
    (format str "It is ~A's turn! ~%"
	    (if (eq *red* turn) "red" "green"))
    (cond
     ((null card)
      (format str "Draw a card! ~%"))
     (t
      (format str "Current card is ~A. ~%" card)))))


;;  TOGGLE-TURN!
;; -------------------------------------------------------
;;  INPUT:  GAME, a SORRY struct
;;  OUTPUT:  none
;;  SIDE EFFECT:  Changes whose turn it is

(defun toggle-turn! (game)
  (let ((current-turn (sorry-whose-turn? game)))
    (setf (sorry-whose-turn? game) (other-plr current-turn))))

;;  OTHER-PLR
;; --------------------------------------------------------
;;  INPUT:  PLR, either *red* or *green*
;;  OUTPUT:  The other player (i.e., *green* or *red*, respectively)

(defun other-plr (plr)
  (- 1 plr))


;; DRAW-CARD
;; -------------------------------------------------------
;; INPUT: G a SORRY struct
;; OUTPUT: a number representing the chosen card

(defun draw-card (g)
  ;; Don't let the user draw another card
  (when (sorry-current-card g) 
    (format t "Already have card! Can't draw another. ~%")
    (return-from draw-card nil))
  ;; Get the current deck (a vector where each
  ;; index represents the number of  a particular
  ;; kind of card
  ;; and select a random value
  ;; in the possible range of all card indexes
  (let* ((curr-cards (sorry-deck g))
	 (num-cards (sorry-num-cards g))
	 (val (+ (random num-cards) 1))
	 (current-sum 0)
	 (selected-index nil))
    (dotimes (i (length curr-cards))
      ;; Add up the number of cards we have passed
      (setf current-sum (+ current-sum (aref curr-cards i)))
      ;; When we are either at the randomly selected index of card
      ;; Or it was somewhere in the "pile" of cards of this type
      ;; that we just added
      (when (<= val current-sum) 
	;; Set the selected index to be the current one
	(setf selected-index i)
	;; And break from the loop
	(return)))
    ;; When we do not have an infinite deck
    (when (not *infinite-deck*)
      ;; Decrement the total number of cards and
      ;; decremenet the total number of cards at the selected
      ;; index
      (decf (sorry-num-cards g))
      (decf (aref curr-cards selected-index)))
    ;; No matter what, set our current card to be the card
    ;; located at the selected index
    (setf (sorry-current-card g) 
      (aref *cards* selected-index))))


;; DO-MOVE!
;; ----------------------------------------------------------
;; INPUTS: GAME, a SORRY struct
;;         CHECK-LEGAL?, a boolean flag
;;         LOC-OLD, position of piece to move
;;         LOC-NEW, new position of piece
;; OUTPUT: Resulting SORRY struct if movel legal
;;         NIL otherwise

(defun do-move! (game check-legal? loc-old loc-new &key (show nil))
  (cond
   ;; If need to check for legal moves, do so
   ((and check-legal? (not (legal-move? game loc-old loc-new)))
    (format t "Can't do illegal move: ~A => ~A ~%" loc-old loc-new))
   (t
    (let* ((turn (sorry-whose-turn? game))
	   (reds (sorry-pieces-r game))
	   (greens (sorry-pieces-g game))
	   (curr-player (if (= turn *red*) reds greens))
	   (index-piece (position loc-old curr-player))
	   (other-player (if (= turn *red*) greens reds))
	   (index-other-piece (position loc-new other-player)))
      ;; When we land on another player
      (when index-other-piece
	;; send them back t start
	(send-to-start game turn loc-new)) 
      ;; Update our piece's location and toggle the turn
      (setf (aref curr-player index-piece) loc-new)
      (toggle-turn! game)
      ;; Record the current move
      (push (list loc-old loc-new index-other-piece) (sorry-move-history game))
      game))))
	   
;; SEND-TO-START
;; ----------------------------------------------------------
;; INPUTS: GAME, a SORRY struct
;;         TURN, a value representing whose turn (red or green)
;;         TAKEN-SPOT, the position of the piece that has been taken
;; OUTPUT: Doesn't matter
;; SIDE EFFECT: the piece is sent back to the start

(defun send-to-start (game turn taken-spot)
  ;; Determine which player has been landed on and get the
  ;; affected piece
  (let* ((affected-player (if (= turn *red*) (sorry-pieces-g game)
			    (sorry-pieces-r game)))
	 (affected-piece (position taken-spot affected-player))
	 (start (if (= turn *red*) *default-green-start* *default-red-start*)))
    ;; Send the effected piece back to the start
    (setf (aref affected-player affected-piece) start)))

;; LEGAL-MOVE?
;; -----------------------------------------------------
;; INPUTS: GAME, a SORRY struct
;;         LOC-OLD, the old spot for the piece
;;         LOC-NEW, the new spot for the piece
;; OUTPUT: T if moving the piece from old to new is legal

(defun legal-move? (game loc-old loc-new)
  (let* ((turn (sorry-whose-turn? game))
	 (reds (sorry-pieces-r game))
	 (greens (sorry-pieces-g game))
	 (curr-player (if (= turn *red*) reds greens))
	 (index-piece (position loc-old curr-player))
	 (affected-player (if (= turn *red*) (sorry-pieces-g game)
			    (sorry-pieces-r game)))
	 (affected-piece (position taken-spot affected-player)))
    (cond
     ;; Cannot move into the same spot as another one of your own
     ;; pieces
     ((or (position loc-new curr-player)
	  ;; And cannot affect another player if they
	  ;; are in their own safe zone
	  (and affected-piece (< loc-new 0)) nil))
     (t t))))


;;  LEGAL-MOVES
;; ------------------------------------------------------
;;  INPUT:  G, a SORRY game struct
;;  OUTPUT:  A list of the legal moves for whoever's turn it is.
;;  NOTE:  Fetches legal moves for all the LIVE pieces of whoever's 
;;         turn it is. 

(defun legal-moves (g)
  (let* ((turn (sorry-whose-turn? g))
	 (current-pieces (if (= turn *red*) (sorry-pieces-r g)
			   (sorry-pieces-g g)))
	 (op-pieces (if (= turn *red*) (sorry-pieces-g g)
			       (sorry-pieces-r g)))
	 (home (if (= turn *red*) *default-red-home* *default-green-home*))
	 (moves nil))
    (dotimes (i *num-pieces*)
      (let ((p (aref current-pieces i)))
	(when (not (= p home))
	  (dotimes (i (length *cards*))
	    (push (use-card (aref *cards* i) p turn current-pieces op-pieces) moves)))))
    moves))
    


;; USE-CARD
;; -----------------------------------------------
;; INPUTS: CARD, a value representing a card
;;         PIECE, the location of the current piece
;;         TURN, a value representing whose turn it is (red or green)
;;         CURR-PIECES, the pieces belonging to the current player
;;         OP-PIECES, the pieces belonging to the other player
;; OUTPUTS: Move accumulator

(defun use-card (card piece turn curr-pieces op-pieces)
  (let ((moves nil))
    (cond
     ;; When get sorry card
     ((= card *sorry*)
      ;; Any move where you send the other player home
      ;; is available
      (dotimes (i *num-pieces*)
	(when (> (aref op-pieces i) 0)
	  (push (list piece (aref op-pieces i)) moves))))
     ;; When we are at start with red,
     ;; can only move to the first square by its start
     ((= piece *default-red-start*)
      (push (list piece 11) moves)
      (return-from use-card moves))
     ;; The same is true for green
     ((= piece *default-green-start*)
      (push (list piece 30) moves)
      (return-from use-card moves))
       ;; Just add as long as not in start
      (t
       (let ((new-val (+ piece card)))
	 (cond
	  ((> new-val 37) (setf new-val (- new-val 37)))
	  ((and (= turn *red*) (< piece 11) (> new-val 11))
	   (setf new-val (+ -10 (- new-val 10))))
	  ((and (= turn *green*) (< piece 30) (> new-val 30))
	   (setf new-val (+ (-20 (- new-val 29))))))
	 (when (not (position new-val curr-pieces))
	   (push (list piece new-val) moves)))))
    moves))


;; GAME-OVER
;; ------------------------------------------
;; INPUTS: G, a SORRY struct
;; OUTPUT: T if either player has finished

(defun game-over (g)
  (let ((score (sorry-eval-totals g)))
    (or (= (aref score 1) *num-pieces*) 
	(= (aref score 2) *num-pieces*))))