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

;; Location of the off-board start positions
(defconstant *default-red-start* -1)
(defconstant *default-green-start* -2)

;; Location where red and green start when they
;; first move onto the board
(defconstant *first-board-red* 11)
(defconstant *first-board-green* 29)

;; Location of the beginning of each player's 
;; safe zone
(defconstant *start-red-safe* -100)
(defconstant *start-green-safe* -200)


;;Location of each player's home
(defconstant *default-red-home* -96)
(defconstant *default-green-home* -196)

;; CARDS

;; the sorry card
(defconstant *sorry* 0)

;; cards available
(defconstant *cards* (vector *sorry* 1 5 8 10))

;; starting number of each card, indexes correspond to above
(defconstant *num-each-card* (vector 2 8 4 4 4))

;; starting number of total cards
(defconstant *start-deck-num* 22)

;; constant for if the deck is infinite or not
(defconstant *infinite-deck* t)

;; WIN-LOSS VALUES

(defconstant *win-value* 4000)
(defconstant *loss-value* -4000)

;; NEGATIVE and POSITIVE INF

(defconstant *neg-inf* -100000)
(defconstant *pos-inf* 100000)

;; PASS MOVE

(defconstant *pass* nil)


;; SORRY struct
;; -----------------------------------
;; Fields:
;;   PIECES-R --- location of pieces for the red player
;;   PIECES-G --- location of pieces for the green player
;;   WHOSE-TURN? -- Either *red* or *green*
;;   DECK -- a vector representing the available cards
;;   MOVE-HISTORY -- List of the moves that got us from the initial
;;      state to the current state

(defstruct (sorry (:print-function print-sorry))
  (pieces-r (make-array *num-pieces* :initial-element *default-red-start*))
  (pieces-g (make-array *num-pieces* :initial-element *default-green-start*))
  (whose-turn? *red*)
  (num-cards *start-deck-num*)
  (deck (copy-seq *num-each-card*))
  (current-card nil)
  move-history nil)

;; GET-SCORE
;; INPUTS: G, a SORRY struct
;; OUTPUTS: vector containing score

(defun get-score (g)
  (let ((red (count *default-red-home* (sorry-pieces-r g)))
	(green (count *default-green-home* (sorry-pieces-g g))))
    (vector red green)))

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
	(score (get-score game)))
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
	       ((find (+ *start-red-safe* home-length-r) red) 
		(concatenate 'string
		  *red-symbol* 
		  (write-to-string (position 
				    (+ *start-red-safe* home-length-r) red))))
	       (t "__"))))
    (format str "RH~%")
    (dotimes (cols 8)
      ;; Printing each of the sides of the board
      (format str "               ~A"
	      (cond 
	       ((find (- 36 cols) red) (concatenate 'string 
					 *red-symbol*
					 (write-to-string (position
							   (- 36 cols) red))))
	       ((find (- 36 cols) green) (concatenate 'string 
					 *green-symbol*
					 (write-to-string (position
							   (- 36 cols) green))))
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
	       ((find (- (- *default-green-home* 1)  home-length-g) green)
		(concatenate 'string
		  *green-symbol*
		  (write-to-string (position (- (- *default-green-home* 1) home-length-g) green))))
	       (t "__"))))
    ;; Printing the bottom part of the board
    (dotimes (b-row 10)
      (format str "~A " 
	      (cond
	       ((find (- 28 b-row) red) (concatenate 'string 
					 *red-symbol*
					 (write-to-string (position
							   (- 28 b-row) red))))
	       ((find (- 28 b-row) green) (concatenate 'string 
					 *green-symbol*
					 (write-to-string (position
							   (- 28 b-row) green))))
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


;; SELECT-CARD
;; -------------------------------------------------------
;; INPUT: G a SORRY struct
;; OUTPUT: the selected card

(defun select-card (g)
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
    ;; No matter what, return the card
    ;; located at the selected index
    (aref *cards* selected-index)))

;; DRAW-CARD
;; -------------------------------------------------------
;; INPUT: G a SORRY struct
;; OUTPUT: the modified game struct now with the card

(defun draw-card (g)
  ;; Don't let the user draw another card
  (cond 
   ((sorry-current-card g) 
    (format t "Already have card! Can't draw another. ~%")
    (return-from draw-card nil))
   ;; Otherwise select a new card using select card and set it
   (t 
    (setf (sorry-current-card g) (select-card g))
    g)))

;; DO-MOVE!
;; ----------------------------------------------------------
;; INPUTS: G, a SORRY struct
;;         CHECK-LEGAL?, a boolean flag
;;         LOC-OLD, position of piece to move
;;         LOC-NEW, new position of piece
;;         CARD, the card that produced the move
;; OUTPUT: Resulting SORRY struct if movel legal
;;         NIL otherwise

(defun do-move! (game check-legal? loc-old loc-new card)
  (cond
   ;; If need to check for legal moves, do so
   ((and check-legal? (not (legal-move? game loc-old loc-new card)))
    (format t "Can't do illegal move. ~%" ))
   ;; When the move is a pass (all are null)
   ((and (null loc-old) (null loc-new))
    ;; pass
    (pass game))
   (t
    (let* ((turn (sorry-whose-turn? game))
	   (reds (sorry-pieces-r game))
	   (greens (sorry-pieces-g game))
	   (curr-player (if (= turn *red*) reds greens))
	   (index-piece  (position loc-old curr-player))
	   (other-player (if (= turn *red*) greens reds))
	   (index-other-piece (position loc-new other-player)))
      ;; When we land on another player
      (when index-other-piece
	;; send them back t start
	(send-to-start game turn loc-new)) 
      ;; Update our piece's location and toggle the turn
      (setf (aref curr-player index-piece) loc-new)
      (toggle-turn! game)
      ;; Reset the card to being null
      (setf (sorry-current-card game) nil)
      ;; Record the current move
      (push (list loc-old loc-new index-other-piece card) (sorry-move-history game))
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
;;         CARD, the card that produced the move
;; OUTPUT: T if moving the piece from old to new is legal

(defun legal-move? (game loc-old loc-new card)
  (let* ((turn (sorry-whose-turn? game))
	 (reds (sorry-pieces-r game))
	 (greens (sorry-pieces-g game))
	 (curr-player (if (= turn *red*) reds greens))
	 (index-piece (position loc-old curr-player))
	 (affected-player (if (= turn *red*) (sorry-pieces-g game)
			    (sorry-pieces-r game)))
	 (affected-piece (position loc-new affected-player)))
    (cond
     ;; If we tried to move a piece with a card that cannot be used
     ;; not legal move
     ((null loc-new) nil)
     ;; If you try to use the sorry card to move a piece
     ;; not in the start or in a safe zone
     ((and (= card *sorry*) (> loc-old 0)) nil)
     ;; Cannot move into the same spot as another one of your own
     ;; pieces or affect a player that is either at the start
     ;; or their safe zone
     ((or (position loc-new curr-player)(and affected-piece (< loc-new 0))) nil)
     (t t))))


;;  LEGAL-MOVES
;; ------------------------------------------------------
;;  INPUT:  G, a SORRY game struct
;;  OUTPUT:  A list of the legal moves for whoever's turn it is.
;;  NOTE:  Fetches legal moves for all the LIVE pieces of whoever's 
;;         turn it is. 

(defun legal-moves (g)
  ;; Get information about the current state of the game
  (let* ((turn (sorry-whose-turn? g))
	 (current-pieces (if (= turn *red*) (sorry-pieces-r g)
			   (sorry-pieces-g g)))
	 (op-pieces (if (= turn *red*) (sorry-pieces-g g)
			       (sorry-pieces-r g)))
	 (home (if (= turn *red*) *default-red-home* *default-green-home*))
	 (deck (sorry-deck g))
	 (seen-start nil)
	 ;; Initialize moves to be empty
	 (moves ())
	 (current-move nil))
    ;; For all the pieces
    (dotimes (i *num-pieces*)
      (let ((p (aref current-pieces i)))
	;; When we have not already gotten the piece home
	(cond
	 ;; When we are at start with red,
	 ;; can only move to the first square by its start or use
	 ;; sorry card
	 ((and (= p *default-red-start*) (null seen-start))
	  ;; When there aren't any pieces on the first sqaure
	  (when (not (position *first-board-red* current-pieces))
	    ;; Push this move on
	    (push (list p *first-board-red* 1) moves))
	  ;; Can also use sorry to move any of oponents
	  ;; pieces back to their start
	  (dotimes (i *num-pieces*)
	    (when (> (aref op-pieces i) 0)
	      (push (list p (aref op-pieces i) *sorry*) moves)))
	  (setf seen-start t))
	 ;; The same is true for green
	 ((and (= p *default-green-start*) (null seen-start))
	  ;; When there isn't something already right
	  ;; by the start, add this move
	  (when (not (position *first-board-green* current-pieces))
	    (push (list p *first-board-green* 1) moves))
	  ;; Can also use sorry to move any of oponents
	  ;; pieces back to their start
	  (dotimes (i *num-pieces*)
	    (when (> (aref op-pieces i) 0)
	      (push (list p (aref op-pieces i) *sorry*) moves)))
	  (setf seen-start t))
	 ;; Otherwise, try each card as long as not already
	 ;; in home base or a start piece when we have already stored
	 ;; the moves from start
	 ((and (not (= p home)) (not (= p *default-red-start*))
		   (not (= p *default-green-start*)))
	  ;; Try each card on the piece 
	  (dotimes (j (length *cards*))
	    ;; As long as there are cards left
	    (when (> (aref deck j) 0)
	      ;; If get the potential places the current move to take this piece
	      (setf current-move (use-card (aref *cards* j) p turn current-pieces op-pieces))
	      ;; When this isn't empty, add to list of moves
	      (when current-move (setf moves (append current-move moves)))))))))
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
     ;; When the card is a 10
     ((= card 10)
      ;; See if you can either move back 1, and if so, update moves
      (setf moves (check-move-open -1 piece turn curr-pieces moves card))
      ;; Or if you can move forward 10, do so
      (setf moves (check-move-open card piece turn curr-pieces moves card)))
     ;; Otherwise
     (t
      ;; Just  get new position and add this new move if available
      (setf moves (check-move-open card piece turn curr-pieces moves card))))
    moves))


;; CHECK-MOVE-OPEN
;; INPUTS: STEPS, the number of steps forward
;;         PIECE, current location of the piece
;;         TURN, a val representing the current turn (red or green)
;;         CURR-PIECES, an array of the current pieces
;;         MOVES, a list of the current moves
;;         CARD, the card that caused this move
;; OUTPUT: add the move to the list of moves if it is available

(defun check-move-open (steps piece turn curr-pieces moves card)
  ;; Get the new location of the piece
  (let ((new-val (move-piece-on-board piece steps turn)))
    ;; When there isn't already a piece there
    (when (or (not (position new-val curr-pieces)) (= new-val *default-green-home*)
		   (= new-val *default-red-home*))
      ;; Add this move to the list
      (push (list piece new-val card) moves)))
  moves)


;; MOVE-PIECE-ON-BOARD
;; INPUTS: LOC-CURR, number representing the current location of piece
;;         STEPS, number representing the steps forward (or backward) the
;;          piece must make
;; OUTPUTS: the new location on the board (deals with rotating back to 1 or
;;           going into safe-zones)

(defun move-piece-on-board (loc-curr steps turn)
  (let ((new-val (+ loc-curr steps)))
    (cond
     ;; If the current location is the start
     ;; and we try to move more than just one
     ((and (or (= loc-curr *default-red-start*) 
	       (= loc-curr *default-green-start*))
	   (not (= steps 1)))
      ;; Return nil, can't move
      nil)
     ;; If we are moving 1 and on the red start
     ((= loc-curr *default-red-start*)
      ;; Move to start spot
      *first-board-red*)
     ;; If we are moving 1 and on green start
     ((= loc-curr *default-green-start*)
      ;; Move to green start
      *first-board-green*)
     ;; If we have passed red's home
     ((and (> new-val *default-red-home*) (< new-val 0))
      ;; Just lock red in the home spot
      *default-red-home*)
     ;; If we have passed green's home
     ((and (> new-val *default-green-home*) (< new-val (+ *default-green-home* 50)))
      ;; Just lock green in the home spot
      *default-green-home*)
     ;; When we reach the top right corner, rotate back to 1
     ((> new-val 36) (- new-val 36))
     ;; When we have just gone around board and can enter
     ;; red's safe zone, enter
     ((and (= turn *red*) (< loc-curr *first-board-red*) (> new-val 10))
      (setf new-val (+ *start-red-safe* (- new-val *first-board-red*)))
      ;; When we pass home, just set the value to home
      (if (< *default-red-home* new-val) *default-red-home*
	;; Otherwise, the new value is fine
	new-val))
     ;; When we have just gone round board and can enter
     ;; green's safe zone, enter
     ((and (= turn *green*) (< loc-curr 29) (> new-val 28))
      (setf new-val (+ *start-green-safe* (- new-val *first-board-green*)))
      ;; When we pass home, just set the value to home
      (if (< *default-green-home* new-val) *default-green-home*
	;; Otherwise, the new value is fine
	new-val))
     ;; When we moved backwards in the top left corner
     ;; Write around on indices
     ((and (< steps 0) (> loc-curr 0) (<= new-val 0))
      (+ 36 (+ loc-curr steps)))
     ;; When we move backwards out of safe zone for red
     ;; move out
     ((and (< steps 0) (< loc-curr 0) (= turn *red*) (< new-val *start-red-safe*))
      (+ 11 (- new-val *start-red-safe*)))
     ;; When we move backwards out of safe zone for green
     ;; move out
     ((and (< steps 0) (< loc-curr 0) (= turn *green*) (< new-val -20))
      (+ 29 (- new-val *start-green-safe*)))
     (t new-val))))


;; UNDO-MOVE!
;; --------------------------------------------------------
;; INPUT: G, a SORRY struct
;; OUTPUT: The modified chess struct
;; SIDE-EFFECT: Destructively undoes the most recent move on the move history

(defun undo-move! (g)
  (cond
   ;; When there are no moves, no need to undo
   ((null (sorry-move-history g))
    (format t "No move to undo! Empty move history! ~%")
    g)
   (t
    ;; Get the past move and the past pieces
    (let* ((move (pop (sorry-move-history g)))
	   (turn (sorry-whose-turn? g))
	   ;; When the current turn is red, we know the last turn was
	   ;; green
	   (past-current-pieces (if (= turn *red*) (sorry-pieces-g g)
				  (sorry-pieces-r g)))
	   ;; When the current turn is red, we know that the oponent
	   ;; was red last time
	   (past-op-pieces (if (= turn *red*) (sorry-pieces-r g)
			     (sorry-pieces-g g)))
	   ;; Get the details of the previous move
	   (orig-spot (first move))
	   (later-spot (second move))
	   (index-old-piece (third move))
	   (card (fourth move)))
      (cond
       ;; When the move is a pass, just toggle whose turn to go back
       ;; and reset the card
       ((and (null orig-spot) (null later-spot))
	(setf (sorry-current-card g) card)
	(toggle-turn! g)
	g)
       (t 
	;; If we need to put a piece back in this spot, do so
	(when index-old-piece (setf (aref past-op-pieces index-old-piece) later-spot))
	;; Otherwise, reset the piece to its old spot
	(setf (aref past-current-pieces (position later-spot past-current-pieces)) orig-spot)
	;; Toggle the turn
	(toggle-turn! g)
	;; Set card to be the card that caused the change
	(setf (sorry-current-card g) card)
	;; Return g after its been reset
	g))))))
   
   


;; GAME-OVER
;; ------------------------------------------
;; INPUTS: G, a SORRY struct
;; OUTPUT: T if either player has finished

(defun game-over (g)
  (let ((score (get-score g)))
    (or (= (aref score 0) *num-pieces*)
	(= (aref score 1) *num-pieces*))))


;; PLAY-CARD
;; -----------------------------------------
;; INPUTS: G, a SORRY struct
;;         INDEX, the index of the piece you would like to move
;;         INDEX-OR-CARD, an additional value used to specify
;;           either the index of the other team's piece you wish
;;           to affect or the secondary use of the card
;; OUTPUTS: the modified game after applying card to this piece
;; NOTE: at the start, regardless of what index you use, the next available
;; piece will be moved onto the start spot

(defun play-card (g index &optional index-or-card)
  ;; Get details of the current state of the game
  (let* ((card (sorry-current-card g))
	 (turn (sorry-whose-turn? g))
	 (current-pieces (if (= turn *red*) (sorry-pieces-r g)
			   (sorry-pieces-g g)))
	 (op-pieces (if (= turn *red*) (sorry-pieces-g g)
		      (sorry-pieces-r g)))
	 (piece-loc (aref current-pieces index)))
    (cond 
     ;; When the card is a sorry, put piece on the spot
     ;; of the piece of the other team if possible
     ((and card (= card *sorry*) index-or-card)
      (do-move! g t piece-loc (aref op-pieces index-or-card) card))
     ((and card (null index-or-card))
      ;; Play it
      (do-move! g t piece-loc (move-piece-on-board piece-loc card turn) card))
      (t
       ;; Otherwise say that there is no available card.
       (format t 
	       "Can't play card because no card has been drawn or wrong info given! ~%")))))

;; PASS
;; INPUT: g, a SORRY struct
;; OUTPUT: a modified g structure that has flipped the turn to the other player

(defun pass (g)
  ;; Add the pass to the move history
  (push (list *pass* *pass* *pass* (sorry-current-card g)) (sorry-move-history g))
  ;; Set the card to be blank
  (setf (sorry-current-card g) nil)
  ;; Toggle the turn
  (toggle-turn! g)
  g)


;; LEGAL-CARD-MOVES
;; INPUT: g, a SORRY struct
;; OUTPUT: a list of legal moves that can be performed using this card

(defun legal-card-moves (g)
  (let* ((turn (sorry-whose-turn? g))
	 (current-pieces (if (= turn *red*) (sorry-pieces-r g)
			   (sorry-pieces-g g)))
	 (op-pieces (if (= turn *red*) (sorry-pieces-g g)
			       (sorry-pieces-r g)))
	 (home (if (= turn *red*) *default-red-home* *default-green-home*))
	 (card (sorry-current-card g))
	 (deck (sorry-deck g))
	 ;; Initialize moves to be empty
	 (moves ())
	 (current-move nil))
    ;; For all the pieces
    (dotimes (i *num-pieces*)
      (let ((p (aref current-pieces i)))
	;; When we have piece is at start and card is sorry
	(cond
	 ((and (or (= p *default-red-start*) (= p *default-green-start*))(= card *sorry*))
	  ;; Can also use sorry to move any of oponents
	  ;; pieces back to their start
	  (dotimes (i *num-pieces*)
	    (when (> (aref op-pieces i) 0)
	      (push (list p (aref op-pieces i) *sorry*) moves))))
	 ;; When we are at start with red with a 1
	 ((and (= p *default-red-start*)(= card 1)
	       ;; And When there aren't any pieces on the first sqaure
	       (not (position *first-board-red*  current-pieces)))
	    ;; Push this move on
	      (push (list p *first-board-red* 1) moves))
	 ;; The same is true for green, when we have a 1
	 ;; and are on the start
	 ((and (= p *default-green-start*) (= card 1)
	  ;; And When there isn't something already right
	       ;; by the start, add this move
	       (not (position *first-board-green* current-pieces)))
	    (push (list p *first-board-green* 1) moves))
	 ;; Otherwise, try the card as long as not already
	 ;; in home base
	 ((not (or (= p home) (= p *default-red-start*) (= p *default-green-start*)
		   (= card *sorry*)))
	 ;; If get the potential places the current move to take this piece
	 (setf current-move (use-card card p turn current-pieces op-pieces))
	 ;; When this isn't empty, add to list of moves
	 (when current-move (setf moves (append current-move moves)))))))
    ;; If there are no valid moves, add the pass
    (if moves moves (cons (list *pass* *pass* card) moves))))
