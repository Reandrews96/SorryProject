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
  (num-cards 10)
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
   ((and check-legal? (not (legal-move? game loc-old loc-new)))
    (format t "Can't do illegal move: ~A => ~A ~%" loc-old loc-new))
   (t
    'Implement)))
