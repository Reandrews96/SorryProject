;;; =====================
;;;  Rebecca Andrews
;;;  Yina Wang
;;;  Expectiminimax.lisp
;;; =====================

(defconstant *init-max-value* -10000)
(defconstant *init-min-value* +10000)

;;  STATS struct for search statistics

;;  COMPUTE-MOVE, COMPUTE-MAX, COMPUTE-MIN functions
;;    for minimax 

;; ===================================================================

;;  STATS struct
;; ---------------------------------------
;;  For keeping track of the number of moves explored
;;  and the number of moves pruned during COMPUTE-MOVE.

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUT:  G, a sorry struct
;;          CUTOFF-DEPTH, depth at which eval func should be used
;;          EVAL-FUNC, a function that calculates the value
;;          of the particular state of the game
;;  OUTPUT:  The best move according to MINIMAX,
;;   using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of *CUTOFF-DEPTH*.

(defun compute-move (g cutoff-depth eval-func)
    (cond
     ;; Case 1:  Game over...
     ((game-over g)
      (format t "Game is over!  Sorry dude!~%")
      nil)
     ;; Case 2:  Game still on, compute best move...
     (t
      (when (null (sorry-current-card g))
	(draw-card g))
      ;; Call COMPUTE-MAX with init alpha/beta values
      (let* ((statty (make-stats))
	     (best-move (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth eval-func)))
	;; Report number of moves considered...
	(format t "NUM-MOVES-DONE: ~A, PRUNED-MOVES: ~A~%" 
		(stats-num-moves-done statty) 
		(- (stats-num-potential-moves statty)
		   (stats-num-moves-done statty)))
	(format t "BEST MOVE: ~A~%" best-move)
	(format t "CARD WAS ~A ~%" (sorry-current-card g))
	best-move))))
  
  
;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  GAME, a SORRY struct
;;           CURR-DEPTH, the current depth in the search
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct
;;           EVAL-FUNC, a function to calculate the value of a particular
;;             move/state in the game
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according to MINIMAX 
;;  SIDE EFFECT:  Modifies contents of STATTY

(defun compute-max (game curr-depth alpha beta statty cutoff-depth eval-func)
  (let ((best-move-so-far nil))
    (cond
     ;; Base Case 0:  Game over
     ((game-over game)
      ;; just return that value:  either a LOSS or a DRAW
      (+ *loss-value* curr-depth))
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Use the static evaluation function: assumes game not over
      (funcall eval-func game))
     ;; Recursive Case:  Need to do minimax with alpha-beta pruning
     (t
      (let* ((moves (legal-card-moves game)))
	(incf (stats-num-potential-moves statty) (length moves))
	;; When we only have one move and we are at depth
	;; zero, just return this move
	(when (and (= 1 (length moves)) (zerop curr-depth))
	  (return-from compute-max (first moves)))
	(dolist (mv moves)
	  (incf (stats-num-moves-done statty))
	  (apply #'do-move! game nil mv)
	  (let* ((child-val (compute-chance game (+ 1 curr-depth) 
					    alpha beta statty cutoff-depth 0
					    eval-func)))
	    (undo-move! game)
	    ;; Check for updating CURR-MAX...
	    (when (> child-val alpha)
	      (setf alpha child-val)
	      (setf best-move-so-far mv)
	      (when (<= beta alpha)
		(return-from compute-max
		  (cond
		   ((zerop curr-depth)
		    (format t "     ROOT NODE VALUE: ~A~%" (fround alpha))
		    best-move-so-far)
		   (t
		    alpha)))))))
	;; return curr-max or best-move-so-far, depending on whether
	;; we're at depth 0 or not
	(cond
	 ((zerop curr-depth)
	  (format t "ROOT NODE VALUE: ~A~%" (fround alpha))
	  best-move-so-far)
	 (t
	  alpha)))))))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a SORRY struct
;;           CURR-DEPTH, the depth of this MIN node
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct,
;;           EVAL-FUNC, a function used to calculate the value of a
;;            move/state in the game
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth eval-func)
  (cond
     ;; Base Case 0: Game over... and score computed
     ((game-over g)
      ;; just return that value: either a WIN or a DRAW
      (- *win-value* curr-depth))
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Let static eval func do its thing: assumes game not over
      (funcall eval-func g))
     ;; Otherwise, we need to use recursion!
     (t
      (let* ((moves (legal-card-moves g)))
	(incf (stats-num-potential-moves statty) (length moves))
	(dolist (mv moves)
	  (incf (stats-num-moves-done statty))
	  (apply #'do-move! g nil mv)
	  (let ((child-val (compute-chance g (+ 1 curr-depth) alpha beta statty cutoff-depth 1
					   eval-func)))
	    (undo-move! g)
	    (when (< child-val beta)
	      (setf beta child-val)
	      (when (<= beta alpha)
		(return-from compute-min beta)))))
	;; return beta
	;;  NOTE:  Depth can't be zero for a MIN node
	beta))))

;; COMPUTE-CHANCE
;; ------------------------
;;  INPUTS:  G, a SORRY struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, the alpha value
;;           BETA, the beta value
;;           STATTY, a STATS struct
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           MAX?, if the max (1) or min(0) should be called
;;           EVAL-FUNC, a function used to calculate the value of
;;             move/state
;;  OUTPUT:  The value of this CHANCE node according to rules
;;           of EXPECTIMINIMAX with ALPHA-BETA pruning

(defun compute-chance 
    (g curr-depth alpha beta statty cutoff-depth max? eval-func)
  (let ((total-sum 0))
    (dotimes (i (length *cards*))
      (let* ((child-val nil)
	     (card (svref *cards* i))
	     (prob (/ (svref (sorry-deck g) i) (sorry-num-cards g))))
	(when (not (= prob 0))
	  (setf (sorry-current-card g) card)
	  (setf child-val 
	    (if (= max? 1)
		(compute-max g curr-depth alpha beta statty cutoff-depth eval-func) 
	      (compute-min g curr-depth alpha beta statty cutoff-depth eval-func)))
	  (incf total-sum (* prob child-val)))))
    total-sum))
	