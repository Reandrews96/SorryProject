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
;;  OUTPUT:  The best move according to MINIMAX,
;;   using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of *CUTOFF-DEPTH*.

(defun compute-move (g cutoff-depth)
    (cond
     ;; Case 1:  Game over...
     ((game-over g)
      (format t "Game is over!  Sorry dude!~%")
      nil)
     ;; Case 2:  Game still on, compute best move...
     (t
      ;; Call COMPUTE-MAX with init alpha/beta values
      (let* ((statty (make-stats))
	     (best-move (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth)))
	;; Report number of moves considered...
	(format t "NUM-MOVES-DONE: ~A, PRUNED-MOVES: ~A~%" 
		(stats-num-moves-done statty) 
		(- (stats-num-potential-moves statty)
		   (stats-num-moves-done statty)))
	(format t "BEST MOVE: ~A~%" best-move)
	best-move))))
  
  
;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a SORRY struct
;;           CURR-DEPTH, the current depth in the search
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according to MINIMAX 
;;  SIDE EFFECT:  Modifies contents of STATTY

(defun compute-max (g curr-depth alpha beta statty cutoff-depth)
  ;;(format t "COMPUTE-MAX:  cd=~A~%" curr-depth)
  (let ((best-move-so-far nil))
    (cond
     ;; Base Case 0:  Game over
     ((game-over g)
      ;; just return that value:  either a LOSS or a DRAW
      (+ *loss-value* curr-depth))
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Use the static evaluation function: assumes game not over
      (eval-func g))     
     ;; Recursive Case:  Need to do minimax with alpha-beta pruning
     (t
      (let* ((moves (legal-moves g)))
	(incf (stats-num-potential-moves statty) (length moves))
	;;(format t "Compute MAX:  legal-moves: ~A~%" moves)
	(dolist (mv moves)
	  (incf (stats-num-moves-done statty))
	  (apply #'do-move! g nil mv)
	  (let* ((index (position mv moves))
		 (prob (/ (svref (sorry-deck g) index) (sorry-num-cards g)))
		 (child-val (* prob 2 (compute-min g (+ 1 curr-depth) alpha beta statty cutoff-depth))))
	    (undo-move! g)
	    ;; Check for updating CURR-MAX...
	    (when (> child-val alpha)
	      (setf alpha child-val)
	      (setf best-move-so-far mv)
	      (when (<= beta alpha)
		(return-from compute-max
		  (cond
		   ((zerop curr-depth)
		    (format t "     ROOT NODE ALPHA: ~A~%" alpha)
		    best-move-so-far)
		   (t
		    alpha)))))))
	;; return curr-max or best-move-so-far, depending on whether
	;; we're at depth 0 or not
	(cond
	 ((zerop curr-depth)
	  (format t "ROOT NODE VALUE: ~A~%" alpha)
	  best-move-so-far)
	 (t
	  alpha)))))))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a SORRY struct
;;           CURR-DEPTH, the depth of this MIN node
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth)
  ;;(format t "COMPUTE-MIN:  cd=~A, alpha=~A, beta=~A~%" curr-depth alpha beta) 
    (cond
     ;; Base Case 0: Game over... and score computed
     ((game-over g)
      ;; just return that value: either a WIN or a DRAW
      (- *win-value* curr-depth))
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Let static eval func do its thing: assumes game not over
      (eval-func g))
     
     ;; Otherwise, we need to use recursion!
     (t
      (let* ((moves (legal-moves g)))
	(incf (stats-num-potential-moves statty) (length moves))
	;;(format t "Compute MIN:  legal-moves: ~A~%" moves)
	(dolist (mv moves)
	  (incf (stats-num-moves-done statty))
	  (apply #'do-move! g nil mv)
	  (let* ((index (position mv moves))
		 (prob (/ (svref (sorry-deck g) index) (sorry-num-cards g)))
		 (child-val (* prob (compute-max g (+ 1 curr-depth) alpha beta statty cutoff-depth))))
	    (undo-move! g)
	    (when (< child-val beta)
	      (setf beta child-val)
	      (when (<= beta alpha)
		(return-from compute-min beta)))))
	;; return beta
	;;  NOTE:  Depth can't be zero for a MIN node
	beta))))
