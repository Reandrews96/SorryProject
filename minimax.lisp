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
  (num-moves 0)
  (num-pruned 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUT:  G, a TTT struct
;;          CUTOFF-DEPTH, depth at which eval func should be used
;;  OUTPUT:  The best move according to MINIMAX,
;;   using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of *CUTOFF-DEPTH*.

(defun compute-move (g cutoff-depth)
  (let* ((me (ttt-whose-turn g))
	 (game-over-results (game-over-score? g me))
	 (statty (make-stats)))
    (cond
     ;; Case 1:  Game over...
     (game-over-results
      (format t "Game is over: ~A!  Sorry dude!~%" game-over-results)
      nil)
     ;; Case 2:  Game still on, compute best move...
     (t
      ;; Call COMPUTE-MAX with init alpha/beta values
      (let ((best-move (compute-max g 0 cutoff-depth statty)))
	;; Report number of moves considered...
	(format t "NUM-MOVES: ~A, PRUNED-MOVES: ~A~%" 
		(stats-num-moves statty) (stats-num-pruned statty))
	best-move)))))
  
  
;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a SORRY struct
;;           CURR-DEPTH, the current depth in the search
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according to MINIMAX 
;;  SIDE EFFECT:  Modifies contents of STATTY

(defun compute-max (g curr-depth cutoff-depth statty)
  ;;(format t "COMPUTE-MAX:  cd=~A~%" curr-depth)
  (let ((best-move-so-far nil)
	(me (ttt-whose-turn g)))
    (cond
     ;; Base Case 0:  Game over
     ((game-over-score? g me)
      ;; just return that value:  either a LOSS or a DRAW
      )
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Use the static evaluation function: assumes game not over
      (eval-func g))     
     ;; Recursive Case:  Need to do minimax with alpha-beta pruning
     (t
      (let* ((moves (legal-moves g))
	     (num-moves-left (length moves))
	     (curr-max *init-max-value*))
	;;(format t "Compute MAX:  legal-moves: ~A~%" moves)
	(dolist (mv moves)
	  (incf (stats-num-moves statty))
	  (decf num-moves-left)
	  (do-move! g nil mv)
	  (let* ((index (position mv moves))
		 (prob (/ (svref (sorry-deck g) index) (sorry-num-cards g)))
		 (child-val (* prob (compute-min g (1+ curr-depth) cutoff-depth statty))))
	    (undo-move! g)
	    ;; Check for updating CURR-MAX...
	    (when (> child-val curr-max)
	      (setf curr-max child-val)
	      (setf best-move-so-far mv)
	      )))
	;; return curr-max or best-move-so-far, depending on whether
	;; we're at depth 0 or not
	(cond
	 ((zerop curr-depth)
	  (format t "ROOT NODE VALUE: ~A~%" curr-max)
	  best-move-so-far)
	 (t
	  curr-max)))))))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           CUTOFF-DEPTH, depth at which eval func should be used
;;           STATTY, a STATS struct
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth cutoff-depth statty)
  ;;(format t "COMPUTE-MIN:  cd=~A, alpha=~A, beta=~A~%" curr-depth alpha beta)
  (let ((me (other-player (ttt-whose-turn g)))) 
    (cond
     ;; Base Case 0: Game over... and score computed
     ((game-over-score? g me)
      ;; just return that value: either a WIN or a DRAW
      )
     ;; Base Case 1:  Game not over, but we're at the cutoff depth
     ((>= curr-depth cutoff-depth)
      ;; Let static eval func do its thing: assumes game not over
      (eval-func g))
     
     ;; Otherwise, we need to use recursion!
     (t
      (let* ((moves (legal-moves g))  
	     (num-moves-left (length moves))
	     (curr-min *init-min-value*))
	;;(format t "Compute MIN:  legal-moves: ~A~%" moves)
	(dolist (mv moves)
	  (incf (stats-num-moves statty))
	  (decf num-moves-left)
	  (do-move! g nil mv)
	  (let* ((index (position mv moves))
		 (prob (/ (svref (sorry-deck g) index) (sorry-num-cards g)))
		 (child-val (* prob (compute-max g (1+ curr-depth) cutoff-depth statty))))
	    (undo-move! g)
	    ;; Update CURR-MIN value if necessary...
	    (setf curr-min (min curr-min child-val))))
	;; return curr-min 
	;;  NOTE:  Depth can't be zero for a MIN node
	curr-min)))))
