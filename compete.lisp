;;  COMPETE
;; --------------------------------------------------
;; INPUTS: RED-DEPTH, the depth that red should simulate with minimax
;;         GREEN-DEPTH, the depth that green should simulate with minimax
;;         G, a SORRY struct
;; OUTPUTS: the end game after competing

(defun compete
    (red-depth green-depth g)
  ;; While the game isn't over
  (while (not (game-over g))
    (cond
     ;; On red's turn:
     ((eq (sorry-whose-turn? g) *red*)
      (format t "Red ~%")
      ;; compute and apply the best move using red's depth
      (apply #'do-move! g nil (compute-move g red-depth both-non-random)))
     ;; On green's turn
     (t
      (format t "Green ~%")
      ;; compute and apply the best move using green's depth
      (apply #'do-move! g nil (compute-move g green-depth both-non-random)))))
  g)


;; COMPETE-VS-RANDOM
;; -------------------------------------------------
;; INPUTS: DEPTH, the depth that red should simulate with minimax
;;         G, a SORRY struct
;; OUTPUTS: the end game after competing against the random player green

(defun compete-vs-random
    (depth g)
  (while (not (game-over g))
    (cond
     ;; On red's turn
     ((eq (sorry-whose-turn? g) *red*)
      (format t "Red (AI) ~%")
      ;; compute and apply the best move based on the given depth
      (apply #'do-move! g nil (compute-move g depth #'default-eval-func)))
     ;; On green's turn
     (t
      (format t "Green (RANDOM) ~%")
      ;; Set the current card to be a card that has been
      ;; randomly selected
      (setf (sorry-current-card g) (select-card g))
      (format t "CARD WAS ~A ~%" (sorry-current-card g))
      ;; Get the list of legal moves
      ;; and select a random move
      (let* ((moves (legal-card-moves g))
	     (n (random (length moves)))
	     (move (nth n moves)))
	(format t "RANDOM MOVE ~A ~%" move)
	;; Apply this randomly selected move
	(apply #'do-move! g nil move)))))
  g)

(defconstant *default-depth* 4)

;; COMPETE-DIFF-EVAL
;; -----------------------------
;; INPUTS: EVAL-RED the eval function used by red player
;;         EVAL-GREEN, the eval function used by green player
;;         G, a SORRY struct
;;         
;; OUTPUTS: the end game after competing

(defun compete-diff-eval (eval-red eval-green g)
  (while (not (game-over g))
    (cond
     ((eq (sorry-whose-turn? g) *red*)
      (format t "Red ~%")
      ;; compute and apply the best move using red's eval
      (apply #'do-move! g nil (compute-move g *default-depth* eval-red)))
     ;; On green's turn
     (t
      (format t "Green ~%")
      ;; compute and apply the best move using green's eval
      (apply #'do-move! g nil (compute-move g *default-depth* eval-green)))))
  g)
