;;; ========================================
;;;   CMPU-365, Spring 2019
;;;   Asmt. 2
;;; ========================================
;;;   FILE:  basic-defns.lisp
;;; ========================================
;;;   Data structures and related low-level functions in preparation
;;;   for implementing the general tree-search algorithm.

;;; --------------------------------------
;;;  The PROBLEM struct 
;;; --------------------------------------

(defstruct search-problem
  init-state		; the initial state
  actions		; the list of actions
  goal-test-func	; the goal test function
  state-eq-func		; the state-equality function
  )

;;; --------------------------------------
;;;  The NODE struct 
;;; --------------------------------------
;;;  Note the use of the :PRINT-FUNCTION keyword argument that enables
;;;  us to specify which function should be used to display a NODE
;;;  struct.  (Kind of like providing a "toString" method in Java.)

(defstruct (node (:print-function print-node))
  state			; the state associated with this node
  (parent nil)		; the parent node for this node
  (action nil)		; the most recent action performed
  (depth 0)		; the depth of this node
  )

;;;  PRINT-NODE  --  print function for NODEs 
;;; ------------------------------------------------------------------
;;;  Note:  The three inputs specified below are required if we
;;;         want to use this function as the printing function for
;;;         the NODE struct defined above.
;;; ------------------------------------------------------------------
;;;  INPUTS:  NODE, a search node
;;;           STR, an output stream (usually t)
;;;           DEPTH, printing depth parameter (ignored)
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Displays given NODE to the given output stream.

(defun print-node (node str depth)
  ;; To avoid compiler warnings about an input we never use.
  (declare (ignore depth))
  
  (let ((st (node-state node)))
    ;; Display the most recent action (i.e., the one that got us here)
    (format str "NODE:  (action = ~A)~%" (node-action node))
    ;; Display the current state
    (format str "         STATE: ~A~%" st)))

;;;  BUILD-PATH
;;; -----------------------------------------------------------
;;;  INPUT:   GOAL-NODE
;;;  OUTPUT:  A list of nodes obtained by tracing the parent
;;;           links starting from the goal-node all the way
;;;           back to the root node

(defun build-path (goal-node)
  
  ;; BUILD-PATH-ACC is a recursive helper function
  ;; Note the use of LABELS (similar to LETREC in Scheme)
  
  (labels ((build-path-acc (node acc)
	     ;; Base Case:  NODE is NIL 
	     ;;   This happens when previous function call involved
	     ;;   the root node.  The root node's parent is NIL.
	     ;;   In this case, we are done!
	     (if (not node)
		 ;; So return the accumulator
		 acc
	       ;; Recursive Case:  
	       ;;   Accumulate the current node as we move
	       ;;   to the PARENT...
	       (build-path-acc (node-parent node) (cons node acc)))))
    
    ;; Body of LABELS:  call the recursive helper with ACC=NIL
    (build-path-acc goal-node nil)))

;;; ------------------------------------------------------------
;;;  RESULTS struct -- used to keep track of info during search 
;;; ------------------------------------------------------------
;;;  The start time, end time, and elapsed time are all measured by
;;;  "internal-run-time" which is typically measured in milliseconds.
;;;  See the global constant INTERNAL-TIME-UNITS-PER-SECOND, which
;;;  equals 1000 on our system.
;;; -----------------------------------------------------------
;;;  Note the PRINT-RESULTS print function

(defstruct (results (:print-function print-results))
  (goal-node nil)       ; a goal node, if found; otherwise, NIL
  (num-nodes 0)         ; the number of nodes generated during the search
  (start-time NIL)      ; when the search started (msecs)
  (end-time NIL)        ; when the search stopped (msecs)
  (elapsed-time NIL)    ; how long the search took (msecs)
  )

;;;  PRINT-RESULTS  --  print function for RESULTS structs
;;; -------------------------------------------------------------
;;;  INPUTS:  REZZY, a RESULTS struct
;;;           STR, the stream to print to (usually T for us)
;;;           DEPTH, ignored by us
;;;  OUTPUT:  Nothing
;;;  SIDE EFFECT:  If REZZY's GOAL-NODE is NIL, then it prints out a failure
;;;    message.  Otherwise, it prints out a success message and displays the
;;;    sequence of nodes from the ROOT all the way to the GOAL-NODE.

(defun print-results (rezzy str depth)
  ;; To avoid compiler warnings about an input we never use
  (declare (ignore depth))
  (let ((nodey (results-goal-node rezzy))
	(num-nodes (results-num-nodes rezzy)))

    (cond
     
     ;; CASE 1:  NODEY is an instance of a search NODE
     ((node-p nodey)
      ;; We assume this means that NODEY is a goal node
      (format str "SUCCESS!!!~%~%")
      ;; use BUILD-PATH to generate the list of nodes from the 
      ;; root node to NODEY, then display these nodes.
      (format str "*** BEGIN SOLUTION PATH ***~%~%")
      (let ((soln-path (build-path nodey)))
	(format str "~A~%" soln-path)
	(format str "==============================================~%")
	(format str "*** END SOLUTION PATH (length: ~A) ***~%"
		(1- (length soln-path)))
	(format str "==============================================~%")))
     
     ;; CASE 2:  NODEY is not a NODE struct...
     (t
      ;; We assume that this means failure...
      (format str "~%No solution path was found... ~%")))
    
    ;; In either case, show how many nodes were explored...
    (format str "Overall Node Count: ~A~%" num-nodes)))


;;;  GEN-SEARCH -- wrapper function for GEN-SEARCH-GUTS
;;; --------------------------------------------------------
;;;   INPUTS:  PROBLEM, a search problem
;;;            QUEUE-FN, a queuing function
;;;   OUTPUT:  A RESULTS data structure, whose contents report
;;;            the results of the search.
;;; --------------------------------------------------------
;;;   NOTE: This is just a wrapper for GEN-SEARCH-GUTS (which you will
;;;   define in "gen-search-starter.lisp").  GEN-SEARCH-GUTS does most
;;;   of the work.  The wrapper function creates a new RESULTS data
;;;   structure, initialized with NUM-NODES = 0 and START-TIME fetched
;;;   from the operating system.

(defun gen-search (problem queue-fn)

  (let (;; Initial RESULTS struct 
	(rezzy (make-results :start-time (get-internal-run-time))))

    ;; GEN-SEARCH-GUTS will update the contents of REZZY:
    ;;   GOAL-NODE will either be NIL (failure) or a goal NODE,
    (gen-search-guts problem queue-fn rezzy)

    ;; Update END-TIME and ELAPSED-TIME
    (setf (results-end-time rezzy) (get-internal-run-time))
    (setf (results-elapsed-time rezzy) (- (results-end-time rezzy)
					  (results-start-time rezzy)))
    
    (cond
     
     ;; Case 1:  The search returned a node... presumably a GOAL node!
     ((node-p (results-goal-node rezzy))
      (format t "Hey!  We found a goal node!!~%"))

     ;; Case 2:  The search returned something else... indicating failure
     (t
      (format t "Uh oh... queue empty!~%")))
    
    ;; Report statistics     
    (let ((num-nodes (results-num-nodes rezzy))
	  (elapsed-time (/ (results-elapsed-time rezzy)
			   internal-time-units-per-second
			   1.0)))
      (format t "~%Generated ~A nodes " num-nodes)
      (if (> num-nodes 0)
	  (format t "in ~A seconds (~A sec/node)~%"
		  elapsed-time (/ elapsed-time num-nodes))
	(format t "~%")))
      
    ;; Return the search-result 
    rezzy
    ))


;;;  Helper functions for BREADTH-FIRST and DEPTH-FIRST search
;;; ------------------------------------------------------------
;;;  FRONT-ENQUEUE! and END-ENQUEUE! are DESTRUCTIVE functions!
;;;  That is why their names end with an exclamation point.
;;;  They use NCONC which is a DESTRUCTIVE function (whose name
;;;  for some reason doesn't have a ! at the end).  NCONC outputs
;;;  the concatenation of two input lists, just like APPEND.  However,
;;;  NCONC does it not by recreating the entire first list, but 
;;;  instead by DESTRUCTIVELY modifying the last cons cell in the
;;;  first list to point to the second list!  Neat trick!  Does
;;;  not require creating *any* new cons cells!  But it is destructive.
;;;  Here it is okay, for efficiency reasons, to use NCONC.  Also,
;;;  we only use it to destructively modify the search queue. 
;;;  Since the search queue is only used by GEN-SEARCH-GUTS, that's
;;;  okay.  Of course, if it makes you nervous, you can replace each
;;;  occurrence of NCONC with APPEND... but your code will probably
;;;  run more slowly, albeit more safely!

;;;  FRONT-ENQUEUE!
;;; -------------------------------------------------
;;;  INPUT:  OLD-NODES, a list of old nodes (i.e., old search queue)
;;;          NEW-NODES, a list of new nodes (i.e., new child nodes)
;;;  OUTPUT:  A list containing new-nodes (at the front) and 
;;;           old nodes (at the rear)

(defun front-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND (see Paul Graham's book)
  ;; We use it here for efficiency reasons
  (nconc new-nodes old-nodes))

;;;  END-ENQUEUE!
;;; ---------------------------------------------------
;;;  Just like FRONT-ENQUEUE! except that the old-nodes go at the front.

(defun end-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND
  ;; We use it here for efficiency reasons
  (nconc old-nodes new-nodes))

;;;  BREADTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  The result of doing breadth-first search on the given
;;;             problem:  either NIL or a goal NODE.

(defun breadth-first-search (prob)
  ;; Note that since END-ENQUEUE! is not the first element of the
  ;; list, we need to use #' to ensure that its "function value"
  ;; is passed as input to the GEN-SEARCH function.
  (gen-search prob #'end-enqueue!))

;;;  DEPTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  Just like breadth-first-search, except that it uses a different
;;;  queuing function.

(defun depth-first-search (prob)
  (gen-search prob #'front-enqueue!))
