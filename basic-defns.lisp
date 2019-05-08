;;  To ensure that the compiler efficiently handles tail recursion

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;;  To avoid annoying garbage-collection messages

(setf *global-gc-behavior* :auto)

;;  The list of files for the OTHELLO implementation:

(defparameter *all-files*
  (list "basic-defns"
	"sorry"
	"eval-func"
    "minimax"
    "compete"))

;;  MAKER
;; ------------------------------------
;;  Compiles and loads all files for the Othello/MCTS implementation

(defun maker
    ()
  (dolist (file *all-files*)
    (compile-file file)
    (load file)))
