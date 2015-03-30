;===============================================================
;DFS function - take tree in list format and return new list containing node
;that were visited by left-to-right DFS in order.
;arguments: list L that represent tree.
;return value: return single, top-level list of terminal nodes 
;in the order they would visited by a left-to-right depth-first search.
;For example, (DFS '((A (B)) C (D))) would return (A B C D).
;case 1: if NIL was passed as an argument, just return NIL.
;case 2: L=atom ( tree with only root node): if atom was passed as an 
;argument, return list containing that atom.
;case 3, L=list (tree have child): if L is list, apply DFS on the 
;most left branch of the tree and apply DFS on the rest of the tree, 
;and append two lists.
(defun DFS(L)
  (cond ((equal L NIL) NIL)
	((atom L) (LIST L))
	(t (append (DFS(First L)) (DFS(Rest L))))
  )
)

;=============================================================
;DFID (Depth-First-Iterative-Deepending) - perform depth-first interative
;deepending for given list with given depth
;argument: L is list representation of tree, m is integer representtion
;of maximum depth of the tree
;return value: return single top-level list of the terminal 
;nodes in the order that they would visited by a left-to-right DFID search.
;For example, (DFID '((A (B)) C (D)) 3) will return (C A C D A B C D).
(defun DFID(L d)
  (DFID_START L d 0)
)

;DFS_limit - perform depth-first search but depth it can go into is limited
;by the argument
;argument: L is tree in list representtion), m is maximum depth it is
;allowed to perform depth-first search.
;return value: return top-level list of terminal that was visited by
;limited depth-first search.
;case 1 to 2 is same as normal DFS as shown above, but there is one more case:
;case 4: if m is less than 0, then this function is performing DFS in level
;where it is not allowed, so return NIL.
(defun DFS_limit(L m)
  (cond ((< m 0) NIL)
	((equal L NIL) NIL)
	((atom L) (LIST L))
	(t (append (DFS_limit (First L) (- m 1)) 
		   (DFS_limit (Rest L) m)))
  )
)

;DFID_START - function that recursively call itself and limited DFS function
;argument: L is tree in list format, d is maximum depth of tree, m is depth
;that limited DFS is allow to search to.
;return value: return top-level list of terminal node that were visited by
;limited depth-first searches.
(defun DFID_START(L d m)
  (cond ((< d m) NIL)
	(t (append (DFS_limit L m) (DFID_START L d (+ m 1))))
  )
)

;==================================================================
; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((equal s '(3 3 NIL)) t)
	(t NIL)
  )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
;=============
; very first thing I did was getting current # of missionary and carnival,
; and which side boat is located. 
; Next, I calculated # of missionary and carnival after given action.
(defun next-state (s m c)
  (let*((c_mis (First s))(c_carn (First(Rest s))) (c_side (First(Rest(Rest s))))
        (n_mis (+ m (- 3 c_mis))) (n_carn(+ c (- 3 c_carn)))) ;next mis and carn
    (cond ((< c_mis m) NIL) ;trying to move too many  missionaries
	  ((< c_carn c) NIL) ;trying to move too many carnivals
	  ((AND (NOT(= n_mis 3)) (> n_mis n_carn)) NIL);missionary will
	  ((AND (NOT(= n_mis 0)) (< n_mis n_carn)) NIL);get eaten by carnival
	  (t (list(list n_mis n_carn (not c_side)))) ;valid list
    )
  )

)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 1 1)
	  (next-state s 2 0)
	  (next-state s 0 2)
	  (next-state s 1 0)
	  (next-state s 0 1)
   )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((equal (First states) NIL) NIL)
	((equal s (First states)) T)
	(t (on-path s (Rest states)))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). path is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
  (cond ((equal states NIL) NIL) ;there is no more states to tes
	;if reached end of one branch, try next successor state
        ((NULL (mc-dfs (first states) path)) (mult-dfs (rest states) path))
	;else continue searching through current branch
        (t (mc-dfs (first states) path))
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((equal s NIL) NIL) ;if state is empty, return NIL
	((on-path s path) NIL) ;if s is already searched, return NIL
        ;if s is already final state, 
        ;just return path it took to get to state s from the initial state
	((final-state s) (append path (list s)))
	;else continue perfoming DFS on next successors
	(t (mult-dfs (succ-fn s) (append path (list s))))
   )
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))


 

