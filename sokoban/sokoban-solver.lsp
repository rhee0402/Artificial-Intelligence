;
; Artificial Intelligence: Sokoban solver
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; 
;  
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "sokoban-solver.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads sokoban-solver.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; I pick each row and search through column looking for box-alone(2), 
; if there is no box-alone, return t,
; if there is box-alone, return NIL.
(defun goal-test (s)
  (cond ((null (first(first s))) t)
	((isTargetInList (first s) 2) NIL) ;2=box-alone
	(t (goal-test(rest s)))
  );end cond
);end defun

; isTargetInList - Look for the target element in the given list.
; argument: list is list to search. 
;           target is element that this function will look for.
; return value: if there is target in the given list, return true,
;               if target is not in the list,return false.
(defun isTargetInList (list target)
  (cond ((< 0 (COUNT target list)) t)
	(t NIL)
  );end cond
);end defun

;=========================================================
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;==================== 
; UP,DOWN,LEFT RIGHT will be represented 11,12,13,14 respectively.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move S 11) (try-move S 12) 
		       (try-move S 13) (try-move S 14)))
	 )
    (cleanUpList result);end
   );end let
  );

;get-square function return integer content of state S at square (r,c)
;If square (r,c) is outside the scope of S, return value of wall, 1.
(defun get-square (S r c)
  (cond ((OR (< r 0) (< c 0)) 1)
        (t (let*((content (first (nthcdr c (first (nthcdr r S))))))
              (cond ((null content) 1) ;if out of scope, return 1
	            (t content) ;else return content
	       );end cond
            );end let*
         )
  );end cond
);end defun


; set-square function change content at sqaure (r,c) in state S into 
; given value of v and return as new state S' that has modified square.
; This function assume (r,c) is NOT out of scope of state S.
(defun set-square (S r c v)
  (cond ((= r 0) (append (list(set-list (first S) c v)) (rest S)))
	(t (append (list(first S)) (set-square (rest S) (- r 1) c v)))
  );end cond

);end defun

; set-list function return new list that content is same as list L 
; except value at c'th element got changed to v.
(defun set-list (L c v)
  (cond ((= c 0) (append (list v) (rest L)))
	(t (append (list(first L)) (set-list (rest L) (- c 1) v)))
  );end cond
);end defun

; coordi function return position after moving object at (r,c) in
; direction D. Returned position is given as the list form (row column)
; UP,DOWN,LEFT RIGHT will be represented 11,12,13,14 respectively.
; (car coordi( r c D)) is row, (cadr coordi(r c D)) is column
; Assuming wall surrounds in state S, no need for checking negative position.
(defun coordi (r c D)
  (cond
        ((= D 11) (list (- r 1) c)) ;MOVE UP
	((= D 12) (list (+ r 1) c)) ;MOVE DOWN
	((= D 13) (list r (- c 1)))  ;MOVE LEFT
	((= D 14) (list r (+ c 1)))  ;MOVE RIGHT
  );end cond
);end defun

; whatObjectAhead function return integer value of encountered object that  
; keeper/box at position (r,c) will encounter if he/it moves in direction D.
(defun whatObjectAhead (S r c D)
  (get-square S (car(coordi r c D)) (cadr(coordi r c D)))
);end defun

; isKeeperMoveValid function return true if it is valid for keeper in state S
; to move in direction D. If it is invalid move, return NIL.
; kr is row position of keeper, kc is column position of keeper.
; ahead is content of next square if keeper move in direction D.
(defun isKeeperMoveValid (S D)
  (let* ((k_pos (getKeeperPosition s 0)) (kr (cadr k_pos)) (kc (car k_pos))
	 (ahead (whatObjectAhead S kr kc D))(next_pos (coordi kr kc D)) 
	 (next_c(cadr next_pos)) (next_r(car next_pos)))
    (cond ((equal ahead 1) NIL) ;encountered wall
	  ((or (equal ahead 2) (equal ahead 5)) ;encountered boxes 
	   (isBoxMoveValid S next_r next_c D)) ;seperate check for box
	  (t t) ;there is no wall or box infront, so it is valid move
    );end cond
  );end let*
);end defun

; isBoxMoveValid function return true if it is valid for the box at (br,bc)
; in state S to move in direction D. if it is invalid for it to move,
; return NIL.
(defun isBoxMoveValid (S br bc D)
  (let* ((ahead (whatObjectAhead S br bc D)))
    (cond ((or (equal ahead 1) (equal ahead 2) (equal ahead 5)) NIL)
	  (t t) ;I could just return 'ahead' to give more information
    );end cond
  );end let*
);end defun

; whereKeeperStand function 
; return 3 if object at (r,c) standing on the empty space 
; return 6 if object at (r,c) is standing on the goal.
(defun whereObjectStand(S r c)
  (let* ((stat (get-square S r c )))
    (cond ((equal stat 3) 0) ;if stand on emtpy space, return 0(empty).
	  (t 4);if stand on goal, return 4(goal).
     );end cond
  );end let*
);end defun


; try-move function takes state S and move keeper in direction D.
; function will return state that is the result of moving 
; keeper in direction D. 
; If move was invalid (i.e there is wall in that direciton), return NIL.
; UP,DOWN,LEFT RIGHT will be represented 11,12,13,14 respectively.
; kr is row position of keeper, kc is column position of keeper.
; k_stand is integer rep. of what keeper is standing on (empty,0 or goal,4)
; ahead is content of next square if keeper move in direction D.
; next_pos, next_r, next_c represent position, row, column # after one move
; nn_pos, nn_r, nn_c represent position, row, column # after two moves.
(defun try-move (S D)
  (cond ((NOT(isKeeperMoveValid S D)) NIL);if not valid, return NIL.

	(t ;else for the first cond; below here, I know move is valid, 
	 (let* ((k_pos (getKeeperPosition s 0)) (kr (cadr k_pos)) 
	        (kc (car k_pos)) (k_stand (whereObjectStand S kr kc))
	        (ahead (whatObjectAhead S kr kc D))
		(next_pos (coordi kr kc D)) (next_c(cadr next_pos)) 
		(next_r(car next_pos))(nn_pos (coordi next_r next_c D)) 
		(nn_c(cadr nn_pos))(nn_r(car nn_pos)))
	   (cond ((equal ahead 0) ;if keeper->empty space 
		  (set-square (set-square S next_r next_c 3) kr kc k_stand))
		 ((equal ahead 4) ;if keeper->goal
		  (set-square (set-square S next_r next_c 6) kr kc k_stand))

		 (t ;else of second cond; below here, ahead=box(+goal)
		  (let*((n_ahead (whatObjectAhead S next_r next_c D)))
		   (cond
		    ;if keeper->box->empty space, 
		    ((AND (equal ahead 2) (equal n_ahead 0))
		      (set-square (set-square (set-square S nn_r nn_c 2) 
				    next_r next_c 3) kr kc k_stand))
		    ;if keeper->box->goal,
		    ((AND (equal ahead 2) (equal n_ahead 4))
		     (set-square (set-square (set-square S nn_r nn_c 5) 
				   next_r next_c 3) kr kc k_stand))
                     ;if keeper->(box+goal)->empty space
		    ((AND (equal ahead 5) (equal n_ahead 0))
		     (set-square (set-square (set-square S nn_r nn_c 2) 
				   next_r next_c 6) kr kc k_stand))

		    (t ;if keeper->(box+goal)->goal
		       (set-square (set-square (set-square S nn_r nn_c 5)
				     next_r next_c 3) kr kc k_stand))
		   );end of third cond
		  );end of second let*
		);end of second else
	   );end second cond 

	  );end let*
	);end of else for first cond
  );end first cond
);end defun 



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0 ;return 0 by the default
 );end defun

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; ========================
; h1 function is admissible because boxes that are not on goal require
; at least one move to reach goal. Actual cost cannot be lower.
;==========================
; Since I cannot change number of argument, I added one more helper function,
; h1_recur, to use recursion and keep the number of misplaced boxes during the
; recursion.
(defun h1 (s)
  (h1_recur s 0)
  )

; h1_recur - recursive call for h1 that calls itself until it check all 
; the rows.
; case 1: if list is empty(which mean it checked every rows already), 
;         return current heuristic value (# of misplaced box)
; case 2: if list is not emtpy (which mean there are still row to check) 
;         count the number of boxes in the first row, add it to the 
;         current heuristic value, and repeat procedure from the next row
;         until it check all the rows.
(defun h1_recur (s hn)
  (cond ((null (first s)) hn)
	(t (h1_recur (rest s) (+ (count 2 (first s)) hn)))
  );end cond
);end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;my h(n) value will be total distances from the boxes to the closest goals.
;(i.e, sum of distances from the boxes to their cloest goals)
;[plus distance from keeper to the cloest box? <this is not sure>]
(defun h504170369 (s)
  (let* ((BnGTable (genBoxnGoalTable s)))
    (sumMinDistances (First BnGTable) (cadr BnGTable) 0)
  );end let*
 );end defun

;genBoxnGoalTable function generate table 
;(or list that contain two lists: one for position of boxes, other for position of goals)
;return one list that contain two sub-list
;( ((pos_box1)(pos_box2)...) ((pos_goal1)(pos_goal2)...)) = BnGTable
(defun genBoxnGoalTable (S)
  (genBoxnGoalTableRow S '() 0)
);end defun

;genBoxnGoalTableRow function take one row and call genBoxnGoalTableColumn function
;to observe and add data to the table about any box or goal in that row.
;After getting all the table input about that row, repeat for next row until all rows 
;are checked
(defun genBoxnGoalTableRow (S BnGTable r)
 (cond ((null (first S)) BnGTable)
       (t (genBoxnGoalTableRow (rest S) (genBoxnGoalTableColumn (first S) BnGTable r 0) 
	    (+ r 1)
	  )
       )
  );end cond
);end defun

;genBoxnGoalTableColumn function take single list (no sub-list inside)
;and add position of box and position of goal into the BnGTable
;idea is look at first element in the list, add position to the table if its box or goal
;else continue to next element.
(defun genBoxnGoalTableColumn (L BnGTable r c)
  (cond ((null (first L)) BnGTable)
	((OR (= (first L) 2) (= (first L) 4))
	  (genBoxnGoalTableColumn (rest L)(addToTable BnGTable (first L) (list r c)) r (+ c 1))
	)
	(t (genBoxnGoalTableColumn (rest L) BnGTable r (+ c 1)))
   );end cond
);end defun

; Add position to BnGTable depending on the type of object given
; BnGTable = ( ((pos_box1)(pos_box2)...) ((pos_goal1)(pos_goal2)...))
(defun addToTable (BnGTable object pos)
  (cond ((= object 2) (list(append(first BnGTable) (list pos)) (cadr BnGTable)))
	(t (list (first BnGTable) (append (cadr BnGTable) (list pos))))
   );end cond

);end defun

;findDistance function returns the number of moves it take for object to go from
;pos1 to pos2 assuming there is no wall or blocking object. This can be easily done by
;taking sum of absolute values of (row1-row2) and (column1-column2)
(defun findDistance(pos1 pos2)
  (let* ((r1 (first pos1)) (c1 (cadr pos1)) (r2 (first pos2)) (c2 (cadr pos2)))
    (+ (abs(- r1 r2)) (abs(- c1 c2)))
 ;   (cond ((AND (> r1 r2) (> c1 c2)) (+ (- r1 r2) (- c1 c2)))
	;  ((AND (> r1 r2) (< c1 c2)) (+ (- r1 r2) (- c2 c1)))
	 ; ((AND (< r1 r2) (> c1 c2)) (+ (- r2 r1) (- c1 c2)))
	  ;(t (+ (- r2 r1) (- c2 c1)))
     );end cond
   );end let*
);end defun

; findMinDistance function returns the minimum distance from given box to cloest goal.
; distance mean number of moves object need to get from source to destiniation point.
; box_pos = (box's row, bow's column) and goalTable = '((goal1_pos)(goal2_pos)(goal3_pos))
; minDist is current minimum distance. (which can be changed as we check through goalTable
; case 1: if we check every elements in the table, return current minimum distance
; case 2: if minDist=NIL (initially) or newly compute distance is smaller than current one,
; set new distance as minimum distance and check next element in the goalTable.
; case 3: if minDist is smaller, keep it as minimum distance and check next element in the table.
(defun findMinDistance(box_pos goalTable minDist)
  (cond ((null (caar goalTable)) minDist) ;if every goals were checked, return min distance
        (t (let* ((distance(findDistance box_pos (first goalTable))))
                (cond ((null minDist)(findMinDistance box_pos (rest goalTable) distance))
	              ((> minDist distance) (findMinDistance box_pos (rest goalTable) distance))
	              (t (findMinDistance box_pos (rest goalTable) minDist))
                 );end cond
            );end let*
	 )
  );end cond
);end defun


; sumMinDistances function return the sum of minimum distances of all the boxes to the closest goals.
(defun sumMinDistances(boxTable goalTable sum)
  (cond ((null (first boxTable)) sum) ;if no more boxes, return current sum.
	((null (first goalTable)) 0) ;if all goals are filled with boxes, cost is 0
	(t (sumMinDistances (Rest boxTable) goalTable (+ sum (findMinDistance (first boxTable) goalTable NIL))))
   );end cond
);end defun




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun