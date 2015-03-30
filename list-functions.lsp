;=========================================================================
;SUB-LIST function - create sub-list of given list according to given arguments
;arguments: L is original list
;           START is position of item you want to start creating sub-list
;           LEN is length of sub-list you want to create
;return value: return list that starts with item you designated 
;with START argument WITH number of items in the list is equal to LEN argument
;I have to worry about 3 cases:
;case 1: If LEN=0, length of list must be 0, so I simply return  NIL
;case 2: If  START=0 but LEN>0, I append the first element on the list with
;        next elements by using recursion and subtracting LEN by 1 for 
;        each recursion until LEN become 0 and return NIL.
;case 3: If START>0, I can remove first element on the list and 
;        subtract START by 1 and call SUB-LIST again.
;NOTE:I do not have to worry about LEN=1 case because case 1 and case 2 
;     take care of it.
(defun SUB-LIST(L START LEN)
  (Cond ((= LEN 0) NIL)
	((= START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
	(t (SUB-LIST (rest L) (- START 1) LEN))
 )
)

;=========================================================================
;SPLIT-LIST function - split list into two lists such that L2 contain 
;same or one more number of elements
;argument: L, list that we want to split into two lists, L1 and L2.
;return value: return two list L1, L2 which was splitted from list L and
;              Length of L2 minus Length of L1 is either 0 or 1.
;Basically, there are two cases:
;case 1: Length of List L is even, then both L1 and L2 will have 
;length half of L(call len), and L1 will be list from position 0 to len-1, 
;L2 will be position len to last element
;case 2: Length of List L is odd, then L2 will have one more element than
;L1, so I can define length of L1 to be len, and L2 to be len+1, 
;where len=((Length of L)-1)/2 
;L1 will be consist of element in position 0 to len-1, 
;and L2 will consist of element at position len to element at last position. 
(defun SPLIT-LIST(L)
  (cond ((evenp (LENGTH L)) 
	 (let*((len (/ (LENGTH L) 2)))
	   (list(SUB-LIST L 0 len) (SUB-LIST L len len))))
	(t (let*((len (/ (- (LENGTH L) 1) 2)))
	   (list(SUB-LIST L 0 len) (SUB-LIST L len (+ len 1)))))
   )
)

;=====================================================================
;LIST2BTREE function - convert list into Binary Tree format
;argument: L, list we want to convert to Btree
;return value: return Btree that was converted from list given in the argument
;case 1: if list has only 1 element, then I can just return that element as atom
;case 2: if list has 2 elements, I can just return same list.
;case 3: if list has 3 or more elements, I can use SPLIT-LIST function to
;create two new lists and apply LIST2BTREE on two lists and put two lists into
;another list that contain two of those lists and return it. 
;Note:problem says list given in argument is non-empty so do not 
;have to consider case where (Length L)=0
(defun LIST2BTREE(L)
  (cond ((= (LENGTH L) 1) (First L))
	((= (LENGTH L) 2) L)
	(t (let*((spList (SPLIT-LIST L)))
	     (list (LIST2BTREE (FIRST spList)) (LIST2BTREE (Second spList)))
	   )
	)
   )
)

;==========================================================================
;BTREE2LIST function - convert BTree formatted list into list that contain 
;leaves in atomic format
;argument: BtList - list in binary tree format that we want to convert 
;to normal list
;return value: return list in normal list format where leaves of binary tree
;is in atomic format and in one whole list
;case 1: if only one element in BtList [e.g. BtList='(1)], return BtList
;case 2: if two atomic elements in BtList [e.g. BtList='(1 2)], return BtList
;case 3: if left node does not have child, but right one does 
;[e.g. BtList='(1 (2 3))], append first element to the list obtained by
;converting right side of tree from binary tree format to normal list format.
;This need to seperated from case 4 because first element is atomic, not list.
;case 4: if both sides of node have child [e.g. BtList='((1 2)(3 4))], I can
;convert both left and right sides of tree from binary tree format to normal
;list format then append them together to create one list. 
(defun BTREE2LIST(BtList)
  (cond ((atom BtList) (LIST BtList))
	((AND (atom (First BtList)) (atom (Second BtList))) BtList)
	((AND (atom (First BtList)) (= (Length BtList) 2)) (append (list (First BtList)) (BTREE2LIST(Second BtList))))
	(t (append (BTREE2LIST(First BtList))
			 (BTREE2LIST(Second BtList))))
  )
)