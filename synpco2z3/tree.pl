% File: tree.pl
% This file deals with static trees in the sense that the structure of the
% tree is once defined cannot be changed.

:- module(cg_treelib,[balanced_tree/2,balanced_tree_init/2,is_tree/1,treeinsert/4,treeheight/2,balanced_tree_init_max/2]).

% treememberchk((?Name,+Key),+Tree) :-
%	(Name,Key) is a node in the tree Tree.
%
% Assumption: Tree is an *ordered* tree (ordered by keys which are integers)
treememberchk((Name,Key),tree((Name,Key),_,_)) :- !.
treememberchk((Name,Key),tree((_,Key1),Left,Right)) :-
	not(Key == Key1),
	( Key < Key1
	-> ( Left == null ->
		fail
	   ; treememberchk((Name,Key),Left)
	   )
	; (Right == null ->
		fail
	  ; treememberchk((Name,Key),Right)
	  )
	).
	
% balanced_tree(MaxKey,Tree) :-
%	Tree is a balanced ordered tree with maximum key MaxKey
balanced_tree(MaxKey,Tree) :-
	integer(MaxKey),
	MaxKey > 0,
	StrictMax is MaxKey+1,
	StrictMin is 0,
	balanced_tree_min_max(StrictMin,StrictMax,Tree).

balanced_tree_min_max(StrictMin,StrictMax,tree((_,Key),null,null)) :-
	StrictMin =:= StrictMax-2, 
	!,
	Key is StrictMax-1.
balanced_tree_min_max(StrictMin,StrictMax,null) :-
	StrictMin > StrictMax-2,
	!.

balanced_tree_min_max(StrictMin,StrictMax,tree((_,Key),Left,Right)) :-
	StrictMin < StrictMax - 2,
	Key is StrictMin+floor((StrictMax-StrictMin)/2),
	balanced_tree_min_max(StrictMin,Key,Left),
	balanced_tree_min_max(Key,StrictMax,Right).

% balanced_tree_init_max(MaxKey,Tree) :-
%	Tree is a balanced ordered tree with maximum key MaxKey where
%	contents are initialized to emptyset.
balanced_tree_init_max(MaxKey,Tree) :-
	integer(MaxKey),
	MaxKey > 0,
	StrictMax is MaxKey+1,
	StrictMin is 0,
	balanced_tree_init_min_max(StrictMin,StrictMax,Tree).

balanced_tree_init_min_max(StrictMin,StrictMax,tree(([],Key),null,null)) :-
	StrictMin =:= StrictMax-2, 
	!,
	Key is StrictMax-1.
balanced_tree_init_min_max(StrictMin,StrictMax,null) :-
	StrictMin > StrictMax-2,
	!.

balanced_tree_init_min_max(StrictMin,StrictMax,tree(([],Key),Left,Right)) :-
	StrictMin < StrictMax - 2,
	Key is StrictMin+floor((StrictMax-StrictMin)/2),
	balanced_tree_init_min_max(StrictMin,Key,Left),
	balanced_tree_init_min_max(Key,StrictMax,Right).

% KeyList is assumed to be sorted.
balanced_tree_init(KeyList,Tree) :-
    length(KeyList,ListLen),
    balanced_tree_init(ListLen,KeyList,Tree).
    % Each X in KeyList is a number
    % not(member(X,KeyList),not(number(X))),

	%MaxKey > 0,
	%StrictMax is MaxKey+1,
	%StrictMin is 0,
	%balanced_tree_init_min_max(StrictMin,StrictMax,Tree).

balanced_tree_init(0,[],null) :- !.
balanced_tree_init(K,KeyList,tree(([],Key),Left,Right)) :-
    K > 0,
    Khalf is K/2,
    % case analysis: depending on whether K is even or odd
    ( integer(Khalf) ->
        LenList1 is Khalf-1,
        LenList2 is Khalf
    ; LenList1 is (K-1)/2,
      LenList2 is (K-1)/2
    ),
    (append(List1,[Key|List2],KeyList),
    length(List1,LenList1),!),
    balanced_tree_init(LenList1,List1,Left),
    balanced_tree_init(LenList2,List2,Right).
         

% is_tree(+Tree) :-
%	Tree is an ordered tree.

is_tree(Tree) :-
	is_tree_bound(1,inf,Tree).

is_tree_bound(_,_,null) :- !.
is_tree_bound(Low,High,tree((_,Key),Left,Right)) :-
	between(Low,High,Key),
	NewHigh is Key-1,
	is_tree_bound(Low,NewHigh,Left),
	NewLow is Key+1,
	is_tree_bound(NewLow,High,Right).
	

% treeinsert(+El,+Key,+Tree,?Tree1)
%	Tree1 is Tree with element El inserted under key Key.
% Assumption: Tree has a node with key Key. Otherwise, failure.
treeinsert(El,Key,
	tree((Els,Key),Left,Right),
	tree(([El|Els],Key),Left,Right)
	) :- !.

treeinsert(El,Key1,
		tree((Els,Key),Left,Right),
		tree((Els,Key),Left1,Right1)
	  ) :-
	not(Key1 == Key),
	( Key1 < Key ->
		Right1 = Right,
		treeinsert(El,Key1,Left,Left1)
	; Left1 = Left,
	  treeinsert(El,Key1,Right,Right1)
	).

% treeheight(+Tree,?Height) :-
%	Height is the height of tree Tree.
treeheight(null,0).
treeheight(tree(_,Left,Right),Height) :-
	treeheight(Left,Height1),
	treeheight(Right,Height2),
	Height is max(Height1,Height2)+1.
