/* File: automata.pl
 * This program implements basic functionalities of automata
 */

/* Predicate: is_automaton(+Alphabet,+Trans,+Qin,+QFin)
   Description: 
	- Alphabet is a positive number X. The actual alphabet of the automaton
		becomes [1,2,...,X]
	- Trans is a list of neighborhood relations, e.g.,:
		[neighbor(Origin,[action(A,[2,3,4])]),...]
	- QIn is a list of initial states
	- QFin is list of final states

	Note that the states will be those in Trans union QIn union QFin.
 */
is_automaton(aut(Alp,Trans,QIn,QFin)) :-
	integer(Alp), Alp > 0,
	% integer(St), St >= 0,
	% integer(QIn), QIn >= 0, QIn =< St,
	% integer(QFin), 0 =< QFin, QFin =< St,
	ground(Trans),
	is_list(QIn), memberchk(_,QIn),
	is_list(QFin), memberchk(_,QFin),
	is_proper_trans(Alp,Trans). 

/* is_proper_trans(+Alp,+Trans) <-
 * 		Trans is a proper (valid) transition function over alphabet
			Alp.
 */
is_proper_trans(_,[]).
is_proper_trans(Alp,[neighbor(_,[])|SubTrans]) :-
	%integer(Source), 0 =< Source, Source =< St,
	is_proper_trans(Alp,SubTrans).
is_proper_trans(Alp,[neighbor(Source,[action(A,Targets)|Actions])|SubTrans]) :-
	%integer(Source), 0 =< Source, Source =< St,
	integer(A), 0 < A, A =< Alp,
	is_list(Targets),
	is_proper_trans(Alp,[neighbor(Source,Actions)|SubTrans]).

/* is_proper_trans(Alp,[neighbor(Source,[action(A,[_|Targets])|Actions])|
	SubTrans]) :-
	integer(Source), 0 =< Source, Source =< St,
	integer(A), 0 < A, A =< Alp,
	integer(Target), 0 =< Target, Target =< St,
	is_proper_trans(Alp,[neighbor(Source,[action(A,Targets)|Actions])|
		SubTrans]). 
*/


/* is_aut_empty(+Aut) :- 
	the language of Aut is empty.

	Comment: this can be improved by incorporating QFin in 
		is_reachable_accum.
 */
is_aut_nonempty(Aut) :-
	is_automaton(Aut),
	Aut = aut(_,Trans,QIn,QFin),
	is_reachable_accum(Trans,QIn,ReachableStates),
	intersection(QFin,ReachableStates,Intersection), 
		memberchk(_,Intersection).

/* is_reachable_accum(+Trans,+Init,-InitClosure) */
is_reachable_accum(_,[],[]).
is_reachable_accum([],Xs,Xs).
is_reachable_accum([neighbor(X,Actions)|SubTrans],Init,InitClosure) :-
	memberchk(X,Init),
	neighborhood(neighbor(X,Actions),Neighbors),
	append(Neighbors,Init,InitPrime),
	is_reachable_accum(SubTrans,InitPrime,InitClosure).

is_reachable_accum([neighbor(X,_)|SubTrans],Init,InitClosure) :-
	not(memberchk(X,Init)),
	is_reachable_accum(SubTrans,Init,InitClosure).

neighborhood(Tran,Neighbors) :-
	neighborhood_unsorted(Tran,NeighborsUnsorted),
	sort(NeighborsUnsorted,NeighborsWithSource),
	Tran = neighbor(Source,_),
	delete(NeighborsWithSource,Source,Neighbors).

neighborhood_unsorted(neighbor(_,[]),[]).
neighborhood_unsorted(neighbor(X,[action(_,[])|Actions]),Neighbors) :-
	neighborhood_unsorted(neighbor(X,Actions),Neighbors).

neighborhood_unsorted(neighbor(X,[action(A,[Y|Ys])|Actions]),[Y|Neighbors]) :-
	neighborhood_unsorted(neighbor(X,[action(A,Ys)|Actions]),Neighbors).
	

/* is_aut_member(+W,+Aut) :-
	the word W is in the language of automaton Aut
 */

