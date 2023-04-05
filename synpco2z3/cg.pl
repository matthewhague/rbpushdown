/* File: cg.pl
 * Description:
 *      This is the file that implements manipulations of context-free grammars,
 *      including:
 *      1. Translation to Z3 code.
 *      2. Eliminating unused nonterminals, terminals, and transitions.
 *      3. Basic backward reachability algorithms to check whether a nonterminal
 *         generates a nonempty language.
 *      4. Eliminating nonterminals generating empty languages.
 *
 *      Implementation issues:
 *      The implementation in this file is quite prototypical. Many basic
 *      things could still be optimized. There are quite a few usage of
 *      list data structures that could be done better with trees or 
 *      immutable arrays.
 *     
 *       cg(N,T,Trans,S) ---
 *			N is a set of nonterminals
 *			T is a set of terminals
 *			Trans is a set of transitions
 *			S is a starting nonterminal
 */

:- module(cg,[nonterminals_cg/2,terminals_cg/2,parikh_cg/2,is_cg_nonempty/1,annotate_cg/2,print_formula/4,print_init/5,remove_unreachable/2,remove_empty_nonterminals/2,annotate_formula/3,get_variables/2,isvalid_formula/2,cg_pre_accel/3,print_cg/4,substitute_term/3,join_formula/3,print_debugging/2,put_cg_transitions_rhs/4,ord_put_list/4,annotate_literal/2,print_cgsize/1]).

:- use_module(pds,[rhs_cg_compare/3]).

:- use_module(tree,[balanced_tree_init/2,treeinsert/4,balanced_tree_init_max/2]).

%nonterminals_cg(Ns,Trans) :-
%	Ns are the nonterminals in Trans.
nonterminals_cg(Ns,Trans) :-
	nonterminals_cg_accum([],Ns1,Trans),
	sort(Ns1,Ns).

nonterminals_cg_accum(Accum,Accum,[]).
nonterminals_cg_accum(Accum,Ns,[tran(n(Source),Targets)|Trans]) :-
    %write('YES NO'), nl,
	nonterminals_cg_accum([n(Source)|Accum],Ns,[tran1(Targets)|Trans]).

nonterminals_cg_accum(Accum,Ns,[tran1([])|Trans]) :-
	nonterminals_cg_accum(Accum,Ns,Trans).

nonterminals_cg_accum(Accum,Ns,[tran1([[]|Targets])|Trans]) :-
	nonterminals_cg_accum(Accum,Ns,[tran1(Targets)|Trans]).

nonterminals_cg_accum(Accum,Ns,[tran1([[(t(_),_)|RHS]
					|Targets])|Trans]) :-
	nonterminals_cg_accum(Accum,Ns,[tran1([RHS|Targets])|Trans]).
	
nonterminals_cg_accum(Accum,Ns,[tran1([[(n(Name),_)|RHS]
					|Targets])|Trans]) :-
	nonterminals_cg_accum([n(Name)|Accum],Ns,[tran1([RHS|Targets])|Trans]).


%terminals_cg(Ts,Trans) :-
%	Ts are the set of terminals in Trans.
terminals_cg(Ts,Trans) :-
	terminals_cg_accum([],Ts1,Trans),
	sort(Ts1,Ts).

terminals_cg_accum(Accum,Accum,[]).
terminals_cg_accum(Accum,Ts,[tran(n(_),Targets)|Trans]) :-
	terminals_cg_accum(Accum,Ts,[tran1(Targets)|Trans]).

terminals_cg_accum(Accum,Ts,[tran1([])|Trans]) :-
	terminals_cg_accum(Accum,Ts,Trans).

terminals_cg_accum(Accum,Ts,[tran1([[]|Targets])|Trans]) :-
	terminals_cg_accum(Accum,Ts,[tran1(Targets)|Trans]).


terminals_cg_accum(Accum,Ts,[tran1([[(t(Name),_)|RHS]
				|Targets])|Trans]) :-
	terminals_cg_accum([t(Name)|Accum],Ts,[tran1([RHS|Targets])|Trans]).
	
terminals_cg_accum(Accum,Ts,[tran1([[(n(_),_)|RHS]
				|Targets])|Trans]) :-
	terminals_cg_accum(Accum,Ts,[tran1([RHS|Targets])|Trans]).

%annotate_cg(cg(N,T,Trans,S),cg(NA,TA,TransA,SA)) :-
% 	Assume that cg(N,T,Trans,S) have passed (non)terminals_cg
annotate_cg(cg(N,T,Trans,n(Start)),cg(NA,TA,TransA,n(Start,StartKey))) :-
	annotate_literal(N,NA),
	annotate_literal_assoc(N,AssocNA),
    write('Terminals annotated'), nl,
	annotate_literal(T,TA),
	annotate_literal_assoc(T,AssocTA),
    write('Nonterminals annotated'), nl,
    %write(NA), nl,
    %write(TA), nl,
	annotate_trans(Trans,AssocNA,AssocTA,TransA),
    write('Transitions annotated'), nl,
    %write(n(Start)), nl,
    %List version
	memberchk(n(Start,StartKey),NA),
    write('StartKey is obtained'), nl.

annotate_literal_assoc(L,AssocLA) :-
    annotate_literal_new(L,LA),
    list_to_assoc(LA,AssocLA).

annotate_literal_new(L,LA) :-
	annotate_literal_count_new(L,LA,1).
	%annotate_literal_count(L,LAUnsort,1),
    %sort(LAUnsort,LA).

annotate_literal_count_new([],[],_).

annotate_literal_count_new([N|Ls],[N-Counter|LsA],Counter) :-
	Counter1 is Counter + 1,
	annotate_literal_count_new(Ls,LsA,Counter1).

	
annotate_literal(L,LA) :-
	annotate_literal_count(L,LA,1).
	%annotate_literal_count(L,LAUnsort,1),
    %sort(LAUnsort,LA).

annotate_literal_count([],[],_).

annotate_literal_count([n(Name)|Ls],[n(Name,Counter)|LsA],Counter) :-
	Counter1 is Counter + 1,
	annotate_literal_count(Ls,LsA,Counter1).

annotate_literal_count([t(Name)|Ls],[t(Name,Counter)|LsA],Counter) :-
	Counter1 is Counter + 1,
	annotate_literal_count(Ls,LsA,Counter1).


annotate_trans(Trans,NA,TA,TransA) :-
	annotate_trans_count(Trans,NA,TA,TransA,1).

annotate_trans_count([],_,_,[],_).
annotate_trans_count([tran(n(SrcName),[])|Trans],NA,TA,
		     [tran(n(SrcName,SrcNameKey),[])|TransA],Counter) :-
	%( memberchk(n(SrcName,SrcNameKey),NA) ->
	( get_assoc(n(SrcName),NA,SrcNameKey) ->
        true
    ; write('ERROR: '), write(n(SrcName,SrcNameKey)), write(' not present'), nl,
      fail
    ),
	annotate_trans_count(Trans,NA,TA,TransA,Counter).

annotate_trans_count([tran(Source,[[]|Targets])|Trans],NA,TA,
		     [tran(SourceA,[(Counter,[])|TargetsA])|TransA],Counter) :-
	Counter1 is Counter+1,
	annotate_trans_count([tran(Source,Targets)|Trans],NA,TA,
		[tran(SourceA,TargetsA)|TransA],Counter1).

annotate_trans_count([tran(Source,[[(n(Name),NumOccur)|Xs]|Targets])|Trans],NA,TA,
   [tran(SourceA,[(Counter,[(n(Name,NameKey),NumOccur)|XsA])|TargetsA])|TransA],Counter) :-
	%( memberchk(n(Name,NameKey),NA) ->
	( get_assoc(n(Name),NA,NameKey) ->
        true
    ; write('ERROR: '), write(n(Name,NameKey)), write(' not present'), nl,
      fail
    ),
	annotate_trans_count(
		[tran(Source,[Xs|Targets])|Trans],NA,TA,
		[tran(SourceA,[(Counter,XsA)|TargetsA])|TransA],Counter).
	

annotate_trans_count([tran(Source,[[(t(Name),NumOccur)|Xs]|Targets])|Trans],NA,
   TA,
   [tran(SourceA,[(Counter,[(t(Name,NameKey),NumOccur)|XsA])|TargetsA])|TransA],
   Counter) :-
	%( memberchk(t(Name,NameKey),TA) ->
	( get_assoc(t(Name),TA,NameKey) ->
        true
    ; write('ERROR: '), write(t(Name,NameKey)), write(' not present'), nl,
      fail
    ),
	annotate_trans_count(
		[tran(Source,[Xs|Targets])|Trans],NA,TA,
		[tran(SourceA,[(Counter,XsA)|TargetsA])|TransA],Counter).

% print_cgsize(+CG) <-
%   print cg size.
print_cgsize(cg(Ns,Ts,Trans,Start)) :-
    ground(Ns),
    ground(Ts),
    ground(Trans),
    ground(Start),
    write('Printing CG size now:'), nl,
    length(Ns,LenNs),
    write('Number of nonterminals: '), write(LenNs), nl,
    length(Ts,LenTs),
    write('Number of terminals: '), write(LenTs), nl,
    get_cgtranssize(Trans,TransSize),
    write('Number of transitions: '), write(TransSize), nl,
    write('End of printing CG size'), nl.

get_cgtranssize(Trans,TransSize) :-
    get_cgtranssize(Trans,0,TransSize).

get_cgtranssize([],TransSize,TransSize).
get_cgtranssize([tran(_Source,RHSs)|Trans],Accum,TransSize) :-
    length(RHSs,LenRHSs),
    NewAccum is Accum+LenRHSs,
    get_cgtranssize(Trans,NewAccum,TransSize).

% Subsequence relation for elements of RHS of cg rules.
project_subsequence([],List) :- !, is_list(List).
project_subsequence([(X,_)|Xs],[X|Ys]) :-
    !,
	project_subsequence(Xs,Ys).
project_subsequence([(X,Number)|Xs],[Y|Ys]) :-
	not(X == Y),
	project_subsequence([(X,Number)|Xs],Ys).

% ord_put_list(+El,+List,List1) <-
%	Assume that List is ordered. 
ord_put_list(El,[],[El]).
ord_put_list(El,[El|Xs],[El|Xs]).
ord_put_list(El,[X|Xs],List1) :-
	not(El == X),
	( El @< X -> List1 = [El,X|Xs]
	; List1 = [X|Ys],
	  ord_put_list(El,Xs,Ys)
	).

ord_put_list(_,El,[],[El]) :- !.
ord_put_list(_,El,[El|Xs],[El|Xs]) :- !.
ord_put_list(CompareRel,El,[X|Xs],List1) :-
	not(El == X),
	( call(CompareRel,<,El,X) -> List1 = [El,X|Xs]
	; List1 = [X|Ys],
	  ord_put_list(CompareRel,El,Xs,Ys)
	).

% put_cg_transitions_rhs(+LHS,+RHS,+Trans,-NewTrans) :-
%   NewTrans is obtained from Trans by adding 
%   to Trans a pair of LHS -> RHS transition. 
%   This is done in such a way that if LHS already appears in some
%   transition tran(LHS,ListofRHS) in Trans, then we simply
%   add RHS to ListofRHS. Otherwise, we create a new such transition
%   in NewTrans.
%   Also, we assume that the list Trans is sorted on the left-hand-side
%   nonterminal.
put_cg_transitions_rhs(LHS,RHS,[],[tran(LHS,[RHS])]) :- !.
put_cg_transitions_rhs(LHS,RHS,[tran(LHS1,ListofRHS)|Trans],NewTrans) :-
    ( LHS @< LHS1 ->
        NewTrans = [tran(LHS,[RHS]),tran(LHS1,ListofRHS)|Trans]
    ; LHS == LHS1 ->
        NewTrans = [tran(LHS,[RHS|ListofRHS])|Trans]
    ; NewTrans = [tran(LHS1,ListofRHS)|NewTrans1],
      put_cg_transitions_rhs(LHS,RHS,Trans,NewTrans1)
    ).

% remove_unreachable(+CGAnnotated,?CGAnnotated1) :-
% 	CGAnnotated1 is CGAnnotated with unreachable nonterminals, terminals,
%	and transitions removed. 
%
% The technique is simply to recursively compute post of the initial nonterminal
% and add reachable (non)terminals until the maximum set is obtained. 
% Unreachable (non)terminals and transitions are then removed.
remove_unreachable(cg(N,T,Trans,Start),cg(N1,T,Trans1,Start)) :-
	unreachable_terms(cg(N,T,Trans,Start),[Start],
				 cg(N2,T,_,Start)),
    findall(X,(member(X,N),not(member(X,N2))),N1),
	%subsequence_remainder(N2,N1,N),
	%subsequence_remainder(T2,T1,T),
	delete_transitions(N2,Trans,Trans1).

unreachable_terms(cg(N,T,Trans,Start),[],cg(N,T,Trans,Start)).

unreachable_terms(cg(N,T,Trans,Start),[NCurrent|NsCurrent],
	NewCG)
	:-
	post_ordered(Trans,[NCurrent|NsCurrent],NPost,_),
	%delete_remainder([NCurrent|NsCurrent],NPostMinus,NPost),
    findall(X,(member(X,NPost),not(member(X,[NCurrent|NsCurrent]))),NPostMinus),
	delete_transitions([NCurrent|NsCurrent],Trans,NewTrans),
	% subsequence_remainder([NCurrent|NsCurrent],NRemainder,N),
    findall(X,(member(X,N),not(member(X,[NCurrent|NsCurrent]))),NRemainder),
	unreachable_terms(cg(NRemainder,T,NewTrans,Start),
		NPostMinus,NewCG).
	

/*
unreachable_terms(cg(N,T,Trans,Start),NCurrent,[TCurrent|TsCurrent],
	NewCG) :-
	subsequence_remainder([TCurrent|TsCurrent],TRemainder,T),
	unreachable_terms(cg(N,TRemainder,Trans,Start),NCurrent,[],NewCG).
	%cg(N1,T1,Trans2,Start)).
*/

% delete_remainder(+List1,?List2,+List3) :-
%	delete List1 from List3. The remainder is List2.
%	Similar to subsequence_remainder but doesn't require that List1 be
%	a subsequence of List3.
%	It is assumed that List1 and List3 contain unique occurrences of
%	each element (and ordered).
/*
delete_remainder(_,[],[]) :- !.
delete_remainder([],List,List) :- !.
delete_remainder([X|Xs],Remainder,[X|List2]) :-
	!, delete_remainder(Xs,Remainder,List2).

delete_remainder([X|Xs],[Y|Ys],[Y|List2]) :-
	not(X == Y),
	delete_remainder([X|Xs],Ys,List2).
*/
	
% delete_transitions(+Ns,+Trans,?NewTrans) :-
%	delete Trans with left hand side Ns.
delete_transitions([],Trans,Trans).
delete_transitions([NCurrent|NsCurrent],Trans,NewTrans) :-
	delete_transition(NCurrent,Trans,NewTrans1),
	delete_transitions(NsCurrent,NewTrans1,NewTrans).

delete_transition(_,[],[]) :- !.
delete_transition(N,[tran(N,_)|Trans],NewTrans) :-
	!, delete_transition(N,Trans,NewTrans).

delete_transition(N,[tran(M,RHSs)|Trans],[tran(M,RHSs)|NewTrans]) :-
	not(N == M),
	delete_transition(N,Trans,NewTrans).

% subsequence_remainder(+List1,-List2,+List3) :-
%	List1 is a subsequence of List3, and that List2 is the remainder
%	of the elements in List3. 
%	It is assumed that List1 and List3 contain unique occurrences of
%	each elements.
subsequence_remainder([],[],[]) :- !.
subsequence_remainder([],List,List).
subsequence_remainder([X|Xs],Remainder,[X|List2]) :-
	!, subsequence_remainder(Xs,Remainder,List2).

subsequence_remainder([X|Xs],[Y|Ys],[Y|List2]) :-
	not(X == Y),
	subsequence_remainder([X|Xs],Ys,List2).
	
% Assume NCurrent are ordered according to (non)terminal_compare.
% post_ordered(+Trans,+NsCurrent,?NPost,?TPost) :-
post_ordered(Trans,NsCurrent,NPost,TPost) :-
	post(Trans,NsCurrent,NPost,TPost).
	%predsort(nonterminal_compare,NPostUnordered,NPost),
	%predsort(terminal_compare,TPostUnordered,TPost).

post(Trans,NsCurrent,NPost,TPost) :-
	post(Trans,NsCurrent,[],[],NPost,TPost).

post(_,[],NPostAccum,TPostAccum,NPostAccum,TPostAccum).

post(Trans,[NCurrent|NsCurrent],NPostAccum,TPostAccum,NPost,TPost) :-
	post_element(Trans,NCurrent,NPostAccum,TPostAccum,
		NPostAccum1,TPostAccum1),
	post(Trans,NsCurrent,NPostAccum1,TPostAccum1,NPost,TPost).
	
post_element([],_,NPostAccum,TPostAccum,NPostAccum,TPostAccum).
post_element([tran(_,[])|Trans],NCurrent,NPostAccum,TPostAccum,NPost,TPost) :-
	!,
	post_element(Trans,NCurrent,NPostAccum,TPostAccum,NPost,TPost).

post_element([tran(n(Name,Key),
		[(_,[])|RHSs]
		  )|Trans],
		n(Name,Key),NPostAccum,TPostAccum,NPost,TPost) :-
	!,
	post_element([tran(n(Name,Key),
		RHSs)|Trans],
		n(Name,Key),NPostAccum,TPostAccum,NPost,TPost).
	
post_element([tran(n(Name,Key),
		[(TranKey,[(t(TName,TKey),_)|RHS])|RHSs]
		  )|Trans],
		n(Name,Key),NPostAccum,TPostAccum,NPost,TPost) :-
	!,
    ord_put_list(terminal_compare,t(TName,TKey),TPostAccum,TPostAccum1),
	post_element([tran(n(Name,Key),
		[(TranKey,RHS)|RHSs])|Trans],
		n(Name,Key),NPostAccum,TPostAccum1,NPost,TPost).

post_element([tran(n(Name,Key),
		[(TranKey,[(n(NName,NKey),_)|RHS])|RHSs]
		  )|Trans],
		n(Name,Key),NPostAccum,TPostAccum,NPost,TPost) :-
	!,
    ord_put_list(nonterminal_compare,n(NName,NKey),NPostAccum,NPostAccum1),
	post_element([tran(n(Name,Key),
		[(TranKey,RHS)|RHSs])|Trans],
		n(Name,Key),NPostAccum1,TPostAccum,NPost,TPost).
	

post_element([tran(Source,_)|Trans],NCurrent,NPostAccum,TPostAccum,NPost,TPost) :-
	not(Source == NCurrent),
	post_element(Trans,NCurrent,NPostAccum,TPostAccum,NPost,TPost).

% nonterminal_compare(?Order,+Nonterminal1,+Nonterminal2)
%	Order is the comparison relation (<, >, =) between Nonterminal1 and 
% 	Nonterminal2. The ordering is based on the Key values.
nonterminal_compare(Order,n(_,Key1),n(_,Key2)) :- compare(Order,Key1,Key2).

% terminal_compare(?Order,+Terminal1,+Terminal2)
%	Order is the comparison relation (<, >, =) between Terminal1 and 
% 	Terminal2. The ordering is based on the Key values.
terminal_compare(Order,t(_,Key1),t(_,Key2)) :- compare(Order,Key1,Key2).

% transition_compare(?Order,+Transition1,+Transition2)
non_annotated_transition_compare(Order,tran(LHS1,_),tran(LHS2,_)) :-
    compare(Order,LHS1,LHS2).

% is_cg_nonempty(cg(+N,+T,+Trans,+S)) :-
%	grammar cg(N,T,Trans,S) is nonempty.
%	
is_cg_nonempty(cg(_,T,Trans,S)) :-
	cg_pre_star(Trans,T,PreStarT),
	memberchk(S,PreStarT).

% remove_empty_nonterminals(+CG1,-CG2) :-
%   CG2 is CG1 but with nonterminals generating empty languages removed.
%   
remove_empty_nonterminals(cg(_,T,Trans,Start),cg(N1,T,Trans1,Start)) :-
    cg_pre_star(Trans,T,PreStarT),
    findall(n(X,Y),member(n(X,Y),PreStarT),N1),
        %findall(n(X,Y),member(n(X,Y),PreStarT),N1Unsort), 
        %sort(N1Unsort,N1),
    induced_transitions(N1,Trans,Trans1),
    ( memberchk(Start,N1) ->
        true
    ; write('WARNING: remove_empty_nonterminals/3 are used with empty CG'),
      nl,
      false
    ).

% induced_transitions(+Ns,+Trans,?Trans1) :-
%   Trans1 is the set of transitions in Trans induced by Ns (i.e.
%   where the nonterminals of RHS and LHS are from Ns).
induced_transitions(Ns,Trans,Trans1) :-
    induced_transitions(Ns,Trans,[],Trans1).

induced_transitions(_,[],Trans,Trans).
induced_transitions(Ns,[tran(N,RHS)|Trans],InterTrans,FinalTrans) :-
    ( memberchk(N,Ns) ->
        findall(X,(
            member(X,RHS), 
            X = (_,TargetList), 
            %setof(n(Name,Key),Count^member((n(Name,Key),Count),TargetList),
                %TargetListWOTs),
            %findall(n(Name,Key),member((n(Name,Key),_),TargetList),
                %TargetListWOTs),
            induced_transitions_get_nonterm(TargetList,TargetListWOTs),
            %subset(TargetListWOTs,Ns)
            %setof(n(Name,Key),Count^member((n(Name,Key),Count),TargetList),
                %TargetListWOTs),
            ord_subset(TargetListWOTs,Ns)
            ),RHSinduced),
        ( RHSinduced == [] ->
            induced_transitions(Ns,Trans,InterTrans,FinalTrans)
        ; induced_transitions(Ns,Trans,[tran(N,RHSinduced)|InterTrans],
            FinalTrans)
        )
    %; ( sub_term(X,RHS), member(X,Ns), ! ->
            %induced_transitions(Ns,Trans,[tran(N,RHS)|InterTrans],FinalTrans)
      ; induced_transitions(Ns,Trans,InterTrans,FinalTrans)
    ).

% avoiding consecutive duplicates
induced_transitions_get_nonterm([],[]) :- !.
induced_transitions_get_nonterm([(t(_,_),_)|TargetList],TargetListWOTs) :-
    !, induced_transitions_get_nonterm(TargetList,TargetListWOTs).

induced_transitions_get_nonterm([(n(Name,Key),Count),(n(Name1,Key1),Count1)|
                                 TargetList],TargetListWOTs) :-
    !,
    ( (Name == Name1, Key == Key1) ->
        induced_transitions_get_nonterm([(n(Name,Key),Count)|
                                 TargetList],TargetListWOTs)
    ; TargetListWOTs = [n(Name,Key)|TargetSubListWOTs],
      induced_transitions_get_nonterm([(n(Name1,Key1),Count1)|
                                 TargetList],TargetSubListWOTs)
    ).

induced_transitions_get_nonterm([(n(Name,Key),_)|
                                 TargetList],TargetListWOTs) :-
    TargetListWOTs = [n(Name,Key)|TargetSubListWOTs],
    induced_transitions_get_nonterm(TargetList,TargetSubListWOTs).
    

%cg_pre_star(+Trans,+Set,PreSet)
% Assume that Set is ordered and have no duplicates.
% One can use cg_pre/3 instead of cg_pre_accel/3, but cg_pre_accel/3 is 
% more efficient.
cg_pre_star(Trans,Set,PreSet) :-
	cg_pre_accel(Trans,Set,PreSet1),
	length(Set,LenSet),
	length(PreSet1,LenPreSet1),
	( LenSet == LenPreSet1 ->
	  PreSet = PreSet1
	; cg_pre_star(Trans,PreSet1,PreSet)
	).
	  

% cg_pre(+Trans,+Set,PreSet) :-
%	PreSet is the one-step pre of Set wrt the cg.
%cg_pre(Trans,Set,PreSet) :-
	%cg_pre_unordered(Trans,Set,UnorderedPreSet),
	%sort(UnorderedPreSet,PreSet).

cg_pre(_,[],[]).
cg_pre([],Set,Set).
cg_pre([tran(_,[])|Trans],Set,PreSet) :-
	cg_pre(Trans,Set,PreSet).

cg_pre([tran(Source,[(_,RHS)|Targets])|Trans],Set,PreSet) :-
	( project_subsequence(RHS,Set) ->
	  cg_pre([tran(Source,Targets)|Trans],Set,PreSet1),
	  ord_put_list(Source,PreSet1,PreSet)
	; cg_pre([tran(Source,Targets)|Trans],Set,PreSet)
	).

% cg_pre_accel(+Trans,+Set,-PreSet).
% Same as cg_pre/3 but has an element of acceleration in
% that PreSet contains not just one-step pre of Set, but possibly more.
% The difference is that as the list Trans is traversed, more elements are
% added to Set. 
%
% This implementation is also more efficient than cg_pre/3 since it
% has tail-recursions.
cg_pre_accel(_,[],[]).
cg_pre_accel([],Set,Set).
cg_pre_accel([tran(_,[])|Trans],Set,PreSet) :-
	cg_pre_accel(Trans,Set,PreSet).

cg_pre_accel([tran(Source,[(_,RHS)|Targets])|Trans],Set,PreSet) :-
	( project_subsequence(RHS,Set) ->
	  ord_put_list(Source,Set,PreSet1),
	  cg_pre_accel([tran(Source,Targets)|Trans],PreSet1,PreSet)
	; cg_pre_accel([tran(Source,Targets)|Trans],Set,PreSet)
    ).

%cg_pre_unordered([tran(Source,[(_,RHS)|Targets])|Trans],Set,PreSet) :-
	%not(project_subsequence(RHS,Set)),
	
	%cg_pre([tran(Source,Targets)|Trans],Set,PreSet1).
	
%cg_pre(cg(N,T,Trans,n(Name,Key)),Set,Pre
%cg_pre(cg(N,T,Trans,n(Name,Key)),Set,PreSet) :-
	
			

% delete_terminals(+Trans1,?Trans2) <-
%	Trans2 is Trans1 with all terminals deleted.
/*
delete_terminals([],[]).
delete_terminals([tran(Source,[])|Trans],[tran(Source,[])|Trans1]) :-
	delete_terminals(Trans,Trans1).

delete_terminals([tran(Source,[(TranName,[])|Targets])|Trans],
		 [tran(Source,[(TranName,[])|Targets1])|Trans1) :-
	delete_terminals([tran(Source,Targets)|Trans],
			 [tran(Source,Targets1)|Trans1).

delete_terminals([tran(Source,
			[(TranName,[(t(_,_),_)|RHS])|Targets])
		 |Trans],
		 [tran(Source,
			[(TranName,RHS1)|Targets1])
		 |Trans1]
		) :-
	delete_terminals([tran(Source,[(TranName,RHS)|Targets])|Trans],
			 [tran(Source,[(TranName,RHS1)|Targets1])|Trans1]).

delete_terminals([tran(Source,
			[(TranName,[(n(Name,Key),Number)|RHS])|
			Targets]
		|Trans],
		 [tran(Source,
			[(TranName,[(n(Name,Key),Number)|RHS1])|
			Targets1]
		 |Trans1]) :-
	delete_terminals([tran(Source,[(TranName,RHS)|Targets])|Trans],
			 [tran(Source,[(TranName,RHS1)|Targets1])|Trans1]).

*/

%%%%%%%% 

% The variables that we need:
% x_A for each A in T --- this denotes the number of occurrences of A in
%			  the Parikh image
% y_p for each production --- this denotes the number of times p is executed
% z_A for each A in N union T --- this denotes distance in the spanning tree
%				  from starting nonterminal.
%
% Our prolog notations for variables:
% we use terms of the form var(x,Key), var(y,Key), var(z,Key)
% 
% Summation: we use list each of whose elemtns are variables of
%	     terms of form k*var(..,...) where k is an integer.

% parikh_cg(+Grammar,-Formula) <-
%	Formula is the corresponding formula encoding the parikh images of
%	the languages of the (annotated) grammar Grammar.
%
% Formula = form(First,Second,Third1,Third2)
%	    We represent arguments as ordered trees.
%parikh_cg(cg(N,T,Trans,n(Start)),Formula) :-
	%annotate_cg(cg(N,T,Trans,n(Start)),cg(NA,TA,TransA,n(Start,StartKey))),
parikh_cg(cg(NA,TA,TransA,n(Start,StartKey)),Formula) :-
    % Old version
	% length(NA,LenNA),
	% length(TA,LenTA),
	% balanced_tree_init(LenNA,TreeNA),
	% balanced_tree_init(LenTA,TreeTA),
    ( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
	     length(NA,LenNA),
	     length(TA,LenTA),
	     balanced_tree_init_max(LenNA,TreeNA),
	     balanced_tree_init_max(LenTA,TreeTA)
    ; findall(Key,member(n(_,Key),NA),NAKeysUnsort),
      findall(Key1,member(t(_,Key1),TA),TAKeysUnsort),
      sort(NAKeysUnsort,NAKeys),
      sort(TAKeysUnsort,TAKeys),
      balanced_tree_init(NAKeys,TreeNA),
      balanced_tree_init(TAKeys,TreeTA)
    ),
	TreeNA1 = TreeNA,
	TreeTA1 = TreeTA,
	parikh_cg_accum(
		cg(NA,TA,TransA,n(Start,StartKey)),
		form(TreeNA,TreeTA,TreeNA1,TreeTA1),
		Formula
		).

parikh_cg_accum(cg(_,_,[],n(_,_)),
		Accum,
		Accum
	       ) :- !.

parikh_cg_accum(cg(N,T,[tran(n(Start,StartKey),[])|
			Trans],
		  n(Start,StartKey)),
		form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),
		Formula
		) :-
	!,
	treeinsert(1,StartKey,ConjunctY,ConjunctY1),
	parikh_cg_accum(cg(N,T,Trans,n(Start,StartKey)),
		form(ConjunctY1,ConjunctX,ConjunctZN,ConjunctZT),
		Formula).

parikh_cg_accum(cg(N,T,[tran(n(Name,Key),[])|
			Trans],
		  n(Start,StartKey)),
		Accum,
		Formula
		) :-
	not(n(Name,Key) == n(Start,StartKey)),
	parikh_cg_accum(cg(N,T,Trans,n(Start,StartKey)),Accum,Formula).

/*
parikh_cg_accum(cg(N,T,[tran(n(Start,StartKey),
				[(TranKey,[])|RHS]
			    )
		       |Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),
		Formula) :-
	!,
	treeinsert(-y(TranKey),StartKey,ConjunctY,ConjunctY1),
	treeinsert(1,StartKey,ConjunctY1,ConjunctY2),
	parikh_cg_accum(cg(N,T,[tran(n(Start,StartKey),
					RHS
				    )
			       |Trans],
			   n(Start,StartKey)
			  ),
			form(ConjunctY2,ConjunctX,ConjunctZN,ConjunctZT),
			Formula).
*/
		
% when n(Name,Key) != n(Start,StartKey)
parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
				[(TranKey,[])|RHS]
			    )
		       |Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),
		Formula) :-
	% not(n(Name,Key) == n(Start,StartKey)),
	treeinsert(-y(TranKey),Key,ConjunctY,ConjunctY1),
	parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
					RHS
				    )
			       |Trans],
			   n(Start,StartKey)
			  ),
			form(ConjunctY1,ConjunctX,ConjunctZN,ConjunctZT),
			Formula).

parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
				[(TranKey,[(n(_,KeyR),NumOccur)|Targets])
				|RHS]
			    )
		       |Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),
		Formula) :-
	( NumOccur =:= 1 ->
		treeinsert(y(TranKey),KeyR,ConjunctY,ConjunctY1)
	; treeinsert(NumOccur*y(TranKey),KeyR,ConjunctY,ConjunctY1)
	),
	treeinsert((TranKey,n(Name,Key)),KeyR,ConjunctZN,ConjunctZN1),
	parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
				[(TranKey,Targets)
				|RHS]
			    )
			|Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY1,ConjunctX,ConjunctZN1,ConjunctZT),
		Formula).
	

parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
				[(TranKey,[(t(_,KeyR),NumOccur)|Targets])
				|RHS]
			    )
		       |Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),
		Formula) :-
	( NumOccur =:= 1 ->
		treeinsert(y(TranKey),KeyR,ConjunctX,ConjunctX1)
	; treeinsert(NumOccur*y(TranKey),KeyR,ConjunctX,ConjunctX1)
	),
	treeinsert((TranKey,n(Name,Key)),KeyR,ConjunctZT,ConjunctZT1),
	parikh_cg_accum(cg(N,T,[tran(n(Name,Key),
					[(TranKey,Targets)
					|RHS]
			    	    )
				|Trans],
		   n(Start,StartKey)
		  ),
		form(ConjunctY,ConjunctX1,ConjunctZN,ConjunctZT1),
		Formula).

%print_init(+N,+T,+Trans,+ExtraForm,+OS) :-
%	print the following to the opean stream OS
%	:logic QF_LIA
%	:extrafuns( list all the variable names of type Int )
print_init(N,T,Trans,ExtraFormVARs,OS) :-
	write(OS,'(benchmark pco'),nl(OS),
	write(OS,':logic QF_LIA'), nl(OS),
	write(OS,':extrafuns('), nl(OS),
	print_init_nonterm(N,OS),
	print_init_term(T,OS),
	print_init_trans(Trans,OS),
    print_init_extraform(ExtraFormVARs,OS),
	write(OS,')'),nl(OS).

print_init_nonterm([],_).
print_init_nonterm([n(_,Key)|Ns],OS) :-
	write(OS,'           ( z_n_'), write(OS,Key), write(OS,' Int )'),nl(OS),
	print_init_nonterm(Ns,OS).

print_init_term([],_).
print_init_term([t(_,Key)|Ts],OS) :-
	write(OS,'           ( z_t_'), write(OS,Key), write(OS,' Int )'),nl(OS),
	write(OS,'           ( x_t_'), write(OS,Key), write(OS,' Int )'),nl(OS),
	print_init_term(Ts,OS).

print_init_trans([],_).
print_init_trans([tran(_,[])|Trans],OS) :-
	!, print_init_trans(Trans,OS).
print_init_trans([tran(Source,[(TranKey,_)|RHS])|Trans],OS) :-
	write(OS,'           ( y_'), write(OS,TranKey), write(OS,' Int )'),nl(OS),
	print_init_trans([tran(Source,RHS)|Trans],OS).

% print_init_extraform(ExtraFormVARs,OS)
% Print the quantified variables of the extra Presburger constraint
% the printed variable will add a 'v_' prefix
print_init_extraform([],_).
print_init_extraform([Var|Vars],OS) :-
	write(OS,'           ( v_'), write(OS,Var), write(OS,' Int )'),nl(OS),
    print_init_extraform(Vars,OS).

% print_formula(cg(+N,+T,+Trans,+Start),+Formula,OS) :-
%	print the following to open stream OS
%	:formula
%	(and (<= 0 variable) .... (<= 0 variable)
%	     (all conjuncts generated from Formula)
%	)
print_formula(cg(N,T,Trans,Start),Formula,ExtraForm,OS) :-
	% print second assumption
	Formula = form(_,ConjunctX,_,_),
	print_conjunctx(ConjunctX,OS),
	% print endstart
    	( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
		synpco:internal_endstart(EndStart),
		write(OS,':assumption'), nl(OS),
		print_extraform(EndStart,OS)
	; true
	),
	% print assumption
	%write(OS,':assumption'), nl(OS),
	%write(OS,'(and '), nl(OS),
    	get_variables(ExtraForm,ExtraVars),
	print_nonneg(N,T,Trans,ExtraVars,OS),
	%write(OS,')'), nl(OS),
	%write(OS,')'), nl(OS),
	% print formula
	write(OS,':formula'), nl(OS),
	write(OS,'(and '), nl(OS),
	print_formula_main(Formula,ExtraForm,Start,OS),
    % Double )) since one ')' is used to match the initial '('
    % before 'benchmarks'
	write(OS,'))').


% print_nonneg(+N,+T,+Trans,OS) :-
%	print (<= 0 variable) .... (<= 0 variable)
print_nonneg(N,T,Trans,ExtraVars,OS) :-
	print_nonneg_nonterm(N,OS),	
	print_nonneg_term(T,OS),	
	print_nonneg_trans(Trans,OS),
    print_nonneg_extraform(ExtraVars,OS).
	

print_nonneg_nonterm([],_).
print_nonneg_nonterm([n(_,Key)|Ns],OS) :-
	write(OS,':assumption (<= 0 z_n_'), write(OS,Key), write(OS,' )'),nl(OS),
	print_nonneg_nonterm(Ns,OS).

print_nonneg_term([],_).
print_nonneg_term([t(_,Key)|Ts],OS) :-
	write(OS,':assumption (<= 0 z_t_'), write(OS,Key), write(OS,' )'),nl(OS),
	write(OS,':assumption (<= 0 x_t_'), write(OS,Key), write(OS,' )'),nl(OS),
	print_nonneg_term(Ts,OS).

print_nonneg_trans([],_).
print_nonneg_trans([tran(_,[])|Trans],OS) :-
	!, print_nonneg_trans(Trans,OS).
print_nonneg_trans([tran(Source,[(TranKey,_)|RHS])|Trans],OS) :-
	write(OS,':assumption (<= 0 y_'), write(OS,TranKey), write(OS,' )'),nl(OS),
	print_nonneg_trans([tran(Source,RHS)|Trans],OS).

print_nonneg_extraform([],_).
print_nonneg_extraform([Var|Vars],OS) :-
	write(OS,':assumption (<= 0 v_'), write(OS,Var), write(OS,' )'),nl(OS),
    print_nonneg_extraform(Vars,OS).

%print_formula_main(form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),Start,OS) :-
%	print the main formula to OS. Start is n(Name,Key) which is the 
%	initial nonterminal.
print_formula_main(form(ConjunctY,ConjunctX,ConjunctZN,ConjunctZT),ExtraForm,
        Start,OS) :-
	%print_conjunctx(ConjunctX,OS),
	print_conjuncty(ConjunctY,OS),
	print_conjunctzt1(ConjunctX,OS),
	print_conjunctzt2(ConjunctZT,Start,OS),
	print_conjunctzn(ConjunctZN,Start,OS),
    	print_extraform(ExtraForm,OS).

% Print conjunctzn
print_conjunctzn(null,_,_) :- !.
print_conjunctzn(tree((_,StartKey),Left,Right),n(StartName,StartKey),OS) 
	:-
	!,
	write(OS,'           (= 0 z_n_'), write(OS,StartKey), write(OS,')'), nl(OS),
	print_conjunctzn(Left,n(StartName,StartKey),OS),
	print_conjunctzn(Right,n(StartName,StartKey),OS).

print_conjunctzn(tree((Content,Key),Left,Right),n(StartName,StartKey),OS) :-
	not(Key == StartKey),
	write(OS,'           (or'), nl(OS),
	% print first disjunct
	write(OS,'              (and (= 0 z_n_'), write(OS,Key), write(OS,') '),
	print_zero_yp(Content,OS),
	write(OS,')'), nl(OS),
	% print second disjunct
	print_disjunct2(n,Content,Key,n(StartName,StartKey),OS),
	write(OS,'           )'), nl(OS), % closing ')' matching '(or') line 1
	% Recursions
	print_conjunctzn(Left,n(StartName,StartKey),OS),
	print_conjunctzn(Right,n(StartName,StartKey),OS).

% Print second disjunct of conjunctzn
%print_conjunctzn_disjunct2([],_,_,_).


% print_conjunctzt1(ConjunctX,OS)
print_conjunctzt1(null,_) :-
	!.

print_conjunctzt1(tree((_,Key),Left,Right),OS) :-
	write(OS,'           (or (= 0 x_t_'), write(OS,Key), write(OS,') '),
	write(OS,'(< 0 z_t_'), write(OS,Key), write(OS,') )'), nl(OS),
	print_conjunctzt1(Left,OS),
	print_conjunctzt1(Right,OS).

%print_conjunctzt2(ConjunctZT,Start,OS)
print_conjunctzt2(null,_,_) :- !.
	
print_conjunctzt2(tree((Content,Key),Left,Right),Start,OS) :-
	write(OS,'           (or'), nl(OS),
	% print first disjunct
	write(OS,'              (and (= 0 z_t_'), write(OS,Key), write(OS,') '),
	print_zero_yp(Content,OS),
	write(OS,')'), nl(OS),
	% print second disjunct
	print_disjunct2(t,Content,Key,Start,OS),
	write(OS,'           )'), nl(OS), % closing ')' matching '(or') line 1
	% Recursions
	print_conjunctzt2(Left,Start,OS),
	print_conjunctzt2(Right,Start,OS).

% print_disjunct2(Flag,Content,Key,Start,OS) :-
% 	print to OS the second disjunct of the last subitem of the last item wrt
%	Verma. Here, Flag tells us whether it is terminal (t) or nonterminal
%	(n).
%	
print_disjunct2(_,[],_,_,_).

print_disjunct2(Flag,[(TranKey,n(_,StartKey))|Ns],RightKey,
		n(StartName,StartKey),OS) :-
	!,
	write(OS,'              (and (= 1 z_'),
	write(OS,Flag), write(OS,'_'),
	write(OS,RightKey), 
		write(OS,') (< 0 y_'), write(OS,TranKey), write(OS,') )'),
	nl(OS),
	print_disjunct2(Flag,Ns,RightKey,n(StartName,StartKey),OS).
	
print_disjunct2(Flag,[(TranKey,n(_,LeftKey))|Ns],RightKey,
		n(StartName,StartKey),OS) :-
	not(LeftKey == StartKey),
	write(OS,'              (and (= z_'),
	write(OS,Flag), write(OS,'_'), write(OS,RightKey), 
		write(OS,' (+ 1 z_n_'), write(OS,LeftKey), write(OS,')) '),
	write(OS,'(< 0 y_'), write(OS,TranKey), write(OS,') '),
	write(OS,'(< 0 z_n_'), write(OS,LeftKey), write(OS,') )'), nl(OS),
	print_disjunct2(Flag,Ns,RightKey,n(StartName,StartKey),OS).
		

% Print that all y_ps equal 0.
print_zero_yp([],_).
print_zero_yp([(TranKey,_)|TranLHSs],OS) :-
	write(OS,'(= 0 y_'), write(OS,TranKey), write(OS,') '),
	print_zero_yp(TranLHSs,OS).

% print_conjunctx(ConjunctX,OS) :-
%	print conjunctx to OS.
print_conjunctx(null,_) :-
	!.

print_conjunctx(tree((Equation,Key),Left,Right),OS) :-
	print_equationx((Equation,Key),OS),
	print_conjunctx(Left,OS),
	print_conjunctx(Right,OS).

print_equationx((Equation,Key),OS) :-
	write(OS,':assumption'), 
	write(OS,'           (= x_t_'), write(OS,Key), tab(OS,1), write(OS,'(+ '),
	%write(OS,'           (= (+ '),
	print_lhs_equationy(Equation,OS), %same relationship as for printing y's
	%write(OS,') x_t_'), write(OS,Key), write(OS,')'), nl(OS).
	write(OS,'))'), nl(OS).

/*
print_equationx((Equation,Key),OS) :-
	write(OS,':assumption'), 
	write(OS,'           (= (+ '),
	print_lhs_equationy(Equation,OS), %same relationship as for printing y's
	write(OS,') x_t_'), write(OS,Key), write(OS,')'), nl(OS).
*/

% print_conjuncty(ConjunctY,OS) :-
%	print conjuncty to OS.
print_conjuncty(null,_) :- 
	!. 

print_conjuncty(tree((Equation,_),Left,Right),OS) :-
	print_equationy(Equation,OS),
	print_conjuncty(Left,OS),
	print_conjuncty(Right,OS).

print_equationy(Equation,OS) :-
	write(OS,'           (= (+ '),
	print_lhs_equationy(Equation,OS),
	write(OS,') 0)'),
	nl(OS).

print_lhs_equationy([],OS) :- 
    write(OS,'0 ').
print_lhs_equationy([y(Key)|Terms],OS) :-
    !,
	write(OS,'y_'), write(OS,Key), write(OS,' '),
	print_lhs_equationy(Terms,OS).

print_lhs_equationy([-y(Key)|Terms],OS) :-
    !,
	write(OS,'(* -1 y_'), write(OS,Key), write(OS,') '),
	print_lhs_equationy(Terms,OS).

print_lhs_equationy([NumOccur*y(Key)|Terms],OS) :-
    !,
	write(OS,'(* '), write(OS,NumOccur), write(OS,' y_'), write(OS,Key), 
		write(OS,') '),
	print_lhs_equationy(Terms,OS).

print_lhs_equationy([Term|Terms],OS) :-
    !,
	number(Term),
	write(OS,Term), write(OS,' '),
	print_lhs_equationy(Terms,OS).

% print_extraform(ExtraForm,OS) :-
%    print ExtraForm to OS. Here ExtraForm is assumed to be annotated
%    using annotate_formula/3.
print_extraform(Formula,OS) :-
    print_extraform(Formula,1,OS).

% Depth records depth of Formula in the original parse tree.
% This assumes that validity of Formula has been verified using isvalid_formula.
print_extraform(true,Depth,OS) :-
    !,
    tab(OS,4*Depth), write(OS,'true'), nl(OS).

print_extraform(Formula,Depth,OS) :-
    compound(Formula),
    functor(Formula,FormulaName,Arity),
    Arity == 2,
    !,
    Depth1 is Depth + 1,
    ( FormulaName = exist ->
        arg(2,Formula,Subformula),
        print_extraform(Subformula,Depth,OS)
    ; ( (FormulaName = and; FormulaName = or; FormulaName = imply) ->
            tab(OS,4*Depth), write(OS,'('), 
            ( FormulaName = imply ->
                write(OS,'=>')
            ; write(OS,FormulaName)
            ),
            nl(OS),
            arg(1,Formula,Form1),
            arg(2,Formula,Form2),
            print_extraform(Form1,Depth1,OS),
            print_extraform(Form2,Depth1,OS),
            tab(OS,4*Depth), write(OS,')'), nl(OS)
      )
    ; ( (FormulaName = eq; FormulaName = leq; FormulaName = geq;
         FormulaName = lt; FormulaName = gt) ->
            tab(OS,4*Depth), write(OS,'('), 
            inequation_code(FormulaName,Code), write(OS,Code), nl(OS),
            arg(1,Formula,Term1),
            arg(2,Formula,Term2),
            print_extraform_term(Term1,Depth1,OS),
            print_extraform_term(Term2,Depth1,OS),
            tab(OS,4*Depth), write(OS,')'), nl(OS)
      )
    ).

% handle the case of formula of the form and([...]) and or([...])
print_extraform(Formula,Depth,OS) :-
    compound(Formula),
    functor(Formula,FormulaName,Arity),
    Arity == 1,
    Depth1 is Depth + 1,
    ( FormulaName = and; FormulaName = or),
    tab(OS,4*Depth), write(OS,'('), write(OS,FormulaName), nl(OS),
    arg(1,Formula,ListSubForms),
    ( FormulaName = and ->
        print_extraform_and(ListSubForms,Depth1,OS)
    ; print_extraform_or(ListSubForms,Depth1,OS)
    ),
    tab(OS,4*Depth), write(OS,')'), nl(OS).

print_extraform_and([],Depth,OS) :-
    !, tab(OS,4*Depth), write(OS,'true'), nl(OS).
print_extraform_and([Form|Formulas],Depth,OS) :-
    print_extraform(Form,Depth,OS),
    print_extraform_and(Formulas,Depth,OS).
print_extraform_or([],Depth,OS) :-
    !, tab(OS,4*Depth), write(OS,'false'), nl(OS).
print_extraform_or([Form|Formulas],Depth,OS) :-
    print_extraform(Form,Depth,OS),
    print_extraform_or(Formulas,Depth,OS).
 
print_extraform_term(Term,Depth,OS) :-
    tab(OS,4*Depth), write(OS,'(+ '),
    print_extraform_termcontent(Term,OS),
    write(OS,')'), nl(OS).

print_extraform_termcontent([],OS) :-
    !, write(OS,'0 ').

print_extraform_termcontent([Literal|Literals],OS) :-
    ( number(Literal) ->
        write(OS,Literal), tab(OS,1)
    ; Literal = t(Name,Key) -> 
        write(OS,'x_t_'), write(OS,Key), tab(OS,1)
    ; Literal = Num*t(Name,Key) ->
        write(OS,'(* '), write(OS,Num), 
        write(OS,' x_t_'), write(OS,Key), write(OS,') ')
    ; Literal =  Num*Var ->
        write(OS,'(* '), write(OS,Num),
        write(OS,' v_'), write(OS,Var), write(OS,') ')
    ; write(OS,'v_'), write(OS,Literal), tab(OS,1)
    ),
    print_extraform_termcontent(Literals,OS).
        
% print_cg(CGA,ParikhInfo,ConstraintA,OS) :-
%   print to the output stream OS the annotated commutative grammar CGA with 
%   the info on the Parikh image ParikhInfo (provided by parikh_cg/2) along with
%   the extra (annotated) existential Presburger constraint ConstraintA on the 
%   Parikh image of the language of CGA.
print_cg(CGA,ParikhInfo,ConstraintA,OS) :-
    CGA = cg(NA,TA,TransA,n(_,_)),
    get_variables(ConstraintA,VARs),
	print_init(NA,TA,TransA,VARs,OS),
	print_formula(CGA,ParikhInfo,ConstraintA,OS).

% print_debugging(+CGA,+OS) :-
%   print the mapping between Names and Keys
print_debugging(cg(NA,TA,_,n(_,_)),OS) :-
    write(OS,'Printing Nonterminal Name and Key pairs:'), nl(OS),
    print_debugging_nonterminal(NA,OS), nl(OS),
    write(OS,'Printing Terminal Name and Key pairs:'), nl(OS),
    print_debugging_terminal(TA,OS), nl(OS).

print_debugging_nonterminal([],OS) :-
    !, nl(OS).

print_debugging_nonterminal([n(Name,Key)|NA],OS) :-
    tab(OS,4), write(OS,Name), tab(OS,4), write(OS,Key), nl(OS),
    print_debugging_nonterminal(NA,OS).

print_debugging_terminal([],OS) :-
    !, nl(OS).

print_debugging_terminal([t(Name,Key)|NA],OS) :-
    tab(OS,4), write(OS,Name), tab(OS,4), write(OS,Key), nl(OS),
    print_debugging_terminal(NA,OS).



%%%%%%

% Extension of the above functionalities with existential Presburger constraints
% 
% Syntax:
%   Formula -> exist(VAR,Formula)       [Exists VAR(Formula)]
%   Formula -> Qfree
%   Qfree   -> and(Qfree,Qfree)
%   Qfree   -> and([Qfree*])
%   Qfree   -> or(Qfree,Qfree)
%   Qfree   -> or([Qfree*])
%   Qfree   -> eq(TERM,TERM)
%   Qfree   -> leq(TERM,TERM)
%   Qfree   -> geq(TERM,TERM)
%   Qfree   -> lt(TERM,TERM)          [TERM<TERM]
%   Qfree   -> gt(TERM,TERM)          [TERM>TERM] 
%   TERM    -> List of literals
%   Literal -> VAR
%   Literal -> Num*VAR
%   Literal -> Num
%   Num     -> (number)
%   VAR     -> Term with arity 0 or with arity 1 of the form t(Name), where
%              Name is of Arity 0.
%
% Checking validity:
% Make sure that variables have been existentially quantified, OR that
% they are of the form t(Name), which is a terminal name of a given commutative
% grammar.
%

%%%%%%

% Extension of the above functionalities with existential Presburger constraints
% 
% Syntax:
%   Formula -> exist(VAR,Formula)       [Exists VAR(Formula)]
%   Formula -> Qfree
%   Qfree   -> true
%   Qfree   -> and(Qfree,Qfree)
%   Qfree   -> and([Qfree*])
%   Qfree   -> or(Qfree,Qfree)
%   Qfree   -> or([Qfree*])
%   Qfree   -> imply(Qfree,Qfree)
%   Qfree   -> eq(TERM,TERM)
%   Qfree   -> leq(TERM,TERM)
%   Qfree   -> geq(TERM,TERM)
%   Qfree   -> lt(TERM,TERM)          [TERM<TERM]
%   Qfree   -> gt(TERM,TERM)          [TERM>TERM] 
%   TERM    -> List of literals
%   Literal -> VAR
%   Literal -> Num*VAR
%   Literal -> Num
%   Num     -> (number)
%   VAR     -> Term with arity 0 or with arity 1 of the form t(Name), where
%              Name is of Arity 0.
%
% Checking validity:
% Make sure that variables have been existentially quantified, OR that
% they are of the form t(Name), which is a terminal name of a given commutative
% grammar.
%
% Note: empty list of literals is equated with 0.


% isvalid_formula(+Formula,+Context) :-
%   Formula is a valid formula with respect to Context, which is a pair
%   context(TAs,VARs) of annotated terminals TAs and variables VARs.
isvalid_formula(Formula,context(TAs,VARs)) :-
    ( Formula = exist(VAR,Subformula) ->
        isvalid_formula(Subformula,context(TAs,[VAR|VARs]))
    ; isvalid_qfree(Formula,context(TAs,VARs))
    ).

% isvalid_qfree(+Formula,+Context) :-
%   Formula is a valid positive quantifier-free formula with respect to 
%   Context (see isvalid_formula/2).
isvalid_qfree(true,_) :- !.
isvalid_qfree(eq(TERM1,TERM2),Context) :-
    !,
    isvalid_term(TERM1,Context),
    isvalid_term(TERM2,Context).

isvalid_qfree(leq(TERM1,TERM2),Context) :-
    !,
    isvalid_term(TERM1,Context),
    isvalid_term(TERM2,Context).

isvalid_qfree(geq(TERM1,TERM2),Context) :-
    !,
    isvalid_term(TERM1,Context),
    isvalid_term(TERM2,Context).

isvalid_qfree(gt(TERM1,TERM2),Context) :-
    !,
    isvalid_term(TERM1,Context),
    isvalid_term(TERM2,Context).

isvalid_qfree(lt(TERM1,TERM2),Context) :-
    !,
    isvalid_term(TERM1,Context),
    isvalid_term(TERM2,Context).

isvalid_qfree(imply(Form1,Form2),Context) :-
    !,
    isvalid_qfree(Form1,Context),
    isvalid_qfree(Form2,Context).

isvalid_qfree(and(Form1,Form2),Context) :-
    !,
    isvalid_qfree(Form1,Context),
    isvalid_qfree(Form2,Context).

isvalid_qfree(and([]),_).
isvalid_qfree(and([Form|Formulas]),Context) :-
    !,
    isvalid_qfree(Form,Context),
    isvalid_qfree(and(Formulas),Context).

isvalid_qfree(or(Form1,Form2),Context) :-
    !,
    isvalid_qfree(Form1,Context),
    isvalid_qfree(Form2,Context).

isvalid_qfree(or([]),_).
isvalid_qfree(or([Form|Formulas]),Context) :-
    !,
    isvalid_qfree(Form,Context),
    isvalid_qfree(or(Formulas),Context).

% isvalid_term(+Term,Context) :-
%   Term is a valid term with respect to Context.
isvalid_term([],_).
isvalid_term([Num|Subterm],Context) :-
    number(Num),
    !,
    isvalid_term(Subterm,Context).
    
isvalid_term([Num*Literal|Subterm],context(TAs,VARs)) :-
    !,
    number(Num),
    isvalid_term_varhead([Literal|Subterm],context(TAs,VARs)).

isvalid_term([Literal|Subterm],context(TAs,VARs)) :-
    isvalid_term_varhead([Literal|Subterm],context(TAs,VARs)).

% isvalid_term_varhead([Literal|Subterm],context(TAs,VARs)) :-
%   same as isvalid_term/2 but Term is nonempty and Literal
%   is a variable (i.e. cannot be of the form Num*Literal1 and Num, where
%   Num is a number).
isvalid_term_varhead([Literal|Subterm],context(TAs,VARs)) :-
    functor(Literal,_,Arity),
    ( Arity == 0 ->
        ( memberchk(Literal,VARs) -> true
        ; write('ERR: isvalid_term_varhead: Literal not in VARs'), nl, fail
        )
    ; ( Literal = t(Name) -> true
      ; write('ERR: isvalid_term_varhead: Literal of rubbish form'), nl,fail
      ),
      %functor(Name,_,0),
      ( memberchk(t(Name,_),TAs) -> true
      ; write('ERR: isvalid_term_varhead: t(Name,_) not in TAs'), 
        write('Name is '), write(Name), nl,fail
      )
    ),
    isvalid_term(Subterm,context(TAs,VARs)).

% It is assumed that no variables in Formula2 occur in Formula 1
% join_formula(+Formula1,+Formula2,-NewFormula).
join_formula(Formula1,exist(Var,Subform),exist(Var,NewSubformula)) :-
        !, join_formula(Formula1,Subform,NewSubformula).

join_formula(exist(Var,Subform1),Formula2,exist(Var,NewSubformula)) :-
        !, join_formula(Subform1,Formula2,NewSubformula).

join_formula(Formula1,Formula2,and(Formula1,Formula2)).


% get_variables(+Formula,-Variables) :-
%   Variables are the quantified variables in the formula Formula.
get_variables(Formula,Variables) :-
    get_variables(Formula,[],Variables).

get_variables(Formula,Variables,Variables) :-
    not(Formula = exist(_,_)),
    !.

get_variables(exist(Var,Formula),VariablesAccum,Variables) :-
    get_variables(Formula,[Var|VariablesAccum],Variables).


% annotate_formula(+Formula,+TerminalsA,-AnnotatedFormula) :-
%   AnnotatedFormula is the annotated version of Formula with respect to
%   annotated terminals TerminalsA.
annotate_formula(Formula,TAs,AnnotatedFormula) :-
    findall(t(Name,Key)/t(Name),member(t(Name,Key),TAs),Substitution),
    substitute_term(Formula,Substitution,AnnotatedFormula).

% Term substitutions
% substitute_term(+Term,+Substitution,?NewTerm) :-
%   NewTerm is Term after Substitution is applied.
% e.g.
% Substitution = [t(a,1)/t(a),t(b,3)/t(b)]
% ? - substitute_term(blah(blah(t(a),t(b))),Substitution,X)
% X = blah(blah(t(a,1),t(b,3))).            
%
% Note: this is similar to the code from Stirling's Art of Prolog Chap 9.
substitute_term(Term,Substitution,NewTerm) :-
    memberchk(NewTerm/Term,Substitution),
    !.

substitute_term(Term,_,Term) :-
    \+ compound(Term),
    !.

substitute_term(Term,Substitution,NewTerm) :-
    compound(Term),
    functor(Term,Name,Arity),
    functor(NewTerm,Name,Arity),
    substitute_term(Arity,Term,Substitution,NewTerm).

substitute_term(N,Term,Substitution,NewTerm) :-
    number(N), N > 0,
    !,
    arg(N,Term,NthSubterm),
    substitute_term(NthSubterm,Substitution,NewNthSubterm),
    arg(N,NewTerm,NewNthSubterm),
    N1 is N -1,
    substitute_term(N1,Term,Substitution,NewTerm).

substitute_term(0,_,_,_).

% inequation_code(?Ineq,?Code) :-
%   Code is the code (i.e. string) corresponding to the inequation Ineq.
inequation_code(eq,'=').
inequation_code(leq,'<=').
inequation_code(geq,'>=').
inequation_code(lt,'<').
inequation_code(gt,'>').
