% synpco.pl
% 
% author: Anthony Widjaja Lin (antto@cs.ox.ac.uk)
%
% Implements translation from synpco to Presburger
%
% The syntax of synpco is as follows:
%
%   synpco(NumThreads,List_of_PCos,GlobalTrans,GlobalInputSym,VarNames)
%
% where:
% 1. NumThreads is the number of threads,
% 2. VarNames is a list of variable names used. Each variable is just a string.
% 3. List_of_PCos is a list of PCos. A PCo is of the form
%    
%       pco(States,StackSym,InputSym,Trans,(Q0,sym(Z_0)),Qacc)
%    where everything, except for Trans, is the same as the case of
%    pda(...). See pds.pl. Trans is a list of transitions given as follows:
%    
%       tran(from(P,Word1,CounterTest),Actions,to(Q,Word2,CounterActions)),
%
%    where everything except for CounterTest and CounterActions are the
%    same as in the case of pda(...). See pds.pl. CounterTest is given by
%    the following grammar:
%    
%    CounterTest -> and(CounterTest,CounterTest)
%    CounterTest -> or(CounterTest,CounterTest)
%    CounterTest -> eq(VAR,Number)
%    CounterTest -> leq(VAR,Number)
%    CounterTest -> geq(VAR,Number)
%    CounterTest -> lt(VAR,Number)
%    CounterTest -> gt(VAR,Number)
%
%    CounterActions is a list of elements of the form (Var,Num) where Var is
%    an element of VarNames and Num is an integer meaning that Var is to be
%    incremented by Num.
%   
% 4. GlobalInputSym is a list of global action names (have to be distinct from
%    other action names)
% 5. GlobalTrans is a list of global transitions of the form
%
%    tran(from(states(P_1,...,P_r),CounterTest),Actions,
%                                to(states(Q_1,...,Q_r),CounterActions)
%
%    The semantics is given in the paper
%
%
% Further change:
% 
% special local synpco rule is introduced of the form:
%
% spectran(P,StackSymSubset,CtrTestActionSeqsCtrIncr,Q)
%
% where:
% 1. P and Q are two states,
% 2. StackSymSubset is a subset of StackSym
% 3. CtrTestActionsCtrIncr is a list of triples
%    
%     (CtrTest,Actions,CtrIncr)
%    
%    where CtrTest is a counter test, Actions is a list of actions,
%    and CtrIncr is a list of counter increments.
%
% Special rules of this form give rise to *less* CFG rules when optimised.


:- module(synpco,[isvalid_synpco/1,synpco_example1/1,synpco_example2/1,cbreach_synpco_to_asynpda/4,cbreach_synpco_to_z3/5,cbreach_synpco_to_z3_new/5,synpco_to_asynpda_state_compare/3,get_closed_interval/3,synpco_conswitch_to_counter/6,cbreach_synpco_to_z3_new2/5]).

%:- use_module([library(clpfd)]).

:- use_module(pds,[atom_sub_atom/3,get_states_from_transitions/2,get_actions_from_transitions/2,asynpda_to_cg/2]).

:- use_module(cg,[substitute_term/3,print_cg/4,annotate_formula/3,parikh_cg/2,remove_empty_nonterminals/2,annotate_cg/2,isvalid_formula/2,join_formula/3,remove_unreachable/2,print_debugging/2,annotate_literal/2,print_cgsize/1]).

% isvalid_countertest(+Formula,+CounterVars) :-
%   Formula is a valid countertest over the counter variables CounterVars.
isvalid_countertest(true,_).
isvalid_countertest(eq([Var],[Number]),CounterVars) :-
    integer(Number),
    memberchk(Var,CounterVars).

isvalid_countertest(leq([Var],[Number]),CounterVars) :-
    integer(Number),
    memberchk(Var,CounterVars).

isvalid_countertest(geq([Var],[Number]),CounterVars) :-
    integer(Number),
    memberchk(Var,CounterVars).

isvalid_countertest(gt([Var],[Number]),CounterVars) :-
    integer(Number),
    memberchk(Var,CounterVars).

isvalid_countertest(lt([Var],[Number]),CounterVars) :-
    integer(Number),
    memberchk(Var,CounterVars).

isvalid_countertest(and(Test1,Test2),CounterVars) :-
    isvalid_countertest(Test1,CounterVars),
    isvalid_countertest(Test2,CounterVars).
    
isvalid_countertest(or(Test1,Test2),CounterVars) :-
    isvalid_countertest(Test1,CounterVars),
    isvalid_countertest(Test2,CounterVars).

% get_constants(+Formula/ListofTransitions/ListofPCos/SynPCo,
%                                       +CounterVar,+Constants) :-
%   Constants are the list of constants that are compared to counter variables
%   CounterVar in Formula or ListofTransitions or ListofPCos or SynPCo. 
%   Constants appear in a sorted order.
get_constants(Term,CounterVar,Constants) :-
    get_constants(Term,CounterVar,[],ConstantsUnsort),
    sort(ConstantsUnsort,Constants).

% accumulator version:
% get_constants(+Formula/Transition/ListofTransitions,+CounterVar,
%                   +ConstantsAccum,-Constants).
% Warn: Constants might have multiple copies of the same constants.
get_constants(true,_,ConAccum,ConAccum).
get_constants(eq([Var],[Number]),Var1,ConAccum,Constants) :-
    ( Var == Var1 -> Constants = [Number|ConAccum]
    ; Constants = ConAccum
    ).
get_constants(leq([Var],[Number]),Var1,ConAccum,Constants) :-
    ( Var == Var1 -> Constants = [Number|ConAccum]
    ; Constants = ConAccum
    ).
get_constants(geq([Var],[Number]),Var1,ConAccum,Constants) :-
    ( Var == Var1 -> Constants = [Number|ConAccum]
    ; Constants = ConAccum
    ).
get_constants(lt([Var],[Number]),Var1,ConAccum,Constants) :-
    ( Var == Var1 -> Constants = [Number|ConAccum]
    ; Constants = ConAccum
    ).
get_constants(gt([Var],[Number]),Var1,ConAccum,Constants) :-
    ( Var == Var1 -> Constants = [Number|ConAccum]
    ; Constants = ConAccum
    ).
        
get_constants(and(Form1,Form2),Var,ConAccum,Constants) :-
    !,
    get_constants(Form1,Var,ConAccum,ConAccum1),
    get_constants(Form2,Var,ConAccum1,Constants).

get_constants(or(Form1,Form2),Var,ConAccum,Constants) :-
    !,
    get_constants(Form1,Var,ConAccum,ConAccum1),
    get_constants(Form2,Var,ConAccum1,Constants).

get_constants([],_,ConAccum,ConAccum).
% Local transitions
get_constants([
    tran(from(_,_,CounterTest),_,to(_,_,_))|
    Trans],
    Var,
    ConAccum,
    Constants) :-
        !,
        get_constants(CounterTest,Var,ConAccum,ConAccum1),
        get_constants(Trans,Var,ConAccum1,Constants).

% Global transitions
get_constants([
    tran(from(_,CounterTest),_,to(_,_))|
    Trans],
    Var,
    ConAccum,
    Constants) :-
        !,
        get_constants(CounterTest,Var,ConAccum,ConAccum1),
        get_constants(Trans,Var,ConAccum1,Constants).

% Special transitions
get_constants([spectran(_,_,[],_)|Trans],Var,ConAccum,Constants) :-
    !,
    get_constants(Trans,Var,ConAccum,Constants).
get_constants([
    spectran(P,Syms,[(Test,_,_)|Triplets],Q)|Trans],Var,ConAccum,Constants) :-
     !,
     get_constants(Test,Var,ConAccum,ConAccum1),
     get_constants([spectran(P,Syms,Triplets,Q)|Trans],Var,ConAccum1,Constants).



get_constants([pco(_,_,_,Trans,_,_)|PCos],Var,ConAccum,Constants) :-
    !,
    get_constants(Trans,Var,ConAccum,ConAccum1),
    get_constants(PCos,Var,ConAccum1,Constants).

get_constants(synpco(_,ListofPCos,GlobalTrans,_,_),Var,ConAccum,Constants) :-
    !,
    get_constants(ListofPCos,Var,ConAccum,ConAccum1),
    get_constants(GlobalTrans,Var,ConAccum1,Constants).

% getcount_modevec(+SynPCo,+RevBound,-Count) :-
%   Count is an upper bound for the number of mode vectors needed for
%   synchronized PCo SynPCo with RevBound number of reversals.
%
%   Count = sum of (# regions for counter i) x (RevBound+1) x 
%                       (number of variables)
%         = (total # regions for all counters) x (RevBound+1) x (num of vars)
getcount_modevec(SynPCo,RevBound,Count) :-
    getcount_totalregions(SynPCo,0,RegionCount),
    SynPCo = synpco(_,_,_,_,Vars),
    length(Vars,NumVars),
    Count is RegionCount*(RevBound+1)*NumVars.

getcount_totalregions(synpco(_,_,_,_,[]),CountAccum,CountAccum).

getcount_totalregions(
    synpco(NumThreads,ListoPCos,GlobalTrans,GlobalInputSym,[Var|Vars]),
    CountAccum,Count) :-
        get_constants(
            synpco(NumThreads,ListoPCos,GlobalTrans,GlobalInputSym,[Var|Vars]),
            Var,Constants),
        length(Constants,LenConstants),
        % Depending on whether 0 is in Constants, the number of regions
        % could differ by 1.
        ( member(0,Constants) ->
            CountAccumNew is CountAccum+(2*LenConstants)
        ; CountAccumNew is CountAccum+(2*LenConstants)+1
        ),
        getcount_totalregions(
            synpco(NumThreads,ListoPCos,GlobalTrans,GlobalInputSym,Vars),
            CountAccumNew,
            Count).

% get_regions(+Boundaries,+Var,+Regions) :-
%
% Boundaries are a sorted list of numbers
get_regions(Boundaries,Var,Regions) :-
    is_list(Boundaries),
    get_regions(Boundaries,0,Var,Regions).

% LowBound is a non-strict lower bound
get_regions([],LowBound,Var,[geq([Var],[LowBound])]).
get_regions([0|Boundaries],_,Var,[eq([Var],[0])|Regions]) :-
    !,
    get_regions(Boundaries,1,Var,Regions).

get_regions([Boundary|Boundaries],LowBound,Var,Regions) :-
    Boundary > 0,
    LowBound =< Boundary,
    ( LowBound =:= Boundary ->
        Regions = [eq([Var],[Boundary])|RestRegions]
    ; Regions = [and(geq([Var],[LowBound]),lt([Var],[Boundary])),
                 eq([Var],[Boundary])|RestRegions]
    ),
    NewLowBound is Boundary+1,
    get_regions(Boundaries,NewLowBound,Var,RestRegions).


% get_modevecs(+SynPCo,+RevBound,+Var,-ModeVecs) :-
%   ModeVecs is a list of mode vectors with respect to SynPCo and RevBound
%   number of reversals. 
/*
get_modevecs(SynPCo,RevBound,Var,ModeVecs) :-
    findall(X,(X #>= 0, X #=< RevBound, indomain(X)),ListRev),
    SynPCo = synpco(_,ListofPCos,GlobalTrans,_,VarNames),
    member(Var,VarNames),
    get_constants(SynPCo,Var,Constants),
    
    Mode = mode(
*/

% interior(+Interior,+SortedList,+LowBound) :-
%   Interior is an interior region of SortedList that is bounded below
%   by LowBound.
/*
interior(geq(LowBound),[],LowBound).
interior(Interior,[X|Xs],LowBound) :-
    not( X =:= LowBound ),
    Interior = (and( 
*/

% isvalid_synpco(+SynPCo) :-
%   SynPCo is a valid SynPCo. It is of form 
%   synpco(NumThreads,List_of_PCos,GlobalTrans,GlobalInputSym,VarNames)
%   where the requirements written above are satisfied.
isvalid_synpco(
    synpco(NumThreads,List_of_PCos,GlobalTrans,GlobalInputSym,VarNames)) :-
    	( current_predicate(synpco_debug,synpco_debug) ->
            write('In isvalid_synpco'), nl
        ; true
        ),
        ( length(List_of_PCos,NumThreads) -> true
	; write('ERROR: isvalid_synpco List of PCos and number of threads not agree'), nl, fail 
	),
        not((member(PCo,List_of_PCos),not(isvalid_pco(PCo,VarNames)))),
        isvalid_globaltrans(GlobalTrans,GlobalInputSym,List_of_PCos,
                NumThreads,VarNames),
    	( current_predicate(synpco_debug,synpco_debug) ->
	        write('Out of isvalid_synpco'), nl
        ; true
        ).
        %agree_local_globaltrans(List_of_PCos,GlobalTrans).

% isvalid_pco(+PCo,+VarNames) :-
%   PCo is a valid PCo over variable names VarNames.
isvalid_pco(pco(States,StackSym,InputSym,Trans,(Q0,sym(Z_0)),Qacc),
    VarNames) :-
    	( current_predicate(synpco_debug,synpco_debug) ->
    	    write('In isvalid_pco'), nl
        ; true
        ),
        ( is_list(InputSym) -> true
	; write('ERROR: is_list(InputSym) failed in isvalid_pco'), nl, fail
	),
        ( not((member(Y,InputSym),not(Y = t(_)))) -> true
	; write('ERROR: not((member(Y,InputSym),...) fails at isvalid_pco'), nl, fail
	),
        ( is_list(StackSym) -> true
	; write('ERROR: is_list(StackSym) failed in isvalid_pco'), nl, fail
	),
        ( not((member(Z,StackSym),not(Z = sym(_)))) -> true
	; write('ERROR: not((member(Z,StackSym),...)) failed in isvalid_pco'), nl, fail
	),
        ( is_list(States) -> true
	; write('ERROR: is_list(States) failed in isvalid_pco'), nl, fail
	),
        ( not((member(X,States),compound(X))) -> true
	; write('ERROR: not((member(X,States),..) failed in isvalid_pco'), nl, fail
	),
        ( memberchk(Q0,States) -> true
	; write('ERROR: memberchk(Q0,States) failed in isvalid_pco'), nl, fail
	),
        (memberchk(sym(Z_0),StackSym) -> true
	; write('ERROR: memberchk(sym(Z_0),StackSym) failed in isvalid_pco'), nl,
	fail),
        ( memberchk(Qacc,States) -> true
	; write('ERROR: memberchk(Qacc,States) failed in isvalid_pco'), nl, fail
	),
        % Checking transitions
        ( is_list(Trans) -> true
	; write('ERROR: is_list(Trans) failed in isvalid_pco'), nl, fail
	),
        ( not((member(X,Trans),
            not( ((X = tran(from(P,Word1,CounterTest),Actions,
                                        to(Q,Word2,CounterActions)),
                ( memberchk(P,States) -> true
		; write('ERROR: P is not in States'), nl, fail
		),
                ( memberchk(Q,States) -> true
		; write('ERROR: Q is not in States'), nl, fail
		),
                ( isvalid_stackactions_pair(Word1,Word2,StackSym) -> true
		; write('ERROR: isvalid_stackactions_pair'), nl, fail
		),
                ( subset(Actions,InputSym) -> true
		; write('ERROR: Actions not subset of InputSym'), nl, fail
		),
                % Checking counter tests and counter actions
                ( isvalid_countertest(CounterTest,VarNames) -> true
		; write('ERROR: invalid countertest'), nl, 
		  write('Counter test is '), write(CounterTest), nl,
		  fail
		),
                ( is_list(CounterActions) -> true
		; write('ERROR: CounterActions not list'), nl, fail
		),
                not((member(Y,CounterActions),not((
                                Y = (Var,Num),
                                memberchk(Var,VarNames),
                                integer(Num)
                                ))
                     )
                    )
                %functor(CounterActions,_,LenVarNames)
               )
               ;
               (X = spectran(P,StackSymSubset,CtrTestActsCtrIncrs,Q),
                ( memberchk(P,States) ->
                    true
                ; write('ERROR: P not in States'), nl, fail
                ),
                ( memberchk(Q,States) ->
                    true
                ; write('ERROR: Q not in States'), nl, fail
                ),
                ( subset(StackSymSubset,StackSym) ->
                    true
                ; write('ERROR; StackSymSubset not in StackSym'), nl, fail
                ),
                forall(member(Triplet,CtrTestActsCtrIncrs),
                       (Triplet = (CtrTest,Acts,CtrIncrs),
                        ( isvalid_countertest(CtrTest,VarNames) -> true
                        ; write('ERR: invalid counter test'), nl,fail
                        ),
                        ( subset(Acts,InputSym) ->
                            true
                        ; write('ERROR: Acts not subset of InputSym'), nl,fail
                        ),
                        is_list(CtrIncrs),
                        not((member(Y,CtrIncrs),not((
                                Y = (Var,Num),
                                ( memberchk(Var,VarNames) -> true
                                ; write('ERR: Var not in VarNames'), nl,fail
                                ),
                                integer(Num)
                                ))
                            ))
                        ))
                )
                ))
            %write(X), nl
            )) -> true
	; write('ERROR: Last big check failed in isvalid_pco'), nl, fail
	),
    	( current_predicate(synpco_debug,synpco_debug) ->
    	    write('Out of isvalid_pco'), nl
        ; true
        ).


% isvalid_stackactions_pair(Word1,Word2,StackSym) :-
%      (Word1,Word2) is a valid pair of stack actions (i.e. that are located
%      in PCo transitions). 
%
% This is the new style of PCo transitions (a la Matt's).
isvalid_stackactions_pair(Word1,Word2,StackSym) :-
                Word1 = [sym(X)], memberchk(sym(X),StackSym),
                (Word2 = []
                ;Word2 = [sym(Y)], memberchk(sym(Y),StackSym)
                ;Word2 = [sym(Y1),sym(Y2)], memberchk(sym(Y1),StackSym),
                memberchk(sym(Y2),StackSym)
                ).


% isvalid_globaltrans(+GlobalTrans,+GlobalInputSym,+ListofPCo,+NumThreads,
%           +VarNames) :-
%   GlobalTrans are valid global transitions with NumThreads number of
%   threads and over variable names VarNames.
isvalid_globaltrans(GlobalTrans,GlobalInputSym,ListofPCo,NumThreads,VarNames) :-
    ( current_predicate(synpco_debug,synpco_debug) ->
        write('In isvalid_globaltrans'), nl
    ; true
    ),
    is_list(GlobalTrans),
    integer(NumThreads),
    is_list(VarNames),
    is_list(ListofPCo),
    ( not((member(X,GlobalTrans),
         not((X = tran(from(StateTuple1,CounterTest),Actions,
                                to(StateTuple2,CounterActions)),
              isvalid_statetuple(StateTuple1,ListofPCo,NumThreads),
              isvalid_statetuple(StateTuple2,ListofPCo,NumThreads),
              %isvalid_globaltrans_formula(Formula1,ListofPCo),
              %isvalid_globaltrans_formula(Formula2,ListofPCo),
              subset(Actions,GlobalInputSym),
              % Checking counter tests and counter actions
              isvalid_countertest(CounterTest,VarNames),
              is_list(CounterActions),
              not((member(Y,CounterActions),not((
                              Y = (Var,Num),
                              memberchk(Var,VarNames),
                              integer(Num)
                              ))
                   )
                  )
             )),
         write(X), nl
         )) -> true
    ; write('Last check for isvalid_globaltrans failed'), nl, fail
    ),
    ( current_predicate(synpco_debug,synpco_debug) ->
        write('Out of isvalid_globaltrans'), nl
    ; true
    ).

isvalid_statetuple(StateTuple,ListofPCo,NumThreads) :-
    functor(StateTuple,states,NumThreads),
    isvalid_state(1,StateTuple,ListofPCo,NumThreads).

isvalid_state(NumThreads,_,_,NumThreads) :- !.
isvalid_state(N,StateTuple,[PCo|PCos],NumThreads) :-
    N < NumThreads,
    arg(N,StateTuple,State),
    PCo = pco(PCoStates,_,_,_,_,_),
    memberchk(State,PCoStates),
    N1 is N+1,
    isvalid_state(N1,StateTuple,PCos,NumThreads).

% isvalid_globaltrans_formula(Formula,ListofPCo) :-
%   Formula is a valid global transition formula with respect to the list
%   ListofPCo of PCos.
/*
isvalid_globaltrans_formula(eq(ProcessIndex,State),ListofPCo) :-
    integer(ProcessIndex),
    nth1(ProcessIndex,ListofPCo,pco(States,_,_,_,_,_)),
    memberchk(State,States).

isvalid_globaltrans_formula(and(Formula1,Formula2),ListofPCo) :-
    isvalid_globaltrans_formula(Formula1,ListofPCo),
    isvalid_globaltrans_formula(Formula2,ListofPCo).

isvalid_globaltrans_formula(or(Formula1,Formula2),ListofPCo) :-
    isvalid_globaltrans_formula(Formula1,ListofPCo),
    isvalid_globaltrans_formula(Formula2,ListofPCo).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%% Start of Translation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cbreach_synpco_to_z3(+SynPco,+RevBound,+ConBound,+ParikhConstraint,+OS) :-
%   translate context-bounded model checking of SynPCo with RevBound number of 
%   reversals and ConBound number of context-switches to Z3 formulas written
%   in the output stream OS.
%
% The source and target configurations of the reachability problem are
% given in SynPCo. 
cbreach_synpco_to_z3(SynPCo,RevBound,ConBound,ParikhConstraint,OS) :-
    % Checking validity of arguments
    isvalid_synpco(SynPCo),
    integer(RevBound), RevBound >= 0,
    integer(ConBound), ConBound >= 0,
    % Initialization of Variables
    getcount_modevec(SynPCo,RevBound,ModeVecCount),
    assert(internal_modeveccount(ModeVecCount)),
    assert(internal_conbound(ConBound)),
    write('ModeVecCount is '), write(ModeVecCount), nl,
    get_closed_interval(1,ModeVecCount,ModeIndices),
    get_closed_interval(0,ConBound,ConSwitchIndices),
    SynPCo = synpco(_,_ListPCos,_,_,VarNames),
    findall(CounterVarPackage,(member(VarName,VarNames),
                               get_constants(SynPCo,VarName,Constants),
                               length(Constants,LenConstants),
                               ( Constants = [0|_] ->
                                    NumRegions is 2*LenConstants
                               ; NumRegions is (2*LenConstants)+1
                               ),
                               CounterVarPackage = 
                                (VarName,Constants,NumRegions)
                              ),
                              CounterVarPackages),

    % check
    %findall(t(pack(A,B,C,D)),(member(PCo,ListPCos),
                      %PCo = pco(_,_,ISS,_,_,_),
                      %write('yes'), nl,
                      %member(t(pack(A,B,C,D)),ISS)),TsCheck2),
    %write('TsCheck2 is: '), write(TsCheck2), nl,

    % Translation
    write('Starting translation from SynPCo to AsynPDA ...'), nl,
    cbreach_synpco_to_asynpda(SynPCo,ModeIndices,ConSwitchIndices,AsynPDA),
    %asynpda_add_action_count(AsynPDA1,AsynPDA),
    write('Translating from SynPCo to AsynPDA successful!'), nl,
    write('Starting translation from AsynPDA to CG ...'), nl,
    asynpda_to_cg(AsynPDA,CG), 
    write('Translating from AsynPDA to CG successful!'), nl,
    print_cgsize(CG),
    open('debugging2.txt',write,OSDebug2),
    write(OSDebug2,CG),
    CG = cg(_,Ts,_,_),
	annotate_cg(CG,CGATemp), 
    %version without remove_unreachable:
    %remove_empty_nonterminals(CGATemp,CGA), 
    CGA = CGATemp,
    %remove_empty_nonterminals(CGATemp,CGATemp1), 
    %remove_unreachable(CGATemp1,CGA), 
    write('Optimizing CG successful!'), nl,
    open('debugging.txt',write,OSDebug),
    print_debugging(CGA,OSDebug),
	parikh_cg(CGA,ParikhInfo), 
    write('Parikh information obtained'), nl,
	CGA = cg(_,TA,_,n(_,_)),

    synpco_get_presburger(quantifiers,Ts,ModeIndices,RevBound,ConBound,
        CounterVarPackages,PresForm),
    ( isvalid_formula(PresForm,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    ( isvalid_formula(ParikhConstraint,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    join_formula(PresForm,ParikhConstraint,NewPresForm),
    annotate_formula(NewPresForm,TA,NewPresFormA),
    %%%
    internal_endstart(EndStart),
    annotate_formula(EndStart,TA,EndStartA),
    retract(internal_endstart(_)),
    assert(internal_endstart(EndStartA)),
    % printing
    print_cg(CGA,ParikhInfo,NewPresFormA,OS). 

%cbreach_synpco_to_asynpda_pres(+SynPCo,+RevBound,+ConBound,-AsynPDA,
%                                                             -PresForm).
%   This is the main translation for cbreach_synpco_to_z3. The output
%   in this case is a pair of asynchronous PDA and existential Presburger 
%   Formula.
%cbreach_synpco_to_asynpda_pres(SynPCo,RevBound,ConBound,AsynPDA,PresForm) :-
    %cbreach_synpco_to_asynpda(SynPCo,ModeIndices,ConSwitchIndices,AsynPDA),
    %Invoke cbreach_synpco_to_asynpda


% synpco_get_presburger(+Flag,+Actions,+ModeIndices,+RevBound,+ConBound,
%                   +CounterVarsPackage,-Pres) :-
%   Pres is the existential Presburger formula (see the format in cg.pl,
%   isvalid_formula/1) that is to be produced together with asynchronous
%   PDA.
%   Flag can be any of  the following:
%   1. 'quantifiers' - meaning that it is currently producing the quantifiers
%                      part.
%   2. 'qfree'       - producing the quantifier-free part
%   3. 'domain'      - producing the Domain subformula (asserting domains
%                      of the mode vector variables, including Init subform)
%   4. 'goodseq'     - producing GoodSeq subformula
%   5. 'endstart'    - producing EndStart subformula (defining end_i_var and
%                      start_i_var)
%   5. 'respect'     - producing Respect subformula
%   6. 'onechange'   - producing OneChange subformula
%   7. 'syncs'       - producing Syncs subformula
%
% Note: (I) CounterVarsPackage here are a list of
%       tuples of the form (VarName,Regions,NumRegions)
%
%       where VarName is a variable name, 
%             Regions are a sorted list of integer constants against which 
%                   VarName is tested,
%             NumRegions is the cardinality of Regions.
%       (II) EndCounters_i_var and StartCounters_i_var are explicitly defined 
%       as variables in the current translation. We call them end_i_var and
%       start_i_var, respectively.
synpco_get_presburger(quantifiers,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres) :-
    !,
    synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,
            CounterVars,ModeIndices,CounterVars,Pres).

synpco_get_presburger(qfree,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres) :-
    !,
    Pres = and([Domain,GoodSeq,EndStart,Respect,OneChange,Syncs]),
    %Pres = and([Domain,GoodSeq,EndStart,Respect,OneChange]),
    synpco_get_presburger(domain,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Domain),
    synpco_get_presburger(goodseq,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,GoodSeq),
    synpco_get_presburger(endstart,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,EndStart),
    synpco_get_presburger(respect,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Respect),
    synpco_get_presburger(onechange,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,OneChange),
    synpco_get_presburger(syncs,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Syncs).

synpco_get_presburger(syncs,Actions,_,_,_,
        _,Formula) :-
        !,
        findall(Conjunct,(member(t((StateTuple1,StateTuple2,ModeIndex,
                                      ConIndex,Flag)),Actions),
                          functor(StateTuple1,states,NumThreads),
                          functor(StateTuple2,states,NumThreads),
                          Conjunct = 
                                imply(gt([t((StateTuple1,StateTuple2,ModeIndex,
                                            ConIndex,Flag))],[0]),
                                      eq([t((StateTuple1,StateTuple2,ModeIndex,
                                            ConIndex,Flag))],[NumThreads]))
                         ),
                         ConjunctList),
        Formula = and(ConjunctList).


synpco_get_presburger(onechange,Actions,_,_,_,
        _,Formula) :-
    !,
    findall(Conjunct,(member(t((VarName,IncrSize1,ModeIndex,1)),Actions),
                      member(t((VarName,IncrSize2,ModeIndex,1)),Actions),
                      not(IncrSize1 =:= IncrSize2),
                      Conjunct = imply(Form1,Form2),
                      Form1 = gt([t((VarName,IncrSize1,ModeIndex,1))],[0]),
                      Form2 = and(eq([t((VarName,IncrSize1,ModeIndex,1))],[1]),
                                  eq([t((VarName,IncrSize2,ModeIndex,1))],[0]))
                     ),ConjunctList),
    Formula = and(ConjunctList).

synpco_get_presburger(respect,Actions,ModeIndices,_,_,
        CounterVars,Formula) :-
    !,
    % Look at the description of the big conjuncts from CAV paper
    Formula = and([BigConjunct1,BigConjunct2,BigConjunct3]),
    findall(Conjunct,(member((VarName,_,_),CounterVars),
                      member(ModeIndex,ModeIndices),
                      Conjunct = imply(Subform1,Subform2),
                      atom_sub_atom('arr',ModeIndex,Arr1), 
                                atom_sub_atom(Arr1,VarName,Arr),
                      % when Arr is 0 (non-incrementing), positive
                      % actions are disallowed
                      ( Subform1 = eq([Arr],[0]),
                        findall(eq([X],[0]),(
                                 member(t((VarName,IncrSize,ModeIndex,Flag)),
                                                        Actions),
                                 IncrSize > 0,
                                 X = t((VarName,IncrSize,ModeIndex,Flag))
                                 ),
                                 ListEquations)
                      % when Arr is 1 (non-decrementing), negative
                      % actions are disallowed
                      ; Subform1 = eq([Arr],[1]),
                        findall(eq([X],[0]),(
                                 member(t((VarName,IncrSize,ModeIndex,Flag)),
                                                        Actions),
                                 IncrSize < 0,
                                 X = t((VarName,IncrSize,ModeIndex,Flag))
                                 ),
                                 ListEquations)
                      ),
                      Subform2 = and(ListEquations)
                      ),
                      ConjunctList1),
    BigConjunct1 = and(ConjunctList1),
    findall(Conjunct,(member((VarName,Regions,NumRegions),CounterVars),
                      member(ModeIndex,ModeIndices),
                      atom_sub_atom('reg',ModeIndex,Reg1), 
                                    atom_sub_atom(Reg1,VarName,Reg),
                      atom_sub_atom('end',ModeIndex,End1), 
                                    atom_sub_atom(End1,VarName,End),
                      atom_sub_atom('start',ModeIndex,Start1),
                                    atom_sub_atom(Start1,VarName,Start),
                      % getting region formulas: misleading naming of variables
                      get_regions(Regions,End,RegionFormulasEnd),
                      get_regions(Regions,Start,RegionFormulasStart),
                      get_closed_interval(1,NumRegions,RegionIndices),
                      member(RegionIndex,RegionIndices),
                      ( nth1(RegionIndex,RegionFormulasEnd,EndForm) ->
                        true
                      ; write('Warning: RegionIndex calculation is wrong'), nl,
                        fail
                      ),
                      nth1(RegionIndex,RegionFormulasStart,StartForm),
                      Conjunct = imply(eq([Reg],[RegionIndex]),
                                       and(EndForm,StartForm))
                     ),
                     ConjunctList2),
    BigConjunct2 = and(ConjunctList2),
    findall(Conjunct,(Conjunct = imply(FormA,FormB),
                      member(ModeIndex,ModeIndices),
                      member(t((CtrTest,ModeIndex)),Actions),
                      FormA = gt([t((CtrTest,ModeIndex))],[0]),
                      findall(Substitution,(member((VarName,_,_),CounterVars),
                                    atom_sub_atom('start',ModeIndex,Start1),
                                    atom_sub_atom(Start1,VarName,Start),
                                    Substitution = Start/VarName
                                    ),SubstitutionList),
                      substitute_term(CtrTest,SubstitutionList,FormB)
                      ),ConjunctList3),
    BigConjunct3 = and(ConjunctList3).

synpco_get_presburger(domain,Actions,ModeIndices,RevBound,ConBound,
    CounterVars,Pres) :-
    !,
    synpco_get_presburger_domain(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,ModeIndices,CounterVars,Pres).

synpco_get_presburger(goodseq,Actions,ModeIndices,RevBound,ConBound,CounterVars,
    Formula) :-
    !,
    synpco_get_presburger_goodseq(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        ModeIndices,CounterVars,Formula).

synpco_get_presburger(endstart,Actions,ModeIndices,_,_,CounterVars,
    Formula) :-
    !,
    findall(Eq,(member(ModeIndex,ModeIndices),
                member((VarName,_,_),CounterVars),
                ( atom_sub_atom('end',ModeIndex,End1), 
                            atom_sub_atom(End1,VarName,End),
                  findall(Summand,(member(t((VarName,IncrSize,I,Flag)),Actions),
                                 ( I < ModeIndex
                                 ; I = ModeIndex,
                                   Flag = 0),
                                 Summand = 
                                        IncrSize*t((VarName,IncrSize,I,Flag))
                                 ),
                          RHS),
                  Eq = eq([End],RHS)
                ; atom_sub_atom('start',ModeIndex,Start1),
                            atom_sub_atom(Start1,VarName,Start),
                  findall(Summand,(member(t((VarName,IncrSize,I,Flag)),Actions),
                                 I < ModeIndex,
                                 Summand = 
                                        IncrSize*t((VarName,IncrSize,I,Flag))
                                 ),
                          RHS),
                  Eq = eq([Start],RHS)
                )),
                Equations),
    %Formula = and(Equations),
    Formula = true,
    assert(internal_endstart(and(Equations))).
    %write('YESYES'), nl.
    %write(Equations).


synpco_get_presburger_goodseq(_,_,_,_,_,[_|[]],_,and([])) :- !.
synpco_get_presburger_goodseq(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [_|CurrModeIndices],[],Formula) :-
    !,
    not(CurrModeIndices == []),
    synpco_get_presburger_goodseq(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        CurrModeIndices,CounterVars,Formula).

synpco_get_presburger_goodseq(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [ModeIndex|CurrModeIndices],[(VarName,_,_)|CurrCounterVars],Formula) :-
    not(CurrModeIndices == []),
    atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
    atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
    atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
    NextModeIndex is ModeIndex + 1,
    atom_sub_atom('reg',NextModeIndex,NextReg1),
        atom_sub_atom(NextReg1,VarName,NextReg),
    atom_sub_atom('rev',NextModeIndex,NextRev1), 
        atom_sub_atom(NextRev1,VarName,NextRev),
    atom_sub_atom('arr',NextModeIndex,NextArr1), 
        atom_sub_atom(NextArr1,VarName,NextArr),
    Formula = and([imply(or(lt([Arr],[NextArr]),gt([Arr],[NextArr])),
                         eq([NextRev],[Rev,1])),
                   imply(eq([Arr],[NextArr]),eq([NextRev],[Rev])),
                   imply(lt([Reg],[NextReg]),eq([NextArr],[1])),
                   imply(gt([Reg],[NextReg]),eq([NextArr],[0]))|
                   Subconjuncts]),
    synpco_get_presburger_goodseq(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [ModeIndex|CurrModeIndices],CurrCounterVars,and(Subconjuncts)).
    

% 

synpco_get_presburger_domain(_,_,_,_,_,[],_,and([])) :- !.

synpco_get_presburger_domain(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [_|CurrModeIndices],
        [],Formula) :-
        !,
        synpco_get_presburger_domain(Actions,ModeIndices,RevBound,ConBound,
            CounterVars,
            CurrModeIndices,
            CounterVars,Formula).

synpco_get_presburger_domain(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [ModeIndex|CurrModeIndices],
        [(VarName,_,NumRegions)|CurrCounterVars],Formula) :-
        ModeIndexLowBound = 1,
        atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
        atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
        atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
        ( ModeIndex = ModeIndexLowBound ->
            Formula = and([eq([Reg],[1]),
                           eq([Rev],[0]),
                           eq([Arr],[1])|Subconjuncts])
        ; Formula = and([geq([Reg],[1]),leq([Reg],[NumRegions]),
                         leq([Rev],[RevBound]),
                         leq([Arr],[1])|Subconjuncts])
        ),
        synpco_get_presburger_domain(Actions,ModeIndices,RevBound,ConBound,
                CounterVars,[ModeIndex|CurrModeIndices],CurrCounterVars,
                and(Subconjuncts)).

% 
synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,
    CounterVars,[],_,Pres) :-
    !,
    synpco_get_presburger(qfree,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres).
synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [_|CurrModeIndices],[],Pres) :-
    !,
    synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,
        CurrModeIndices,CounterVars,Pres).
    
synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [ModeIndex|CurrModeIndices],[(VarName,_,_)|CurrCounterVars],
    Pres) :-
    atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
    atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
    atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
    atom_sub_atom('end',ModeIndex,End1), atom_sub_atom(End1,VarName,End),
    atom_sub_atom('start',ModeIndex,Start1),atom_sub_atom(Start1,VarName,Start),
    Pres = exist(Reg,
            exist(Rev,
              exist(Arr,
                exist(End,
                  exist(Start,Subformula)
                     )
                   )
                  )
                 ),
    synpco_get_presburger_quantifiers(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,[ModeIndex|CurrModeIndices],CurrCounterVars,Subformula).
        
    
% cbreach_synpco_to_asynpda(+SynPCo,+ModeVecCount,+ConBound,
%                            -AsynPDA).
%cbreach_synpco_to_asynpda(SynPCo,ModeVecCount,ConBound,AsynPDA) :-
%findall(X,(X #>= 1, X #=< ModeVecCount, indomain(X)),ModeIndices),
%findall(X,(X #>= 0, X #=< ConBound, indomain(X)),ConSwitchIndices),

cbreach_synpco_to_asynpda(SynPCo,ModeIndices,ConSwitchIndices,AsynPDA) :-
    SynPCo = synpco(NumThreads,List_of_PCos,GlobalTrans,_,_),
    get_closed_interval(1,NumThreads,Interval),
    findall(PDA,(member(ThreadIndex,Interval),
                 nth1(ThreadIndex,List_of_PCos,PCo),
                 pco_globaltrans_to_pda(PCo,GlobalTrans,ThreadIndex,ModeIndices,
                    ConSwitchIndices,PDA)
                 ),
             AsynPDA).

    
pco_globaltrans_to_pda(PCo,GlobalTrans,ThreadIndex,ModeIndices,
            ConSwitchIndices,PDA) :-
    PCo = pco(States,StackSyms,_,Trans,(Q0,sym(Z_0)),Qacc),
    localtrans_pco_to_pda(Trans,ModeIndices,ConSwitchIndices,LocalPDATrans1),
    idle_localtrans(States,StackSyms,ModeIndices,ConSwitchIndices,PDATransIdle),
    append(PDATransIdle,LocalPDATrans1,LocalPDATrans),
    globaltrans_to_pda(GlobalTrans,StackSyms,ThreadIndex,ModeIndices,
            ConSwitchIndices,GlobalPDATrans),
    %write(GlobalPDATrans), nl,
    append(GlobalPDATrans,LocalPDATrans,PDATrans1),
    new_finaltrans(ThreadIndex,Qacc,StackSyms,ModeIndices,ConSwitchIndices,
            FinalTrans),
    append(FinalTrans,PDATrans1,PDATrans), 
    new_finalstate(ThreadIndex,NewFinState),
    PDA = pda(PDAStates,StackSyms,NewInputSyms,PDATrans,((Q0,1,0),sym(Z_0)),
        NewFinState),
    get_states_from_transitions(PDAStates,PDATrans),
    get_actions_from_transitions(NewInputSyms,PDATrans),
    % Printing
    length(PDATrans,LenPDATrans),
    length(PDAStates,LenPDAStates),
    write('PDA '), write(ThreadIndex), write(' has '), write(LenPDAStates), write(' states'), nl,
    write('PDA '), write(ThreadIndex), write(' has '), write(LenPDATrans), write(' rules'), nl.
    %%%

%
globaltrans_to_pda(GlTrans,StackSyms,ThreadIndex,ModeVecIndices,
    ConSwitchIndices,PDATrans) :-
        globaltrans_to_pda_accum(GlTrans,StackSyms,ThreadIndex,ModeVecIndices,
            ConSwitchIndices,[],PDATrans).

globaltrans_to_pda_accum([],_,_,_,_,PDATransAccum,PDATransAccum).
globaltrans_to_pda_accum([GlTran|GlTrans],StackSyms,ThreadIndex,ModeVecIndices,
        ConSwitchIndices,PDATransAccum,PDATrans) :-
            globaltran_to_pda(GlTran,StackSyms,ThreadIndex,ModeVecIndices,
                ConSwitchIndices,PDATransAccum,PDATransAccum1),
            globaltrans_to_pda_accum(GlTrans,StackSyms,ThreadIndex,
                ModeVecIndices,ConSwitchIndices,PDATransAccum1,PDATrans).

% 
globaltran_to_pda(GlTran,StackSyms,ThreadIndex,ModeVecIndices,ConSwitchIndices,
        PDATransAccum,PDATrans) :-
    GlTran = tran(from(StateTuple1,CtrTest),Actions,to(StateTuple2,CtrActions)),
    arg(ThreadIndex,StateTuple1,State1),
    arg(ThreadIndex,StateTuple2,State2),
    findall(PDATran,(
               member(ModeVecIndex,ModeVecIndices),
               member(ConSwitchIndex,ConSwitchIndices),
               member(StackSym,StackSyms),
               not(max_list(ConSwitchIndices,ConSwitchIndex)),
               ( ThreadIndex == 1 ->
                    findall(PDACtrAction,(member(CtrAction,CtrActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,
                                                        ModeVecIndex,0))
                                          ),
                                          PDACtrActions),
                    append(Actions,[t((StateTuple1,StateTuple2,ModeVecIndex,
                         ConSwitchIndex,0)),
                       t((CtrTest,ModeVecIndex))|PDACtrActions],
                      PDAActionsUnsort),
                    msort(PDAActionsUnsort,PDAActions)
               ; PDAActions = [t((StateTuple1,StateTuple2,ModeVecIndex,
                         ConSwitchIndex,0))]
               ),
               NewConIndex is ConSwitchIndex + 1,
               PDATran = tran(from((State1,ModeVecIndex,ConSwitchIndex),
                                   [StackSym]),
                              PDAActions,
                              to((State2,ModeVecIndex,NewConIndex),[StackSym])
                             )
               ),
           PDATrans0),
    ( CtrActions == [] ->
        PDATrans1 = []
    ; findall(PDATran,(
               member(ModeVecIndex,ModeVecIndices),
               member(ConSwitchIndex,ConSwitchIndices),
               member(StackSym,StackSyms),
               not(max_list(ModeVecIndices,ModeVecIndex)),
               not(max_list(ConSwitchIndices,ConSwitchIndex)),
               ( ThreadIndex == 1 ->
                    findall(PDACtrAction,(member(CtrAction,CtrActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,
                                                        ModeVecIndex,1))
                                          ),
                                          PDACtrActions),
                    append(Actions,[t((StateTuple1,StateTuple2,ModeVecIndex,
                         ConSwitchIndex,1)),
                       t((CtrTest,ModeVecIndex))|PDACtrActions],
                      PDAActionsUnsort),
                    msort(PDAActionsUnsort,PDAActions)
               ; PDAActions = [t((StateTuple1,StateTuple2,ModeVecIndex,
                         ConSwitchIndex,1))]
               ),
               NewConIndex is ConSwitchIndex + 1,
               NewModeIndex is ModeVecIndex + 1,
               PDATran = tran(from((State1,ModeVecIndex,ConSwitchIndex),
                                   [StackSym]),
                              PDAActions,
                              to((State2,NewModeIndex,NewConIndex),[StackSym])
                             )
               ),
           PDATrans1)
    ),
    append(PDATrans0,PDATrans1,PDATrans01),
    append(PDATrans01,PDATransAccum,PDATrans).


%
new_finalstate(Index,NewFinState) :-
    atom_sub_atom('_new_final',Index,NewFinState).

% Index is the index of the thread
new_finaltrans(Index,OldFinState,StackSyms,ModeIndices,ConSwitchIndices,
        PDATrans) :-
    new_finalstate(Index,NewFinState),
    %findall(X,(X #>= 1, X #=< ModeVecCount, indomain(X)),ModeIndices),
    %findall(X,(X #>= 0, X #=< ConBound, indomain(X)),ConSwitchIndices),
    findall(PDATran,(member(StackSym,StackSyms),
                    (PDATran = tran(from(NewFinState,[StackSym]),[],
                                     to(NewFinState,[])
                                   )
                    ;member(ModeIndex,ModeIndices),
                     member(ConSwitchIndex,ConSwitchIndices),
                     PDATran = tran(
                                 from((OldFinState,ModeIndex,ConSwitchIndex),
                                      [StackSym]),
                                 [],
                                 to(NewFinState,[StackSym])
                                 )
                     )
                     ),
             PDATrans).
                                        


% Idle Local Transitions
idle_localtrans(PCoStates,StackSyms,ModeIndices,ConSwitchIndices,PDATrans) :-
    is_list(PCoStates),
    is_list(StackSyms),
    %findall(X,(X #>= 1, X #< ModeVecCount, indomain(X)),ModeIndices),
    %findall(X,(X #>= 0, X #=< ConBound, indomain(X)),ConSwitchIndices),
    findall(PDATran,(member(PCoState,PCoStates),
                     member(StackSym,StackSyms),
                     member(ModeIndex,ModeIndices),
                     not(max_list(ModeIndices,ModeIndex)),
                     member(ConSwitchIndex,ConSwitchIndices),
                     NewModeIndex is ModeIndex + 1,
                     PDATran = 
                        tran(from((PCoState,ModeIndex,ConSwitchIndex),
                                  [StackSym]),
                             [],
                             to((PCoState,NewModeIndex,ConSwitchIndex),
                                [StackSym]))
                    ),
            PDATrans).

% Translate (a list of) PCo local transitions to (a list of) PDA transitions
localtrans_pco_to_pda(PCoTrans,ModeIndices,ConSwitchIndices,PDATrans) :-
    localtrans_pco_to_pda(PCoTrans,ModeIndices,ConSwitchIndices,[],PDATrans).
localtrans_pco_to_pda([],_,_,PDATransAccum,PDATransAccum).
localtrans_pco_to_pda([PCoTran|PCoTrans],ModeIndices,ConSwitchIndices,
        PDATransAccum,PDATrans) :-
    localtran_pco_to_pda_accum(PCoTran,ModeIndices,ConSwitchIndices,PDATransAccum,
                               PDATransAccum1),
    localtrans_pco_to_pda(PCoTrans,ModeIndices,ConSwitchIndices,PDATransAccum1,
                               PDATrans).

localtran_pco_to_pda_accum(PCoTran,ModeIndices,ConSwitchIndices,PDATransAccum,
                    PDATrans) :-
    PCoTran = tran(from(P,Word1,CounterTest),Actions,
                        to(Q,Word2,CounterActions)),
    !,
    %findall(X,(X #>= 1, X #=< ModeVecCount, indomain(X)),ModeIndices),
    %findall(X,(X #>= 0, X #=< ConBound, indomain(X)),ConSwitchIndices),
    findall(PDATran,(member(ModeIndex,ModeIndices),
                     member(ConSwitchIndex,ConSwitchIndices),
                     findall(PDACtrAction,(member(CtrAction,CounterActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,ModeIndex,
                                                             0))
                                          ),
                                          PDACtrActions),
                     append(Actions,[t((CounterTest,ModeIndex))|PDACtrActions],
                            PDAActionsUnsort),
                     msort(PDAActionsUnsort,PDAActions),
                     PDATran = tran(from((P,ModeIndex,ConSwitchIndex),Word1),
                                    PDAActions,
                                    to((Q,ModeIndex,ConSwitchIndex),Word2))
                     ),
                     PDATrans1),
    ( CounterActions == [] ->
        PDATrans2 = []
    %* This seems redundant
    ; findall(PDATran,(member(ModeIndex,ModeIndices),
                     not(max_list(ModeIndices,ModeIndex)),
                     member(ConSwitchIndex,ConSwitchIndices),
                     findall(PDACtrAction,(member(CtrAction,CounterActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,ModeIndex,
                                                             1))
                                          ),
                                          PDACtrActions),
                     append(Actions,[t((CounterTest,ModeIndex))|PDACtrActions],
                            PDAActionsUnsort),
                     msort(PDAActionsUnsort,PDAActions),
                     NewModeIndex is ModeIndex + 1,
                     PDATran = tran(from((P,ModeIndex,ConSwitchIndex),Word1),
                                    PDAActions,
                                    to((Q,NewModeIndex,ConSwitchIndex),Word2))
                     ),
                     PDATrans2)
     ),
     append(PDATrans1,PDATrans2,PDATrans12),
     append(PDATrans12,PDATransAccum,PDATrans).

localtran_pco_to_pda_accum(PCoTran,ModeIndices,ConSwitchIndices,PDATransAccum,
                    PDATrans) :-
    PCoTran = spectran(P,StackSymSubset,Triplets,Q),
    /*
    findall((Test,Acts,Incrs),(member((Test,Acts,Incrs),Triplets),
                        Incrs == []),TripletSubset0),
    findall((Test,Acts,Incrs),(member((Test,Acts,Incrs),Triplets),
                        not(Incrs == [])),TripletSubset1),
    */

    max_list(ModeIndices,MaxModeIndex),
    ( Triplets == [] ->
        PDATrans0 = []
    ; findall(PDATran,(member(ModeIndex,ModeIndices),
                       not(ModeIndex =:= MaxModeIndex),
                       member(ConSwitchIndex,ConSwitchIndices),
                       K is 1,
                     findall(PDAActSeq,
                        (member((Test,Acts,CtrActions),Triplets),
                         findall(PDACtrAction,(member(CtrAction,CtrActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,ModeIndex,
                                                             K))
                                          ),
                                          PDACtrActions),
                         TestAct = t((Test,ModeIndex)),
                         append(Acts,[TestAct|PDACtrActions],PDAActSeqUnsort),
                         msort(PDAActSeqUnsort,PDAActSeq)
                         ),
                         PDAActSeqs),
                        NxtModeIndex is ModeIndex+K,
                        PDATran = spectran((P,ModeIndex,ConSwitchIndex),
                                        StackSymSubset,
                                        PDAActSeqs,
                                          (Q,NxtModeIndex,ConSwitchIndex))
                      ),
                     PDATrans0)
    ),
    /*
    % Computing for TripletSubset0
    ( TripletSubset0 == [] ->
        PDATrans0 = []
    ; findall(PDATran,(member(ModeIndex,ModeIndices),
                       member(ConSwitchIndex,ConSwitchIndices),
                     %findall(PDACtrAction,(member(CtrAction,CounterActions),
                                           %CtrAction = (Var,Num),
                                           %not(Num =:= 0),
                                           %PDACtrAction = t((Var,Num,ModeIndex,
                                                             %0))
                                          %),
                                          %PDACtrActions),
                     findall(PDAActSeq,
                        (member((Test,Acts,[]),TripletSubset0),
                         TestAct = t((Test,ModeIndex)),
                         PDAActSeqUnsort = [TestAct|Acts],
                         msort(PDAActSeqUnsort,PDAActSeq)
                         ),
                         PDAActSeqs),
                     PDATran = spectran((P,ModeIndex,ConSwitchIndex),
                                        StackSymSubset,
                                        PDAActSeqs,
                                          (Q,ModeIndex,ConSwitchIndex))
                     ),
                     PDATrans0)
    ),
    % Computing for TripletSubset1
    ( TripletSubset1 == [] ->
        PDATrans1 = []
    ; findall(PDATran,(member(ModeIndex,ModeIndices),
                       member(ConSwitchIndex,ConSwitchIndices),
                       member(K,[0,1]),
                       not((K =:= 1,ModeIndex =:= MaxModeIndex)),
                     findall(PDAActSeq,
                        (member((Test,Acts,CtrActions),TripletSubset1),
                         findall(PDACtrAction,(member(CtrAction,CtrActions),
                                           CtrAction = (Var,Num),
                                           not(Num =:= 0),
                                           PDACtrAction = t((Var,Num,ModeIndex,
                                                             K))
                                          ),
                                          PDACtrActions),
                         TestAct = t((Test,ModeIndex)),
                         append(Acts,[TestAct|PDACtrActions],PDAActSeqUnsort),
                         msort(PDAActSeqUnsort,PDAActSeq)
                         ),
                         PDAActSeqs),
                        NxtModeIndex is ModeIndex+K,
                        PDATran = spectran((P,ModeIndex,ConSwitchIndex),
                                        StackSymSubset,
                                        PDAActSeqs,
                                          (Q,NxtModeIndex,ConSwitchIndex))
                      ),
                     PDATrans1)
    ),
    */
    % Conclusion
    %append(PDATrans0,PDATrans1,PDATrans01),
    %write(PDATrans01), nl,
    append(PDATrans0,PDATransAccum,PDATrans).

% get_closed_interval(+LowBound,+UpBound,List) :-
%   List is [LowBound .... UpBound].
get_closed_interval(LowBound,UpBound,List) :-
    get_closed_interval(LowBound,UpBound,[],List).

get_closed_interval(LowBound,UpBound,ListAccum,List) :-
    integer(LowBound),
    integer(UpBound),
    ( LowBound > UpBound ->
        List = ListAccum
    ; UpBound1 is UpBound -1,
      get_closed_interval(LowBound,UpBound1,[UpBound|ListAccum],List)
    ).

% synpco_to_asynpda_state_compare(CompareRel,Term,Term)
synpco_to_asynpda_state_compare(CompareRel,(_,ModeIndex1,ConIndex1),
                                           (_,ModeIndex2,ConIndex2)) :-
    !,
    ( (ModeIndex1 =< ModeIndex2, ConIndex1 =< ConIndex2) ->
        CompareRel = '<'
    ; CompareRel = incomparable
    ).

synpco_to_asynpda_state_compare(CompareRel,Term,
                                           (_,_,_)) :-
    functor(Term,_,0),
    !,
    CompareRel = incomparable.

synpco_to_asynpda_state_compare(CompareRel,(_,_,_),
                                           Term) :-
    functor(Term,_,0),
    !,
    CompareRel = '<'.

synpco_to_asynpda_state_compare(CompareRel,Term1,Term2) :-
    functor(Term1,_,0),
    functor(Term2,_,0),
    CompareRel = '<'.

%%%%%%%%%%%%%%%%%%%%%%%%%% End of Translation   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% New Improved Part    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   Count = sum of (# regions for counter i) x (RevBound for ctr i plus 1) x 
%                       (number of variables)
getcount_modevec_new(CounterVarPackages,RevBoundsAssoc,ModeVecCount) :-
    getcount_modevec_new(CounterVarPackages,RevBoundsAssoc,0,ModeVecCount).

getcount_modevec_new([],_,ModeVecCount,ModeVecCount).
getcount_modevec_new([(VarName,_,NumRegions)|Packages],RevBAssoc,Accum,
    ModeVecCount) :-
        ( get_assoc(VarName,RevBAssoc,RevB) ->
            true
        ; write('Error: Some variables not appear in RevBounds'), nl,
          write('Variable is '), write(VarName), nl,
          write('NumRegions is '), write(NumRegions), nl,
          fail
        ),
        NextAccum is Accum + NumRegions*(RevB+1),
        getcount_modevec_new(Packages,RevBAssoc,NextAccum,ModeVecCount).
        

getcount_nonempty_intervals(ListConstants,NumIntervals) :-
    get_regions(ListConstants,x,RegionList),
    write(ListConstants), nl,
    length(RegionList,NumIntervals).

% cbreach_synpco_to_z3_new(+SynPco,+RevBounds,+ConBound,+ParikhConstraint,+OS):-
%   Same as cbreach_synpco_to_z3 but RevBounds here is a list of 
%   Variable-ReversalBound.
%   e.g. RevBounds = [var1-4,var2-7,var3-2].
cbreach_synpco_to_z3_new(SynPCo,RevBounds,ConBound,ParikhConstraint,OS) :-
    % Checking validity of arguments
    isvalid_synpco(SynPCo),
    %integer(RevBound), RevBound >= 0,
    ( is_list(RevBounds) ->
        true
    ; write('ERROR: RevBounds are not provided as lists'), nl,
      fail
    ),
    ( list_to_assoc(RevBounds,RevBoundsAssoc) ->
        true
    ; write('ERROR: RevBounds incorrectly supplied'), nl,
      fail
    ),
    integer(ConBound), ConBound >= 0,
    % Initialization of Variables
    SynPCo = synpco(_,_ListPCos,_,_,VarNames),
    findall(CounterVarPackage,(member(VarName,VarNames),
                               get_constants(SynPCo,VarName,Constants),
                               %length(Constants,LenConstants),
                               getcount_nonempty_intervals(Constants,
                                    NumRegions),
                               %( Constants = [0|_] ->
                                    %NumRegions is 2*LenConstants
                               %; NumRegions is (2*LenConstants)+1
                               %),
                               CounterVarPackage = 
                                (VarName,Constants,NumRegions)
                              ),
                              CounterVarPackages),
    getcount_modevec_new(CounterVarPackages,RevBoundsAssoc,ModeVecCount),
    assert(internal_modeveccount(ModeVecCount)),
    assert(internal_conbound(ConBound)),
    write('ModeVecCount is '), write(ModeVecCount), nl,
    get_closed_interval(1,ModeVecCount,ModeIndices),
    get_closed_interval(0,ConBound,ConSwitchIndices),
    
    % check
    %findall(t(pack(A,B,C,D)),(member(PCo,ListPCos),
                      %PCo = pco(_,_,ISS,_,_,_),
                      %write('yes'), nl,
                      %member(t(pack(A,B,C,D)),ISS)),TsCheck2),
    %write('TsCheck2 is: '), write(TsCheck2), nl,

    % Translation
    write('Starting translation from SynPCo to AsynPDA ...'), nl,
    cbreach_synpco_to_asynpda(SynPCo,ModeIndices,ConSwitchIndices,AsynPDA),
    %asynpda_add_action_count(AsynPDA1,AsynPDA),
    write('Translating from SynPCo to AsynPDA successful!'), nl,
    write('Starting translation from AsynPDA to CG ...'), nl,
    asynpda_to_cg(AsynPDA,CG), 
    write('Translating from AsynPDA to CG successful!'), nl,
    print_cgsize(CG),
    open('debugging2.txt',write,OSDebug2),
    write(OSDebug2,CG),
    CG = cg(_,Ts,_,_),
	annotate_cg(CG,CGATemp), 
    %version without remove_unreachable:
    %remove_empty_nonterminals(CGATemp,CGA), 
    CGA = CGATemp,
    %remove_empty_nonterminals(CGATemp,CGATemp1), 
    %remove_unreachable(CGATemp1,CGA), 
    write('Optimizing CG successful!'), nl,
    open('debugging.txt',write,OSDebug),
    print_debugging(CGA,OSDebug),
	parikh_cg(CGA,ParikhInfo), 
    write('Parikh information obtained'), nl,
	CGA = cg(_,TA,_,n(_,_)),

    synpco_get_presburger_new(quantifiers,Ts,ModeIndices,RevBoundsAssoc,
        ConBound,CounterVarPackages,PresForm),
    ( isvalid_formula(PresForm,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    ( isvalid_formula(ParikhConstraint,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    join_formula(PresForm,ParikhConstraint,NewPresForm),
    annotate_formula(NewPresForm,TA,NewPresFormA),
    %%%
    internal_endstart(EndStart),
    annotate_formula(EndStart,TA,EndStartA),
    retract(internal_endstart(_)),
    assert(internal_endstart(EndStartA)),
    % printing
    print_cg(CGA,ParikhInfo,NewPresFormA,OS). 

% synpco_get_presburger_new(+Flag,+Actions,+ModeIndices,+RevBoundAssoc,
%        +ConBound,+CounterVarsPackage,-Pres) :-
%   Pres is the existential Presburger formula (see the format in cg.pl,
%   isvalid_formula/1) that is to be produced together with asynchronous
%   PDA.
%   Flag can be any of  the following:
%   1. 'quantifiers' - meaning that it is currently producing the quantifiers
%                      part.
%   2. 'qfree'       - producing the quantifier-free part
%   3. 'domain'      - producing the Domain subformula (asserting domains
%                      of the mode vector variables, including Init subform)
%   4. 'goodseq'     - producing GoodSeq subformula
%   5. 'endstart'    - producing EndStart subformula (defining end_i_var and
%                      start_i_var)
%   5. 'respect'     - producing Respect subformula
%   6. 'onechange'   - producing OneChange subformula
%   7. 'syncs'       - producing Syncs subformula
%
% Note: (I) CounterVarsPackage here are a list of
%       tuples of the form (VarName,Regions,NumRegions)
%
%       where VarName is a variable name, 
%             Regions are a sorted list of integer constants against which 
%                   VarName is tested,
%             NumRegions is the cardinality of Regions.
%       (II) EndCounters_i_var and StartCounters_i_var are explicitly defined 
%       as variables in the current translation. We call them end_i_var and
%       start_i_var, respectively.
synpco_get_presburger_new(quantifiers,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres) :-
    !,
    synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,
            CounterVars,ModeIndices,CounterVars,Pres).

synpco_get_presburger_new(qfree,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres) :-
    !,
    Pres = and([Domain,GoodSeq,EndStart,Respect,OneChange,Syncs]),
    %Pres = and([Domain,GoodSeq,EndStart,Respect,OneChange]),
    synpco_get_presburger_new(domain,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Domain),
    synpco_get_presburger_new(goodseq,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,GoodSeq),
    synpco_get_presburger_new(endstart,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,EndStart),
    synpco_get_presburger_new(respect,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Respect),
    synpco_get_presburger_new(onechange,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,OneChange),
    synpco_get_presburger_new(syncs,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Syncs).

synpco_get_presburger_new(syncs,Actions,_,_,_,
        _,Formula) :-
        !,
        findall(Conjunct,(member(t((StateTuple1,StateTuple2,ModeIndex,
                                      ConIndex,Flag)),Actions),
                          functor(StateTuple1,states,NumThreads),
                          functor(StateTuple2,states,NumThreads),
                          Conjunct = 
                                imply(gt([t((StateTuple1,StateTuple2,ModeIndex,
                                            ConIndex,Flag))],[0]),
                                      eq([t((StateTuple1,StateTuple2,ModeIndex,
                                            ConIndex,Flag))],[NumThreads]))
                         ),
                         ConjunctList),
        Formula = and(ConjunctList).


synpco_get_presburger_new(onechange,Actions,_,_,_,
        _,Formula) :-
    !,
    findall(Conjunct,(member(t((VarName,IncrSize1,ModeIndex,1)),Actions),
                      member(t((VarName,IncrSize2,ModeIndex,1)),Actions),
                      not(IncrSize1 =:= IncrSize2),
                      Conjunct = imply(Form1,Form2),
                      Form1 = gt([t((VarName,IncrSize1,ModeIndex,1))],[0]),
                      Form2 = and(eq([t((VarName,IncrSize1,ModeIndex,1))],[1]),
                                  eq([t((VarName,IncrSize2,ModeIndex,1))],[0]))
                     ),ConjunctList),
    Formula = and(ConjunctList).

synpco_get_presburger_new(respect,Actions,ModeIndices,_,_,
        CounterVars,Formula) :-
    !,
    % Look at the description of the big conjuncts from CAV paper
    Formula = and([BigConjunct1,BigConjunct2,BigConjunct3]),
    findall(Conjunct,(member((VarName,_,_),CounterVars),
                      member(ModeIndex,ModeIndices),
                      Conjunct = imply(Subform1,Subform2),
                      atom_sub_atom('arr',ModeIndex,Arr1), 
                                atom_sub_atom(Arr1,VarName,Arr),
                      % when Arr is 0 (non-incrementing), positive
                      % actions are disallowed
                      ( Subform1 = eq([Arr],[0]),
                        findall(eq([X],[0]),(
                                 member(t((VarName,IncrSize,ModeIndex,Flag)),
                                                        Actions),
                                 IncrSize > 0,
                                 X = t((VarName,IncrSize,ModeIndex,Flag))
                                 ),
                                 ListEquations)
                      % when Arr is 1 (non-decrementing), negative
                      % actions are disallowed
                      ; Subform1 = eq([Arr],[1]),
                        findall(eq([X],[0]),(
                                 member(t((VarName,IncrSize,ModeIndex,Flag)),
                                                        Actions),
                                 IncrSize < 0,
                                 X = t((VarName,IncrSize,ModeIndex,Flag))
                                 ),
                                 ListEquations)
                      ),
                      Subform2 = and(ListEquations)
                      ),
                      ConjunctList1),
    BigConjunct1 = and(ConjunctList1),
    findall(Conjunct,(member((VarName,Regions,NumRegions),CounterVars),
                      member(ModeIndex,ModeIndices),
                      atom_sub_atom('reg',ModeIndex,Reg1), 
                                    atom_sub_atom(Reg1,VarName,Reg),
                      atom_sub_atom('end',ModeIndex,End1), 
                                    atom_sub_atom(End1,VarName,End),
                      atom_sub_atom('start',ModeIndex,Start1),
                                    atom_sub_atom(Start1,VarName,Start),
                      % getting region formulas: misleading naming of variables
                      get_regions(Regions,End,RegionFormulasEnd),
                      get_regions(Regions,Start,RegionFormulasStart),
                      get_closed_interval(1,NumRegions,RegionIndices),
                      member(RegionIndex,RegionIndices),
                      ( nth1(RegionIndex,RegionFormulasEnd,EndForm) ->
                        true
                      ; write('Warning: RegionIndex calculation is wrong'), nl,
                        fail
                      ),
                      nth1(RegionIndex,RegionFormulasStart,StartForm),
                      Conjunct = imply(eq([Reg],[RegionIndex]),
                                       and(EndForm,StartForm))
                     ),
                     ConjunctList2),
    BigConjunct2 = and(ConjunctList2),
    findall(Conjunct,(Conjunct = imply(FormA,FormB),
                      member(ModeIndex,ModeIndices),
                      member(t((CtrTest,ModeIndex)),Actions),
                      FormA = gt([t((CtrTest,ModeIndex))],[0]),
                      findall(Substitution,(member((VarName,_,_),CounterVars),
                                    atom_sub_atom('start',ModeIndex,Start1),
                                    atom_sub_atom(Start1,VarName,Start),
                                    Substitution = Start/VarName
                                    ),SubstitutionList),
                      substitute_term(CtrTest,SubstitutionList,FormB)
                      ),ConjunctList3),
    BigConjunct3 = and(ConjunctList3).

synpco_get_presburger_new(domain,Actions,ModeIndices,RevBound,ConBound,
    CounterVars,Pres) :-
    !,
    synpco_get_presburger_domain_new(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,ModeIndices,CounterVars,Pres).

synpco_get_presburger_new(goodseq,Actions,ModeIndices,RevBound,ConBound,
    CounterVars,Formula) :-
    !,
    synpco_get_presburger_goodseq_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        ModeIndices,CounterVars,Formula).

synpco_get_presburger_new(endstart,Actions,ModeIndices,_,_,CounterVars,
    Formula) :-
    !,
    findall(Eq,(member(ModeIndex,ModeIndices),
                member((VarName,_,_),CounterVars),
                ( atom_sub_atom('end',ModeIndex,End1), 
                            atom_sub_atom(End1,VarName,End),
                  findall(Summand,(member(t((VarName,IncrSize,I,Flag)),Actions),
                                 ( I < ModeIndex
                                 ; I = ModeIndex,
                                   Flag = 0),
                                 Summand = 
                                        IncrSize*t((VarName,IncrSize,I,Flag))
                                 ),
                          RHS),
                  Eq = eq([End],RHS)
                ; atom_sub_atom('start',ModeIndex,Start1),
                            atom_sub_atom(Start1,VarName,Start),
                  findall(Summand,(member(t((VarName,IncrSize,I,Flag)),Actions),
                                 I < ModeIndex,
                                 Summand = 
                                        IncrSize*t((VarName,IncrSize,I,Flag))
                                 ),
                          RHS),
                  Eq = eq([Start],RHS)
                )),
                Equations),
    %Formula = and(Equations),
    Formula = true,
    assert(internal_endstart(and(Equations))).
    %write(Equations).


synpco_get_presburger_goodseq_new(_,_,_,_,_,[_|[]],_,and([])) :- !.
synpco_get_presburger_goodseq_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [_|CurrModeIndices],[],Formula) :-
    !,
    not(CurrModeIndices == []),
    synpco_get_presburger_goodseq_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        CurrModeIndices,CounterVars,Formula).

synpco_get_presburger_goodseq_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [ModeIndex|CurrModeIndices],[(VarName,_,_)|CurrCounterVars],Formula) :-
    not(CurrModeIndices == []),
    atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
    atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
    atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
    NextModeIndex is ModeIndex + 1,
    atom_sub_atom('reg',NextModeIndex,NextReg1),
        atom_sub_atom(NextReg1,VarName,NextReg),
    atom_sub_atom('rev',NextModeIndex,NextRev1), 
        atom_sub_atom(NextRev1,VarName,NextRev),
    atom_sub_atom('arr',NextModeIndex,NextArr1), 
        atom_sub_atom(NextArr1,VarName,NextArr),
    Formula = and([imply(or(lt([Arr],[NextArr]),gt([Arr],[NextArr])),
                         eq([NextRev],[Rev,1])),
                   imply(eq([Arr],[NextArr]),eq([NextRev],[Rev])),
                   imply(lt([Reg],[NextReg]),eq([NextArr],[1])),
                   imply(gt([Reg],[NextReg]),eq([NextArr],[0]))|
                   Subconjuncts]),
    synpco_get_presburger_goodseq_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [ModeIndex|CurrModeIndices],CurrCounterVars,and(Subconjuncts)).
    

% 

synpco_get_presburger_domain_new(_,_,_,_,_,[],_,and([])) :- !.

synpco_get_presburger_domain_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [_|CurrModeIndices],
        [],Formula) :-
        !,
        synpco_get_presburger_domain_new(Actions,ModeIndices,RevBound,ConBound,
            CounterVars,
            CurrModeIndices,
            CounterVars,Formula).

synpco_get_presburger_domain_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
        [ModeIndex|CurrModeIndices],
        [(VarName,_,NumRegions)|CurrCounterVars],Formula) :-
        ModeIndexLowBound = 1,
        atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
        atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
        atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
        ( ModeIndex = ModeIndexLowBound ->
            Formula = and([eq([Reg],[1]),
                           eq([Rev],[0]),
                           eq([Arr],[1])|Subconjuncts])
        ; ( get_assoc(VarName,RevBound,RB) ->
                true
          ; write('ERROR: Variables not appearing in RevBounds list'), nl,
            fail
          ),
          Formula = and([geq([Reg],[1]),leq([Reg],[NumRegions]),
                         leq([Rev],[RB]),
                         leq([Arr],[1])|Subconjuncts])
        ),
        synpco_get_presburger_domain_new(Actions,ModeIndices,RevBound,ConBound,
                CounterVars,[ModeIndex|CurrModeIndices],CurrCounterVars,
                and(Subconjuncts)).

% 
synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,
    CounterVars,[],_,Pres) :-
    !,
    synpco_get_presburger_new(qfree,Actions,ModeIndices,RevBound,ConBound,
        CounterVars,Pres).
synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [_|CurrModeIndices],[],Pres) :-
    !,
    synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,
        CurrModeIndices,CounterVars,Pres).
    
synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,CounterVars,
    [ModeIndex|CurrModeIndices],[(VarName,_,_)|CurrCounterVars],
    Pres) :-
    atom_sub_atom('reg',ModeIndex,Reg1), atom_sub_atom(Reg1,VarName,Reg),
    atom_sub_atom('rev',ModeIndex,Rev1), atom_sub_atom(Rev1,VarName,Rev),
    atom_sub_atom('arr',ModeIndex,Arr1), atom_sub_atom(Arr1,VarName,Arr),
    atom_sub_atom('end',ModeIndex,End1), atom_sub_atom(End1,VarName,End),
    atom_sub_atom('start',ModeIndex,Start1),atom_sub_atom(Start1,VarName,Start),
    Pres = exist(Reg,
            exist(Rev,
              exist(Arr,
                exist(End,
                  exist(Start,Subformula)
                     )
                   )
                  )
                 ),
    synpco_get_presburger_quantifiers_new(Actions,ModeIndices,RevBound,ConBound,
        CounterVars,[ModeIndex|CurrModeIndices],CurrCounterVars,Subformula).
%%%%%%%%%%%%%%%%%%%%%%%%%% End of New Improved Part    %%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%% Context-switch as a counter %%%%%%%%%%%%%%%%%%%%%%%%%
% synpco_conswitch_to_counter(+SynPCo0,+OldRevBound,+ConBound,-SynPCo1,-PresConstraint,
%   -CtxVarName) :-
%   SynPCo1 is SynPCo0 but with context-switch bound ConBound incorporated as 
%   a counter. CtxVarName is the name of the new variable introduced
%   from the context switches. OldRevBound is the RevBound list for SynPCo0
%
% Assume: SynPCo0 is a valid synpco.
synpco_conswitch_to_counter(SynPCo0,OldRevBounds,ConBound,SynPCo1,PresConst,CtxVar) :-
    SynPCo0 = synpco(NumThreads,ListPCos0,GlobalTrans,_,VarNames),
    %%% Getting Mode vector count of SynPCo0
    ( is_list(OldRevBounds) ->
        true
    ; write('ERROR: RevBounds are not provided as lists'), nl,
      fail
    ),
    ( list_to_assoc(OldRevBounds,RevBoundsAssoc) ->
        true
    ; write('ERROR: RevBounds incorrectly supplied'), nl,
      fail
    ),
    % Initialization of Variables
    findall(CounterVarPackage,(member(VarName,VarNames),
                               get_constants(SynPCo0,VarName,Constants),
                               %length(Constants,LenConstants),
                               getcount_nonempty_intervals(Constants,
                                    NumRegions),
                               %( Constants = [0|_] ->
                                    %NumRegions is 2*LenConstants
                               %; NumRegions is (2*LenConstants)+1
                               %),
                               CounterVarPackage = 
                                (VarName,Constants,NumRegions)
                              ),
                              CounterVarPackages),
    getcount_modevec_new(CounterVarPackages,RevBoundsAssoc,OldModeVecCount),
    ModeVecCount is OldModeVecCount + ConBound,
    %% Get Mode Vec Count for SynPCo0

    SynPCo1 = synpco(NumThreads,ListPCos1,[],[],VarNames1),
    get_newvar(VarNames,NewVar),
    CtxVar = NewVar,
    VarNames1 = [NewVar|VarNames],
    get_closed_interval(1,NumThreads,Interval),
    findall(PCo1,(member(ThreadIndex,Interval),
                 nth1(ThreadIndex,ListPCos0,PCo0),
                 PCo0 = pco(States,StackSyms,InputSym0,Trans0,(Q0,sym(Z)),Qacc),
                 PCo1 = pco(States,StackSyms,InputSym1,Trans1,(Q0,sym(Z)),Qacc),
                 globaltrans_to_pco(GlobalTrans,NewVar,StackSyms,ThreadIndex,
                    ConBound,GlobalPCoTrans),
                 findall(NewInputSym,(member(GlobalPCoTran,GlobalPCoTrans),
                            GlobalPCoTran = 
                                spectran(_,_,Triplets,_),
                                member((_,Actions,_),Triplets),
                                member(NewInputSym,Actions)
                            ),
                            UnsortedNewInputSyms),
                 sort(UnsortedNewInputSyms,NewInputSyms),
                 append(InputSym0,NewInputSyms,UnsortedInputSym1),
                 sort(UnsortedInputSym1,InputSym1),
                 append(GlobalPCoTrans,Trans0,Trans1Unsort),
                 sort(Trans1Unsort,Trans1)
                 %write('New Input Symbols: '), write(InputSym1), nl
                 ),
                 ListPCos1),
    globaltrans_to_presconst(GlobalTrans,ConBound,NumThreads,ModeVecCount,CtxVar,PresConst),
    assert(internal_conbound_new(ConBound)).
    % Sanity check:
    /*
    findall(ISym,
           (member(PCo1,ListPCos1),
            PCo1 = pco(States,StackSyms,InputSym1,Trans1,(Q0,sym(Z)),Qacc),
            member(ISym,InputSym1)
            ),ISymsUnsort),
    sort(ISymsUnsort,ISyms),
    annotate_literal(ISyms,TAs),
    write(TAs), nl,
    isvalid_formula(PresConst,context(TAs,[])).
    */
    %write(PresConst), nl.

%
globaltrans_to_presconst(GlobalTrans,ConBound,NumThreads,ModeVecCount,CtxVar,PresConst) :-
    ConBoundStrict is ConBound-1,
    get_closed_interval(0,ConBoundStrict,ConSwitchIndices),
    get_closed_interval(1,NumThreads,ThreadIndices),
    get_closed_interval(1,ModeVecCount,ModeIndices),
    PresConst = and([Domain,OneTranSwitch,Sync,AtMostConBound,CtrTestSync]),
    % Sync formula
    findall(SyncConjunct,
        (append(_,[ThreadIndex1,ThreadIndex2|_],ThreadIndices),
         member(GlobalTran,GlobalTrans),
         GlobalTran = tran(from(StateTuple1,_),_,
                           to(StateTuple2,_)),
         ( ground(StateTuple2) -> true
         ; write('ERR: StateTuple2 is not ground'), nl, fail
         ),
         member(ConSwitchIndex,ConSwitchIndices),
         SyncConjunct = 
          eq([t(pack(StateTuple1,StateTuple2,ConSwitchIndex,ThreadIndex1))],
             [t(pack(StateTuple1,StateTuple2,ConSwitchIndex,ThreadIndex2))])
        ),
        SyncConjuncts),
    Sync = and(SyncConjuncts),
    %write('Sync formula produced'), nl,
    % Domain formula
    findall(DomainConjunct,(
        member(GlobalTran,GlobalTrans),
        GlobalTran = tran(from(StateTuple1,_CtrTest),_Actions,
                           to(StateTuple2,_CtrActions)),
        member(ConSwitchIndex,ConSwitchIndices),
        %member(ThreadIndex,ThreadIndices),
        ThreadIndex is 1,
        DomainConjunct = and(
                            leq([t(pack(StateTuple1,StateTuple2,ConSwitchIndex,
                                    ThreadIndex))],[1]),
                            geq([t(pack(StateTuple1,StateTuple2,ConSwitchIndex,
                                    ThreadIndex))],[0])
                            )
              ),
              DomainConjuncts),
    Domain = and(DomainConjuncts),
    %write('Domain formula produced'), nl,
    % OneTranSwitch formula
    findall(OneTranSwitchConjunct,(
        append(GlobalTransA,[GlobalTran|GlobalTransB],GlobalTrans),
        GlobalTran = tran(from(StateTuple1,_),_,
                           to(StateTuple2,_)),
        member(ConSwitchIndex,ConSwitchIndices),
        ThreadIndex is 1,
        findall(ConsequenceConjunct,
            ((member(GlobalTranOther,GlobalTransA);
              member(GlobalTranOther,GlobalTransB)
             ),
             GlobalTranOther = tran(from(ST1,_),_,
                           to(ST2,_)),
             % Before Fix
             %not(ST1 == StateTuple1), not(ST2 == StateTuple2),
             (not(ST1 == StateTuple1); not(ST2 == StateTuple2)),
             ConsequenceConjunct = 
                eq([t(pack(ST1,ST2,ConSwitchIndex,ThreadIndex))],
                                      [0])
            ),
            ConsequenceConjuncts),
        Consequence = and(ConsequenceConjuncts),
        OneTranSwitchConjunct = 
            imply(eq([t(pack(StateTuple1,StateTuple2,ConSwitchIndex,ThreadIndex))],
                     [1]),
                  Consequence)
        ),
        OneTranSwitchConjuncts),
    OneTranSwitch = and(OneTranSwitchConjuncts),
    % AtMostConBound formula
    findall(AtMostConBoundConjunct,(
        member(ConSwitchIndex,ConSwitchIndices),
        ConSwitchIndex < ConBoundStrict,
        %member(ThreadIndex,ThreadIndices),
        ThreadIndex is 1,
        findall(PremCon,(
            member(GlobalTran,GlobalTrans),
            GlobalTran = tran(from(ST1,_),_,
                           to(ST2,_)),
            PremCon = eq([t(pack(ST1,ST2,ConSwitchIndex,ThreadIndex))],[0])
            ),
            PremConjuncts),
        Premise = and(PremConjuncts),
        NxtConSwitchIndex is ConSwitchIndex + 1,
        findall(ConsCon,(
            member(GlobalTran,GlobalTrans),
            GlobalTran = tran(from(ST1,_),_,
                           to(ST2,_)),
            ConsCon = eq([t(pack(ST1,ST2,NxtConSwitchIndex,ThreadIndex))],[0])
            ),
            ConsConjuncts),
        Consequence = and(ConsConjuncts),
        AtMostConBoundConjunct = imply(Premise,Consequence)
        ),
        AtMostConBoundConjuncts),
    AtMostConBound = and(AtMostConBoundConjuncts),
    % Counter Test Sync.
    findall(CtrTestSyncCon,
        (member(GlobalTran,GlobalTrans),
        GlobalTran = tran(from(_,Test),_,to(_,_)),
        TestMod = and(Test,eq([CtxVar],[ConSwitchIndex])),
        member(ConSwitchIndex,ConSwitchIndices),
        append(ModeIndicesA,[ModeIndex|ModeIndicesB],ModeIndices),
        Act1 = t((TestMod,ModeIndex)),
        (member(ModeIndex2,ModeIndicesA)
        ;member(ModeIndex2,ModeIndicesB)
        ),
        Act2 = t((TestMod,ModeIndex2)),
        CtrTestSyncCon = imply(geq([Act1],[1]),eq([Act2],[0]))
        ),CtrTestSyncConjuncts),
    CtrTestSync = and(CtrTestSyncConjuncts).
        

% CtxVar is the context variable.
globaltrans_to_pco(GlTrans,CtxVar,StackSyms,ThreadIndex,ConBound,PCoTrans) :-
    globaltrans_to_pco(GlTrans,CtxVar,StackSyms,ThreadIndex,ConBound,[],
        PCoTrans).

globaltrans_to_pco([],_,_,_,_,PCoTrans,PCoTrans).
globaltrans_to_pco([GlobalTran|GlobalTrans],CtxVar,StackSyms,ThreadIndex,
            ConBound,PCoTransAccum,PCoTrans) :-
    globaltran_to_pco(GlobalTran,CtxVar,StackSyms,ThreadIndex,ConBound,
        NewPCoTrans),
    append(NewPCoTrans,PCoTransAccum,NewPCoTransAccum),
    globaltrans_to_pco(GlobalTrans,CtxVar,StackSyms,ThreadIndex,ConBound,
        NewPCoTransAccum,PCoTrans).

globaltran_to_pco(GlTran,CtxVar,StackSyms,ThreadIndex,ConBound,PCoTrans) :-
        %PDATransAccum,PDATrans) :-
    GlTran = tran(from(StateTuple1,CtrTest),Actions,to(StateTuple2,CtrActions)),
    arg(ThreadIndex,StateTuple1,State1),
    arg(ThreadIndex,StateTuple2,State2),
    ConBoundStrict is ConBound-1,
    get_closed_interval(0,ConBoundStrict,ConSwitchIndices),
    findall(Triplet,(
        ( ThreadIndex == 1 ->
            PCoCtrTest = and(CtrTest,eq([CtxVar],[ConSwitchIndex])),
            PCoActions = 
             [t(pack(StateTuple1,StateTuple2,ConSwitchIndex,ThreadIndex))|Actions],
            PCoCtrActions = [(CtxVar,1)|CtrActions]
        ; PCoCtrTest = and(CtrTest,eq([CtxVar],[ConSwitchIndex])),
        %; PCoCtrTest = true, % CHANGE
          PCoActions = 
            [t(pack(StateTuple1,StateTuple2,ConSwitchIndex,ThreadIndex))],
          PCoCtrActions = []
        ),
        member(ConSwitchIndex,ConSwitchIndices),
        Triplet = (PCoCtrTest,PCoActions,PCoCtrActions)
      ),
      TestActsIncrs),
    PCoTrans = [spectran(State1,StackSyms,TestActsIncrs,State2)].


% get_newvar(VarNames,NewVar) :-
%   NewVar is a variable name that doesn't appear in the list VarNames of
%   variables (atoms).
get_newvar(VarNames,NewVar) :-
    ( VarNames = [] ->
        MaxLength is 0
    ; findall(Length,(member(Var,VarNames),
                    atom_codes(Var,Code),
                    length(Code,Length)),
                    Lengths),
      max_list(Lengths,MaxLength)
    ),
    findall(Char,(get_closed_interval(1,MaxLength,Indices),member(_,Indices),
                  Char is 114), % atom_char(r,114)
                 Chars),
    atom_codes(newvar,NewVarCode),
    append(NewVarCode,Chars,NewVarCode1),
    atom_codes(NewVar,NewVarCode1).
    
%%%%%%%%%%%%%%%%%%% End of Context-switch as a counter %%%%%%%%%%%%%%%%%%%%%%%%%

cbreach_synpco_to_z3_new2(SynPCo0,OldRevBound,ConBound,ParikhConstraint,OS) :-
    synpco_conswitch_to_counter(SynPCo0,OldRevBound,ConBound,SynPCo,PresConst,
            CtxVar),
    RevBound = [CtxVar-0|OldRevBound],
    ( isvalid_synpco(SynPCo) -> 
        true
    ; write('ERROR: SynPCo is invalid after synpco_conswitch'),
      write(SynPCo), nl,
      fail
    ),
    NewParikhConstraint = and(PresConst,ParikhConstraint),
    write('Starting Translation'), nl,
    cbreach_synpco_to_z3_new(SynPCo,RevBound,0,NewParikhConstraint,OS).

%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% synpco_example1(PCo) :-
synpco_example1(SynPCo) :-
    SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
    PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
    PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    States1 = [p0,p1],
    States2 = [q0,q1],
    StackSym1 = [sym(bot1),sym(a1)],
    StackSym2 = [sym(bot2),sym(a2)],
    InputSym1 = [t(a)],
    InputSym2 = [t(b)],
    Q01 = p0,
    Q02 = q0,
    Z_01 = bot1,
    Z_02 = bot2,
    Qacc1 = p1,
    Qacc2 = q1,
    Trans1 = [tran(from(p0,[sym(bot1)],true),[t(a)],
                   to(p0,[sym(bot1)],[(var1,1)])),
              tran(from(p0,[sym(bot1)],eq(var1,0)),[],
                   to(p1,[],[]))],
    Trans2 = [tran(from(q0,[sym(bot2)],gt([var1],[0])),[t(b)],
                   to(q0,[sym(bot2)],[(var1,-1)])),
              tran(from(q0,[sym(bot2)],eq(var1,0)),[],
                   to(q1,[],[]))],
    GlobalInputSym = [sym(s)],
    VarNames = [var1],
    GlobalTrans = [].

% synpco_example2(PCo) :-
synpco_example2(SynPCo) :-
    SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
    PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
    PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    %States1 = [p0,p1,p2],
    %States2 = [q0,q1,q2],
    States1 = [p0,p1],
    States2 = [q0,q1],
    StackSym1 = [sym(bot1),sym(a1)],
    StackSym2 = [sym(bot2),sym(a2)],
    InputSym1 = [t(a)],
    InputSym2 = [t(a)],
    Q01 = p0,
    Q02 = q0,
    Z_01 = bot1,
    Z_02 = bot2,
    Qacc1 = p1,
    Qacc2 = q1,
    Trans1 = [tran(from(p0,[sym(bot1)],true),[t(a)],
                   to(p0,[sym(bot1)],[(var1,1)]))
              %tran(from(p1,[sym(bot1)],true),[],
                   %to(p2,[],[]))
              ],
              %tran(from(p0,[sym(bot1)],eq(var1,0)),[],
                   %to(p1,[],[]))],
    Trans2 = [tran(from(q0,[sym(bot2)],gt([var1],[0])),[t(a)],
                   to(q0,[sym(bot2)],[(var1,-1)]))
              %tran(from(q1,[sym(bot2)],true),[],
                   %to(q2,[],[]))
             ],
    GlobalInputSym = [t(s)],
    VarNames = [var1],
    GlobalTrans = [tran(from(states(p0,q0),eq([var1],[0])),
                        [t(s)],
                        to(states(p1,q1),[]))
                    ].
