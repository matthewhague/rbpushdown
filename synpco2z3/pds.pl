% pds.pl
% 
% Implements relations that translate from PDA to CG.
%
% PDA is of the form pda(States,StackSym,InputSym,Trans,(Q_0,Z_0),Q_acc)
% where
% 1. States is a list of states. States are assumed to be NOT compound.
% 2. StackSym is a list of stack symbols. Elements are of form sym(...).
% 3. InputSym is a list of input symbols. Elements are of form t(...).
% 4. Trans is a list of transitions of the form
%       tran(from(q,Word1),Actions,to(q,Word2))
%       meaning that when the PDA is on state q with (a list) Word1 on top of 
%       the stack and executes the list of actions Actions and replace Word1 
%       with Word2. 
%       Currently, Word1 and Word2 pairs are allowed to be one of the following
%       (i) Word1 = [] and ( Word2 = [] or Word2 = [sym(a)] ), where
%           member(sym(a),StackSym).
%       (ii) Word1 = [sym(a)] and ( Word2 = [sym(b)] or Word2 = [sym(b),sym(c)])
%            where [sym(a),sym(b),sym(c)] is a subset of StackSym.
% 5. Q_0 is the initial state, and Z_0 is an initial stack content.
% 6. Q_acc is the accepting state.
%
% PDS is of the form pds(States,StackSym,InputSym,Trans) with the
% same description as above.
%
% See some examples below (search for the relations pda_example1/1).

:- module(pds,[pda_example1/1,pda_to_cg/2,pda_example2/1,pda_example3/1,asynpda_to_cg/2,atom_sub_atom/3,get_states_from_transitions/2,get_actions_from_transitions/2,rhs_cg_compare/3]).

:- use_module(cg,[nonterminals_cg/2,ord_put_list/4,put_cg_transitions_rhs/4,annotate_literal/2]).
:- use_module(synpco,[synpco_to_asynpda_state_compare/3,get_closed_interval/3]).

/*
% annotate_pda(+PDA,-PDAAn) :-
%   PDAAn is PDA but with its action labels annotated.
annotate_pda(PDA,PDAAn) :-
    PDA = pda(States,StackSym,InputSym,Trans,(Q_0,Z_0),Q_acc),
    PDAAn = pda(States,StackSym,InputSymAn,TransAn,(Q_0,Z_0),Q_acc),
    annotate_literal(InputSym,InputSymAn),
    annotate_pda_trans(Trans,InputSymAn,TransAn).

annotate_pda_trans([],_,[]).
annotate_pda_trans([Tran|Trans],InputSymAn,[TranAn|TransAn]) :-
    Tran = tran(from(P,Word1),Actions,to(Q,Word2)),
    findall(ActionAn,(member(t(Name),Actions),
                      memberchk(t(Name,Key),InputSymAn),
                      ActionAn = t(Name,Key)),
            ActionsAn),
    TranAn = tran(from(P,Word1),ActionsAn,to(Q,Word2)),
    annotate_pda_trans(Trans,InputSymAn,TransAn).
*/

/*
% dyn_nonterminals_from_pda(States,PDATrans) :-
%   put the set of nonterminals corresponding to PDA transitions PDATrans in a 
%   dynamic lookup table. 
dyn_nonterminals_from_pda(States,PDATrans) :-
    %findall(n(P,Z,Q),(member(P,States),
                      %member(Z,StackSyms),
                      %member(Q,States)),
                      %Ns),
    clear_dyn_nonterminals,
    dyn_nonterminals_from_pda(States,PDATrans,1).

dyn_nonterminals_from_pda(_,[],MaxCtr) :-
    assert(internal_nonterminal_maxctr(MaxCtr)).
dyn_nonterminals_from_pda(States,
        [tran(from(P,Word1),_Actions,to(Q,Word2))|Trans],Ctr):-
    Word1 = [sym(Z)],
    ( Word2 = [] ->
        ( internal_nonterminal_lookup(n((P,Z,Q)),Key) ->
            NewCtr is Ctr
        ; assertz(n((P,Z,Q)),Ctr),
          NewCtr is Ctr+1
        )
    ; Word2 = [sym(X)] -> 
        findall(tran(from(Q1,Word),[],to(Q2,[])),
            (member(Q2,States),
             ((Q1 = P,Word = Word1);
              (Q1 = Q,Word = Word2))
            ),
            NewTrans),
        append(NewTrans,Trans,Trans1),
        NewCtr is Ctr
    ; Word2 = [sym(X),sym(Y)] ->
        findall(tran(from(Q1,Word),[],to(Q2,[])),
            (member(Q2,States),
             ((Q1 = P,Word = Word1);
              (Q1 
    ; false
    ),
    dyn_
*/

% clear_dyn_nonterminals :-
%   clear the dynamic nonterminal lookup table.
clear_internal_nonterminal_key :-
    assert(internal_nonterminal_key(blah,blah)),
    (clear_internal_nonterminal_key_fail; true).

clear_internal_nonterminal_key_fail :-
    retract(internal_nontermnal_key(_,_)),
    fail.

% get_states_from_transitions(-States,+Trans) :-
%   Trans is a list of PDA transitions and States are a list of states
%   that occur in Trans.
get_states_from_transitions(States,Trans) :-
    is_list(Trans),
    not((member(X,Trans),
        not((X = tran(from(_,Word1),Actions,to(_,Word2)),
            is_list(Word1),
            is_list(Word2),
            is_list(Actions))
            ;
            (X = spectran(_,_,_,_))
            )
       )),
    findall(State,(member(Tran,Trans),
                   ( Tran = tran(from(State1,_),_,to(State2,_))
                   ; Tran = spectran(State1,_,_,State2)
                   ),
                   (State = State1; State = State2)
                  ),
                  StatesUnsort),
    sort(StatesUnsort,States).


% get_actions_from_transitions(-Actions,+Trans) :-
%   Trans is a list of PDA transitions and Actions are a list of actions
%   that occur in Trans.
get_actions_from_transitions(Actions,Trans) :-
    is_list(Trans),
    not((member(X,Trans),
        not((X = tran(from(_,Word1),Actions,to(_,Word2)),
            is_list(Word1),
            is_list(Word2),
            is_list(Actions))
            ;
            (X = spectran(_,_,_,_))
            )
       )),
    findall(Action,(member(Tran,Trans),
                    (Tran = tran(from(_,_),ActionsTran,to(_,_))
                    ; Tran = spectran(_,_,ActionSeqs,_),
                      member(ActionsTran,ActionSeqs)
                    ),
                    member(Action,ActionsTran)
                   ),
                   ActionsUnsort),
    sort(ActionsUnsort,Actions).

% isvalid_pda(PDA) :-
%   PDA is a valid PDA.
isvalid_pda(pda(States,StackSym,InputSym,Trans,(Q_0,Z_0),Q_acc)) :-
    memberchk(Q_0,States),
    memberchk(Z_0,StackSym),
    memberchk(Q_acc,States),
    isvalid_pds(pds(States,StackSym,InputSym,Trans)).

% isvalid_pds(PDS) :-
%   PDS is a valid PDS.
isvalid_pds(pds(States,StackSym,InputSym,[])) :-
    !,
    is_list(StackSym),
    is_list(InputSym),
    not((member(Y,InputSym),not(Y = t(_)),
         write('84: '), write(Y), nl)),
    is_list(States),
    not((member(Z,StackSym),not(Z = sym(_)),
         write('88: '), write(Z), nl)).
    %not((member(X,States),compound(X))).

isvalid_pds(pds(States,StackSym,InputSym,[Tran|Trans])) :-
    Tran = tran(from(P,Word1),Actions,to(Q,Word2)),
    !,
    ( memberchk(P,States) ->
        true
    ; write('95: '), write(P), nl, fail),
    ( memberchk(Q,States) ->
        true
    ; write('98: '), write(P), nl, fail),
    /* Old Version (a la Sipser)
    ( Word1 = [], (Word2 = []; Word2 = [sym(Z)], memberchk(sym(Z),StackSym) )
    ; Word1 = [sym(X)], memberchk(sym(X),StackSym),
      (Word2 = []
      ;Word2 = [sym(Y)], memberchk(sym(Y),StackSym)
      ;Word2 = [sym(Y1),sym(Y2)], memberchk(sym(Y1),StackSym),
       memberchk(sym(Y2),StackSym)
      )
    ),
    */
    % New style (a la Matt's)
    ((
    Word1 = [sym(X)], memberchk(sym(X),StackSym),
    (Word2 = []
    ;Word2 = [sym(Y)], memberchk(sym(Y),StackSym)
    ;Word2 = [sym(Y1),sym(Y2)], memberchk(sym(Y1),StackSym),
     memberchk(sym(Y2),StackSym)
    )
    ) -> true
    ;write('Checking word pairs fail'), write(Word1), write(Word2), fail
    ),
    ( subset(Actions,InputSym) -> true
    ; write('113: '), write(Actions), nl, fail),
    isvalid_pds(pds(States,StackSym,InputSym,Trans)).

isvalid_pds(pds(States,StackSym,InputSym,[Tran|Trans])) :-
    Tran = spectran(P,StackSymSubset,ActSeqs,Q),
    ( memberchk(P,States) ->
        true
    ; write('ERROR: isvalid_pds: P not member of States'), nl, fail
    ),
    ( memberchk(Q,States) ->
        true
    ; write('ERROR: isvalid_pds: Q not member of States'), nl, fail),
    ( subset(StackSymSubset,StackSym) ->
        true
    ; write('ERROR: isvalid_pds: StackSymSubset is not a subset of StackSym'), nl, fail),
    forall(member(ActSeq,ActSeqs),
        (subset(ActSeq,InputSym) ->
            true
        ; write('ERROR: isvalid_pds: ActSeq not member of InputSym'), nl, fail
        )
        ),
    isvalid_pds(pds(States,StackSym,InputSym,Trans)).


/*
asynpda_add_action_count(AsynPDA1,AsynPDA2) :-
    findall(PDA2,(member(PDA1,AsynPDA1),pda_add_action_count(PDA1,PDA2)),
        AsynPDA2).

pda_add_action_count(PDA1,PDA2) :-
    PDA1 = pda(States,StackSym,InputSym,Trans1,Start,Q_acc),
    PDA2 = pda(States,StackSym,InputSym,Trans2,Start,Q_acc),
    findall(Tran2,(member(Tran1,Trans1),
                   Tran1 = tran(from(P,Stack1),Actions,to(Q,Stack2)),
                   findall((Action,1),member(Action,Actions),A
             tran(from(p_0,[sym(0)]),[t((0,0))],to(p_0,[sym(0),sym(0)])),
*/

% asynpda_to_cg(+AsynPDA,-CG) :-
%   CG is a commutative grammar corresponding to asynchronous PDA AsynPDA.
% 
% Asynchronous PDA is nothing but a nonempty list of PDAs. It is assumed that
% NO TWO PDAs HAVE THE SAME STATE NAMES.
asynpda_to_cg(AsynPDA,cg(Ns,Ts,Trans,Start)) :-
    is_list(AsynPDA),
    not(AsynPDA == []),
    findall(CG,(member(PDA,AsynPDA),pda_to_cg(PDA,CG)),CGs),
    % Sanity Check
    length(AsynPDA,Len),
    length(CGs,Len),
    % Make a new start nonterminal called 'start'
    % By construction, this name will not be a name of any nonterminal in
    % CGs.
    Start = n(start),
    findall(N,(member(cg(Nsi,_,_,_),CGs),
               member(N,Nsi)),
               NsMinStartUnsort),
    sort(NsMinStartUnsort,NsMinStart), % no same state names in two PDAs :)
    Ns = [Start|NsMinStart],
    findall(T,(member(cg(_,Tsi,_,_),CGs),
               member(T,Tsi)),
               TsUnsort),
    sort(TsUnsort,Ts), % to eliminate double occurrences of same terminal names
    % Check
    /*
    findall(t(pack(A,B,C,D)),member(t(pack(A,B,C,D)),Ts),TsCheck),
    write('TsCheck is: '), write(TsCheck), nl,
    findall(t(pack(A,B,C,D)),(member(PDA,AsynPDA),
                      PDA = pda(_,_,ISS,_,_,_),
                      write('yes'), nl,
                      member(t(pack(A,B,C,D)),ISS)),TsCheck1),
    write('TsCheck1 is: '), write(TsCheck1), nl,
    */
    %% End Check
    findall(Tran,(member(cg(_,_,Transi,_),CGs),
                  member(Tran,Transi)),
                  TransMinStart),
    TransStart = tran(Start,[RHS]),
    findall((Starti,1),member(cg(_,_,_,Starti),CGs),RHSTemp),
    sort(RHSTemp,RHS),
    Trans = [TransStart|TransMinStart].

    



% eliminates transitions with more than one actions.
% Eacn transition in the resulting PDA has Actions with at most one member.
% simplify_pds

pda_to_cg(pda(States,StackSym,InputSym,Trans,(Q_0,sym(Z_0)),Q_acc),CG) :-
    % sanity check
    write('Start Checking PDA ...'), nl,
    isvalid_pda(pda(States,StackSym,InputSym,Trans,(Q_0,sym(Z_0)),Q_acc)),
    write('Checking PDA successful'), nl,
    % actual work
    CG = cg(Ns,Ts,CGTrans,Start),
    Ts = InputSym,
    /*
    findall(n(PSubZSubQ), (member(P,States), 
                     member(sym(Z),StackSym),
                     member(Q,States),
                     %atom_sub_atom(P,Z,PSubZ),
                     %atom_sub_atom(PSubZ,Q,PSubZSubQ)
                     PSubZSubQ = (P,Z,Q)
                     ),
                     Ns),
    findall(tran(n(P_Z_Q),TransP_Z_Q), (member(P,States),
                                 member(sym(Z),StackSym),
                                 member(Q,States),
                                 %atom_sub_atom(P,Z,P_Z),
                                 %atom_sub_atom(P_Z,Q,P_Z_Q),
                                 P_Z_Q = (P,Z,Q),
                                 findall(TranP_Z_Q,
                                     p_z_q_transitions(P,Z,Q,
                                        pds_part(States,Trans),
                                        TranP_Z_Q),
                                     TransP_Z_Q)
                                    ),
                                   CGTrans),
    */
    Q0_Z0_Qacc = (Q_0,Z_0,Q_acc),
    Start = n(Q0_Z0_Qacc),
    %write('YES YES'), nl,
    pda_to_cg_transitions(Trans,(Q_0,sym(Z_0)),Q_acc,CGTrans),
    %write('YES YES'), nl,
    ( nonterminals_cg(Ns,CGTrans) -> true
    ; write('ERR: cannot get nonterminals for CGTrans'), nl, fail
    ).
    %write('YES YES'), nl.
    %atom_sub_atom(Q_0,Z_0,Q0_Z0),
    %atom_sub_atom(Q0_Z0,Q_acc,Q0_Z0_Qacc),

% Assumed that internal_nonterminal_key has been initialized
get_nonterminal_key(N,NKey) :-
    ( internal_nonterminal_key(N,Key) ->
        NKey = Key
    ; internal_nonterminal_key(_,MaxKey), !, % this is because of asserta
      NewMaxKey is MaxKey + 1,
      asserta(internal_nonterminal_key(N,NewMaxKey)),
      NKey = NewMaxKey
    ).


% put_cg_transitions_rhs_assoc(+Key,+LHS,+RHS,+Assoc,-NewAssoc).
% similar to put_cg_transitions_rhs in cg.pl but with association lists
% implementation.
put_cg_transitions_rhs_assoc(Key,LHS,RHS,Assoc,NewAssoc) :-
    ( get_assoc(Key,Assoc,Value) ->
        Value = tran(LHS,ListofRHS),
        put_assoc(Key,Assoc,tran(LHS,[RHS|ListofRHS]),NewAssoc)
        %( memberchk(RHS,ListofRHS) -> write('Warning: sth is wrong'), nl
        %; put_assoc(Key,Assoc,tran(LHS,[RHS|ListofRHS]),NewAssoc)
        %)
    ; put_assoc(Key,Assoc,tran(LHS,[RHS]),NewAssoc)
    ).

% for more than one rhs (i.e. sequences of rhs)
put_cg_transitions_rhss_assoc(_Key,_LHS,[],Assoc,Assoc) :- !.
put_cg_transitions_rhss_assoc(Key,LHS,[RHS|RHSs],Assoc,NewAssoc) :-
    put_cg_transitions_rhs_assoc(Key,LHS,RHS,Assoc,NewAssoc1),
    put_cg_transitions_rhss_assoc(Key,LHS,RHSs,NewAssoc1,NewAssoc).

get_endstates(Q,Z1,EndStates) :-
    ( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
          synpco:internal_modeveccount(ModeVecCount),
          synpco:internal_conbound(ConBound),
        ( Q = (_,_,_) ->
           Q = (State,Mode,Con),
           findall(EndState,(
            (internal_pda_lookup_Sold_opt(State,Z1,State1,DiffI,DiffJ),
            NextModeLow is Mode+DiffI,
            NextModeLow =< ModeVecCount,
            NextCon is Con+DiffJ,
            NextCon =< ConBound,
            %NextConLow is Con+DiffJ,
            %NextConLow =< ConBound,
            get_closed_interval(NextModeLow,ModeVecCount,ModeIndices),
            %get_closed_interval(NextConLow,ConBound,ConSwitchIndices),
            member(NextMode,ModeIndices),
            %member(NextCon,ConSwitchIndices),
            EndState = (State1,NextMode,NextCon)
            ; internal_pda_lookup_Sold_opt(State,Z1,Qacc),
              EndState = Qacc
            )),EndStates)
        ; EndStates = [Q]
        )
    ; findall(EndState,internal_pda_lookup_Sold(Q,Z1,EndState),EndStates)
    ).

/*
put_cg_transitions_rhs_assoc(LHS,RHS,[],[tran(LHS,[RHS])]) :- !.
put_cg_transitions_rhs(LHS,RHS,[tran(LHS1,ListofRHS)|Trans],NewTrans) :-
    ( LHS @< LHS1 ->
        NewTrans = [tran(LHS,[RHS]),tran(LHS1,ListofRHS)|Trans]
    ; LHS == LHS1 ->
        NewTrans = [tran(LHS,[RHS|ListofRHS])|Trans]
    ; NewTrans = [tran(LHS1,ListofRHS)|NewTrans1],
      put_cg_transitions_rhs(LHS,RHS,Trans,NewTrans1)
    ).
*/

% annotate pda rules actions with count information
%annotate_pda_rules([],[]).
%annotate_pda_rules([tran(From,Actions,To)|Trans],
                   %[tran(From,ActionsMod,To)|NewTrans]) :-
    %findall((X,1),member(X,Actions),ActionsMod),

% pda_to_cg_transitions(+States,+PDATrans,-CGTrans) :-
%   CGTrans are the CG transitions obtained from the PDA transitions
%   PDTrans and states States of the PDA.
pda_to_cg_transitions(PDATrans,(Q0,sym(Z0)),Qacc,CGTrans) :-
    ( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
        build_reachability_table_synpco(PDATrans),
        optimize_pda_lookup_table_synpco((Q0,Z0),Qacc)
        %move_Sold_opt
    ; build_reachability_table(PDATrans),
      optimize_pda_lookup_table((Q0,Z0),Qacc)
    ),
    %print_reachability_table,
    !,
    write('Reachability table is successfully built'), nl,
    reachability_table_count(Count),
    write('There are '), write(Count), write(' entries in reachability table'),nl,
    % Test
    %( internal_pda_lookup_Sold_opt(v_q_lk0_ret0_pd1_cs,v_dm_register_target_0_rv0_v0_pd1,v_q_lk1_ret0_pd1,DiffI0,DiffJ0) ->
        %write('Found such entry!!'), write((DiffI0,DiffJ0)), nl
    %; true
    %),
    %( internal_pda_lookup_Sold(q0,bot,qacc) -> write('success'), nl
    %( (internal_pda_lookup_Sold_opt(P,Z,Q,DiffI0,DiffJ0),DiffJ0 > 1,
       %not((internal_pda_lookup_Sold_opt(P,Z,Q,_DiffI1,DiffJ1),
            %DiffJ1 =:= 0))
       %) ->
        %write((P,Z,Q,DiffI0,DiffJ0)), nl
    %; write('NOTHING'), nl
    %),
    %; fail
    %),%test
    asserta(internal_nonterminal_key(null,0)),
    %retract(internal_nonterminal_key(blah,blah)),
    empty_assoc(AssocInit),
    pda_to_cg_transitions(PDATrans,AssocInit,AssocFinal),
    assoc_to_values(AssocFinal,CGTransRev),
    reverse(CGTransRev,CGTrans), % for the hell of it, makes z3 faster
    %CGTrans = CGTransRev,
    %%
    ( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
        remove_Sold_synpco
    ; remove_Sold
    ), 
    clear_internal_nonterminal_key.

pda_to_cg_transitions([],CGTransAccum,CGTransAccum) :- !.
pda_to_cg_transitions(
    [tran(from(P,[sym(Z)]),Actions,to(Q,[]))|
    PDATrans],CGTransAccum,CGTrans) :-
    !,
    findall((X,1),member(X,Actions),ActionsMod),
    %predsort(rhs_cg_compare,ActionsMod,ActionsSorted),
    ActionsSorted = ActionsMod,
    %%
    get_nonterminal_key(n((P,Z,Q)),Key),
    put_cg_transitions_rhs_assoc(Key,n((P,Z,Q)),ActionsSorted,CGTransAccum,
                                                    CGTransAccum1),
    pda_to_cg_transitions(PDATrans,CGTransAccum1,CGTrans).

pda_to_cg_transitions(
    [tran(from(P,[sym(Z)]),Actions,to(Q,[sym(Z1)]))|
    PDATrans],CGTransAccum,CGTrans) :-
    !,
    findall((X,1),member(X,Actions),ActionsMod),
    %predsort(rhs_cg_compare,ActionsMod,ActionsSorted),
    ActionsSorted = ActionsMod,
    get_endstates(Q,Z1,EndStates),
    pda_to_cg_transitions_samestacklevel(
        package(P,Z,Q,Z1,EndStates,ActionsSorted),
        PDATrans,CGTransAccum,CGTrans).

pda_to_cg_transitions(
    [spectran(_P,_StackSymSubset,[],_Q)|
    PDATrans],CGTransAccum,CGTrans) :-
    !,
    pda_to_cg_transitions(PDATrans,CGTransAccum,CGTrans).

pda_to_cg_transitions(
    [spectran(P,StackSymSubset,ActSeqs,Q)|
    PDATrans],CGTransAccum,CGTrans) :-
    not( ActSeqs == [] ),
    !,
    /* Test */
    %( member(Z,StackSymSubset) -> true
    %; write('StackSymSubset is empty'), nl
    %),
    %member(_,StackSymSubset),
    %forall(member(Z,StackSymSubset),
        %(( get_endstates(Q,Z,EndStates) -> true
        %; write('ERR: gen_endstates fails'), nl
        %),
        %write(EndStates), nl)
        %),
    %get_endstates(Q,Z,EndStates),
    %write(EndStates), nl,
    %%%%
    ( (member(sym(Z),StackSymSubset),
            get_endstates(Q,Z,EndStates),not(EndStates == []))
       ->
      findall(ActSeqMod,
        (member(ActSeq,ActSeqs),
         findall((X,1),member(X,ActSeq),ActSeqMod)
        ),ActSeqsMod),
      get_nonterminal_key(n(spec(P,StackSymSubset,Q)),NKey),
      put_cg_transitions_rhss_assoc(NKey,n(spec(P,StackSymSubset,Q)),
                    ActSeqsMod,CGTransAccum,CGTransAccum1),
      findall(PDATranAux,
          (member(sym(A),StackSymSubset),
           PDATranAux = 
                tran(from(P,[sym(A)]),[n(spec(P,StackSymSubset,Q))],
                     to(Q,[sym(A)]))
          ),PDATransAux),
      %write('Im here'), nl,
      %not( PDATransAux == [] ),
      %write('Im here out'), nl,
      append(PDATransAux,PDATrans,PDATrans1),
      pda_to_cg_transitions(PDATrans1,CGTransAccum1,CGTrans)
    ; %write('Im here 2'), nl,
      pda_to_cg_transitions(PDATrans,CGTransAccum,CGTrans)
    ).

pda_to_cg_transitions(
    [tran(from(Q,[sym(A)]),Actions,to(Q1,[sym(B),sym(C)]))|
    PDATrans],CGTransAccum,CGTrans) :-
    findall((X,1),member(X,Actions),ActionsMod),
    %predsort(rhs_cg_compare,ActionsMod,ActionsSorted),
    ActionsSorted = ActionsMod,
    ( current_predicate(synpco_optimize_flag,synpco_optimize_flag) -> 
        synpco:internal_modeveccount(ModeVecCount),
        synpco:internal_conbound(ConBound),
        ( Q1 = (_,_,_) ->
          Q1 = (State,Mode,Con),
          findall((RTuple,PTuple),
            ((internal_pda_lookup_Sold_opt(State,C,R,DiffI0,DiffJ0),
             NextModeLow0 is Mode+DiffI0,
             NextModeLow0 =< ModeVecCount,
             NextConR is Con+DiffJ0,
             NextConR =< ConBound,
             %NextConLow0 is Con+DiffJ0,
             %NextConLow0 =< ConBound,
               (internal_pda_lookup_Sold_opt(R,B,P,DiffI1,DiffJ1),
               NextModeLow0+DiffI1 =< ModeVecCount,
               NextConP is NextConR+DiffJ1,
               NextConP =< ConBound,
               NextModeUp is ModeVecCount - DiffI1,
               %NextConUp is ConBound - DiffJ1,
               get_closed_interval(NextModeLow0,NextModeUp,ModeIndices0),
               %get_closed_interval(NextConLow0,NextConUp,ConSwitchIndices0),
               member(NextModeR,ModeIndices0),
               %member(NextConR,ConSwitchIndices0),
               get_closed_interval(NextModeR,ModeVecCount,ModeIndices1),
               %get_closed_interval(NextConR,ConBound,ConSwitchIndices1),
               member(NextModeP,ModeIndices1),
               %member(NextConP,ConSwitchIndices1),
               RTuple = (R,NextModeR,NextConR),
               PTuple = (P,NextModeP,NextConP)
               ;internal_pda_lookup_Sold_opt(R,B,P),
               get_closed_interval(NextModeLow0,ModeVecCount,ModeIndices0),
               member(NextModeR,ModeIndices0),
               RTuple = (R,NextModeR,NextConR),
               PTuple = P
               )
             ; internal_pda_lookup_Sold_opt(State,C,Qacc),
               RTuple = Qacc,
               PTuple = Qacc
             )),
             MidEndStatePairs)
        ; MidEndStatePairs = [(Q1,Q1)]
        )
    ; findall((R,P),
        (internal_pda_lookup_Sold(Q1,C,R),
         internal_pda_lookup_Sold(R,B,P)),
         MidEndStatePairs)
    ),
    pda_to_cg_transitions_push(
        package(Q,A,Q1,B,C,MidEndStatePairs,ActionsSorted),
        PDATrans,CGTransAccum,CGTrans).

pda_to_cg_transitions_samestacklevel(
    package(_P,_Z,_Q,_Z1,[],_),PDATrans,CGTransAccum,CGTrans) :-
        !,
        pda_to_cg_transitions(PDATrans,CGTransAccum,CGTrans).

pda_to_cg_transitions_samestacklevel(
    package(P,Z,Q,Z1,[P1|StatePack],Actions),PDATrans,CGTransAccum,
        CGTrans) :-
    %( current_predicate(synpco_optimize_flag,_) ->
        %synpco_to_asynpda_state_compare(Rel,P,P1),
        %synpco_to_asynpda_state_compare(Rel1,Q,P1),
        %( (Rel == '<',Rel1 == '<') -> 
            %ord_put_list(rhs_cg_compare,
                    %(n((Q,Z1,P1)),1),Actions,Actions1),
            %put_cg_transitions_rhs(n((P,Z,P1)),Actions1,
                    %CGTransAccum,CGTransAccum1)
        %; CGTransAccum1 = CGTransAccum
        %)
    %; 
    ord_put_list(rhs_cg_compare,(n((Q,Z1,P1)),1),Actions,Actions1),
    get_nonterminal_key(n((P,Z,P1)),Key),
    put_cg_transitions_rhs_assoc(Key,n((P,Z,P1)),Actions1,CGTransAccum,
        CGTransAccum1),
    pda_to_cg_transitions_samestacklevel(
        package(P,Z,Q,Z1,StatePack,Actions),PDATrans,CGTransAccum1,
        CGTrans).

pda_to_cg_transitions_push(
    package(_Q,_A,_Q1,_B,_C,[],_),PDATrans,CGTransAccum,CGTrans) :-
    !,
    pda_to_cg_transitions(PDATrans,CGTransAccum,CGTrans).

/*
pda_to_cg_transitions_push(
    package(Q,A,Q1,B,C,[_P|StatesPack],[],Actions),
    PDATrans,CGTransAccum,CGTrans) :-
    !,
    pda_to_cg_transitions_push(
        package(Q,A,Q1,B,C,StatesPack,States,Actions),
        PDATrans,CGTransAccum,CGTrans).
*/

pda_to_cg_transitions_push(
    package(Q,A,Q1,B,C,[(R,P)|StatesPack],Actions),
    PDATrans,CGTransAccum,CGTrans) :-
    %( current_predicate(synpco_optimize_flag,_) ->
        %synpco_to_asynpda_state_compare(Rel1,Q,P),
        %synpco_to_asynpda_state_compare(Rel2,Q1,R),
        %synpco_to_asynpda_state_compare(Rel3,R,P),
        %( (Rel1 == '<',Rel2 == '<', Rel3 == '<') -> 
            %ord_put_list(rhs_cg_compare,
                %(n((Q1,C,R)),1),Actions,Actions1),
            %ord_put_list(rhs_cg_compare,
                %(n((R,B,P)),1),Actions1,Actions2),
            %put_cg_transitions_rhs(n((Q,A,P)),Actions2,
                %CGTransAccum,CGTransAccum1)
        %; CGTransAccum1 = CGTransAccum
        %)
    %; 
    ord_put_list(rhs_cg_compare,
        (n((Q1,C,R)),1),Actions,Actions1),
    ord_put_list(rhs_cg_compare,
        (n((R,B,P)),1),Actions1,Actions2),
    get_nonterminal_key(n((Q,A,P)),Key),
    put_cg_transitions_rhs_assoc(Key,n((Q,A,P)),Actions2,CGTransAccum,
        CGTransAccum1),
    pda_to_cg_transitions_push(
        package(Q,A,Q1,B,C,StatesPack,Actions),
        PDATrans,CGTransAccum1,CGTrans).



% p_z_q_transitions(P,Z,Q,pds_part(States,Trans),TranP_Z_Q) :-
%   TranP_Q is a RHS of CG transition (in the above translation with respect to
%   pds_part(States,Trans)) where the LHS is P_Z_Q.
p_z_q_transitions(P,Z,Q,pds_part(_,Trans),TranP_Z_Q) :-
    member(tran(from(P,[sym(Z)]),Actions,to(Q,[])),Trans),
    findall((X,1),member(X,Actions),ActionsMod),
    predsort(rhs_cg_compare,ActionsMod,TranP_Z_Q).

p_z_q_transitions(P,Z,Q,pds_part(_,Trans),TranP_Z_Q) :-
    member(tran(from(P,[sym(Z)]),Actions,to(Q1,[sym(Z1)])),Trans),
    findall((X,1),member(X,Actions),ActionsMod),
    %atom_sub_atom(Q1,Z1,Q1_Z1),
    %atom_sub_atom(Q1_Z1,Q,Q1_Z1_Q),
    Q1_Z1_Q = (Q1,Z1,Q),
    predsort(rhs_cg_compare,[(n(Q1_Z1_Q),1)|ActionsMod],TranP_Z_Q).

p_z_q_transitions(P,Z,Q,pds_part(States,Trans),TranP_Z_Q) :-
    member(tran(from(P,[sym(Z)]),Actions,to(Q1,[sym(Z1),sym(Z2)])),Trans),
    member(R,States),
    findall((X,1),member(X,Actions),ActionsMod),
    %atom_sub_atom(Q1,Z2,Q1_Z2),
    %atom_sub_atom(Q1_Z2,R,Q1_Z2_R),
    Q1_Z2_R = (Q1,Z2,R),
    %atom_sub_atom(R,Z1,R_Z1),
    %atom_sub_atom(R_Z1,Q,R_Z1_Q),
    R_Z1_Q = (R,Z1,Q),
    predsort(rhs_cg_compare,
        [(n(Q1_Z2_R),1),(n(R_Z1_Q),1)|ActionsMod],TranP_Z_Q).
      

p_q_transitions(P,P,_,[]).
p_q_transitions(P,Q,pds_part(States,_),TranP_Q) :-
    member(R,States),
    atom_sub_atom(P,R,P_R),
    atom_sub_atom(R,Q,R_Q),
    TranP_QUnsort = [(n(P_R),1),(n(R_Q),1)],
    predsort(rhs_cg_compare,TranP_QUnsort,TranP_Q).
    
p_q_transitions(P,Q,pds_part(_,Trans),TranP_Q) :-
    member(tran(from(P,[]),Actions1,to(R,[T])),Trans),
    member(tran(from(S,[T]),Actions2,to(Q,[])),Trans),
    findall((X,1),member(X,Actions1),Actions1Mod),
    findall((Y,1),member(Y,Actions2),Actions2Mod),
    atom_sub_atom(R,S,R_S),
    append(Actions1Mod,[(n(R_S),1)|Actions2Mod],TranP_QUnsort),
    predsort(rhs_cg_compare,TranP_QUnsort,TranP_Q).

% Warning: This is old version
% p_q_transitions(P,Q,pds_part(States,Trans),TranP_Q) :-
%   TranP_Q is a CG transition (in the above translation with respect to
%   pds_part(States,Trans)) where the LHS is P_Q.
p_q_transitions(P,P,_,[]).
p_q_transitions(P,Q,pds_part(States,_),TranP_Q) :-
    member(R,States),
    atom_sub_atom(P,R,P_R),
    atom_sub_atom(R,Q,R_Q),
    TranP_QUnsort = [(n(P_R),1),(n(R_Q),1)],
    predsort(rhs_cg_compare,TranP_QUnsort,TranP_Q).
    
p_q_transitions(P,Q,pds_part(_,Trans),TranP_Q) :-
    member(tran(from(P,[]),Actions1,to(R,[T])),Trans),
    member(tran(from(S,[T]),Actions2,to(Q,[])),Trans),
    findall((X,1),member(X,Actions1),Actions1Mod),
    findall((Y,1),member(Y,Actions2),Actions2Mod),
    atom_sub_atom(R,S,R_S),
    append(Actions1Mod,[(n(R_S),1)|Actions2Mod],TranP_QUnsort),
    predsort(rhs_cg_compare,TranP_QUnsort,TranP_Q).

rhs_cg_compare(Order,(Term1,_),(Term2,_)) :- 
    compare(Order,Term1,Term2).

% atom_sub_atom(+Atom1,+Atom2,?Atom1_Atom2) :-
%   Atom1_Atom2 is simply Atom1 appended by '_' appended by Atom2.
atom_sub_atom(Atom1,Atom2,Atom1_Atom2) :-
                     atom_codes(Atom1,Atom1Code),
                     atom_codes('_',Sub),
                     atom_codes(Atom2,Atom2Code),
                     append(Sub,Atom2Code,SubAtom2Code),
                     append(Atom1Code,SubAtom2Code,Atom1_Atom2Code),
                     atom_codes(Atom1_Atom2,Atom1_Atom2Code).

% Convert special transitions in Trans to normal transitions
% and remove the action symbols.
spectrans_to_trans(Trans,Trans1) :-
    ( member(spectran(_,_,_,_),Trans) ->
        synpco:internal_conbound_new(ConBound),
        get_closed_interval(0,ConBound,ConSwitchIndices),
        findall(TranAux,
            (member(Tran,Trans),
            (Tran = spectran(P,StackSymSubset,_,Q) ->
                P = (StateP,ModeP,_),
                Q = (StateQ,ModeQ,_),
                append(_,[I,SuccI|_],ConSwitchIndices),
                NewP = (StateP,ModeP,I),
                NewQ = (StateQ,ModeQ,SuccI),
                member(Sym,StackSymSubset),
                TranAux = tran(from(NewP,[Sym]),[],to(NewQ,[Sym]))
            ; TranAux = Tran
            )
        ),TransAuxUnsort),
        sort(TransAuxUnsort,Trans1),
        ( member(tran(from(_,[_X]),_,to(_,[])),Trans1) -> true
        ; write('ERROR somewhere'), nl, 
          write('New Trans'), nl,
          write(Trans1), nl, 
          write('Old Trans'), nl,
          write(Trans), nl, fail
        )
     ; Trans1 = Trans
     ).


% build_reachability_table(+Trans) :-
%       build a reachability table internal_pda_reachability/3 for Trans.
% The relation internal_pda_reachability/3 has arguments (P,Z,Q) which
% records that (P,Z) ->* Q, where P,Q are states and Z is a stack symbol.
% This relation is the same as the one given in Walukiewicz's FSTTCS'00 paper.
%
% This predicate assumes that PDA is a valid pushdown automaton.
build_reachability_table(Trans) :-
         build_pda_lookup_table(Trans),
         init_Ss,
         remove_first_type_rule,
         %print_snew_table,
         %write('blahblah'), nl,
         build_reachability_table_fixpoint,
         remove_internal_tables.
         %print_reachability_table.

build_reachability_table_synpco(Trans) :-

         spectrans_to_trans(Trans,Trans1),
         build_pda_lookup_table_synpco(Trans1),
         %print_internal_pda_lookup_synpco, for debugging
         init_Ss_synpco,
         remove_first_type_rule_synpco,
         %write('blahblah'), nl,
         build_reachability_table_fixpoint_synpco,
         %print_reachability_table_synpco, % for debugging
         remove_internal_tables_synpco.

reachability_table_count(Count) :-
    findall(Rule,
    (internal_pda_lookup_Sold_opt(P,Z,Q,DiffI0,DiffJ0),
     Rule = (P,Z,Q,DiffI0,DiffJ0)
    ;internal_pda_lookup_Sold_opt(P,Z,Q),
     Rule = (P,Z,Q)
    ),Rules),
    length(Rules,Count).
    

move_Sold_opt :-
    synpco:internal_modeveccount(ModeVecCount),
    synpco:internal_conbound(ConBound),
    write(ModeVecCount), nl,
    write(ConBound), nl,
    get_closed_interval(1,ModeVecCount,ModeIndices),
    get_closed_interval(0,ConBound,ConSwitchIndices),
    (move_Sold_opt_fail(ModeIndices,ConSwitchIndices); true).
    

move_Sold_opt_fail(ModeIndices,ConSwitchIndices) :-
    internal_pda_lookup_Sold_opt(P,Z,Q,DiffI,DiffJ),
    retract(internal_pda_lookup_Sold_opt(P,Z,Q,DiffI,DiffJ)),
    synpco:internal_modeveccount(ModeVecCount),
    synpco:internal_conbound(ConBound),
    forall( (member(Mode,ModeIndices), 
             NewMode is Mode + DiffI,
             NewMode =< ModeVecCount,
             member(Con,ConSwitchIndices),
             NewCon is Con + DiffJ,
             NewCon =< ConBound),
           ( internal_pda_lookup_Sold((P,Mode,Con),Z,(Q,NewMode,NewCon)) ->
                true
           ; assert(internal_pda_lookup_Sold((P,Mode,Con),Z,(Q,NewMode,NewCon)))
           )
           ),
    fail.

move_Sold_opt_fail(ModeIndices,ConSwitchIndices) :-
    internal_pda_lookup_Sold_opt(P,Z,Q),
    not(P == Q),
    retract(internal_pda_lookup_Sold_opt(P,Z,Q)),
    forall( (member(X,ModeIndices),member(Y,ConSwitchIndices)),
            ( internal_pda_lookup_Sold((P,X,Y),Z,Q) ->
                true
            ; assert(internal_pda_lookup_Sold((P,X,Y),Z,Q)) 
            )
          ),
    fail.
             
move_Sold_opt_fail(_,_) :-
    internal_pda_lookup_Sold_opt(P,Z,Q),
    P == Q, %this is when both are final states
    retract(internal_pda_lookup_Sold_opt(P,Z,Q)),
    ( internal_pda_lookup_Sold(P,Z,Q) ->
        true
    ; assert(internal_pda_lookup_Sold(P,Z,Q)) 
    ),
    fail.

print_snew_table :-
    print_snew_table_fail; true.

print_snew_table_fail :-
    internal_pda_lookup_Snew(P,Z,Q),
    write((P,Z,Q)), nl,
    fail.

print_reachability_table :-
    print_reachability_table_fail; true.

print_reachability_table_fail :-
    internal_pda_lookup_Sold_opt(P,Z,Q),
    write((P,Z,Q)), nl,
    fail.

print_reachability_table_synpco :-
    open('reach_table.txt',write,OS),
    (print_reachability_table_fail_synpco(OS); true),
    close(OS).

print_reachability_table_fail_synpco(OS) :-
    internal_pda_lookup_Sold_opt(P,Z,Q),
    write(OS,(P,Z,Q)), nl(OS),
    fail.

print_reachability_table_fail_synpco(OS) :-
    internal_pda_lookup_Sold_opt(P,Z,Q,DiffI,DiffJ),
    write(OS,(P,Z,Q,DiffI,DiffJ)), nl(OS),
    fail.

print_internal_pda_lookup_synpco :-
    print_internal_pda_lookup_fail_synpco; true.

print_internal_pda_lookup_fail_synpco :-
    internal_pda_lookup(P,Stack1,Q,Stack2,DiffI,DiffJ),
    write((P,Stack1,Q,Stack2,DiffI,DiffJ)), nl,
    fail.

print_internal_pda_lookup_fail_synpco :-
    internal_pda_lookup(P,Stack1,Q,Stack2),
    write((P,Stack1,Q,Stack2)), nl,
    fail.

build_reachability_table_fixpoint :-
    ( internal_pda_lookup_Snew(_,_,_) ->
        %remove_Snew,
        internal_pda_lookup_Snew(P,Z,Q),
        !,
        retract(internal_pda_lookup_Snew(P,Z,Q)),
        ( internal_pda_lookup_Sold(P,Z,Q) ->
            true
        ; assert(internal_pda_lookup_Sold(P,Z,Q)),
          add_rules_to_Snew(P,Z,Q)
          %update_Sold,
          %update_Sprev,
        ),
        build_reachability_table_fixpoint
    ; true
    ).

build_reachability_table_fixpoint_synpco :-
    ( internal_pda_lookup_Snew_opt(_,_,_) ->
        %remove_Snew,
        internal_pda_lookup_Snew_opt(P,Z,Q),
        !,
        retract(internal_pda_lookup_Snew_opt(P,Z,Q)),
        ( internal_pda_lookup_Sold_opt(P,Z,Q) ->
            true
        ; assert(internal_pda_lookup_Sold_opt(P,Z,Q)),
          add_rules_to_Snew_synpco(P,Z,Q)
          %update_Sold,
          %update_Sprev,
        ),
        build_reachability_table_fixpoint_synpco
    ; internal_pda_lookup_Snew_opt(_,_,_,_,_) ->
        internal_pda_lookup_Snew_opt(P,Z,Q,DiffI,DiffJ),
        !,
        retract(internal_pda_lookup_Snew_opt(P,Z,Q,DiffI,DiffJ)),
        ( (internal_pda_lookup_Sold_opt(P,Z,Q,DiffI1,DiffJ),
           DiffI1 =< DiffI) ->
            true
        ; (internal_pda_lookup_Sold_opt(P,Z,Q,DiffI1,DiffJ),
           DiffI1 > DiffI) ->
            retract(internal_pda_lookup_Sold_opt(P,Z,Q,DiffI1,DiffJ)),
            assert(internal_pda_lookup_Sold_opt(P,Z,Q,DiffI,DiffJ)),
            add_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ)
        ; assert(internal_pda_lookup_Sold_opt(P,Z,Q,DiffI,DiffJ)),
          add_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ)
        ),
        build_reachability_table_fixpoint_synpco
    ; true
    ).

add_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ) :-
    add_second_type_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ),
    add_third_type_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ).

add_second_type_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ) :-
    add_second_type_rules_to_Snew_fail_synpco(P,Z,Q,DiffI,DiffJ); true.

add_second_type_rules_to_Snew_fail_synpco(Q1,Z,Q2,DiffI,DiffJ) :-
    (internal_pda_lookup(P1,[sym(Z1)],Q1,[sym(Z)],D1,D2);
     internal_pda_lookup_deltapr(P1,Z1,Q1,Z,D1,D2)
    ),
    synpco:internal_modeveccount(ModeVecCount),
    MaxModeVecDiff is ModeVecCount - 1,
    synpco:internal_conbound(ConBound),
    C1 is DiffI + D1,
    C2 is DiffJ + D2,
    C1 =< MaxModeVecDiff,
    C2 =< ConBound,
    ( (internal_pda_lookup_Sold_opt(P1,Z1,Q2,C11,C2),
        C11 =< C1) ->
            true
    ; (internal_pda_lookup_Snew_opt(P1,Z1,Q2,C11,C2),
        C11 =< C1) ->
            true
    ; (internal_pda_lookup_Snew_opt(P1,Z1,Q2,C11,C2),
        C11 > C1) ->
      retract(internal_pda_lookup_Snew_opt(P1,Z1,Q2,C11,C2)),
      assert(internal_pda_lookup_Snew_opt(P1,Z1,Q2,C1,C2))
    ; assert(internal_pda_lookup_Snew_opt(P1,Z1,Q2,C1,C2))
    ),
    fail.

add_rules_to_Snew_synpco(P,Z,Q) :-
    add_second_type_rules_to_Snew_synpco(P,Z,Q),
    add_third_type_rules_to_Snew_synpco(P,Z,Q).

add_second_type_rules_to_Snew_synpco(P,Z,Q) :-
    add_second_type_rules_to_Snew_fail_synpco(P,Z,Q); true.

add_second_type_rules_to_Snew_fail_synpco(Q1,Z,Q2) :-
    (internal_pda_lookup(P1,[sym(Z1)],Q1,[sym(Z)]);
     internal_pda_lookup_deltapr(P1,Z1,Q1,Z);
     internal_pda_lookup(P1,[sym(Z1)],Q1,[sym(Z)],_,_)
     %internal_pda_lookup_deltapr(P1,Z1,Q1,Z,_,_)
    ),
    %internal_pda_lookup(tran(from(P1,[sym(Z1)]),to(Q1,[sym(Z)]))),
    %internal_pda_lookup_Sprev(P2,Z2,Q),
    %( internal_pda_lookup_Sold(P1,Z1,Q) ->
        %true
    %; 
    %write('lalala '), write((P1,Z1,Q2)), nl,
    ( internal_pda_lookup_Snew_opt(P1,Z1,Q2) ->
        true
    ; assert(internal_pda_lookup_Snew_opt(P1,Z1,Q2))
    ),
    %assert(internal_pda_lookup_Snew(P1,Z1,Q2)),
    fail.

add_third_type_rules_to_Snew_synpco(P,Z,Q,DiffI,DiffJ) :-
    add_third_type_rules_to_Snew_fail_synpco(P,Z,Q,DiffI,DiffJ); true.

add_third_type_rules_to_Snew_fail_synpco(Q1,Z,Q2,DiffI,DiffJ) :-
    internal_pda_lookup(P1,[sym(Z1)],Q1,[sym(Z2),sym(Z)],D1,D2),
    synpco:internal_modeveccount(ModeVecCount),
    MaxModeVecDiff is ModeVecCount - 1,
    synpco:internal_conbound(ConBound),
    C1 is DiffI + D1,
    C2 is DiffJ + D2,
    C1 =< MaxModeVecDiff,
    C2 =< ConBound,
    ( (internal_pda_lookup(P1,[sym(Z1)],Q2,[sym(Z2)],C11,C2),
        C11 =< C1) ->
        true
    ; ( (internal_pda_lookup_deltapr(P1,Z1,Q2,Z2,C11,C2),
         C11 =< C1) ->
            true
      ; (internal_pda_lookup_deltapr(P1,Z1,Q2,Z2,C11,C2),
         C11 > C1) ->
            retract(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2,C11,C2)),
            assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2,C1,C2)),
            ( internal_pda_lookup_deltapr(P1,Z1,Q2,Z2) ->
                true
            ; assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2))
            )
      ; assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2,C1,C2)),
        ( internal_pda_lookup_deltapr(P1,Z1,Q2,Z2) ->
            true
        ; assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2))
        )
      )
    ),
    ( internal_pda_lookup_Sold_opt(Q2,Z2,Q3,E1,E2), 
      F1 is C1 + E1,
      F2 is C2 + E2,
      F1 =< MaxModeVecDiff,
      F2 =< ConBound,
      ( (internal_pda_lookup_Sold_opt(P1,Z1,Q3,F11,F2),
         F11 =< F1) ->
            true
      ; (internal_pda_lookup_Snew_opt(P1,Z1,Q3,F11,F2),
         F11 =< F1) ->
            true
      ; (internal_pda_lookup_Snew_opt(P1,Z1,Q3,F11,F2),
         F11 > F1) ->
            retract(internal_pda_lookup_Snew_opt(P1,Z1,Q3,F11,F2)),
            assert(internal_pda_lookup_Snew_opt(P1,Z1,Q3,F1,F2))
      ; assert(internal_pda_lookup_Snew_opt(P1,Z1,Q3,F1,F2))
      )
    ; internal_pda_lookup_Sold_opt(Q2,Z2,Q3),
      ( internal_pda_lookup_Snew_opt(P1,Z1,Q3) ->
          true
      ; assert(internal_pda_lookup_Snew_opt(P1,Z1,Q3))
      )
    ),
    fail.

add_third_type_rules_to_Snew_synpco(P,Z,Q) :-
    add_third_type_rules_to_Snew_fail_synpco(P,Z,Q); true.

add_third_type_rules_to_Snew_fail_synpco(Q1,Z,Q2) :-
    internal_pda_lookup(P1,[sym(Z1)],Q1,[sym(Z2),sym(Z)],_,_),
    ( internal_pda_lookup(P1,[sym(Z1)],Q2,[sym(Z2)]) ->
        true
    ; ( internal_pda_lookup_deltapr(P1,Z1,Q2,Z2) ->
        true
      ;  assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2))
      )
    ),
    internal_pda_lookup_Sold_opt(Q2,Z2,Q3), % in this case Q3 = Q2 since it's 
                                            % new_final
    ( internal_pda_lookup_Snew_opt(P1,Z1,Q3) ->
        true
    ; assert(internal_pda_lookup_Snew_opt(P1,Z1,Q3))
    ),
    fail.

/*
add_second_type_rules_to_Snew_synpco(Q1,Z,Q2) :-
    add_second_type_rules_to_Snew_fail_synpco(Q1,Z,Q2); true.

add_second_type_rules_to_Snew_fail_synpco(Q1,Z,Q2) :-
    functor(Q1,NameQ1,ArgQ1),
    functor(Q2,NameQ2,ArgQ2),
    ( (ArgQ1 =:= 3, ArgQ2 =:= 3) ->        
        Q1 = (Q1State,I1,J1),
        Q2 = (Q2State,I2,J2),
        (internal_pda_lookup(
           tran(from(P1,[sym(Z1)]),to((Q1State,R1,T1),[sym(Z)])))
        ; internal_pda_lookup_deltapr(P1,Z1,(Q1State,R1,T1),Z)
        ),
        P1 = (P1State,R0,T0),
        D1 is R1 - R0,
        D2 is T1 - T0,
        I0 is I1 - D1, I0 >= 1,
        J0 is J1 - D2, J0 >= 0,
        ( internal_pda_lookup_Sold((P1State,I0,J0),Z1,Q2) ->
            true
        ; internal_pda_lookup_Snew((P1State,I0,J0),Z1,Q2) ->
             true
        ; DiffI is I2-I0,
          DiffJ is J2-J0,
          add_second_type_rules_to_Snew_synpco_33(P1State,Z1,Q2State,
            DiffI,DiffJ)
        %assert(internal_pda_lookup_Snew(P1,Z1,Q2))
        )
    ; (ArgQ1 =:= 3, ArgQ2 =:= 0) ->
        Q1 = (Q1State,I1,J1),
        (internal_pda_lookup(
           tran(from(P1,[sym(Z1)]),to((Q1State,R1,T1),[sym(Z)])))
        ; internal_pda_lookup_deltapr(P1,Z1,(Q1State,R1,T1),Z)
        ),
        P1 = (P1State,R0,T0),
        
    ; ArgQ1 =:= 0,
      ArgQ2 =:= 0,
      internal_pda_lookup(tran(from(P1,[sym(Z1)]),to(Q1,[sym(Z)]))),
      ( internal_pda_lookup_Snew(P1,Z1,Q2) ->
            true
      ; assert(internal_pda_lookup_Snew(P1,Z1,Q2))
      ),
    ),
    %internal_pda_lookup_Sprev(P2,Z2,Q),
    %( internal_pda_lookup_Sold(P1,Z1,Q) ->
        %true
    %; 
    %assert(internal_pda_lookup_Snew(P1,Z1,Q2)),
    fail.
*/

/*
add_second_type_rules_to_Snew_fail_synpco(Q1,Z,Q2) :-
    internal_pda_lookup_deltapr(P1,Z1,Q1,Z),
    %internal_pda_lookup_Sprev(P2,Z2,Q),
    %( internal_pda_lookup_Sold(P1,Z1,Q) ->
        %true
    %; 
    ( internal_pda_lookup_Snew(P1,Z1,Q2) ->
        true
    ; assert(internal_pda_lookup_Snew(P1,Z1,Q2))
    ),
    %assert(internal_pda_lookup_Snew(P1,Z1,Q2)),
    fail.
*/

/*
add_second_type_rules_to_Snew_synpco_33(P1St,Z1,Q2St,DiffMode,DiffCon) :-
    internal_modeveccount(ModeVecCount),
    internal_conbound(ConBound),
    add_second_type_rules_to_Snew_synpco_33(P1St,Z1,Q2St,DiffMode,DiffCon,ModeVecCount,ConBound).

add_second_type_rules_to_Snew_synpco_33(P1St,Z1,Q2St,DiffMode,DiffCon,
    CountMode,CountCon) :-
        %( internal_pda_lookup_Snew(P1
        I2 is CountMode,
        J2 is CountCon,
        I0 is I2-DiffMode,
        J0 is J2-DiffCon,
        ( J0 < 0 ->
            true
        ; I0 < 1 ->
            internal_modeveccount(ModeVecCount),
            CountCon1 is CountCon - 1,
            add_second_type_rules_to_Snew_synpco_33(P1St,Z1,Q2St,DiffMode,
                DiffCon,ModeVecCount,CountCon1)
        ; ( internal_pda_lookup_Snew((P1St,I0,J0),Z1,(Q2St,I2,J2)) ->
                true
          ;  assert(internal_pda_lookup_Snew((P1St,I0,J0),Z1,(Q2St,I2,J2)))
          ),
          CountMode1 is CountMode - 1,
          add_second_type_rules_to_Snew_synpco_33(P1St,Z1,Q2St,DiffMode,DiffCon,
                CountMode1,CountCon)
        ).
*/

add_rules_to_Snew(P,Z,Q) :-
    add_second_type_rules_to_Snew(P,Z,Q),
    add_third_type_rules_to_Snew(P,Z,Q).

add_second_type_rules_to_Snew(Q1,Z,Q2) :-
    add_second_type_rules_to_Snew_fail(Q1,Z,Q2); true.

add_second_type_rules_to_Snew_fail(Q1,Z,Q2) :-
    internal_pda_lookup(tran(from(P1,[sym(Z1)]),to(Q1,[sym(Z)]))),
    %internal_pda_lookup_Sprev(P2,Z2,Q),
    %( internal_pda_lookup_Sold(P1,Z1,Q) ->
        %true
    %; 
    ( internal_pda_lookup_Snew(P1,Z1,Q2) ->
        true
    ; assert(internal_pda_lookup_Snew(P1,Z1,Q2))
    ),
    %assert(internal_pda_lookup_Snew(P1,Z1,Q2)),
    fail.

add_second_type_rules_to_Snew_fail(Q1,Z,Q2) :-
    internal_pda_lookup_deltapr(P1,Z1,Q1,Z),
    %internal_pda_lookup_Sprev(P2,Z2,Q),
    %( internal_pda_lookup_Sold(P1,Z1,Q) ->
        %true
    %; 
    ( internal_pda_lookup_Snew(P1,Z1,Q2) ->
        true
    ; assert(internal_pda_lookup_Snew(P1,Z1,Q2))
    ),
    %assert(internal_pda_lookup_Snew(P1,Z1,Q2)),
    fail.

add_third_type_rules_to_Snew(P,Z,Q) :-
    add_third_type_rules_to_Snew_fail(P,Z,Q); true.

add_third_type_rules_to_Snew_fail(Q1,Z,Q2) :-
    internal_pda_lookup(tran(from(P1,[sym(Z1)]),
                                        to(Q1,[sym(Z2),sym(Z)]))),
    ( internal_pda_lookup(tran(from(P1,[sym(Z1)]),to(Q2,[sym(Z2)]))) ->
        true
    ; ( internal_pda_lookup_deltapr(P1,Z1,Q2,Z2) ->
        true
      ;  assert(internal_pda_lookup_deltapr(P1,Z1,Q2,Z2))
      )
    ),
    internal_pda_lookup_Sold(Q2,Z2,Q3),
    ( internal_pda_lookup_Snew(P1,Z1,Q3) ->
        true
    ; assert(internal_pda_lookup_Snew(P1,Z1,Q3))
    ),
    %( internal_pda_lookup_Sold(P2,Z,Q1),
      %internal_pda_lookup_Sprev(Q1,Y,Q2)
    %; internal_pda_lookup_Sprev(P2,Z,Q1),
      %internal_pda_lookup_Sold(Q1,Y,Q2)
    %),
    %not(internal_pda_lookup_Sold(P1,X,Q2)),
    %assert(internal_pda_lookup_Snew(P1,X,Q2)),
    % Reordered version
    /*
    internal_pda_lookup_Sprev(Q1,Y,Q2),
    ( internal_pda_lookup(tran(from(P1,[sym(X)]),_,
                                        to(P2,[sym(Y),sym(Z)]))),
      internal_pda_lookup_Sold(P2,Z,Q1), 
      not(internal_pda_lookup_Sold(P1,X,Q2)),
      assert(internal_pda_lookup_Snew(P1,X,Q2))
    ; internal_pda_lookup(tran(from(P1,[sym(X)]),_,
                                        to(Q1,[sym(Z),sym(Y)]))),
      internal_pda_lookup_Sold(Q2,Z,P2),
      not(internal_pda_lookup_Sold(P1,X,P2)),
      assert(internal_pda_lookup_Snew(P1,X,P2))
    ),
    */
    fail.
    
update_Sold :-
    update_Sold_fail; true.

update_Sold_fail :-
    internal_pda_lookup_Snew(P,Z,Q),
    assert(internal_pda_lookup_Sold(P,Z,Q)),
    fail.

%update_Sprev :-
    %( remove_Sprev, update_Sprev_fail ); true.

%update_Sprev_fail :-
    %internal_pda_lookup_Snew(P,Z,Q),
    %assert(internal_pda_lookup_Sprev(P,Z,Q)),
    %fail.

remove_internal_tables :-
    remove_trans_lookup, remove_deltapr.

remove_internal_tables_synpco :-
    remove_trans_lookup_synpco, remove_deltapr_synpco.

%remove_Snew :-
    %remove_Snew_fail; true.

remove_Sold :-
    remove_Sold_fail; true.

remove_Sold_synpco :-
    remove_Sold_fail_synpco; true.

%remove_Sprev :-
    %remove_Sprev_fail; true.

remove_trans_lookup :-
    remove_trans_lookup_fail; true.

remove_trans_lookup_synpco :-
    remove_trans_lookup_fail_synpco; true.

remove_trans_lookup_fail :-
    retract(internal_pda_lookup(_)),
    fail.

remove_trans_lookup_fail_synpco :-
    retract(internal_pda_lookup(_,_,_,_)),
    fail.

remove_trans_lookup_fail_synpco :-
    retract(internal_pda_lookup(_,_,_,_,_,_)),
    fail.

remove_deltapr :-
    remove_deltapr_fail; true.

remove_deltapr_synpco :-
    remove_deltapr_fail_synpco; true.

remove_deltapr_fail :-
    retract(internal_pda_lookup_deltapr(_,_,_,_)),
    fail.

remove_deltapr_fail_synpco :-
    retract(internal_pda_lookup_deltapr(_,_,_,_)),
    fail.

remove_deltapr_fail_synpco :-
    retract(internal_pda_lookup_deltapr(_,_,_,_,_,_)),
    fail.

%remove_Snew_fail :-
    %retract(internal_pda_lookup_Snew(_,_,_)),
    %fail.

remove_Sold_fail :-
    retract(internal_pda_lookup_Sold(_,_,_)),
    fail.

remove_Sold_fail_synpco :-
    retract(internal_pda_lookup_Sold_opt(_,_,_)),
    fail.

remove_Sold_fail_synpco :-
    retract(internal_pda_lookup_Sold_opt(_,_,_,_,_)),
    fail.

%remove_Sprev_fail :-
    %retract(internal_pda_lookup_Sprev(_,_,_)),
    %fail.

remove_first_type_rule :-
    remove_first_type_rule_fail; true.

remove_first_type_rule_fail :-
    retract(internal_pda_lookup(tran(from(_P,[sym(_Z)]),to(_Q,[])))),
    fail.

remove_first_type_rule_synpco :-
    remove_first_type_rule_fail_synpco; true.

remove_first_type_rule_fail_synpco :-
    retract(internal_pda_lookup(_Q1,[sym(_Z)],_Q2,[],_DiffI,_DiffJ)),
    fail.

remove_first_type_rule_fail_synpco :-
    retract(internal_pda_lookup(_Q1,[sym(_Z)],_Q2,[])),
    fail.
         
% assume that build_pda_lookup_table has been invoked.
init_Ss :-
    init_Ss_fail; 
    (assert(internal_pda_lookup_Snew('_crazy1','_crazy2','_crazy3')),
     retract(internal_pda_lookup_Snew('_crazy1','_crazy2','_crazy3')),
     assert(internal_pda_lookup_Sold('_crazy1','_crazy2','_crazy3')),
     retract(internal_pda_lookup_Sold('_crazy1','_crazy2','_crazy3')),
     assert(internal_pda_lookup_deltapr('_crazy1','_crazy2','_crazy3','_crazy4')),
     retract(internal_pda_lookup_deltapr('_crazy1','_crazy2','_crazy3','_crazy4')),
     true).

init_Ss_fail :-
    internal_pda_lookup(tran(from(P,[sym(Z)]),to(Q,[]))),
    assert(internal_pda_lookup_Snew(P,Z,Q)),
    %assert(internal_pda_lookup_Sold(P,Z,Q)),
    %assert(internal_pda_lookup_Sprev(P,Z,Q)),
    fail.

init_Ss_synpco :-
    (assert(internal_pda_lookup_Snew_opt(1,2,3,4,5)),
     retract(internal_pda_lookup_Snew_opt(1,2,3,4,5)),
     assert(internal_pda_lookup_Sold_opt(1,2,3,4,5)),
     retract(internal_pda_lookup_Sold_opt(1,2,3,4,5)),
     assert(internal_pda_lookup_Sold_opt(1,2,3)),
     retract(internal_pda_lookup_Sold_opt(1,2,3)),
     assert(internal_pda_lookup_deltapr(1,2,3,4,5,6)),
     retract(internal_pda_lookup_deltapr(1,2,3,4,5,6)),
     assert(internal_pda_lookup_deltapr(1,2,3,4)),
     retract(internal_pda_lookup_deltapr(1,2,3,4)),
     assert(internal_pda_lookup_Sold('_crazy1','_crazy2','_crazy3')),
     retract(internal_pda_lookup_Sold('_crazy1','_crazy2','_crazy3')),
     init_Ss_synpco_fail); 
     true.

init_Ss_synpco_fail :-
    internal_pda_lookup(Q1,[sym(Z)],Q2,[],DiffI,DiffJ),
    assert(
        internal_pda_lookup_Snew_opt(Q1,Z,Q2,DiffI,DiffJ)
          ),
    %init_Ss_synpco_iterate(P,Z,Q,0,0),
    %assert(internal_pda_lookup_Snew(P,Z,Q)),
    fail.

init_Ss_synpco_fail :-
    internal_pda_lookup(Q1,[sym(Z)],Q2,[]),
    assert(internal_pda_lookup_Snew_opt(Q1,Z,Q2)),
    fail.

/*
init_Ss_synpco_iterate(P,Z,Q,I,J) :-
    functor(P,NameP,ArgP),
    functor(Q,NameQ,ArgQ),
    ( (ArgP =:= 3, ArgQ =:= 3) ->
        P = (PState,I1,J1),
        Q = (QState,I2,J2),
        R1 is I1 - I,
        T1 is J1 - J,
        R2 is I2 - I,
        T2 is J2 - J,
        ( (R1 >= 1, T1 >= 0, R2 >= 1, T2 >= 0) ->
            assert(internal_pda_lookup_Snew((PState,R1,T1),Z,
                                            (QState,R2,T2))
                  ),
            INew is I + 1,
            init_Ss_synpco_iterate(P,Z,Q,INew,J)
        ; (R1 =:= 0, T1 >= 0) ->
            JNew is J + 1,
            init_Ss_synpco_iterate(P,Z,Q,0,JNew)
        ; T1 =:= -1
        )
    ; (ArgP =:= 3, ArgQ =:= 0) ->
        P = (PState,I1,J1),
        R1 is I1 - I,
        T1 is J1 - J,
        ( (R1 >= 1, T1 >= 0) ->
            assert(internal_pda_lookup_Snew((PState,R1,T1),Z,Q)
                  ),
            INew is I + 1,
            init_Ss_synpco_iterate(P,Z,Q,INew,J)
        ; (R1 =:= 0, T1 >= 0) ->
            JNew is J + 1,
            init_Ss_synpco_iterate(P,Z,Q,0,JNew)
        ; T1 =:= -1
        )
    ; ArgP =:= 0,
      ArgQ =:= 0
      assert(internal_pda_lookup_Snew(P,Z,Q))
    ).
*/

build_pda_lookup_table_fail(Trans) :-
    member(Tran,Trans),
    ( Tran = tran(From,_Actions,To) ->
        assert(internal_pda_lookup(tran(From,To)))
    ; Tran = spectran(P,StackSymSubset,_,Q),
      forall(member(Sym,StackSymSubset),
             ( internal_pda_lookup(tran(from(P,[Sym]),to(Q,[Sym]))) ->
                true
             ; assert(internal_pda_lookup(tran(from(P,[Sym]),to(Q,[Sym]))))
             )
            )
    ),
    fail.

build_pda_lookup_table(Trans) :-
    build_pda_lookup_table_fail(Trans); true.

/*
build_pda_lookup_table_synpco_fail(Trans) :-
    member(tran(from(From,Stack1),_Actions,to(To,Stack2)),Trans),
    functor(From,NameFrom,ArgFrom),
    functor(To,NameTo,ArgTo),
    ( (ArgFrom =:= 3, ArgTo =:= 3) ->
        arg(1,From,Q1),
        arg(2,From,I1),
        arg(3,From,J1),
        arg(1,To,Q2),
        arg(2,To,I2),
        arg(3,To,J2),
        ( (internal_pda_lookup(tran(from((Q1,R1,T1),Stack1),_,
                                    to((Q2,R2,T2),Stack2))),
           R1 >= I1,
           T1 >= J1,
           R2 >= I2,
           T2 >= J2,
           R1 - I1 =:= R2 - I2, % not sure about this constraint yet ...
           T1 - J1 =:= T2 - J2) ->
                true
        ; assert(internal_pda_lookup(tran(from(From,Stack1),to(To,Stack2))))
        )
    ; ( (ArgFrom =:= 3, ArgTo =:= 0) % Means NameTo = new_final_i
        ->
        arg(1,From,Q1),
        arg(2,From,I1),
        arg(3,From,J1),
        ( (internal_pda_lookup(tran(from((Q1,R1,T1),Stack1),_,
                                    to(To,Stack2))),
           R1 >= I1,
           T1 >= J1) ->
                true
        ; assert(internal_pda_lookup(tran(from(From,Stack1),to(To,Stack2))))
        )
    ; ArgFrom =:= 0,
      ArgTo   =:= 0,
      ( internal_pda_lookup(tran(from(From,Stack1),to(To,Stack2))) ->
           true
      ; assert(internal_pda_lookup(tran(from(From,Stack1),to(To,Stack2))))
      )
    ),
    fail.
*/

build_pda_lookup_table_synpco_fail(Trans) :-
    member(tran(from(From,Stack1),_Actions,to(To,Stack2)),Trans),
    %write((From,Stack1,To,Stack2)), nl,
    functor(From,_,ArgFrom),
    functor(To,_,ArgTo),
    %write((ArgFrom,ArgTo)), nl,
    ( (ArgFrom =:= 2, ArgTo =:= 2) ->
        From = (Q1,I1,J1),
        To   = (Q2,I2,J2),
        DiffI is I2 - I1,
        DiffJ is J2 - J1,
        ( (internal_pda_lookup(Q1,Stack1,Q2,Stack2,DiffI1,DiffJ),
            DiffI1 =< DiffI) ->
            true
        ; (internal_pda_lookup(Q1,Stack1,Q2,Stack2,DiffI1,DiffJ),
            DiffI1 > DiffI) ->
                retract(internal_pda_lookup(Q1,Stack1,Q2,Stack2,DiffI1,DiffJ)),
                assert(internal_pda_lookup(Q1,Stack1,Q2,Stack2,DiffI,DiffJ))
        ; assert(internal_pda_lookup(Q1,Stack1,Q2,Stack2,DiffI,DiffJ))
        )
    ; (ArgFrom =:= 2, ArgTo =:= 0) -> % Means NameTo = new_final_i
        From = (FromState,_,_),
        ( internal_pda_lookup(FromState,Stack1,To,Stack2) ->
            true
        ; assert(internal_pda_lookup(FromState,Stack1,To,Stack2))
        )
    ; ArgFrom =:= 0,
      ArgTo   =:= 0,
      ( internal_pda_lookup(From,Stack1,To,Stack2) ->
            true
      ; assert(internal_pda_lookup(From,Stack1,To,Stack2))
      )
    ),
    fail.

build_pda_lookup_table_synpco(Trans) :-
    ( assert(internal_pda_lookup(1,2,3,4,5,6)),
      retract(internal_pda_lookup(1,2,3,4,5,6)),
      assert(internal_pda_lookup(1,2,3,4)),
      retract(internal_pda_lookup(1,2,3,4)),
      build_pda_lookup_table_synpco_fail(Trans)
    ); true.

% Getting rid of states in Sold that cannot be reached from (Q0,Z0) and
% those that cannot reach (Qacc,\epsilon).
optimize_pda_lookup_table((Q0,Z0),Qacc) :-
   %remove_unreachable_snew(Q0,Z0),
   %write(((Q0,Z0),Qacc)), nl,
   ( internal_pda_lookup_Sold(Q0,Z0,Qacc) ->
        remove_empty_Sold(Qacc)
   ; write('ERROR: final state is already not reachable from initial state'),nl,
     fail
   ).

optimize_pda_lookup_table_synpco((Q0,Z0),Qacc) :-
   %remove_unreachable_snew(Q0,Z0),
   %write(((Q0,Z0),Qacc)), nl,
   Q0 = (State0,_Mode0,_Con0),
   ( internal_pda_lookup_Sold_opt(State0,Z0,Qacc) ->
        remove_empty_Sold_synpco(Qacc)
   ; write('ERROR: final state is already not reachable from initial state'),nl,
     fail
   ).


% This is simply doing reachability computation
remove_empty_Sold(Qacc) :-
    reach_fin_wrt_Sold(Qacc),
    (remove_empty_Sold_fail; true),
    remove_reach_fin_old.

remove_empty_Sold_synpco(Qacc) :-
    reach_fin_wrt_Sold_synpco(Qacc),
    (remove_empty_Sold_fail_synpco; true),
    remove_reach_fin_old_synpco.

remove_empty_Sold_fail :-
    internal_pda_lookup_Sold(X,A,Y),
    ( internal_pda_lookup_reach_fin_old(Y) ->
        true
    ; retract(internal_pda_lookup_Sold(X,A,Y))
    ),
    %%% Not sure about the following block
    ( internal_pda_lookup_reach_fin_old(X) ->
        true
    ; retract(internal_pda_lookup_Sold(X,A,Y))
    ),
    %%%%
    fail.

remove_empty_Sold_fail_synpco :-
    internal_pda_lookup_Sold_opt(X,A,Y,DiffI,DiffJ),
    ( internal_pda_lookup_reach_fin_old(Y) ->
        true
    ; retract(internal_pda_lookup_Sold_opt(X,A,Y,DiffI,DiffJ))
    ),
    %%% Not sure about the following block
    ( internal_pda_lookup_reach_fin_old(X) ->
        true
    ; retract(internal_pda_lookup_Sold_opt(X,A,Y,DiffI,DiffJ))
    ),
    %%%
    fail.

reach_fin_wrt_Sold(Qacc) :-
    assert(internal_pda_lookup_reach_fin_new(Qacc)),
    reach_fin_wrt_Sold.

reach_fin_wrt_Sold_synpco(Qacc) :-
    assert(internal_pda_lookup_reach_fin_new(Qacc)),
    reach_fin_wrt_Sold_synpco.

reach_fin_wrt_Sold :-
    ( internal_pda_lookup_reach_fin_new(_) ->
        internal_pda_lookup_reach_fin_new(X),
        !,
        retract(internal_pda_lookup_reach_fin_new(X)),
        assert(internal_pda_lookup_reach_fin_old(X)),
        add_reach_fin_new(X),
        %forall( internal_pda_lookup_Sold(Y,_,X),
               %( internal_pda_lookup_reach_fin_old(Y) ->
                    %true
               %; assert(internal_pda_lookup_reach_fin_new(Y)
               %))
              %),
        reach_fin_wrt_Sold
    ; true
    ).

reach_fin_wrt_Sold_synpco :-
    ( internal_pda_lookup_reach_fin_new(_) ->
        internal_pda_lookup_reach_fin_new(X),
        !,
        retract(internal_pda_lookup_reach_fin_new(X)),
        assert(internal_pda_lookup_reach_fin_old(X)),
        add_reach_fin_new_synpco(X),
        %forall( internal_pda_lookup_Sold(Y,_,X),
               %( internal_pda_lookup_reach_fin_old(Y) ->
                    %true
               %; assert(internal_pda_lookup_reach_fin_new(Y)
               %))
              %),
        reach_fin_wrt_Sold_synpco
    ; true
    ).

add_reach_fin_new(X) :-
    add_reach_fin_new_fail(X); true.

add_reach_fin_new_synpco(X) :-
    add_reach_fin_new_fail_synpco(X); true.

add_reach_fin_new_fail(X) :-
    internal_pda_lookup_Sold(Y,_,X),
    ( internal_pda_lookup_reach_fin_old(Y) ->
        true
    ; assert(internal_pda_lookup_reach_fin_new(Y))
    ),
    fail.

add_reach_fin_new_fail_synpco(X) :-
    %functor(_,_,
    functor(X,_,Arity),
    Arity =:= 2,
    !,
    internal_pda_lookup_Sold_opt(Y,_,X,_,_),
    ( internal_pda_lookup_reach_fin_old(Y) ->
        true
    ; assert(internal_pda_lookup_reach_fin_new(Y))
    ),
    fail.

add_reach_fin_new_fail_synpco(X) :-
    %functor(_,_,
    functor(X,_,Arity),
    Arity =:= 0,
    internal_pda_lookup_Sold_opt(Y,_,X),
    ( internal_pda_lookup_reach_fin_old(Y) ->
        true
    ; assert(internal_pda_lookup_reach_fin_new(Y))
    ),
    fail.

remove_reach_fin_old :-
    remove_reach_fin_old_fail; true.

remove_reach_fin_old_fail :-
    retract(internal_pda_lookup_reach_fin_old(_)),
    fail.

remove_reach_fin_old_synpco :-
    remove_reach_fin_old.


/*
remove_empty_Sold_fail(Qacc) :-
    internal_pda_lookup_Sold(P1,Z,P2),
    ( internal_pda_lookup_Sold(P1,Z,Qacc) ->
        ( internal_pda_lookup_Sold(P2,_,Qacc) ->
            true
        ; retract(internal_pda_lookup_Sold(P1,Z,P2))
        )
    ; retract(internal_pda_lookup_Sold(P1,Z,P2))
    ),
    fail.
*/

%%%%%%%%%%%%%%%%%%%%%% Some Examples of PDA. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is from Figure 2.6 Sipser.
% It recognizes { 0^n 1^n : n >= 0 }.
pda_example1(pda(States,StackSym,InputSym,Trans,(Q_0,sym(bot)),Q_acc)) :-
    States = [q_0,q_1,q_2],
    StackSym = [sym(bot),sym(0)],
    InputSym = [t(0),t(1)],
    Trans = [tran(from(q_0,[sym(bot)]),[t(0)],to(q_0,[sym(bot),sym(0)])),
             tran(from(q_0,[sym(0)]),[t(0)],to(q_0,[sym(0),sym(0)])),
             tran(from(q_0,[sym(bot)]),[],to(q_1,[sym(bot)])),
             tran(from(q_0,[sym(0)]),[],to(q_1,[sym(0)])),
             tran(from(q_1,[sym(0)]),[t(1)],to(q_1,[])),
             tran(from(q_1,[sym(bot)]),[],to(q_2,[]))],
    Q_0 = q_0,
    Q_acc = q_2.

% This recognizes { a^7 }
pda_example2(pda(States,StackSym,InputSym,Trans,(Q_0,sym(bot)),Q_acc)) :-
    States = [q0,q1,q2,q3,p0,p1,p2,p3,qacc],
    StackSym = [sym(bot),sym(0),sym(1)],
    InputSym = [t(a)],
    Trans = [
             tran(from(q0,[sym(bot)]),[],to(q1,[sym(bot),sym(0)])),
             tran(from(q1,[sym(0)]),[],to(q2,[sym(0),sym(0)])),
             tran(from(q2,[sym(0)]),[],to(q3,[sym(0),sym(0)])),
             tran(from(q3,[sym(0)]),[t(a)],to(p3,[sym(1)])),
             tran(from(p3,[sym(1)]),[],to(p2,[])),
             tran(from(p2,[sym(1)]),[],to(p1,[])),
             tran(from(p1,[sym(1)]),[],to(p0,[])),
             tran(from(p2,[sym(0)]),[t(a)],to(q3,[sym(1),sym(0)])),
             tran(from(p1,[sym(0)]),[t(a)],to(q2,[sym(1),sym(0)])),
             tran(from(p0,[sym(bot)]),[],to(qacc,[]))
             ],
     Q_0 = q0,
     Q_acc = qacc.

% It recognizes { 0^n 1^n : n >= 0 }.
% This is the same as example 1 but the state name is changed from 
% q_i to p_i. 
% This is to be used for asynpda_to_cg/2 since the states are assumed
% to be different.
pda_example3(pda(States,StackSym,InputSym,Trans,(Q_0,sym(bot)),Q_acc)) :-
    States = [p_0,p_1,p_2],
    StackSym = [sym(bot),sym(0)],
    InputSym = [t((0,0)),t((1,1))],
    Trans = [tran(from(p_0,[sym(bot)]),[t((0,0))],to(p_0,[sym(bot),sym(0)])),
             tran(from(p_0,[sym(0)]),[t((0,0))],to(p_0,[sym(0),sym(0)])),
             tran(from(p_0,[sym(bot)]),[],to(p_1,[sym(bot)])),
             tran(from(p_0,[sym(0)]),[],to(p_1,[sym(0)])),
             tran(from(p_1,[sym(0)]),[t((1,1))],to(p_1,[])),
             tran(from(p_1,[sym(bot)]),[],to(p_2,[]))],
    Q_0 = p_0,
    Q_acc = p_2.

%pda_example2(pda(States,StackSym,InputSym,Trans,Q_0,Q_acc)) :-
    %States = [q_0,q_1],
    %StackSym = [sym(0)],
    %InputSym = [t(a),t(b)],
    %Trans = [tran(from(q_0,[]),[t(a)],to(q_0,[sym(0)])),
             %tran(from(q_0,[sym(0)]),[t(a)],to(q_0,[sym(0)])),
             %tran(from(q_0,[sym(0)]),[t(a)],to(q_0,[sym(0)])),
%
