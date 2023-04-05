% Main file containing wrapper functionalities.
% 
% author: Anthony Widjaja Lin (antto@cs.ox.ac.uk)

:- use_module(cg,[nonterminals_cg/2,terminals_cg/2,parikh_cg/2,annotate_cg/2,print_formula/4,print_init/5,remove_unreachable/2,remove_empty_nonterminals/2,annotate_formula/3,get_variables/2,isvalid_formula/2,cg_pre_accel/3,print_cg/4]).

:- use_module(pds,[pda_example1/1,pda_to_cg/2,pda_example2/1,pda_example3/1,asynpda_to_cg/2]).

:- use_module(synpco,[synpco_example1/1,synpco_example2/1,cbreach_synpco_to_asynpda/4,cbreach_synpco_to_z3/5,cbreach_synpco_to_z3_new/5,isvalid_synpco/1,synpco_conswitch_to_counter/6,cbreach_synpco_to_z3_new2/5]).

% This predicate checks producer-consumer example (correct version)
mycall6 :-
    assert(synpco_optimize_flag), %set the optimize flag for synpco
    synpco_example_prod_cons_corr(SynPCo0),
    isvalid_synpco(SynPCo0),
    open('output.txt',write,OS), % where the output formula to be produced
    ConBound = 2, % context bound is 2
    RevBound = [emptycount-1,fillcount-1], % both counters have reversals 1
    ParikhConstraint = true, % no extra Parikh constraint on output
    cbreach_synpco_to_z3_new(SynPCo0,RevBound,ConBound,ParikhConstraint,OS),
    close(OS),
    % comment out below if you don't want to run Z3 afterwards
    write('Starting Z3'), nl,
    !,
    shell('z3 output.txt').

mycall5 :-
    assert(synpco_optimize_flag), %set the optimize flag for synpco
    synpco_example_prod_cons(SynPCo0),
    %synpco_example2(SynPCo),
    isvalid_synpco(SynPCo0),
        %write('WARNING: example is not valid'),
        %fail
    open('output.txt',write,OS),
    %ParikhConstraint = and(eq([t(b)],[t(a)]),eq([t(a)],[1000])),
    %ParikhConstraint = lt([t(b)],[t(a)]),
    %RevBound = 1,
    ConBound = 2,
    RevBound = [itemcount-1],
    ParikhConstraint = true,
    cbreach_synpco_to_z3_new2(SynPCo0,RevBound,ConBound,ParikhConstraint,OS),
    close(OS),
    write('Starting Z3'), nl,
    !,
    shell('z3 output.txt').

mycall4 :-
    assert(synpco_optimize_flag), %set the optimize flag for synpco
    synpco_example_prod_cons_stack(SynPCo),
    %synpco_example2(SynPCo),
    isvalid_synpco(SynPCo),
        %write('WARNING: example is not valid'),
        %fail
    open('output.txt',write,OS),
    %ParikhConstraint = and(eq([t(b)],[t(a)]),eq([t(a)],[1000])),
    %ParikhConstraint = lt([t(b)],[t(a)]),
    ParikhConstraint = true,
    %RevBound = 1,
    RevBound = 1,
    ConBound = 2,
    write('Starting Translation'), nl,
    cbreach_synpco_to_z3(SynPCo,RevBound,ConBound,ParikhConstraint,OS),
    close(OS),
    write('Starting Z3'), nl,
    !,
    shell('z3 output.txt').

call_gries :-
    assert(synpco_optimize_flag), %set the optimize flag for synpco
    synpco_example_gries(SynPCo),
    isvalid_synpco(SynPCo), 
    open('output.txt',write,OS), % where the output formula is produced
    ParikhConstraint = true,
    RevBound = [y-1], % counter itemcount has reversal 1
    ConBound = 0, % set context bound parameter to 2
    write('Starting Translation'), nl,
    cbreach_synpco_to_z3_new(SynPCo,RevBound,ConBound,ParikhConstraint,OS),
    close(OS),
    % comment out if you don't want to run Z3 afterwards
    write('Starting Z3'), nl,
    !,
    shell('z3 output.txt').

% This predicate checks producer-consumer example (buggy version)
mycall3 :-
    assert(synpco_optimize_flag), %set the optimize flag for synpco
    synpco_example_prod_cons(SynPCo),
    isvalid_synpco(SynPCo), 
    open('output.txt',write,OS), % where the output formula is produced
    ParikhConstraint = true,
    RevBound = [itemcount-1], % counter itemcount has reversal 1
    ConBound = 2, % set context bound parameter to 2
    write('Starting Translation'), nl,
    cbreach_synpco_to_z3_new(SynPCo,RevBound,ConBound,ParikhConstraint,OS),
    close(OS),
    % comment out if you don't want to run Z3 afterwards
    write('Starting Z3'), nl,
    !,
    shell('z3 output.txt').

mycall2(Ts,TA) :-
    synpco_example2(SynPCo),
    ModeIndices = [1,2],
    ConSwitchIndices = [0,1],
    cbreach_synpco_to_asynpda(SynPCo,ModeIndices,ConSwitchIndices,AsynPDA),
    asynpda_to_cg(AsynPDA,CG),
    CG = cg(_,Ts,_,_),
    annotate_cg(CG,CGA1),
    remove_empty_nonterminals(CGA1,CGA),
    parikh_cg(CGA,ParikhInfo),
	CGA = cg(_,TA,_,n(_,_)),
    Constraint = imply(eq([t(s)],[1]),eq([t(a)],[10000])),
    annotate_formula(Constraint,TA,ConstraintA),
    open('output.txt',write,OS),
    print_cg(CGA,ParikhInfo,ConstraintA,OS).
    %Constraint = and([eq([t(a)],[1]),eq([t((0,0))],[t((1,1))]),gt([t(0)],[1000])]),

mycallold :-
    pda_example2(PDA),
    pda_to_cg(PDA,CG),
    %write(CG), nl,
    annotate_cg(CG,CGAnnotated),
    parikh_cg(CGAnnotated,ParikhInfo),
    CGAnnotated = cg(_,TA,_,n(_,_)),
    %Constraint = gt([t(a)],[7]),
    Constraint = gt([t(a)],[3]),
    %Constraint = or(gt([t(0)],[t(1)]),eq([t(0)],[10000000])),
    ( isvalid_formula(Constraint,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    annotate_formula(Constraint,TA,ConstraintA),
	open('output.txt',write,OS),
    print_cg(CGAnnotated,ParikhInfo,ConstraintA,OS),
    close(OS),
    write('Starting Z3'), nl,
    shell('z3 /m output.txt').

mycall(CGAnnotated) :-
	%example1(Trans),
	%nonterminals_cg(Ns,Trans),
	%terminals_cg(Ts,Trans),
    %pda_example2(PDA),
    pda_example1(PDA1),
    pda_example3(PDA2),
    asynpda_to_cg([PDA1,PDA2],CG),
    %pda_to_cg(PDA,CG),
	annotate_cg(CG,CGAnnotated1),
    %remove_unreachable(CGAnnotated1,CGAnnotated2),
    remove_empty_nonterminals(CGAnnotated1,CGAnnotated),
    % CGAnnotated = CGAnnotated1,
	parikh_cg(CGAnnotated,ParikhInfo),
	CGAnnotated = cg(_,TA,_,n(_,_)),
    %constraint1_2(Constraint),
    %Constraint = and(eq([t(0)],[1000]),or(lt([t(1)],[1000]),gt([t(1)],[1000]))),
    %Constraint = gt([t(1)],[t(0)]),
    %Constraint = or(lt([t(a)],[7]),gt([t(a)],[7])),
    Constraint = and([eq([t((0,0))],[1]),eq([t((0,0))],[t((1,1))]),gt([t(0)],[1000])]),
    %write(TA),
    ( isvalid_formula(Constraint,context(TA,[])) -> true
    ; write('WARNING: isvalid_formula/2 test fails'), false
    ),
    annotate_formula(Constraint,TA,ConstraintA),
	open('output.txt',write,OS),
    print_cg(CGAnnotated,ParikhInfo,ConstraintA,OS).
    /*
    get_variables(ConstraintA,VARs),
	open('output.txt',write,OS),
	print_init(NA,TA,TransA,VARs,OS),
	print_formula(CGAnnotated,Formula,ConstraintA,OS).
    */

mycall1(CG1,CG) :-
	example1(Trans),
	nonterminals_cg(Ns,Trans),
	terminals_cg(Ts,Trans),
	annotate_cg(cg(Ns,Ts,Trans,n(s)),CGAnnotated),
	remove_unreachable(CGAnnotated,CG1),
    remove_empty_nonterminals(CG1,CG).

% S -> aaSbbb
% S ->
example1(Trans) :-
	Trans = [tran(n(s),[ [], [(t(a),2),(n(s),1),(t(b),3)] ]) ].

constraint1_1(Constraint) :-
    Constraint = gt([t(a)],[10]).

constraint1_2(Constraint) :-
    Constraint = exist(e,or(and(geq([t(a)],[e]),eq([e],[10])),
                            leq([t(a)],[3]))).


% S -> AA
% A -> BB
% B -> CC
% C -> DD
% D -> EE
% E -> a
% L = { a^{32} }
example2(Trans) :-
	Trans = [tran(n(s),[ [(n(a),2)] ]),
		 tran(n(a),[ [(n(b),2)] ]),
		 tran(n(b),[ [(n(c),2)] ]),
		 tran(n(c),[ [(n(d),2)] ]),
		 tran(n(d),[ [(n(e),2)] ]),
		 tran(n(e),[ [(t(a),1)] ])].

example3(Trans) :-
	Trans = [tran(n(s),[ [(n(a),2)] ]),
		 tran(n(a),[ [(n(b),2)] ]),
		 tran(n(b),[ [(n(c),2)] ]),
		 tran(n(c),[ [(n(d),2)] ]),
		 tran(n(d),[ [(n(e),2)] ]),
		 tran(n(e),[ [(t(a),1)] ]),
		 tran(n(k),[ [(t(a),2)] ])].


% 
example4(Trans) :-
	Trans = [tran(n(s),[ [(n(a),2)] ]),
         tran(n(a),[ [(n(z),10)] ]), % here n(z) is empty
		 tran(n(a),[ [(n(b),2)] ]),
		 tran(n(b),[ [(n(c),2)] ]),
		 tran(n(c),[ [(n(d),2)] ]),
		 tran(n(d),[ [(n(e),2)] ]),
		 tran(n(e),[ [(t(a),1)] ]),
		 tran(n(k),[ [(t(a),2)] ])].

% Buggy example from wikipedia:
%
% int itemCount;
%
%procedure producer() {
% p1:   while (true) {
% p2:       item = produceItem();
% 
% p3:       if (itemCount == BUFFER_SIZE) {
% p4:          sleep();
%           }
% 
% p5:       putItemIntoBuffer(item);
% p6:       itemCount = itemCount + 1;
% 
% p7:       if (itemCount == 1) {
% p8:             wakeup(consumer);
%        }
%    }
%}
% 
%procedure consumer() {
% q1:   while (true) {
% 
% q2:       if (itemCount == 0) {
% q3:           sleep();
%          }
% 
% q4:       item = removeItemFromBuffer();
% q5:       itemCount = itemCount - 1;
% 
% q6:       if (itemCount == BUFFER_SIZE - 1) {
% q7:           wakeup(producer);
%          }
% 
% q8:      consumeItem(item);
%    }
%}
%
% Modeling as synpco:
% Each thread keeps a bit for the value {sleep,awake} as well as the statement 
% number above.
%
% For now buffer is ignored.

synpco_example_prod_cons(SynPCo) :-
    BufferSize is 10000,
    BufferSizeMinOne is BufferSize-1,
    SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
    PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
    PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    %States1 = [p1,p2,p3,p4,p5,p6,p7,p8,pacc],
    %States2 = [q1,q2,q3,q4,q5,q6,q7,q8,qacc],
    States1 = [p1,p2,p3,p4,p4s,p5,p6,p7,p8],
    States2 = [q1,q2,q3,q3s,q4,q5,q6,q7,q8],
    StackSym1 = [sym(bot1)],
    StackSym2 = [sym(bot2)],
    InputSym1 = [t(a)],
    InputSym2 = [t(b)],
    Q01 = p1,
    Q02 = q1,
    Z_01 = bot1,
    Z_02 = bot2,
    Qacc1 = p4s,
    Qacc2 = q3s,
    VarNames = [itemcount],
    Trans1 = [tran(from(p1,[sym(bot1)],true),[t(a)],
                   to(p2,[sym(bot1)],[])),
              tran(from(p2,[sym(bot1)],true),[],
                   to(p3,[sym(bot1)],[])),
              tran(from(p3,[sym(bot1)],eq([itemcount],[BufferSize])),[],
                   to(p4,[sym(bot1)],[])),
              tran(from(p3,[sym(bot1)],or(lt([itemcount],[BufferSize]),
                                          gt([itemcount],[BufferSize]))
                       ),
                   [],
                   to(p5,[sym(bot1)],[])),
              tran(from(p4,[sym(bot1)],true),[],
                   to(p4s,[sym(bot1)],[])),
              tran(from(p5,[sym(bot1)],true),[],
                   to(p6,[sym(bot1)],[])),
              tran(from(p6,[sym(bot1)],true),[],
                   to(p7,[sym(bot1)],[(itemcount,1)])),
              tran(from(p7,[sym(bot1)],eq([itemcount],[1])),[],
                   to(p8,[sym(bot1)],[])),
              tran(from(p7,[sym(bot1)],or(lt([itemcount],[1]),
                                          gt([itemcount],[1]))
                       ),
                   [],
                   to(p1,[sym(bot1)],[])),
              tran(from(p8,[sym(bot1)],true),[],
                   to(p1,[sym(bot1)],[]))
                   ],
    Trans2 = [tran(from(q1,[sym(bot2)],true),[t(b)],
                   to(q2,[sym(bot2)],[])),
              tran(from(q2,[sym(bot2)],eq([itemcount],[0])),[],
                   to(q3,[sym(bot2)],[])),
              tran(from(q2,[sym(bot2)],gt([itemcount],[0])),[],
                   to(q4,[sym(bot2)],[])),
              tran(from(q3,[sym(bot2)],true),[],
                   to(q3s,[sym(bot2)],[])),
              tran(from(q4,[sym(bot2)],true),[],
                   to(q5,[sym(bot2)],[])),
              tran(from(q5,[sym(bot2)],true),[],
                   to(q6,[sym(bot2)],[(itemcount,-1)])),
              tran(from(q6,[sym(bot2)],eq([itemcount],[BufferSizeMinOne])),[],
                   to(q7,[sym(bot2)],[])),
              tran(from(q6,[sym(bot2)],or(lt([itemcount],[BufferSizeMinOne]),
                                          gt([itemcount],[BufferSizeMinOne])
                                         )),
                   [],
                   to(q8,[sym(bot2)],[])),
              tran(from(q7,[sym(bot2)],true),[],
                   to(q8,[sym(bot2)],[])),
              tran(from(q8,[sym(bot2)],true),[],
                   to(q1,[sym(bot2)],[]))
              ],
    GlobalInputSym = [t(s)],
    GlobalTrans = [tran(from(states(p4s,q7),true),[t(s)],
                        to(states(p5,q8),[])),
                   tran(from(states(p8,q3s),true),[t(s)],
                        to(states(p1,q4),[]))].
                   %tran(from(states(p4,q3),true),[t(s)],
                        %to(states(pacc,qacc),[]))].

% Correct examples using semaphores from wikipedia
%
% semaphore fillCount = 0; // items produced
% semaphore emptyCount = BUFFER_SIZE; // remaining space
% 
% procedure producer() {
% p1:   while (true) {
% p2:       // item = produceItem();
% p3:       down_ec();
% p4:       // putItemIntoBuffer(item);
% p5:       up_fc();
%    }
% }
%
% procedure consumer() {
% q1:   while (true) {
% q2:       down_fc();
% q3:       // item = removeItemFromBuffer();
% q4:       up_ec();
% q5:       // consumeItem(item);
%    }
% }

synpco_example_prod_cons_corr(SynPCo) :-
   BufferSize is 10000,
   BufferSizeMinOne is BufferSize-1,
   SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
   PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
   PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    States1 = [p0,p1,p2,p3,p4,p5,p_f],
    States2 = [q0,q1,q2,q3,q4,q5,q_f],
    StackSym1 = [sym(bot1)],
    StackSym2 = [sym(bot2)],
    InputSym1 = [],
    InputSym2 = [],
    Q01 = p0,
    Q02 = q0,
    Z_01 = bot1,
    Z_02 = bot2,
    Qacc1 = p_f,
    Qacc2 = q_f,
    VarNames = [fillcount,emptycount],
    Trans1 = [%incrementing emptycount
    	      tran(from(p0,[sym(bot1)],leq([emptycount],[BufferSizeMinOne])),[],
                   to(p0,[sym(bot1)],[(emptycount,1)])),
	      tran(from(p1,[sym(bot1)],true),[],to(p2,[sym(bot1)],[])),
	      tran(from(p2,[sym(bot1)],true),[],to(p3,[sym(bot1)],[])),
	      tran(from(p3,[sym(bot1)],gt([emptycount],[0])),[],
	      			to(p4,[sym(bot1)],[(emptycount,-1)])),
	      tran(from(p4,[sym(bot1)],true),[],to(p5,[sym(bot1)],[])),
	      tran(from(p5,[sym(bot1)],lt([fillcount],[BufferSize])),[],
	      			to(p1,[sym(bot1)],[(fillcount,1)]))
	     ],
    Trans2 = [
	     tran(from(q1,[sym(bot2)],true),[],to(q2,[sym(bot2)],[])),
    	     tran(from(q2,[sym(bot2)],gt([fillcount],[0])),[],
	     			to(q3,[sym(bot2)],[(fillcount,-1)])),
	     tran(from(q3,[sym(bot2)],true),[],to(q4,[sym(bot2)],[])),
	     tran(from(q4,[sym(bot2)],lt([emptycount],[BufferSize])),[],
	     			to(q5,[sym(bot2)],[(emptycount,1)])),
	     tran(from(q5,[sym(bot2)],true),[],to(q1,[sym(bot2)],[]))
    	],
    GlobalInputSym = [t(s)],
    GlobalTrans = [
    		tran(from(states(p0,q0),eq([emptycount],[BufferSize])),[t(s)],
                        to(states(p1,q1),[])),
		tran(from(states(p3,q2),and(eq([emptycount],[0]),
					    eq([fillcount],[0]))),
		     [t(s)],
		     to(states(p_f,q_f),[])),
		tran(from(states(p5,q4),and(eq([fillcount],[BufferSize]),
					    eq([fillcount],[BufferSize]))),
		     [t(s)],
		     to(states(p_f,q_f),[]))
			].

   

/*
synpco_example_prod_cons_stack(SynPCo) :-
    BufferSize is 10000,
    BufferSizeMinOne is BufferSize-1,
    SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
    PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
    PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    %States1 = [p1,p2,p3,p4,p5,p6,p7,p8,pacc],
    %States2 = [q1,q2,q3,q4,q5,q6,q7,q8,qacc],
    States1 = [pawake,p8,pasleep],
    States2 = [qawake,q7,qasleep],
    StackSym1 = [sym(p1),sym(p2),sym(p3),sym(p5),
                 sym(p6),sym(p7)],
    StackSym2 = [sym(q1),sym(q2),sym(q4),sym(q5),
                 sym(q6),sym(q8)],
    InputSym1 = [t(a)],
    InputSym2 = [t(b)],
    Q01 = pawake,
    Q02 = qawake,
    Z_01 = p1,
    Z_02 = q1,
    Qacc1 = pasleep,
    Qacc2 = qasleep,
    VarNames = [itemcount],
    Trans1 = [tran(from(pawake,[sym(p1)],true),[t(a)],
                   to(pawake,[sym(p2)],[])),
              tran(from(pawake,[sym(p2)],true),[],
                   to(pawake,[sym(p3)],[])),
              tran(from(pawake,[sym(p3)],eq([itemcount],[BufferSize])),[],
                   to(pasleep,[sym(p5)],[])),
              tran(from(pawake,[sym(p3)],or(lt([itemcount],[BufferSize]),
                                          gt([itemcount],[BufferSize]))
                       ),
                   [],
                   to(pawake,[sym(p5)],[])),
              tran(from(pawake,[sym(p5)],true),[],
                   to(pawake,[sym(p6)],[])),
              tran(from(pawake,[sym(p6)],true),[],
                   to(pawake,[sym(p7)],[(itemcount,1)])),
              tran(from(pawake,[sym(p7)],eq([itemcount],[1])),[],
                   to(p8,[sym(p1)],[])),
              tran(from(pawake,[sym(p7)],or(lt([itemcount],[1]),
                                          gt([itemcount],[1]))
                       ),
                   [],
                   to(pawake,[sym(p1)],[]))],
    Trans2 = [tran(from(qawake,[sym(q1)],true),[t(b)],
                   to(qawake,[sym(q2)],[])),
              tran(from(qawake,[sym(q2)],eq([itemcount],[0])),[],
                   to(qasleep,[sym(q4)],[])),
              tran(from(qawake,[sym(q2)],gt([itemcount],[0])),[],
                   to(qawake,[sym(q4)],[])),
              tran(from(qawake,[sym(q4)],true),[],
                   to(qawake,[sym(q5)],[])),
              tran(from(qawake,[sym(q5)],true),[],
                   to(qawake,[sym(q6)],[(itemcount,-1)])),
              tran(from(qawake,[sym(q6)],eq([itemcount],[BufferSizeMinOne])),[],
                   to(q7,[sym(q8)],[])),
              tran(from(qawake,[sym(q6)],or(lt([itemcount],[BufferSizeMinOne]),
                                          gt([itemcount],[BufferSizeMinOne])
                                         )),
                   [],
                   to(qawake,[sym(q8)],[])),
              tran(from(qawake,[sym(q8)],true),[],
                   to(qawake,[sym(q1)],[]))
              ],
    GlobalInputSym = [t(s)],
    GlobalTrans = [tran(from(states(pasleep,q7),true),[t(s)],
                        to(states(pawake,qawake),[])),
                   tran(from(states(p8,qasleep),true),[t(s)],
                        to(states(pawake,qawake),[]))].
                   %tran(from(states(p4,q3),true),[t(s)],
                        %to(states(pacc,qacc),[]))].
*/

% question: can last bean be black if y is even initially?
synpco_example_gries(SynPCo) :-
   %BufferSize is 10000,
   %BufferSizeMinOne is BufferSize-1,
   SynPCo = synpco(1,[PCo1],GlobalTrans,GlobalInputSym,VarNames),
   PCo1 = pco(States1,StackSym1,InputSym1,Trans1,(Q01,sym(Z_01)),Qacc1),
   %PCo2 = pco(States2,StackSym2,InputSym2,Trans2,(Q02,sym(Z_02)),Qacc2),
    States1 = [p,q,xtest,r],
    StackSym1 = [sym(bot),sym(a)],
    InputSym1 = [],
    Q01 = p,
    Z_01 = bot,
    Qacc1 = r,
    VarNames = [y],
    Trans1 = [%incrementing x,y
          tran(from(p,[sym(bot)],true),[],to(p,[sym(bot),sym(a)],[])),
          tran(from(p,[sym(a)],true),[],to(p,[sym(a),sym(a)],[])),
          tran(from(p,[sym(bot)],true),[],to(p,[sym(bot)],[(y,2)])),
          tran(from(p,[sym(a)],true),[],to(p,[sym(a)],[(y,2)])),
          tran(from(p,[sym(bot)],true),[],to(q,[sym(bot)],[])),
          tran(from(p,[sym(a)],true),[],to(q,[sym(a)],[])),
          
          tran(from(q,[sym(a)],true),[],to(xtest,[],[])),
          tran(from(xtest,[sym(bot)],true),[],to(q,[sym(bot),sym(a)],[])),
          tran(from(xtest,[sym(a)],true),[],to(q,[sym(a)],[])),

          tran(from(q,[sym(a)],gt([y],[0])),[],to(q,[],[])),

          tran(from(q,[sym(a)],gt([y],[1])),[],to(q,[sym(a),sym(a)],[(y,-2)])),
          tran(from(q,[sym(bot)],gt([y],[1])),[],
                    to(q,[sym(bot),sym(a)],[(y,-2)])),

          tran(from(q,[sym(bot)],eq([y],[1])),[],to(r,[sym(bot)],[]))
          %tran(from(xtest2,[sym(a)],true),[],to(q,[sym(a),sym(a)],[])),
          %tran(from(xtest2,[sym(bot)],true),[],to(r,[sym(bot)],[]))
	     ],
    GlobalInputSym = [],
    GlobalTrans = [].
			
