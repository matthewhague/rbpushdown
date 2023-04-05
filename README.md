
# Model-Checking Tools for Reversal-Bounded Pushdown Systems

I am not entirely sure what's in these directories, they are projects dug up from history… It looks like:

* pushdowncounters -- CAV 2011 implementation of model-checking pushdown systems with reversal-bounded counters. Translates to SMTLIB (2011 version) formula that can be checking with an SMT solver.
* syncpco2z3 and pushdowntranslator -- CAV 2012 implementation of model-checking concurrent pushdown systems with reversal-bounded counters. Translates to SMTLIB (2012 version) formula. pushdowntranslator takes input similar to CAV 2011 and outputs a Prolog definition of the pushdown system that can be converted to SMTLIB with syncpco2z3.
