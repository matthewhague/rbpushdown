#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/home/phoenix/object/new.hpp>
#include <boost/spirit/home/phoenix/object/construct.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/home/phoenix/bind/bind_member_function.hpp>
#include <boost/make_shared.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>

#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <iterator>

#include "problem.h"
#include "epresburger.h"
#include "pds.h"

using namespace pres;
using namespace pds;

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phoenix = boost::phoenix;
namespace spirit = boost::spirit;


template <typename Iterator>
struct problem_parser 
  : qi::grammar<Iterator, Problem(), ascii::space_type>
{
    problem_parser()
      : problem_parser::base_type(problem)
    {
        using qi::lit;
        using qi::int_;
        using qi::lexeme;
        using phoenix::new_;
        using phoenix::construct;
        using phoenix::push_back;
        using phoenix::insert;
        using phoenix::bind;
        using namespace qi::labels;

        problem = (lit("pds") >> '{'
                  >> *(id_p >> id_p >> 
                       lit("--") >> id_p >> lit("-->") >> 
                       id_p >> id_list >> rule_guard >> 
                       rule_incs >> rule_decs >> ';')
                         [bind(&Problem::add_pds_rule, 
                               _val, 
                               construct<rule_const_ptr>(new_<Rule>(_1,_2,_3,_4,_5,_6,_7,_8)))]
                  >> '}') 
                  >> lit("inits") >> '{'
                  >> id_p [bind(&Problem::set_init_control, _val, _1)]
                  >> id_p [bind(&Problem::set_init_char, _val, _1)]
                  >> '}'
                  >> lit("final") >> '{'
                  >> id_p [bind(&Problem::set_final_control, _val, _1)]
                  >> '}'
                  >> lit("reversals") >> '{'
                  >> int_ [bind(&Problem::set_reversals, _val, _1)]
                  >> '}'
                  >> lit("constraint") >> '{'
                  >> p_fmla [bind(&Problem::set_constraint, _val, _1)] 
                  >> '}';

        id_list = *id_p;
        id_p= lexeme[+(qi::char_("a-zA-Z_0-9"))];

        rule_guard = -(lit("[") >> rg_fmla >> lit("]"));
        rg_fmla = (rg_atom | rg_conj | rg_disj | rg_neg);

        rg_atom = lit("(") >> (rg_zero | rg_nonzero) >> lit(")");
        rg_zero = (id_p >> lit("==") >> lit("0"))
                      [_val = construct<rule_guard_ptr>(new_<RGZero>(_1))];
        rg_nonzero = (id_p >> lit(">") >> lit("0"))
                      [_val = construct<rule_guard_ptr>(new_<RGNonZero>(_1))];

        rg_conj = (lit("(") >> rg_conj_list >> lit(")"))
                      [_val = construct<rule_guard_ptr>(new_<RGConj>(_1))];
        rg_conj_list = +(rg_fmla >> lit("&&")) >> rg_fmla;

        rg_disj = (lit("(") >> rg_disj_list >> lit(")"))
                      [_val = construct<rule_guard_ptr>(new_<RGDisj>(_1))];
        rg_disj_list = +(rg_fmla >> lit("||")) >> rg_fmla;

        rg_neg = lit("!") >> rg_fmla[_val = construct<rule_guard_ptr>(new_<RGNeg>(_1))];

        rule_incs = -(lit("+(") >> id_set >> lit(")"));

        rule_decs = -(lit("-(") >> id_set >> lit(")"));

        id_set = *id_p;

        p_fmla = (p_atom | p_conj | p_disj | p_neg | p_implies | p_exists | p_true | p_false);

        p_atom = lit("(") >> (p_eq | p_lt | p_lte | p_gt | p_gte) >> lit(")");
        p_eq = (p_val >> lit("==") >> p_val)
                      [_val = construct<pres_ptr>(new_<EPEqual>(_1, _2))];
        p_lt = (p_val >> lit("<") >> p_val)
                      [_val = construct<pres_ptr>(new_<EPLessThan>(_1, _2))];
        p_lte = (p_val >> lit("<=") >> p_val)
                      [_val = construct<pres_ptr>(new_<EPLessThanEq>(_1, _2))];
        p_gt = (p_val >> lit(">") >> p_val)
                      [_val = construct<pres_ptr>(new_<EPLessThan>(_2, _1))];
        p_gte = (p_val >> lit(">=") >> p_val)
                      [_val = construct<pres_ptr>(new_<EPLessThanEq>(_2, _1))];

        p_val = p_sum_list [_val = construct<pres_val_ptr>(new_<EPPlus>(_1))];
        p_sum_list = *(p_operand >> '+') >> p_operand;
        p_operand = int_ [_val = construct<pres_val_ptr>(new_<EPInteger>(_1))]   
                    | (int_ >> '*' >> id_p) 
                         [_val = construct<pres_val_ptr>(new_<EPConstMult>(_1, construct<pres_val_ptr>(new_<EPVar>(_2))))] 
                    | (int_ >> '*' >> '(' >> p_val >> ')') 
                         [construct<pres_val_ptr>(new_<EPConstMult>(_1, _2))]
                    | id_p [_val = construct<pres_val_ptr>(new_<EPVar>(_1))];


        p_conj = (lit("(") >> p_conj_list >> lit(")"))
                      [_val = construct<pres_ptr>(new_<EPConj>(_1))];
        p_conj_list = +(p_fmla >> lit("&&")) >> p_fmla;

        p_disj = (lit("(") >> p_disj_list >> lit(")"))
                      [_val = construct<pres_ptr>(new_<EPDisj>(_1))];
        p_disj_list = +(p_fmla >> lit("||")) >> p_fmla;

        p_neg = lit("!") >> p_fmla[_val = construct<pres_ptr>(new_<EPNeg>(_1))];

        p_implies = ('(' >> p_fmla >> lit("==>") >> p_fmla >> ')')
                          [_val = construct<pres_ptr>(new_<EPImplies>(_1, _2))];

        p_exists = (lit("(E)") >> '(' >> p_var_list >> ')' >> '(' >> p_fmla >> ')')
                          [_val = construct<pres_ptr>(new_<EPExists>(_1, _2))];

        p_var_list = *((id_p >> ',')[insert(_val, _1)]) 
                     >> id_p [insert(_val, _1)];

        p_true = lit("true")
                     [_val = construct<pres_ptr>(new_<EPBool>(1))];
        p_false = lit("false")
                     [_val = construct<pres_ptr>(new_<EPBool>(0))];

    }
 
    qi::rule<Iterator, Problem(), ascii::space_type> problem;
    qi::rule<Iterator, Problem(), ascii::space_type> pds;
    qi::rule<Iterator, vector<string>(), ascii::space_type> id_list;
    qi::rule<Iterator, string(), ascii::space_type> id_p;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rule_guard;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_fmla;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_atom;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_zero;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_nonzero;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_conj;
    qi::rule<Iterator, vector<rule_guard_ptr>(), ascii::space_type> rg_conj_list;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_disj;
    qi::rule<Iterator, vector<rule_guard_ptr>(), ascii::space_type> rg_disj_list;
    qi::rule<Iterator, rule_guard_ptr(), ascii::space_type> rg_neg;
    qi::rule<Iterator, set<string>(), ascii::space_type> rule_incs;
    qi::rule<Iterator, set<string>(), ascii::space_type> rule_decs;
    qi::rule<Iterator, set<string>(), ascii::space_type> id_set;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_fmla;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_atom;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_eq;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_lt;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_lte;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_gt;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_gte;
    qi::rule<Iterator, pres_val_ptr(), ascii::space_type> p_val;
    qi::rule<Iterator, vector<pres_val_ptr>(), ascii::space_type> p_sum_list;
    qi::rule<Iterator, pres_val_ptr(), ascii::space_type> p_operand ;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_conj;
    qi::rule<Iterator, vector<pres_ptr>(), ascii::space_type> p_conj_list;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_disj;
    qi::rule<Iterator, vector<pres_ptr>(), ascii::space_type> p_disj_list;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_neg;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_implies;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_true;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_false;
    qi::rule<Iterator, pres_ptr(), ascii::space_type> p_exists;
    qi::rule<Iterator, set<string>(), ascii::space_type> p_var_list;
};

///////////////////////////////////////////////////////////////////////////////
bool Problem::parse_problem(string const& description)
{
    namespace qi = boost::spirit::qi;

    std::string::const_iterator begin = description.begin();
    std::string::const_iterator end = description.end();

    problem_parser<std::string::const_iterator> p;
    
    bool r = qi::phrase_parse(begin, end, p, ascii::space, *this);

    return !(!r || begin != end);
}

bool Problem::parse_problem_file(string const& file_name)
{
    namespace qi = boost::spirit::qi;

    ifstream file(file_name);
    if (!file) {
        cerr << "Failed to open " << file_name << endl;
        return 0;
    }
    file.unsetf(ios_base::skipws);

    typedef std::istream_iterator<char> base_iterator_type;
    spirit::multi_pass<base_iterator_type> first = 
        spirit::make_default_multi_pass(base_iterator_type(file));


    problem_parser<spirit::multi_pass<base_iterator_type>> p;
    
    bool r = qi::phrase_parse(first, 
                              spirit::make_default_multi_pass(base_iterator_type()), 
                              p, 
                              ascii::space, 
                              *this);

    return !(!r || first != spirit::make_default_multi_pass(base_iterator_type()));
}


Problem::~Problem() {
}


ostream& operator<<(ostream& output, Problem const& prob) {
    output << "Pds:\n" << prob.pds << "\n";
    output << "Inits: " << prob.init_control << " " << prob.init_char << "\n";
    output << "Final: " << prob.final_control << "\n";
    output << "Reversals: " << prob.nreversals << "\n";
}

