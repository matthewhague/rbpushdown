%{
#include <sstream>

#include "../structures/program.h"

#include "parser.hpp"

#define TOKEN(t)   (yylval.token = t)
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define SAVE_INT   { std::istringstream ss(yytext); ss >> yylval.integer; }


extern "C" int yywrap() { return 1; }
%}

%option nounput
%option yylineno

%%

"goto"                  return TOKEN(GOTO);
"skip"                  return TOKEN(SKIPCMD);
"tt"                    return TOKEN(BTRUE);
"ff"                    return TOKEN(BFALSE);
"switches"              return TOKEN(CONTEXTSWITCHES);
"reversals"             return TOKEN(REVERSALS);
"constraint"            return TOKEN(CONSTRAINT);
"unlock"                return TOKEN(UNLOCK);
"lock"                  return TOKEN(LOCK);
"shared bool"           return TOKEN(SHAREDBOOL);
"assert"                return TOKEN(ASSERT);
"case:"                 return TOKEN(CASE);
"switch"                return TOKEN(SWITCH);
"??"                    return TOKEN(NONDET);
"return"                return TOKEN(RETURN);
"echo"                  return TOKEN(EECHO);
"begin"                 return TOKEN(BLK_BEGIN);
"end"                   return TOKEN(BLK_END);
"bool"                  return TOKEN(BOOL);
"counter"               return TOKEN(COUNTER);
"freevar"               return TOKEN(FREEVARIABLE);
"procedure"             return TOKEN(PDSPROCEDURE);
"if"                    return TOKEN(IF);
"then"                  return TOKEN(THEN);
"else"                  return TOKEN(ELSE);
"while"                 return TOKEN(WHILE);
"do"                    return TOKEN(DO);
"start"                 return TOKEN(START);
[0-9]+                  SAVE_INT; return INTEGER;
[a-zA-Z_][a-zA-Z0-9_]*  SAVE_TOKEN; return IDENTIFIER;
"=="                    return TOKEN(CEQ);
"<"                     return TOKEN(CLT);
">"                     return TOKEN(CGT);
"<="                    return TOKEN(CLTE);
">="                    return TOKEN(CGTE);
"!="                    return TOKEN(CNEQ);
","                     return TOKEN(COMMA);
"("                     return TOKEN(LPAREN);
")"                     return TOKEN(RPAREN);
"{"                     return TOKEN(LBRACE);
"}"                     return TOKEN(RBRACE);
"["                     return TOKEN(LSQUARE);
"]"                     return TOKEN(RSQUARE);
"="                     return TOKEN(ASSIGN);
":"                     return TOKEN(COLON);
";"                     return TOKEN(SEMI_COLON);
"!"                     return TOKEN(NOT);
"&&"                    return TOKEN(AND);
"||"                    return TOKEN(OR);
"=>"                    return TOKEN(IMPLIES);
"++"                    return TOKEN(INCR);
"--"                    return TOKEN(DECR);
"+="                    return TOKEN(ADD);
"-="                    return TOKEN(SUBTRACT);
"+"                     return TOKEN(EPLUS);
"-"                     return TOKEN(EMINUS);
"*"                     return TOKEN(EMULT);
[ \t\n]					;
.						yyterminate();

%%
