/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_GRAM_H_INCLUDED
# define YY_YY_GRAM_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    EOL = 258,                     /* EOL  */
    AUTH = 259,                    /* AUTH  */
    ACCT = 260,                    /* ACCT  */
    SEND = 261,                    /* SEND  */
    EXPECT = 262,                  /* EXPECT  */
    T_BEGIN = 263,                 /* T_BEGIN  */
    T_END = 264,                   /* T_END  */
    IF = 265,                      /* IF  */
    ELSE = 266,                    /* ELSE  */
    WHILE = 267,                   /* WHILE  */
    DO = 268,                      /* DO  */
    BREAK = 269,                   /* BREAK  */
    CONTINUE = 270,                /* CONTINUE  */
    INPUT = 271,                   /* INPUT  */
    SHIFT = 272,                   /* SHIFT  */
    GETOPT = 273,                  /* GETOPT  */
    CASE = 274,                    /* CASE  */
    IN = 275,                      /* IN  */
    T_RETURN = 276,                /* T_RETURN  */
    SET = 277,                     /* SET  */
    PRINT = 278,                   /* PRINT  */
    EXIT = 279,                    /* EXIT  */
    T_BOGUS = 280,                 /* T_BOGUS  */
    ARGCOUNT = 281,                /* ARGCOUNT  */
    IDENT = 282,                   /* IDENT  */
    PARM = 283,                    /* PARM  */
    NAME = 284,                    /* NAME  */
    NUMBER = 285,                  /* NUMBER  */
    QUOTE = 286,                   /* QUOTE  */
    BSTRING = 287,                 /* BSTRING  */
    IPADDRESS = 288,               /* IPADDRESS  */
    PRITEM = 289,                  /* PRITEM  */
    OR = 290,                      /* OR  */
    AND = 291,                     /* AND  */
    EQ = 292,                      /* EQ  */
    NE = 293,                      /* NE  */
    LT = 294,                      /* LT  */
    LE = 295,                      /* LE  */
    GT = 296,                      /* GT  */
    GE = 297,                      /* GE  */
    UMINUS = 298,                  /* UMINUS  */
    NOT = 299                      /* NOT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 149 "gram.y"

	int i;
	long number;
	char *string;
	radtest_node_t *node;
	radtest_node_deref_var_t deref;
	radtest_node_deref_parm_t parm;
	uint32_t ipaddr;
	enum grad_operator op;
	radtest_pair_t *pair;
	grad_list_t *list;
	radtest_variable_t *var;
	grad_symtab_t *symtab;
	struct {
		int argc;
		char **argv;
	} set;
	radtest_case_branch_t *case_branch;
	radtest_function_t *fun;
	radtest_bstring_t bstring;

#line 130 "gram.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_GRAM_H_INCLUDED  */
