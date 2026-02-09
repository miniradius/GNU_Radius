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

#ifndef YY_UYY_USERS_GRAM_H_INCLUDED
# define YY_UYY_USERS_GRAM_H_INCLUDED
/* Debug traces.  */
#ifndef UYYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define UYYDEBUG 1
#  else
#   define UYYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define UYYDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined UYYDEBUG */
#if UYYDEBUG
extern int uyydebug;
#endif
/* "%code requires" blocks.  */
#line 56 "users-gram.y"

#define yylloc uyylloc
#define yylval uyylval

#line 62 "users-gram.h"

/* Token kinds.  */
#ifndef UYYTOKENTYPE
# define UYYTOKENTYPE
  enum uyytokentype
  {
    UYYEMPTY = -2,
    UYYEOF = 0,                    /* "end of file"  */
    UYYerror = 256,                /* error  */
    UYYUNDEF = 257,                /* "invalid token"  */
    EQ = 258,                      /* EQ  */
    LT = 259,                      /* LT  */
    GT = 260,                      /* GT  */
    NE = 261,                      /* NE  */
    LE = 262,                      /* LE  */
    GE = 263,                      /* GE  */
    NUL = 264,                     /* NUL  */
    BOGUS = 265,                   /* BOGUS  */
    STRING = 266,                  /* STRING  */
    QUOTE = 267                    /* QUOTE  */
  };
  typedef enum uyytokentype uyytoken_kind_t;
#endif

/* Value type.  */
#if ! defined UYYSTYPE && ! defined UYYSTYPE_IS_DECLARED
union UYYSTYPE
{
#line 74 "users-gram.y"

	char *string;
	grad_matching_rule_t *rule;
	struct {
		grad_avp_t *lhs, *rhs;
	} descr;
	grad_avp_t *pair;
	enum grad_operator op;

#line 101 "users-gram.h"

};
typedef union UYYSTYPE UYYSTYPE;
# define UYYSTYPE_IS_TRIVIAL 1
# define UYYSTYPE_IS_DECLARED 1
#endif


extern UYYSTYPE uyylval;


int uyyparse (void);


#endif /* !YY_UYY_USERS_GRAM_H_INCLUDED  */
