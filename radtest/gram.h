/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EOL = 258,
     AUTH = 259,
     ACCT = 260,
     SEND = 261,
     EXPECT = 262,
     T_BEGIN = 263,
     T_END = 264,
     IF = 265,
     ELSE = 266,
     WHILE = 267,
     DO = 268,
     BREAK = 269,
     CONTINUE = 270,
     INPUT = 271,
     SHIFT = 272,
     GETOPT = 273,
     CASE = 274,
     IN = 275,
     T_RETURN = 276,
     SET = 277,
     PRINT = 278,
     EXIT = 279,
     T_BOGUS = 280,
     ARGCOUNT = 281,
     IDENT = 282,
     PARM = 283,
     NAME = 284,
     NUMBER = 285,
     QUOTE = 286,
     BSTRING = 287,
     IPADDRESS = 288,
     PRITEM = 289,
     OR = 290,
     AND = 291,
     NE = 292,
     EQ = 293,
     GE = 294,
     GT = 295,
     LE = 296,
     LT = 297,
     NOT = 298,
     UMINUS = 299
   };
#endif
/* Tokens.  */
#define EOL 258
#define AUTH 259
#define ACCT 260
#define SEND 261
#define EXPECT 262
#define T_BEGIN 263
#define T_END 264
#define IF 265
#define ELSE 266
#define WHILE 267
#define DO 268
#define BREAK 269
#define CONTINUE 270
#define INPUT 271
#define SHIFT 272
#define GETOPT 273
#define CASE 274
#define IN 275
#define T_RETURN 276
#define SET 277
#define PRINT 278
#define EXIT 279
#define T_BOGUS 280
#define ARGCOUNT 281
#define IDENT 282
#define PARM 283
#define NAME 284
#define NUMBER 285
#define QUOTE 286
#define BSTRING 287
#define IPADDRESS 288
#define PRITEM 289
#define OR 290
#define AND 291
#define NE 292
#define EQ 293
#define GE 294
#define GT 295
#define LE 296
#define LT 297
#define NOT 298
#define UMINUS 299




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 149 "gram.y"
{
	int i;
	long number;
	char *string;
	radtest_node_t *node;
	radtest_node_deref_var_t deref;
	radtest_node_deref_parm_t parm;
	grad_uint32_t ipaddr;
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
}
/* Line 1489 of yacc.c.  */
#line 159 "gram.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

