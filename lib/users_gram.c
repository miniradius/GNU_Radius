/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with uyy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum uyytokentype {
     EQ = 258,
     LT = 259,
     GT = 260,
     NE = 261,
     LE = 262,
     GE = 263,
     NUL = 264,
     BOGUS = 265,
     STRING = 266,
     QUOTE = 267
   };
#endif
/* Tokens.  */
#define EQ 258
#define LT 259
#define GT 260
#define NE 261
#define LE 262
#define GE 263
#define NUL 264
#define BOGUS 265
#define STRING 266
#define QUOTE 267




/* Copy the first part of user declarations.  */
#line 1 "users.y"

/* This file is part of GNU Radius.
   Copyright (C) 2000,2001,2002,2003,2004,2007,
   2008 Free Software Foundation, Inc.

   Written by Sergey Poznyakoff
  
   GNU Radius is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.
  
   GNU Radius is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public
   License along with GNU Radius; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301 USA. */

#if defined(HAVE_CONFIG_H)
# include <config.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <netinet/in.h>
         
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>

#include <radlib.h>
#include "intprops.h"
	
#define YYMAXDEPTH 10

static grad_locus_t start_loc;

static void *closure;
static register_rule_fp add_entry;
static grad_avp_t *grad_create_pair0(char *name, int op, char *valstr);

int uyyerror(char *s);
extern int uyylex();
 


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 70 "users.y"
{
        char *string;
        grad_matching_rule_t *rule;
        struct {
                grad_avp_t *lhs, *rhs;
        } descr;
        grad_avp_t *pair;
        enum grad_operator op;
}
/* Line 187 of yacc.c.  */
#line 188 "users_gram.c"
	YYSTYPE;
# define uyystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 201 "users_gram.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 uyytype_uint8;
#else
typedef unsigned char uyytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 uyytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char uyytype_int8;
#else
typedef short int uyytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 uyytype_uint16;
#else
typedef unsigned short int uyytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 uyytype_int16;
#else
typedef short int uyytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined uyyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined uyyoverflow || YYERROR_VERBOSE */


#if (! defined uyyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union uyyalloc
{
  uyytype_int16 uyyss;
  YYSTYPE uyyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union uyyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (uyytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T uyyi;				\
	  for (uyyi = 0; uyyi < (Count); uyyi++)	\
	    (To)[uyyi] = (From)[uyyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T uyynewbytes;						\
	YYCOPY (&uyyptr->Stack, Stack, uyysize);				\
	Stack = &uyyptr->Stack;						\
	uyynewbytes = uyystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	uyyptr += uyynewbytes / sizeof (*uyyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  14
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  11
/* YYNRULES -- Number of rules.  */
#define YYNRULES  24
/* YYNRULES -- Number of states.  */
#define YYNSTATES  30

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   267

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? uyytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const uyytype_uint8 uyytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    13,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const uyytype_uint8 uyyprhs[] =
{
       0,     0,     3,     4,     6,     8,    11,    14,    17,    20,
      22,    25,    27,    29,    31,    35,    39,    43,    45,    47,
      49,    51,    53,    55,    57
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const uyytype_int8 uyyrhs[] =
{
      15,     0,    -1,    -1,    16,    -1,    17,    -1,    16,    17,
      -1,    16,     1,    -1,    18,    19,    -1,    18,     1,    -1,
      24,    -1,    20,    20,    -1,     9,    -1,    21,    -1,    22,
      -1,    21,    13,    22,    -1,    11,    23,    24,    -1,    11,
      23,    10,    -1,     3,    -1,     4,    -1,     5,    -1,     6,
      -1,     7,    -1,     8,    -1,    11,    -1,    12,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const uyytype_uint8 uyyrline[] =
{
       0,    82,    82,    83,    86,    89,    90,    96,   100,   112,
     118,   125,   129,   132,   133,   144,   148,   154,   158,   162,
     166,   170,   174,   180,   181
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const uyytname[] =
{
  "$end", "error", "$undefined", "EQ", "LT", "GT", "NE", "LE", "GE",
  "NUL", "BOGUS", "STRING", "QUOTE", "','", "$accept", "input", "list",
  "entry", "user", "descr", "npairlist", "pairlist", "pair", "op", "value", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const uyytype_uint16 uyytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const uyytype_uint8 uyyr1[] =
{
       0,    14,    15,    15,    16,    16,    16,    17,    17,    18,
      19,    20,    20,    21,    21,    22,    22,    23,    23,    23,
      23,    23,    23,    24,    24
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const uyytype_uint8 uyyr2[] =
{
       0,     2,     0,     1,     1,     2,     2,     2,     2,     1,
       2,     1,     1,     1,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const uyytype_uint8 uyydefact[] =
{
       2,    23,    24,     0,     0,     4,     0,     9,     1,     6,
       5,     8,    11,     0,     7,     0,    12,    13,    17,    18,
      19,    20,    21,    22,     0,    10,     0,    16,    15,    14
};

/* YYDEFGOTO[NTERM-NUM].  */
static const uyytype_int8 uyydefgoto[] =
{
      -1,     3,     4,     5,     6,    14,    15,    16,    17,    24,
       7
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -9
static const uyytype_int8 uyypact[] =
{
      -2,    -9,    -9,     7,     0,    -9,     4,    -9,    -9,    -9,
      -9,    -9,    -9,    13,    -9,    -3,     1,    -9,    -9,    -9,
      -9,    -9,    -9,    -9,    -8,    -9,    11,    -9,    -9,    -9
};

/* YYPGOTO[NTERM-NUM].  */
static const uyytype_int8 uyypgoto[] =
{
      -9,    -9,    -9,    19,    -9,    -9,     9,    -9,    -1,    -9,
       2
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -4
static const uyytype_int8 uyytable[] =
{
      -3,     9,    27,     1,     2,    11,    12,     8,    13,     1,
       2,     1,     2,    12,    26,    13,    18,    19,    20,    21,
      22,    23,    13,    10,    25,    29,    28
};

static const uyytype_uint8 uyycheck[] =
{
       0,     1,    10,    11,    12,     1,     9,     0,    11,    11,
      12,    11,    12,     9,    13,    11,     3,     4,     5,     6,
       7,     8,    11,     4,    15,    26,    24
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const uyytype_uint8 uyystos[] =
{
       0,    11,    12,    15,    16,    17,    18,    24,     0,     1,
      17,     1,     9,    11,    19,    20,    21,    22,     3,     4,
       5,     6,     7,     8,    23,    20,    13,    10,    24,    22
};

#define uyyerrok		(uyyerrstatus = 0)
#define uyyclearin	(uyychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto uyyacceptlab
#define YYABORT		goto uyyabortlab
#define YYERROR		goto uyyerrorlab


/* Like YYERROR except do call uyyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto uyyerrlab

#define YYRECOVERING()  (!!uyyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (uyychar == YYEMPTY && uyylen == 1)				\
    {								\
      uyychar = (Token);						\
      uyylval = (Value);						\
      uyytoken = YYTRANSLATE (uyychar);				\
      YYPOPSTACK (1);						\
      goto uyybackup;						\
    }								\
  else								\
    {								\
      uyyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `uyylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX uyylex (YYLEX_PARAM)
#else
# define YYLEX uyylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (uyydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (uyydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      uyy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
uyy_symbol_value_print (FILE *uyyoutput, int uyytype, YYSTYPE const * const uyyvaluep)
#else
static void
uyy_symbol_value_print (uyyoutput, uyytype, uyyvaluep)
    FILE *uyyoutput;
    int uyytype;
    YYSTYPE const * const uyyvaluep;
#endif
{
  if (!uyyvaluep)
    return;
# ifdef YYPRINT
  if (uyytype < YYNTOKENS)
    YYPRINT (uyyoutput, uyytoknum[uyytype], *uyyvaluep);
# else
  YYUSE (uyyoutput);
# endif
  switch (uyytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
uyy_symbol_print (FILE *uyyoutput, int uyytype, YYSTYPE const * const uyyvaluep)
#else
static void
uyy_symbol_print (uyyoutput, uyytype, uyyvaluep)
    FILE *uyyoutput;
    int uyytype;
    YYSTYPE const * const uyyvaluep;
#endif
{
  if (uyytype < YYNTOKENS)
    YYFPRINTF (uyyoutput, "token %s (", uyytname[uyytype]);
  else
    YYFPRINTF (uyyoutput, "nterm %s (", uyytname[uyytype]);

  uyy_symbol_value_print (uyyoutput, uyytype, uyyvaluep);
  YYFPRINTF (uyyoutput, ")");
}

/*------------------------------------------------------------------.
| uyy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
uyy_stack_print (uyytype_int16 *bottom, uyytype_int16 *top)
#else
static void
uyy_stack_print (bottom, top)
    uyytype_int16 *bottom;
    uyytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (uyydebug)							\
    uyy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
uyy_reduce_print (YYSTYPE *uyyvsp, int uyyrule)
#else
static void
uyy_reduce_print (uyyvsp, uyyrule)
    YYSTYPE *uyyvsp;
    int uyyrule;
#endif
{
  int uyynrhs = uyyr2[uyyrule];
  int uyyi;
  unsigned long int uyylno = uyyrline[uyyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     uyyrule - 1, uyylno);
  /* The symbols being reduced.  */
  for (uyyi = 0; uyyi < uyynrhs; uyyi++)
    {
      fprintf (stderr, "   $%d = ", uyyi + 1);
      uyy_symbol_print (stderr, uyyrhs[uyyprhs[uyyrule] + uyyi],
		       &(uyyvsp[(uyyi + 1) - (uyynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (uyydebug)				\
    uyy_reduce_print (uyyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int uyydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef uyystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define uyystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
uyystrlen (const char *uyystr)
#else
static YYSIZE_T
uyystrlen (uyystr)
    const char *uyystr;
#endif
{
  YYSIZE_T uyylen;
  for (uyylen = 0; uyystr[uyylen]; uyylen++)
    continue;
  return uyylen;
}
#  endif
# endif

# ifndef uyystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define uyystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
uyystpcpy (char *uyydest, const char *uyysrc)
#else
static char *
uyystpcpy (uyydest, uyysrc)
    char *uyydest;
    const char *uyysrc;
#endif
{
  char *uyyd = uyydest;
  const char *uyys = uyysrc;

  while ((*uyyd++ = *uyys++) != '\0')
    continue;

  return uyyd - 1;
}
#  endif
# endif

# ifndef uyytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for uyyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from uyytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
uyytnamerr (char *uyyres, const char *uyystr)
{
  if (*uyystr == '"')
    {
      YYSIZE_T uyyn = 0;
      char const *uyyp = uyystr;

      for (;;)
	switch (*++uyyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++uyyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (uyyres)
	      uyyres[uyyn] = *uyyp;
	    uyyn++;
	    break;

	  case '"':
	    if (uyyres)
	      uyyres[uyyn] = '\0';
	    return uyyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! uyyres)
    return uyystrlen (uyystr);

  return uyystpcpy (uyyres, uyystr) - uyyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
uyysyntax_error (char *uyyresult, int uyystate, int uyychar)
{
  int uyyn = uyypact[uyystate];

  if (! (YYPACT_NINF < uyyn && uyyn <= YYLAST))
    return 0;
  else
    {
      int uyytype = YYTRANSLATE (uyychar);
      YYSIZE_T uyysize0 = uyytnamerr (0, uyytname[uyytype]);
      YYSIZE_T uyysize = uyysize0;
      YYSIZE_T uyysize1;
      int uyysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *uyyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int uyyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *uyyfmt;
      char const *uyyf;
      static char const uyyunexpected[] = "syntax error, unexpected %s";
      static char const uyyexpecting[] = ", expecting %s";
      static char const uyyor[] = " or %s";
      char uyyformat[sizeof uyyunexpected
		    + sizeof uyyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof uyyor - 1))];
      char const *uyyprefix = uyyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int uyyxbegin = uyyn < 0 ? -uyyn : 0;

      /* Stay within bounds of both uyycheck and uyytname.  */
      int uyychecklim = YYLAST - uyyn + 1;
      int uyyxend = uyychecklim < YYNTOKENS ? uyychecklim : YYNTOKENS;
      int uyycount = 1;

      uyyarg[0] = uyytname[uyytype];
      uyyfmt = uyystpcpy (uyyformat, uyyunexpected);

      for (uyyx = uyyxbegin; uyyx < uyyxend; ++uyyx)
	if (uyycheck[uyyx + uyyn] == uyyx && uyyx != YYTERROR)
	  {
	    if (uyycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		uyycount = 1;
		uyysize = uyysize0;
		uyyformat[sizeof uyyunexpected - 1] = '\0';
		break;
	      }
	    uyyarg[uyycount++] = uyytname[uyyx];
	    uyysize1 = uyysize + uyytnamerr (0, uyytname[uyyx]);
	    uyysize_overflow |= (uyysize1 < uyysize);
	    uyysize = uyysize1;
	    uyyfmt = uyystpcpy (uyyfmt, uyyprefix);
	    uyyprefix = uyyor;
	  }

      uyyf = YY_(uyyformat);
      uyysize1 = uyysize + uyystrlen (uyyf);
      uyysize_overflow |= (uyysize1 < uyysize);
      uyysize = uyysize1;

      if (uyysize_overflow)
	return YYSIZE_MAXIMUM;

      if (uyyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *uyyp = uyyresult;
	  int uyyi = 0;
	  while ((*uyyp = *uyyf) != '\0')
	    {
	      if (*uyyp == '%' && uyyf[1] == 's' && uyyi < uyycount)
		{
		  uyyp += uyytnamerr (uyyp, uyyarg[uyyi++]);
		  uyyf += 2;
		}
	      else
		{
		  uyyp++;
		  uyyf++;
		}
	    }
	}
      return uyysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
uyydestruct (const char *uyymsg, int uyytype, YYSTYPE *uyyvaluep)
#else
static void
uyydestruct (uyymsg, uyytype, uyyvaluep)
    const char *uyymsg;
    int uyytype;
    YYSTYPE *uyyvaluep;
#endif
{
  YYUSE (uyyvaluep);

  if (!uyymsg)
    uyymsg = "Deleting";
  YY_SYMBOL_PRINT (uyymsg, uyytype, uyyvaluep, uyylocationp);

  switch (uyytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int uyyparse (void *YYPARSE_PARAM);
#else
int uyyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int uyyparse (void);
#else
int uyyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int uyychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE uyylval;

/* Number of syntax errors so far.  */
int uyynerrs;



/*----------.
| uyyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
uyyparse (void *YYPARSE_PARAM)
#else
int
uyyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
uyyparse (void)
#else
int
uyyparse ()

#endif
#endif
{
  
  int uyystate;
  int uyyn;
  int uyyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int uyyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int uyytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char uyymsgbuf[128];
  char *uyymsg = uyymsgbuf;
  YYSIZE_T uyymsg_alloc = sizeof uyymsgbuf;
#endif

  /* Three stacks and their tools:
     `uyyss': related to states,
     `uyyvs': related to semantic values,
     `uyyls': related to locations.

     Refer to the stacks thru separate pointers, to allow uyyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  uyytype_int16 uyyssa[YYINITDEPTH];
  uyytype_int16 *uyyss = uyyssa;
  uyytype_int16 *uyyssp;

  /* The semantic value stack.  */
  YYSTYPE uyyvsa[YYINITDEPTH];
  YYSTYPE *uyyvs = uyyvsa;
  YYSTYPE *uyyvsp;



#define YYPOPSTACK(N)   (uyyvsp -= (N), uyyssp -= (N))

  YYSIZE_T uyystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE uyyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int uyylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  uyystate = 0;
  uyyerrstatus = 0;
  uyynerrs = 0;
  uyychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  uyyssp = uyyss;
  uyyvsp = uyyvs;

  goto uyysetstate;

/*------------------------------------------------------------.
| uyynewstate -- Push a new state, which is found in uyystate.  |
`------------------------------------------------------------*/
 uyynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  uyyssp++;

 uyysetstate:
  *uyyssp = uyystate;

  if (uyyss + uyystacksize - 1 <= uyyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T uyysize = uyyssp - uyyss + 1;

#ifdef uyyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *uyyvs1 = uyyvs;
	uyytype_int16 *uyyss1 = uyyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if uyyoverflow is a macro.  */
	uyyoverflow (YY_("memory exhausted"),
		    &uyyss1, uyysize * sizeof (*uyyssp),
		    &uyyvs1, uyysize * sizeof (*uyyvsp),

		    &uyystacksize);

	uyyss = uyyss1;
	uyyvs = uyyvs1;
      }
#else /* no uyyoverflow */
# ifndef YYSTACK_RELOCATE
      goto uyyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= uyystacksize)
	goto uyyexhaustedlab;
      uyystacksize *= 2;
      if (YYMAXDEPTH < uyystacksize)
	uyystacksize = YYMAXDEPTH;

      {
	uyytype_int16 *uyyss1 = uyyss;
	union uyyalloc *uyyptr =
	  (union uyyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (uyystacksize));
	if (! uyyptr)
	  goto uyyexhaustedlab;
	YYSTACK_RELOCATE (uyyss);
	YYSTACK_RELOCATE (uyyvs);

#  undef YYSTACK_RELOCATE
	if (uyyss1 != uyyssa)
	  YYSTACK_FREE (uyyss1);
      }
# endif
#endif /* no uyyoverflow */

      uyyssp = uyyss + uyysize - 1;
      uyyvsp = uyyvs + uyysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) uyystacksize));

      if (uyyss + uyystacksize - 1 <= uyyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", uyystate));

  goto uyybackup;

/*-----------.
| uyybackup.  |
`-----------*/
uyybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  uyyn = uyypact[uyystate];
  if (uyyn == YYPACT_NINF)
    goto uyydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (uyychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      uyychar = YYLEX;
    }

  if (uyychar <= YYEOF)
    {
      uyychar = uyytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      uyytoken = YYTRANSLATE (uyychar);
      YY_SYMBOL_PRINT ("Next token is", uyytoken, &uyylval, &uyylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  uyyn += uyytoken;
  if (uyyn < 0 || YYLAST < uyyn || uyycheck[uyyn] != uyytoken)
    goto uyydefault;
  uyyn = uyytable[uyyn];
  if (uyyn <= 0)
    {
      if (uyyn == 0 || uyyn == YYTABLE_NINF)
	goto uyyerrlab;
      uyyn = -uyyn;
      goto uyyreduce;
    }

  if (uyyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (uyyerrstatus)
    uyyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", uyytoken, &uyylval, &uyylloc);

  /* Discard the shifted token unless it is eof.  */
  if (uyychar != YYEOF)
    uyychar = YYEMPTY;

  uyystate = uyyn;
  *++uyyvsp = uyylval;

  goto uyynewstate;


/*-----------------------------------------------------------.
| uyydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
uyydefault:
  uyyn = uyydefact[uyystate];
  if (uyyn == 0)
    goto uyyerrlab;
  goto uyyreduce;


/*-----------------------------.
| uyyreduce -- Do a reduction.  |
`-----------------------------*/
uyyreduce:
  /* uyyn is the number of a rule to reduce with.  */
  uyylen = uyyr2[uyyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  uyyval = uyyvsp[1-uyylen];


  YY_REDUCE_PRINT (uyyn);
  switch (uyyn)
    {
        case 4:
#line 87 "users.y"
    {
           }
    break;

  case 6:
#line 91 "users.y"
    {
                   grad_parser_lex_sync(); uyyerrok; uyyclearin;
           }
    break;

  case 7:
#line 97 "users.y"
    {
                   add_entry(closure, &start_loc, (uyyvsp[(1) - (2)].string), (uyyvsp[(2) - (2)].descr).lhs, (uyyvsp[(2) - (2)].descr).rhs);
           }
    break;

  case 8:
#line 101 "users.y"
    {
                   grad_log(GRAD_LOG_ERR, _("discarding user `%s'"), (uyyvsp[(1) - (2)].string));
                   if (grad_parser_lex_sync() <= 0)
			   uyychar = 0; /* force end-of-file */
		   else {
			   uyyerrok;
			   uyyclearin;
		   }
           }
    break;

  case 9:
#line 113 "users.y"
    {
		   start_loc = grad_parser_source_locus;
           }
    break;

  case 10:
#line 119 "users.y"
    {
                   (uyyval.descr).lhs = (uyyvsp[(1) - (2)].pair);
                   (uyyval.descr).rhs = (uyyvsp[(2) - (2)].pair);
           }
    break;

  case 11:
#line 126 "users.y"
    {
                   (uyyval.pair) = NULL;
           }
    break;

  case 14:
#line 134 "users.y"
    {
                   if ((uyyvsp[(1) - (3)].pair)) {
                           if ((uyyvsp[(3) - (3)].pair)) 
                                   grad_avl_add_list(&(uyyvsp[(1) - (3)].pair), (uyyvsp[(3) - (3)].pair));
                           (uyyval.pair) = (uyyvsp[(1) - (3)].pair);
                   } else
                           (uyyval.pair) = (uyyvsp[(3) - (3)].pair);
           }
    break;

  case 15:
#line 145 "users.y"
    {
                   (uyyval.pair) = grad_create_pair0((uyyvsp[(1) - (3)].string), (uyyvsp[(2) - (3)].op), (uyyvsp[(3) - (3)].string));   
           }
    break;

  case 16:
#line 149 "users.y"
    {
                   YYERROR;
           }
    break;

  case 17:
#line 155 "users.y"
    {
                   (uyyval.op) = grad_operator_equal;
           }
    break;

  case 18:
#line 159 "users.y"
    {
                   (uyyval.op) = grad_operator_less_than;
           }
    break;

  case 19:
#line 163 "users.y"
    { 
                   (uyyval.op) = grad_operator_greater_than;
           }
    break;

  case 20:
#line 167 "users.y"
    {
                   (uyyval.op) = grad_operator_not_equal;
           }
    break;

  case 21:
#line 171 "users.y"
    {
                   (uyyval.op) = grad_operator_less_equal;
           }
    break;

  case 22:
#line 175 "users.y"
    {
                   (uyyval.op) = grad_operator_greater_equal;
           }
    break;


/* Line 1267 of yacc.c.  */
#line 1531 "users_gram.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", uyyr1[uyyn], &uyyval, &uyyloc);

  YYPOPSTACK (uyylen);
  uyylen = 0;
  YY_STACK_PRINT (uyyss, uyyssp);

  *++uyyvsp = uyyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  uyyn = uyyr1[uyyn];

  uyystate = uyypgoto[uyyn - YYNTOKENS] + *uyyssp;
  if (0 <= uyystate && uyystate <= YYLAST && uyycheck[uyystate] == *uyyssp)
    uyystate = uyytable[uyystate];
  else
    uyystate = uyydefgoto[uyyn - YYNTOKENS];

  goto uyynewstate;


/*------------------------------------.
| uyyerrlab -- here on detecting error |
`------------------------------------*/
uyyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!uyyerrstatus)
    {
      ++uyynerrs;
#if ! YYERROR_VERBOSE
      uyyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T uyysize = uyysyntax_error (0, uyystate, uyychar);
	if (uyymsg_alloc < uyysize && uyymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T uyyalloc = 2 * uyysize;
	    if (! (uyysize <= uyyalloc && uyyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      uyyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (uyymsg != uyymsgbuf)
	      YYSTACK_FREE (uyymsg);
	    uyymsg = (char *) YYSTACK_ALLOC (uyyalloc);
	    if (uyymsg)
	      uyymsg_alloc = uyyalloc;
	    else
	      {
		uyymsg = uyymsgbuf;
		uyymsg_alloc = sizeof uyymsgbuf;
	      }
	  }

	if (0 < uyysize && uyysize <= uyymsg_alloc)
	  {
	    (void) uyysyntax_error (uyymsg, uyystate, uyychar);
	    uyyerror (uyymsg);
	  }
	else
	  {
	    uyyerror (YY_("syntax error"));
	    if (uyysize != 0)
	      goto uyyexhaustedlab;
	  }
      }
#endif
    }



  if (uyyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (uyychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (uyychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  uyydestruct ("Error: discarding",
		      uyytoken, &uyylval);
	  uyychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto uyyerrlab1;


/*---------------------------------------------------.
| uyyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
uyyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label uyyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto uyyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (uyylen);
  uyylen = 0;
  YY_STACK_PRINT (uyyss, uyyssp);
  uyystate = *uyyssp;
  goto uyyerrlab1;


/*-------------------------------------------------------------.
| uyyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
uyyerrlab1:
  uyyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      uyyn = uyypact[uyystate];
      if (uyyn != YYPACT_NINF)
	{
	  uyyn += YYTERROR;
	  if (0 <= uyyn && uyyn <= YYLAST && uyycheck[uyyn] == YYTERROR)
	    {
	      uyyn = uyytable[uyyn];
	      if (0 < uyyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (uyyssp == uyyss)
	YYABORT;


      uyydestruct ("Error: popping",
		  uyystos[uyystate], uyyvsp);
      YYPOPSTACK (1);
      uyystate = *uyyssp;
      YY_STACK_PRINT (uyyss, uyyssp);
    }

  if (uyyn == YYFINAL)
    YYACCEPT;

  *++uyyvsp = uyylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", uyystos[uyyn], uyyvsp, uyylsp);

  uyystate = uyyn;
  goto uyynewstate;


/*-------------------------------------.
| uyyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
uyyacceptlab:
  uyyresult = 0;
  goto uyyreturn;

/*-----------------------------------.
| uyyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
uyyabortlab:
  uyyresult = 1;
  goto uyyreturn;

#ifndef uyyoverflow
/*-------------------------------------------------.
| uyyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
uyyexhaustedlab:
  uyyerror (YY_("memory exhausted"));
  uyyresult = 2;
  /* Fall through.  */
#endif

uyyreturn:
  if (uyychar != YYEOF && uyychar != YYEMPTY)
     uyydestruct ("Cleanup: discarding lookahead",
		 uyytoken, &uyylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (uyylen);
  YY_STACK_PRINT (uyyss, uyyssp);
  while (uyyssp != uyyss)
    {
      uyydestruct ("Cleanup: popping",
		  uyystos[*uyyssp], uyyvsp);
      YYPOPSTACK (1);
    }
#ifndef uyyoverflow
  if (uyyss != uyyssa)
    YYSTACK_FREE (uyyss);
#endif
#if YYERROR_VERBOSE
  if (uyymsg != uyymsgbuf)
    YYSTACK_FREE (uyymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (uyyresult);
}


#line 184 "users.y"


int
uyyerror(char *s)
{
	grad_log_loc(GRAD_LOG_ERR, &grad_parser_source_locus, "%s", s);
	return 0;
}

grad_avp_t *
grad_create_pair0(char *name, int op, char *valstr)
{
	return grad_create_pair(&grad_parser_source_locus, name, op, valstr);
}

grad_avp_t *
grad_create_pair(grad_locus_t *loc, char *name, 
                 enum grad_operator op, char *valstr)
{
        grad_dict_attr_t *attr = NULL;
        grad_dict_value_t *dval;
        grad_avp_t *pair, *pair2;
        char *s;
        int x;
        time_t timeval;
        struct tm *tm, tms;
        
        if ((attr = grad_attr_name_to_dict(name)) == NULL) {
                grad_log_loc(GRAD_LOG_ERR, loc, _("unknown attribute `%s'"),
			     name);
                return NULL;
        }

        pair = grad_avp_alloc();
        
        pair->next = NULL;
        pair->name = attr->name;
        pair->attribute = attr->value;
        pair->type = attr->type;
        pair->prop = attr->prop;
        pair->operator = op;

        if (valstr[0] == '=') {
                pair->eval_type = grad_eval_interpret;
                pair->avp_strvalue = grad_estrdup(valstr+1);
                pair->avp_strlength = strlen(pair->avp_strvalue);
                return pair;
        }

        pair->eval_type = grad_eval_const;
        
        switch (pair->type) {
        case GRAD_TYPE_STRING:
                if (pair->attribute == DA_EXEC_PROGRAM ||
                    pair->attribute == DA_EXEC_PROGRAM_WAIT) {
                        if (valstr[0] != '/' && valstr[0] != '|') {
                                grad_log_loc(GRAD_LOG_ERR, loc,
					     _("%s: not an absolute pathname"),
					     name);
                                grad_avp_free(pair);
                                return NULL;
                        }
                }

		pair->avp_strvalue = grad_estrdup(valstr);
                pair->avp_strlength = strlen(pair->avp_strvalue);
		
		if (attr->parser && attr->parser(pair, &s)) {
			grad_log_loc(GRAD_LOG_ERR, loc,
				     "%s %s: %s",
				     _("attribute"),
				     pair->name, s);
			free(s);
			grad_avp_free(pair);
			return NULL;
		}

                break;

        case GRAD_TYPE_INTEGER:
                /*
                 *      For DA_NAS_PORT_ID, allow a
                 *      port range instead of just a port.
                 */
                if (attr->value == DA_NAS_PORT_ID) {
                        for (s = valstr; *s; s++)
                                if (!isdigit(*s))
                                        break;
                        if (*s) {
                                pair->type = GRAD_TYPE_STRING;
                                pair->avp_strvalue = grad_estrdup(valstr);
                                pair->avp_strlength = strlen(pair->avp_strvalue);
                                break;
                        }
                }
                if (isdigit(*valstr)) {
                        pair->avp_lvalue = atoi(valstr);
                } else if ((dval = grad_value_name_to_value(valstr,
							    pair->attribute))
			         == NULL) {
                        grad_avp_free(pair);
                        grad_log_loc(GRAD_LOG_ERR, loc,
				_("value %s is not declared for attribute %s"),
				     valstr, name);
                        return NULL;
                } else {
                        pair->avp_lvalue = dval->value;
                }
                break;

        case GRAD_TYPE_IPADDR:
                if (pair->attribute != DA_FRAMED_IP_ADDRESS) {
                        pair->avp_lvalue = grad_ip_gethostaddr(valstr);
                } else {
                        /*
                         *      We allow a "+" at the end to
                         *      indicate that we should add the
                         *      portno. to the IP address.
                         */
                        x = 0;
                        if (valstr[0]) {
                                for(s = valstr; s[1]; s++)
                                        ;
                                if (*s == '+') {
                                        *s = 0;
                                        x = 1;
                                }
                        }
                        pair->avp_lvalue = grad_ip_gethostaddr(valstr);

			if (x) {
				char *s;
				char buf[INT_BUFSIZE_BOUND(long)];
				grad_longtostr(pair->avp_lvalue,
					       buf, sizeof buf);
				grad_astrcat(&s, buf, "+", "%{NAS-Port-Id}",
					     NULL);
				pair->avp_strvalue = grad_estrdup(s);
				pair->avp_strlength = strlen(pair->avp_strvalue);
				pair->eval_type = grad_eval_interpret;
				free(s);
			}
                }
                break;
                
        case GRAD_TYPE_DATE:
                timeval = time(0);
                tm = localtime_r(&timeval, &tms);
                if (grad_parse_time_string(valstr, tm)) {
                        grad_log_loc(GRAD_LOG_ERR, loc, _("%s: can't parse date"),
				     name);
                        grad_avp_free(pair);
                        return NULL;
                }
#ifdef TIMELOCAL
                pair->avp_lvalue = (grad_uint32_t)timelocal(tm);
#else /* TIMELOCAL */
                pair->avp_lvalue = (grad_uint32_t)mktime(tm);
#endif /* TIMELOCAL */
                break;

        default:
                grad_log_loc(GRAD_LOG_ERR, loc,
			     _("%s: unknown attribute type %d"),
			     name, pair->type);
                grad_avp_free(pair);
                return NULL;
        }

        return pair;
}

extern int uyydebug;

int
grad_parse_rule_file(char *file, void *c, register_rule_fp f)
{
        int rc;
        
        if (grad_parser_lex_init(file))
                return -1;
        closure = c;
        add_entry = f;

        uyydebug = 0;
        rc = uyyparse();
        grad_parser_lex_finish();
        return rc;
}

void
grad_enable_rule_debug(int val)
{
        uyydebug = val;
	grad_log_loc(GRAD_LOG_NOTICE, &grad_parser_source_locus,
		     uyydebug ? _("enabled userfile parser debugging") :
             	 	       _("disabled userfile parser debugging"));
}

