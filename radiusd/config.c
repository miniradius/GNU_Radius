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

/* All symbols defined below should begin with cc or YY, to avoid
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
   enum cctokentype {
     T_EOL = 258,
     T_WORD = 259,
     T_STRING = 260,
     T_NUMBER = 261,
     T_PUNCT = 262,
     T_BOOL = 263,
     T_IPADDR = 264
   };
#endif
/* Tokens.  */
#define T_EOL 258
#define T_WORD 259
#define T_STRING 260
#define T_NUMBER 261
#define T_PUNCT 262
#define T_BOOL 263
#define T_IPADDR 264




/* Copy the first part of user declarations.  */
#line 1 "config.y"

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
  
   You should have received a copy of the GNU General Public License
   along with GNU Radius; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>	
#include <ctype.h>
	
#include <radiusd.h>
#include <cfg.h>
 
#define YYMAXDEPTH 16

struct cfg_memblock {
	void (*destructor)();
	int line_num;
};

static grad_list_t /* of struct cfg_memblock */ *cfg_memory_pool;
 
static grad_list_t *_cfg_vlist_create(cfg_value_t *val);
static void _cfg_vlist_append(grad_list_t *vlist, cfg_value_t *val);

void *cfg_malloc(size_t size, void (*destructor)(void *));
 
static void _cfg_free_memory_pool();
static void _cfg_run_begin(struct cfg_stmt *stmt, void *up_data);

static int cclex();
static int ccerror(char *s); 

static char *typestr[] = {
	"integer",
	"boolean",
	"string",
	"network",
	"ipaddr",
	"port",
	"char",
	"host",
	"unsigned",
	"size"
};
	 
struct syntax_block {
	struct syntax_block *prev;
	struct cfg_stmt *stmt;
	cfg_end_fp end;
	void *data;
};

static struct syntax_block *block;

static void _cfg_push_block(struct cfg_stmt *stmt, cfg_end_fp end, void *data);
static struct syntax_block *_cfg_pop_block();

int _cfg_make_argv(cfg_value_t **argv, char *keyword, grad_list_t *vlist);
void _cfg_free_argv(int argc, cfg_value_t *argv);

struct cfg_stmt *_cfg_find_keyword(struct cfg_stmt *stmt, char *str);
static int _get_value(cfg_value_t *arg, int type, void *base);

static grad_slist_t cfg_slist;
char *cfg_filename;
int cfg_line_num;
static char *buffer;
static char *curp;



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
#line 100 "config.y"
{
        size_t number;
        int bool;
        grad_uint32_t ipaddr;
        char *string;
        cfg_value_t value;
        cfg_network_t network;
	grad_list_t *vlist;
	struct cfg_stmt *stmt;
}
/* Line 187 of yacc.c.  */
#line 224 "config.c"
	YYSTYPE;
# define ccstype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 237 "config.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 cctype_uint8;
#else
typedef unsigned char cctype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 cctype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char cctype_int8;
#else
typedef short int cctype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 cctype_uint16;
#else
typedef unsigned short int cctype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 cctype_int16;
#else
typedef short int cctype_int16;
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

#if ! defined ccoverflow || YYERROR_VERBOSE

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
#endif /* ! defined ccoverflow || YYERROR_VERBOSE */


#if (! defined ccoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union ccalloc
{
  cctype_int16 ccss;
  YYSTYPE ccvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union ccalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (cctype_int16) + sizeof (YYSTYPE)) \
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
	  YYSIZE_T cci;				\
	  for (cci = 0; cci < (Count); cci++)	\
	    (To)[cci] = (From)[cci];		\
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
	YYSIZE_T ccnewbytes;						\
	YYCOPY (&ccptr->Stack, Stack, ccsize);				\
	Stack = &ccptr->Stack;						\
	ccnewbytes = ccstacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	ccptr += ccnewbytes / sizeof (*ccptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  13
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   43

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  14
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  17
/* YYNRULES -- Number of rules.  */
#define YYNRULES  33
/* YYNRULES -- Number of states.  */
#define YYNSTATES  42

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   264

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? cctranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const cctype_uint8 cctranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    12,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    13,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    10,     2,    11,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const cctype_uint8 ccprhs[] =
{
       0,     0,     3,     5,     7,    10,    11,    13,    15,    17,
      20,    22,    24,    29,    33,    35,    36,    38,    42,    44,
      47,    51,    53,    55,    57,    59,    61,    63,    65,    69,
      71,    75,    77,    79
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const cctype_int8 ccrhs[] =
{
      15,     0,    -1,    16,    -1,    18,    -1,    16,    18,    -1,
      -1,    16,    -1,     3,    -1,    19,    -1,     1,     3,    -1,
      24,    -1,    20,    -1,    21,    17,    22,     3,    -1,    26,
      23,    10,    -1,    11,    -1,    -1,    25,    -1,    26,    25,
       3,    -1,    27,    -1,    25,    27,    -1,    25,    12,    27,
      -1,     4,    -1,     4,    -1,     5,    -1,     6,    -1,     8,
      -1,     7,    -1,    28,    -1,     9,    13,     6,    -1,     9,
      -1,     9,    29,    30,    -1,     7,    -1,     9,    -1,     6,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const cctype_uint16 ccrline[] =
{
       0,   125,   125,   128,   129,   132,   133,   136,   137,   138,
     144,   145,   148,   151,   180,   188,   191,   194,   218,   222,
     227,   234,   240,   245,   250,   255,   260,   265,   270,   278,
     283,   290,   297,   298
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const cctname[] =
{
  "$end", "error", "$undefined", "T_EOL", "T_WORD", "T_STRING",
  "T_NUMBER", "T_PUNCT", "T_BOOL", "T_IPADDR", "'{'", "'}'", "','", "':'",
  "$accept", "input", "list", "opt_list", "line", "stmt", "block_stmt",
  "block_open", "block_close", "tag", "simple_stmt", "value_list",
  "keyword", "value", "network", "slash", "netmask", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const cctype_uint16 cctoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     123,   125,    44,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const cctype_uint8 ccr1[] =
{
       0,    14,    15,    16,    16,    17,    17,    18,    18,    18,
      19,    19,    20,    21,    22,    23,    23,    24,    25,    25,
      25,    26,    27,    27,    27,    27,    27,    27,    27,    28,
      28,    29,    30,    30
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const cctype_uint8 ccr2[] =
{
       0,     2,     1,     1,     2,     0,     1,     1,     1,     2,
       1,     1,     4,     3,     1,     0,     1,     3,     1,     2,
       3,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const cctype_uint8 ccdefact[] =
{
       0,     0,     7,    21,     0,     0,     3,     8,    11,     0,
      10,    15,     9,     1,     4,     0,     0,    22,    23,    24,
      26,    25,    29,     0,    16,    18,    27,    14,     0,    31,
       0,     0,    13,    17,     0,    19,    12,    28,    33,    32,
      30,    20
};

/* YYDEFGOTO[NTERM-NUM].  */
static const cctype_int8 ccdefgoto[] =
{
      -1,     4,     5,    16,     6,     7,     8,     9,    28,    23,
      10,    24,    11,    25,    26,    31,    40
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -20
static const cctype_int8 ccpact[] =
{
      23,     5,   -20,   -20,     9,    36,   -20,   -20,   -20,    -1,
     -20,    26,   -20,   -20,   -20,     3,     1,   -20,   -20,   -20,
     -20,   -20,    16,    18,    13,   -20,   -20,   -20,    10,   -20,
      37,    32,   -20,   -20,    26,   -20,   -20,   -20,   -20,   -20,
     -20,   -20
};

/* YYPGOTO[NTERM-NUM].  */
static const cctype_int8 ccpgoto[] =
{
     -20,   -20,    33,   -20,    -4,   -20,   -20,   -20,   -20,   -20,
     -20,   -20,   -20,   -19,   -20,   -20,   -20
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -7
static const cctype_int8 cctable[] =
{
       1,    14,     2,     3,     1,    35,     2,     3,    12,    13,
      -5,    14,    27,    36,    -6,    41,    33,    17,    18,    19,
      20,    21,    22,    29,     1,    34,     2,     3,    32,    30,
      17,    18,    19,    20,    21,    22,    -2,     1,    38,     2,
       3,    39,    15,    37
};

static const cctype_uint8 cccheck[] =
{
       1,     5,     3,     4,     1,    24,     3,     4,     3,     0,
      11,    15,    11,     3,    11,    34,     3,     4,     5,     6,
       7,     8,     9,     7,     1,    12,     3,     4,    10,    13,
       4,     5,     6,     7,     8,     9,     0,     1,     6,     3,
       4,     9,     9,     6
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const cctype_uint8 ccstos[] =
{
       0,     1,     3,     4,    15,    16,    18,    19,    20,    21,
      24,    26,     3,     0,    18,    16,    17,     4,     5,     6,
       7,     8,     9,    23,    25,    27,    28,    11,    22,     7,
      13,    29,    10,     3,    12,    27,     3,     6,     6,     9,
      30,    27
};

#define ccerrok		(ccerrstatus = 0)
#define ccclearin	(ccchar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto ccacceptlab
#define YYABORT		goto ccabortlab
#define YYERROR		goto ccerrorlab


/* Like YYERROR except do call ccerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto ccerrlab

#define YYRECOVERING()  (!!ccerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (ccchar == YYEMPTY && cclen == 1)				\
    {								\
      ccchar = (Token);						\
      cclval = (Value);						\
      cctoken = YYTRANSLATE (ccchar);				\
      YYPOPSTACK (1);						\
      goto ccbackup;						\
    }								\
  else								\
    {								\
      ccerror (YY_("syntax error: cannot back up")); \
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


/* YYLEX -- calling `cclex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX cclex (YYLEX_PARAM)
#else
# define YYLEX cclex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (ccdebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (ccdebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      cc_symbol_print (stderr,						  \
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
cc_symbol_value_print (FILE *ccoutput, int cctype, YYSTYPE const * const ccvaluep)
#else
static void
cc_symbol_value_print (ccoutput, cctype, ccvaluep)
    FILE *ccoutput;
    int cctype;
    YYSTYPE const * const ccvaluep;
#endif
{
  if (!ccvaluep)
    return;
# ifdef YYPRINT
  if (cctype < YYNTOKENS)
    YYPRINT (ccoutput, cctoknum[cctype], *ccvaluep);
# else
  YYUSE (ccoutput);
# endif
  switch (cctype)
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
cc_symbol_print (FILE *ccoutput, int cctype, YYSTYPE const * const ccvaluep)
#else
static void
cc_symbol_print (ccoutput, cctype, ccvaluep)
    FILE *ccoutput;
    int cctype;
    YYSTYPE const * const ccvaluep;
#endif
{
  if (cctype < YYNTOKENS)
    YYFPRINTF (ccoutput, "token %s (", cctname[cctype]);
  else
    YYFPRINTF (ccoutput, "nterm %s (", cctname[cctype]);

  cc_symbol_value_print (ccoutput, cctype, ccvaluep);
  YYFPRINTF (ccoutput, ")");
}

/*------------------------------------------------------------------.
| cc_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
cc_stack_print (cctype_int16 *bottom, cctype_int16 *top)
#else
static void
cc_stack_print (bottom, top)
    cctype_int16 *bottom;
    cctype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (ccdebug)							\
    cc_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
cc_reduce_print (YYSTYPE *ccvsp, int ccrule)
#else
static void
cc_reduce_print (ccvsp, ccrule)
    YYSTYPE *ccvsp;
    int ccrule;
#endif
{
  int ccnrhs = ccr2[ccrule];
  int cci;
  unsigned long int cclno = ccrline[ccrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     ccrule - 1, cclno);
  /* The symbols being reduced.  */
  for (cci = 0; cci < ccnrhs; cci++)
    {
      fprintf (stderr, "   $%d = ", cci + 1);
      cc_symbol_print (stderr, ccrhs[ccprhs[ccrule] + cci],
		       &(ccvsp[(cci + 1) - (ccnrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (ccdebug)				\
    cc_reduce_print (ccvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int ccdebug;
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

# ifndef ccstrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define ccstrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
ccstrlen (const char *ccstr)
#else
static YYSIZE_T
ccstrlen (ccstr)
    const char *ccstr;
#endif
{
  YYSIZE_T cclen;
  for (cclen = 0; ccstr[cclen]; cclen++)
    continue;
  return cclen;
}
#  endif
# endif

# ifndef ccstpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define ccstpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
ccstpcpy (char *ccdest, const char *ccsrc)
#else
static char *
ccstpcpy (ccdest, ccsrc)
    char *ccdest;
    const char *ccsrc;
#endif
{
  char *ccd = ccdest;
  const char *ccs = ccsrc;

  while ((*ccd++ = *ccs++) != '\0')
    continue;

  return ccd - 1;
}
#  endif
# endif

# ifndef cctnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for ccerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from cctname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
cctnamerr (char *ccres, const char *ccstr)
{
  if (*ccstr == '"')
    {
      YYSIZE_T ccn = 0;
      char const *ccp = ccstr;

      for (;;)
	switch (*++ccp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++ccp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (ccres)
	      ccres[ccn] = *ccp;
	    ccn++;
	    break;

	  case '"':
	    if (ccres)
	      ccres[ccn] = '\0';
	    return ccn;
	  }
    do_not_strip_quotes: ;
    }

  if (! ccres)
    return ccstrlen (ccstr);

  return ccstpcpy (ccres, ccstr) - ccres;
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
ccsyntax_error (char *ccresult, int ccstate, int ccchar)
{
  int ccn = ccpact[ccstate];

  if (! (YYPACT_NINF < ccn && ccn <= YYLAST))
    return 0;
  else
    {
      int cctype = YYTRANSLATE (ccchar);
      YYSIZE_T ccsize0 = cctnamerr (0, cctname[cctype]);
      YYSIZE_T ccsize = ccsize0;
      YYSIZE_T ccsize1;
      int ccsize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *ccarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int ccx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *ccfmt;
      char const *ccf;
      static char const ccunexpected[] = "syntax error, unexpected %s";
      static char const ccexpecting[] = ", expecting %s";
      static char const ccor[] = " or %s";
      char ccformat[sizeof ccunexpected
		    + sizeof ccexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof ccor - 1))];
      char const *ccprefix = ccexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int ccxbegin = ccn < 0 ? -ccn : 0;

      /* Stay within bounds of both cccheck and cctname.  */
      int ccchecklim = YYLAST - ccn + 1;
      int ccxend = ccchecklim < YYNTOKENS ? ccchecklim : YYNTOKENS;
      int cccount = 1;

      ccarg[0] = cctname[cctype];
      ccfmt = ccstpcpy (ccformat, ccunexpected);

      for (ccx = ccxbegin; ccx < ccxend; ++ccx)
	if (cccheck[ccx + ccn] == ccx && ccx != YYTERROR)
	  {
	    if (cccount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		cccount = 1;
		ccsize = ccsize0;
		ccformat[sizeof ccunexpected - 1] = '\0';
		break;
	      }
	    ccarg[cccount++] = cctname[ccx];
	    ccsize1 = ccsize + cctnamerr (0, cctname[ccx]);
	    ccsize_overflow |= (ccsize1 < ccsize);
	    ccsize = ccsize1;
	    ccfmt = ccstpcpy (ccfmt, ccprefix);
	    ccprefix = ccor;
	  }

      ccf = YY_(ccformat);
      ccsize1 = ccsize + ccstrlen (ccf);
      ccsize_overflow |= (ccsize1 < ccsize);
      ccsize = ccsize1;

      if (ccsize_overflow)
	return YYSIZE_MAXIMUM;

      if (ccresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *ccp = ccresult;
	  int cci = 0;
	  while ((*ccp = *ccf) != '\0')
	    {
	      if (*ccp == '%' && ccf[1] == 's' && cci < cccount)
		{
		  ccp += cctnamerr (ccp, ccarg[cci++]);
		  ccf += 2;
		}
	      else
		{
		  ccp++;
		  ccf++;
		}
	    }
	}
      return ccsize;
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
ccdestruct (const char *ccmsg, int cctype, YYSTYPE *ccvaluep)
#else
static void
ccdestruct (ccmsg, cctype, ccvaluep)
    const char *ccmsg;
    int cctype;
    YYSTYPE *ccvaluep;
#endif
{
  YYUSE (ccvaluep);

  if (!ccmsg)
    ccmsg = "Deleting";
  YY_SYMBOL_PRINT (ccmsg, cctype, ccvaluep, cclocationp);

  switch (cctype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int ccparse (void *YYPARSE_PARAM);
#else
int ccparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int ccparse (void);
#else
int ccparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int ccchar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE cclval;

/* Number of syntax errors so far.  */
int ccnerrs;



/*----------.
| ccparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
ccparse (void *YYPARSE_PARAM)
#else
int
ccparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
ccparse (void)
#else
int
ccparse ()

#endif
#endif
{
  
  int ccstate;
  int ccn;
  int ccresult;
  /* Number of tokens to shift before error messages enabled.  */
  int ccerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int cctoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char ccmsgbuf[128];
  char *ccmsg = ccmsgbuf;
  YYSIZE_T ccmsg_alloc = sizeof ccmsgbuf;
#endif

  /* Three stacks and their tools:
     `ccss': related to states,
     `ccvs': related to semantic values,
     `ccls': related to locations.

     Refer to the stacks thru separate pointers, to allow ccoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  cctype_int16 ccssa[YYINITDEPTH];
  cctype_int16 *ccss = ccssa;
  cctype_int16 *ccssp;

  /* The semantic value stack.  */
  YYSTYPE ccvsa[YYINITDEPTH];
  YYSTYPE *ccvs = ccvsa;
  YYSTYPE *ccvsp;



#define YYPOPSTACK(N)   (ccvsp -= (N), ccssp -= (N))

  YYSIZE_T ccstacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE ccval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int cclen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  ccstate = 0;
  ccerrstatus = 0;
  ccnerrs = 0;
  ccchar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  ccssp = ccss;
  ccvsp = ccvs;

  goto ccsetstate;

/*------------------------------------------------------------.
| ccnewstate -- Push a new state, which is found in ccstate.  |
`------------------------------------------------------------*/
 ccnewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  ccssp++;

 ccsetstate:
  *ccssp = ccstate;

  if (ccss + ccstacksize - 1 <= ccssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T ccsize = ccssp - ccss + 1;

#ifdef ccoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *ccvs1 = ccvs;
	cctype_int16 *ccss1 = ccss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if ccoverflow is a macro.  */
	ccoverflow (YY_("memory exhausted"),
		    &ccss1, ccsize * sizeof (*ccssp),
		    &ccvs1, ccsize * sizeof (*ccvsp),

		    &ccstacksize);

	ccss = ccss1;
	ccvs = ccvs1;
      }
#else /* no ccoverflow */
# ifndef YYSTACK_RELOCATE
      goto ccexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= ccstacksize)
	goto ccexhaustedlab;
      ccstacksize *= 2;
      if (YYMAXDEPTH < ccstacksize)
	ccstacksize = YYMAXDEPTH;

      {
	cctype_int16 *ccss1 = ccss;
	union ccalloc *ccptr =
	  (union ccalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (ccstacksize));
	if (! ccptr)
	  goto ccexhaustedlab;
	YYSTACK_RELOCATE (ccss);
	YYSTACK_RELOCATE (ccvs);

#  undef YYSTACK_RELOCATE
	if (ccss1 != ccssa)
	  YYSTACK_FREE (ccss1);
      }
# endif
#endif /* no ccoverflow */

      ccssp = ccss + ccsize - 1;
      ccvsp = ccvs + ccsize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) ccstacksize));

      if (ccss + ccstacksize - 1 <= ccssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", ccstate));

  goto ccbackup;

/*-----------.
| ccbackup.  |
`-----------*/
ccbackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  ccn = ccpact[ccstate];
  if (ccn == YYPACT_NINF)
    goto ccdefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (ccchar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      ccchar = YYLEX;
    }

  if (ccchar <= YYEOF)
    {
      ccchar = cctoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      cctoken = YYTRANSLATE (ccchar);
      YY_SYMBOL_PRINT ("Next token is", cctoken, &cclval, &cclloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  ccn += cctoken;
  if (ccn < 0 || YYLAST < ccn || cccheck[ccn] != cctoken)
    goto ccdefault;
  ccn = cctable[ccn];
  if (ccn <= 0)
    {
      if (ccn == 0 || ccn == YYTABLE_NINF)
	goto ccerrlab;
      ccn = -ccn;
      goto ccreduce;
    }

  if (ccn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (ccerrstatus)
    ccerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", cctoken, &cclval, &cclloc);

  /* Discard the shifted token unless it is eof.  */
  if (ccchar != YYEOF)
    ccchar = YYEMPTY;

  ccstate = ccn;
  *++ccvsp = cclval;

  goto ccnewstate;


/*-----------------------------------------------------------.
| ccdefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
ccdefault:
  ccn = ccdefact[ccstate];
  if (ccn == 0)
    goto ccerrlab;
  goto ccreduce;


/*-----------------------------.
| ccreduce -- Do a reduction.  |
`-----------------------------*/
ccreduce:
  /* ccn is the number of a rule to reduce with.  */
  cclen = ccr2[ccn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  ccval = ccvsp[1-cclen];


  YY_REDUCE_PRINT (ccn);
  switch (ccn)
    {
        case 9:
#line 139 "config.y"
    {
		      ccclearin; ccerrok;
	      }
    break;

  case 13:
#line 152 "config.y"
    {
		      if ((ccvsp[(1) - (3)].stmt) && (ccvsp[(1) - (3)].stmt)->type == CS_BLOCK) {
			      if ((ccvsp[(1) - (3)].stmt)->handler) {
				      cfg_value_t *argv;
				      int rc;
				      int argc = _cfg_make_argv(&argv,
								(ccvsp[(1) - (3)].stmt)->keyword,
								(ccvsp[(2) - (3)].vlist));
				      rc = (ccvsp[(1) - (3)].stmt)->handler(argc, argv,
						       block->data,
						       (ccvsp[(1) - (3)].stmt)->data);
				      _cfg_free_argv(argc, argv);
				      if (rc)
					      ccerror("syntax error");
			      }
			      _cfg_push_block((ccvsp[(1) - (3)].stmt)->block, (ccvsp[(1) - (3)].stmt)->end, (ccvsp[(1) - (3)].stmt)->data);
		      } else {
			      if (block->stmt) {
				      grad_log(GRAD_LOG_ERR,
					       "%s:%d: %s",
					       cfg_filename, cfg_line_num,
					       _("unknown block statement"));
			      }
			      _cfg_push_block(NULL, NULL, NULL);
		      }
	      }
    break;

  case 14:
#line 181 "config.y"
    {
		      if (block->prev)
			      _cfg_pop_block();
	      }
    break;

  case 15:
#line 188 "config.y"
    {
		      (ccval.vlist) = NULL;
	      }
    break;

  case 17:
#line 195 "config.y"
    {
		      if ((ccvsp[(1) - (3)].stmt)) {
			      if ((ccvsp[(1) - (3)].stmt)->handler) {
				      cfg_value_t *argv;
				      int rc;
				      int argc = _cfg_make_argv(&argv,
								(ccvsp[(1) - (3)].stmt)->keyword,
								(ccvsp[(2) - (3)].vlist));
				      rc = (ccvsp[(1) - (3)].stmt)->handler(argc, argv,
						       block->data,
						       (ccvsp[(1) - (3)].stmt)->data);
				      _cfg_free_argv(argc, argv);
				      if (rc)
					      ccerror("syntax error");
			      }
		      } else if (block->stmt)
			      grad_log(GRAD_LOG_ERR,
				       "%s:%d: %s",
				       cfg_filename, cfg_line_num,
				       _("unknown keyword"));
	      }
    break;

  case 18:
#line 219 "config.y"
    {
		      (ccval.vlist) = _cfg_vlist_create(&(ccvsp[(1) - (1)].value));
	      }
    break;

  case 19:
#line 223 "config.y"
    {
		      _cfg_vlist_append((ccvsp[(1) - (2)].vlist), &(ccvsp[(2) - (2)].value));
		      (ccval.vlist) = (ccvsp[(1) - (2)].vlist);
	      }
    break;

  case 20:
#line 228 "config.y"
    {
		      _cfg_vlist_append((ccvsp[(1) - (3)].vlist), &(ccvsp[(3) - (3)].value));
		      (ccval.vlist) = (ccvsp[(1) - (3)].vlist);
	      }
    break;

  case 21:
#line 235 "config.y"
    {
		      (ccval.stmt) = _cfg_find_keyword(block->stmt, (ccvsp[(1) - (1)].string));
	      }
    break;

  case 22:
#line 241 "config.y"
    {
		      (ccval.value).type = CFG_STRING;
		      (ccval.value).v.string = (ccvsp[(1) - (1)].string);
	      }
    break;

  case 23:
#line 246 "config.y"
    {
		      (ccval.value).type = CFG_STRING;
		      (ccval.value).v.string = (ccvsp[(1) - (1)].string);
	      }
    break;

  case 24:
#line 251 "config.y"
    {
		      (ccval.value).type = CFG_INTEGER;
		      (ccval.value).v.number = (ccvsp[(1) - (1)].number);
	      }
    break;

  case 25:
#line 256 "config.y"
    {
		      (ccval.value).type = CFG_BOOLEAN;
		      (ccval.value).v.bool = (ccvsp[(1) - (1)].bool);
	      }
    break;

  case 26:
#line 261 "config.y"
    {
		      (ccval.value).type = CFG_CHAR;
		      (ccval.value).v.ch = (ccvsp[(1) - (1)].number);
	      }
    break;

  case 27:
#line 266 "config.y"
    {
		      (ccval.value).type = CFG_NETWORK;
		      (ccval.value).v.network = (ccvsp[(1) - (1)].network);
	      }
    break;

  case 28:
#line 271 "config.y"
    {
		      (ccval.value).type = CFG_HOST;
		      (ccval.value).v.host.ipaddr = (ccvsp[(1) - (3)].ipaddr);
		      (ccval.value).v.host.port = (ccvsp[(3) - (3)].number);
	      }
    break;

  case 29:
#line 279 "config.y"
    {
		      (ccval.network).ipaddr = (ccvsp[(1) - (1)].ipaddr);
		      (ccval.network).netmask = 0xffffffffL;
	      }
    break;

  case 30:
#line 284 "config.y"
    {
		      (ccval.network).ipaddr = (ccvsp[(1) - (3)].ipaddr);
		      (ccval.network).netmask = (ccvsp[(3) - (3)].ipaddr);
	      }
    break;

  case 31:
#line 291 "config.y"
    {
		      if ((ccvsp[(1) - (1)].number) != '/')
			      YYERROR;
	      }
    break;

  case 33:
#line 299 "config.y"
    {
		      if ((ccvsp[(1) - (1)].number) > 32) {
			      grad_log(GRAD_LOG_ERR,
				       _("invalid netmask length: %d"), (ccvsp[(1) - (1)].number));
			      YYERROR;
		      }
		      (ccval.ipaddr) = (0xfffffffful >> (32-(ccvsp[(1) - (1)].number))) << (32-(ccvsp[(1) - (1)].number));
	      }
    break;


/* Line 1267 of yacc.c.  */
#line 1663 "config.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", ccr1[ccn], &ccval, &ccloc);

  YYPOPSTACK (cclen);
  cclen = 0;
  YY_STACK_PRINT (ccss, ccssp);

  *++ccvsp = ccval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  ccn = ccr1[ccn];

  ccstate = ccpgoto[ccn - YYNTOKENS] + *ccssp;
  if (0 <= ccstate && ccstate <= YYLAST && cccheck[ccstate] == *ccssp)
    ccstate = cctable[ccstate];
  else
    ccstate = ccdefgoto[ccn - YYNTOKENS];

  goto ccnewstate;


/*------------------------------------.
| ccerrlab -- here on detecting error |
`------------------------------------*/
ccerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!ccerrstatus)
    {
      ++ccnerrs;
#if ! YYERROR_VERBOSE
      ccerror (YY_("syntax error"));
#else
      {
	YYSIZE_T ccsize = ccsyntax_error (0, ccstate, ccchar);
	if (ccmsg_alloc < ccsize && ccmsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T ccalloc = 2 * ccsize;
	    if (! (ccsize <= ccalloc && ccalloc <= YYSTACK_ALLOC_MAXIMUM))
	      ccalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (ccmsg != ccmsgbuf)
	      YYSTACK_FREE (ccmsg);
	    ccmsg = (char *) YYSTACK_ALLOC (ccalloc);
	    if (ccmsg)
	      ccmsg_alloc = ccalloc;
	    else
	      {
		ccmsg = ccmsgbuf;
		ccmsg_alloc = sizeof ccmsgbuf;
	      }
	  }

	if (0 < ccsize && ccsize <= ccmsg_alloc)
	  {
	    (void) ccsyntax_error (ccmsg, ccstate, ccchar);
	    ccerror (ccmsg);
	  }
	else
	  {
	    ccerror (YY_("syntax error"));
	    if (ccsize != 0)
	      goto ccexhaustedlab;
	  }
      }
#endif
    }



  if (ccerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (ccchar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (ccchar == YYEOF)
	    YYABORT;
	}
      else
	{
	  ccdestruct ("Error: discarding",
		      cctoken, &cclval);
	  ccchar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto ccerrlab1;


/*---------------------------------------------------.
| ccerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
ccerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label ccerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto ccerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (cclen);
  cclen = 0;
  YY_STACK_PRINT (ccss, ccssp);
  ccstate = *ccssp;
  goto ccerrlab1;


/*-------------------------------------------------------------.
| ccerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
ccerrlab1:
  ccerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      ccn = ccpact[ccstate];
      if (ccn != YYPACT_NINF)
	{
	  ccn += YYTERROR;
	  if (0 <= ccn && ccn <= YYLAST && cccheck[ccn] == YYTERROR)
	    {
	      ccn = cctable[ccn];
	      if (0 < ccn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (ccssp == ccss)
	YYABORT;


      ccdestruct ("Error: popping",
		  ccstos[ccstate], ccvsp);
      YYPOPSTACK (1);
      ccstate = *ccssp;
      YY_STACK_PRINT (ccss, ccssp);
    }

  if (ccn == YYFINAL)
    YYACCEPT;

  *++ccvsp = cclval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", ccstos[ccn], ccvsp, cclsp);

  ccstate = ccn;
  goto ccnewstate;


/*-------------------------------------.
| ccacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
ccacceptlab:
  ccresult = 0;
  goto ccreturn;

/*-----------------------------------.
| ccabortlab -- YYABORT comes here.  |
`-----------------------------------*/
ccabortlab:
  ccresult = 1;
  goto ccreturn;

#ifndef ccoverflow
/*-------------------------------------------------.
| ccexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
ccexhaustedlab:
  ccerror (YY_("memory exhausted"));
  ccresult = 2;
  /* Fall through.  */
#endif

ccreturn:
  if (ccchar != YYEOF && ccchar != YYEMPTY)
     ccdestruct ("Cleanup: discarding lookahead",
		 cctoken, &cclval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (cclen);
  YY_STACK_PRINT (ccss, ccssp);
  while (ccssp != ccss)
    {
      ccdestruct ("Cleanup: popping",
		  ccstos[*ccssp], ccvsp);
      YYPOPSTACK (1);
    }
#ifndef ccoverflow
  if (ccss != ccssa)
    YYSTACK_FREE (ccss);
#endif
#if YYERROR_VERBOSE
  if (ccmsg != ccmsgbuf)
    YYSTACK_FREE (ccmsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (ccresult);
}


#line 309 "config.y"


static void
skipws()
{
        while (*curp && isspace(*curp)) {
                if (*curp == '\n')
                        cfg_line_num++;
                curp++;
        }
}

static void
skipline()
{
        while (*curp && *curp != '\n')
                curp++;
}

static int
isword(int c)
{
        return isalnum(c) || c == '_' || c == '-';
}

static char *
copy_alpha()
{
        do {
		grad_slist_append_char(cfg_slist, *curp);
                curp++;
        } while (*curp && isword(*curp));
	grad_slist_append_char(cfg_slist, 0);
	return grad_slist_finish(cfg_slist);
}

static char *
copy_string()
{
        int quote = *curp++;

        while (*curp) {
		if (*curp == '\\') {
			grad_slist_grow_backslash(cfg_slist, curp, &curp);
		} else if (*curp == quote) {
                        curp++;
                        break;
                } else {
			grad_slist_append_char(cfg_slist, *curp);
			curp++;
		}
        } 
	grad_slist_append_char(cfg_slist, 0);
	return grad_slist_finish(cfg_slist);
}

static int
copy_digit()
{
        int dot = 0;

        if (*curp == '0') {
                if (curp[1] == 'x' || curp[1] == 'X') {
			grad_slist_append_char(cfg_slist, *curp);
			curp++;
			grad_slist_append_char(cfg_slist, *curp);
			curp++;
                }
        }
        
        do {
		grad_slist_append_char(cfg_slist, *curp);
                if (*curp++ == '.')
                        dot++;
        } while (*curp && (isdigit(*curp) || *curp == '.'));
	grad_slist_append_char(cfg_slist, 0);
	cclval.string = grad_slist_finish(cfg_slist);
        return dot;
}

static grad_keyword_t booleans[] = {
	{ "on", 1 }, 
	{ "off", 0 },
	{ "yes", 1 }, 
	{ "no", 0 },
	{ 0 }
};

static int
keyword()
{
	int tok;
	
	if ((tok = grad_xlat_keyword(booleans, cclval.string, -1)) != -1) {
		cclval.bool = tok;
		return T_BOOL;
	}
	return T_WORD;
}

#define ismath(c) (strchr("=!+-/*.", c)!=NULL)

static int
cclex()
{
again:
        skipws();

        if (*curp == '#') {
                skipline();
                goto again;
        } 
        if (*curp == '/' && curp[1] == '*') {
                int keep_line = cfg_line_num;

                curp += 2;
                do {
                        while (*curp != '*') {
                                if (*curp == 0) {
                                        grad_log(GRAD_LOG_ERR, 
                       _("%s:%d: unexpected EOF in comment started at line %d"),
                                                 cfg_filename, 
                                                 cfg_line_num, 
                                                 keep_line);
                                        return 0;
                                } else if (*curp == '\n')
                                        cfg_line_num++;
                                ++curp;
                        }
                } while (*++curp != '/');
                ++curp;
                goto again;
        }

        if (*curp == 0)
                return 0;
        
        if (isalpha(*curp)) {
                cclval.string = copy_alpha();
                return keyword();
        }

        if (*curp == '\"') {
                cclval.string = copy_string();
                return T_STRING;
        }

	if (*curp == '-' && !isspace(curp[1])) {
		/* For the sake of passing keyword arguments
		   to scheme */
		cclval.string = copy_alpha();
		return T_STRING;
	}
	
        if (isdigit(*curp)) {
                if (copy_digit()) {
                        /* IP address */
                        cclval.ipaddr = grad_ip_strtoip(cclval.string);
                        return T_IPADDR;
                }
                cclval.number = strtol(cclval.string, NULL, 0);
                return T_NUMBER;
        } 

        if (*curp == ';') {
                curp++;
                return T_EOL;
        }

        if (ismath(*curp)) {
		cclval.number = *curp++;
		return T_PUNCT;
	}
        return *curp++;
}

static int
ccerror(char *s)
{
        grad_log(GRAD_LOG_ERR, "%s:%d: %s", cfg_filename, cfg_line_num, s);
}
                
/* ************************************************************************* */
/* Internal functions */

void
_cfg_run_begin(struct cfg_stmt *stmt, void *up_data)
{
	for ( ; stmt->keyword; stmt++) {
		if (stmt->term)
			stmt->term(0, stmt->data, up_data);
		if (stmt->type == CS_BLOCK)
			_cfg_run_begin(stmt->block, stmt->data);
	}
}

void
_cfg_run_finish(struct cfg_stmt *stmt, void *up_data)
{
	for ( ; stmt->keyword; stmt++) {
		if (stmt->term)
			stmt->term(1, stmt->data, up_data);
		if (stmt->type == CS_BLOCK)
			_cfg_run_finish(stmt->block, stmt->data);
	}
}

static int
_cfg_free_item(void *item, void *data)
{
	struct cfg_memblock *p = item;
	if (p->destructor)
		p->destructor(p+1);
	grad_free(p);
	return 0;
}

void
_cfg_free_memory_pool()
{
	grad_list_destroy(&cfg_memory_pool, _cfg_free_item, NULL);
}

int
_cfg_make_argv(cfg_value_t **argv, char *keyword, grad_list_t *vlist)
{
	int argc;

	if (vlist)
		argc = grad_list_count(vlist) + 1;
	else
		argc = 1;
	*argv = grad_emalloc(sizeof(**argv)*argc);
	(*argv)[0].type = CFG_STRING;
	(*argv)[0].v.string = keyword;
	if (vlist) {
		int i;
		cfg_value_t *val;
		grad_iterator_t *itr = grad_iterator_create(vlist);

		if (itr) {
			for (i = 1, val = grad_iterator_first(itr); val;
		     	     i++, val = grad_iterator_next(itr))
				(*argv)[i] = *val;
			grad_iterator_destroy(&itr);
		}
	}
	return argc;
}

void
_cfg_free_argv(int argc, cfg_value_t *argv)
{
	grad_free(argv);
}

static void
_cfg_vlist_destroy(void *arg)
{
	grad_list_t **pl = arg;
	grad_list_destroy(pl, NULL, NULL);
}

void
_cfg_vlist_append(grad_list_t *vlist, cfg_value_t *val)
{
	cfg_value_t *vp = cfg_malloc(sizeof(*vp), NULL);
	*vp = *val;
	grad_list_append(vlist, vp);
}

grad_list_t *
_cfg_vlist_create(cfg_value_t *val)
{
	grad_list_t *vlist = grad_list_create();
	grad_list_t **lp = cfg_malloc(sizeof(*lp), _cfg_vlist_destroy);
	*lp = vlist;
	_cfg_vlist_append(vlist, val);
	return vlist;
}

void
_cfg_push_block(struct cfg_stmt *stmt, cfg_end_fp end, void *block_data)
{
	struct syntax_block *p = grad_emalloc(sizeof(*p));
	p->stmt = stmt;
	p->end  = end;
	p->data = block_data;
	p->prev = block;
	block = p;
}

struct syntax_block *
_cfg_pop_block()
{
	struct syntax_block *p = block;

	if (p) {
		block = p->prev;
		if (p->end)
			p->end(block ? block->data : NULL, p->data);
		grad_free(p);
	}
	return block;
}

struct cfg_stmt *
_cfg_find_keyword(struct cfg_stmt *stmt, char *str)
{
	if (stmt)
		for (; stmt->keyword; stmt++) {
			if (strcmp(stmt->keyword, str) == 0)
				return stmt;
		}
	return NULL;
}

int
_get_value(cfg_value_t *arg, int type, void *base)
{
        struct servent *s;
        grad_uint32_t ipaddr;
        cfg_value_t value;

	value = *arg;
        switch (type) {
        case CFG_PORT:
                switch (value.type) {
                case CFG_INTEGER:
                        type = CFG_INTEGER;
                        break;
			
                case CFG_STRING:
                          s = getservbyname(value.v.string, "udp");
                          if (s) 
                                  value.v.number = ntohs(s->s_port);
                          else {
                                  grad_log(GRAD_LOG_ERR, 
                                           _("%s:%d: no such service: %s"),
                                           cfg_filename, cfg_line_num,
                                           value.v.string);
                                  return 0;
                          }
                          type = value.type = CFG_INTEGER;
                          break;
			  
                default:
                        break;
                }
                break;
                        
        case CFG_IPADDR:
                switch (value.type) {
                case CFG_IPADDR:
                        break;
			
                case CFG_INTEGER:
                        type = CFG_IPADDR;
                        break;
			
                case CFG_STRING:
                        ipaddr = grad_ip_gethostaddr(value.v.string);
                        if (ipaddr == 0) {
                                grad_log(GRAD_LOG_ERR, 
                                         _("%s:%d: unknown host: %s"),
                                         cfg_filename, cfg_line_num,
                                         value.v.string);
                        }
                        value.v.ipaddr = ipaddr;
                        value.type = CFG_IPADDR;
                        break;

		case CFG_NETWORK:
			if (value.v.network.netmask != 0xffffffffL) 
				break;
			value.v.ipaddr = value.v.network.ipaddr;
			value.type = CFG_IPADDR;
			break;

                default:
                        break;
                }
		break;
		
	case CFG_UNSIGNED:
	case CFG_SIZE_T:
	case CFG_INTEGER:
		switch (value.type) {
		case CFG_UNSIGNED:
		case CFG_SIZE_T:
		case CFG_INTEGER:
			value.type = type;
			break;
		} 
        }
        
        if (type != value.type) {
		cfg_type_error(type);
                return 0;
        }

        switch (type) {
        case CFG_INTEGER:
                *(int*) base = value.v.number;
                break;
		
        case CFG_UNSIGNED:
                *(unsigned*) base = value.v.number;
                break;
		
        case CFG_SIZE_T:
                *(size_t*) base = value.v.number;
                break;
		
        case CFG_STRING:
                grad_string_replace((char**)base, value.v.string);
                break;
		
        case CFG_IPADDR:
                *(grad_uint32_t*) base = value.v.ipaddr;
                break;
		
        case CFG_BOOLEAN:
                *(int*) base = value.v.bool;
                break;
		
	case CFG_NETWORK:
		*(cfg_network_t *) base = value.v.network;
		break;
		
        default:
                grad_log(GRAD_LOG_CRIT,
                         _("INTERNAL ERROR at %s:%d: unknown datatype %d"),
                         __FILE__, __LINE__, type);
        }
	return 0;
}


/* ************************************************************************* */
/* Global functions */

void *
cfg_malloc(size_t size,	void (*destructor)(void *))
{
	struct cfg_memblock *p = grad_emalloc(size + sizeof(*p));
	p->destructor = destructor;
	p->line_num = cfg_line_num;
	if (!cfg_memory_pool)
		cfg_memory_pool = grad_list_create();
	grad_list_append(cfg_memory_pool, p);
	return p+1;
}

void
cfg_type_error(int type)
{
	grad_log(GRAD_LOG_ERR, 
	         _("%s:%d: wrong datatype (should be %s)"),
	         cfg_filename, cfg_line_num, typestr[type]);
}

void
cfg_argc_error(int few)
{
	grad_log(GRAD_LOG_ERR,
	         "%s:%d: %s",
	         cfg_filename, cfg_line_num,
	         few ? _("too few arguments") : _("too many arguments"));
}

#define _check_argc(argc, max) \
 if (argc-1 > max) {\
     grad_log(GRAD_LOG_ERR, "%s:%d: %s", \
              cfg_filename, cfg_line_num, _("too many arguments"));\
     return 0;\
 }		

int
cfg_ignore(int argc ARG_UNUSED, cfg_value_t *argv ARG_UNUSED,
	   void *block_data ARG_UNUSED, void *handler_data ARG_UNUSED)
{
	return 0;
}

int
cfg_obsolete(int argc ARG_UNUSED, cfg_value_t *argv ARG_UNUSED,
	     void *block_data ARG_UNUSED, void *handler_data ARG_UNUSED)
{
	grad_log(GRAD_LOG_WARN, _("%s:%d: obsolete statement"),
	       cfg_filename, cfg_line_num);
	return 0;
}

int
cfg_get_ipaddr(int argc, cfg_value_t *argv, void *block_data,
	       void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_IPADDR, handler_data);
}

int
cfg_get_uint32_t(int argc, cfg_value_t *argv, void *block_data,
		 void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_IPADDR, handler_data);
}

int
cfg_get_integer(int argc, cfg_value_t *argv, void *block_data,
		void *handler_data)
{
	int val;
	int rc;
	
	_check_argc(argc, 1);
	rc = _get_value(&argv[1], CFG_INTEGER, &val);
	*(int*)handler_data = val;
	return rc;
}

int
cfg_get_unsigned(int argc, cfg_value_t *argv, void *block_data,
		 void *handler_data)
{
	unsigned val;
	int rc;
	
	_check_argc(argc, 1);
	rc = _get_value(&argv[1], CFG_UNSIGNED, &val);
	*(unsigned*)handler_data = val;
	return rc;
}

int
cfg_get_size_t(int argc, cfg_value_t *argv, void *block_data,
	       void *handler_data)
{
	size_t val;
	int rc;
	
	_check_argc(argc, 1);
	rc = _get_value(&argv[1], CFG_INTEGER, &val);
	*(size_t*)handler_data = val;
	return rc;
}

int
cfg_get_number(int argc, cfg_value_t *argv, void *block_data,
	       void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_INTEGER, handler_data);
}

int
cfg_get_string(int argc, cfg_value_t *argv, void *block_data,
	       void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_STRING, handler_data);
}

int
cfg_get_boolean(int argc, cfg_value_t *argv, void *block_data,
		void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_BOOLEAN, handler_data);
}

int
cfg_get_network(int argc, cfg_value_t *argv,
		void *block_data, void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_NETWORK, handler_data);
}

int
cfg_get_port(int argc, cfg_value_t *argv,
	     void *block_data, void *handler_data)
{
	_check_argc(argc, 1);
	return _get_value(&argv[1], CFG_PORT, handler_data);
}

int
cfg_read(char *fname, struct cfg_stmt *syntax, void *data)
{
        struct stat st;
        int fd;
        extern int ccdebug;

	cfg_memory_pool = NULL;
	block = NULL;
	
        cfg_filename = fname;
	_cfg_push_block(syntax, NULL, data);
        if (stat(cfg_filename, &st)) {
                grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, 
                         _("can't stat `%s'"), cfg_filename);
                return -1;
        }
        fd = open(cfg_filename, O_RDONLY);
        if (fd == -1) {
                if (errno != ENOENT)
                        grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, 
                                _("can't open config file `%s'"), cfg_filename);
                return -1;
        }
        buffer = cfg_malloc(st.st_size+1, NULL);
        
        read(fd, buffer, st.st_size);
        buffer[st.st_size] = 0;
        close(fd);
        curp = buffer;

        grad_log(GRAD_LOG_INFO, _("reading %s"), cfg_filename);
        cfg_line_num = 1;

        if (strncmp(curp, "#debug", 6) == 0) {
		/* Note: can't check YYDEBUG here, because some yaccs
		   (most notably, sun's) define YYDEBUG after including
		   code block */     
                ccdebug = 1;
        } else {
                ccdebug = 0;
        }

	cfg_slist = grad_slist_create();
	_cfg_run_begin(syntax, data);
	
        /* Parse configuration */
        ccparse();

	_cfg_run_finish(syntax, data);

        /* Clean up the things */
	while (_cfg_pop_block())
		;

	_cfg_free_memory_pool();
	grad_slist_free(&cfg_slist);

        return 0;
}       


