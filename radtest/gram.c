/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "gram.y"

/* This file is part of GNU Radius.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.

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
   along with GNU Radius.  If not, see <http://www.gnu.org/licenses/>. */

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

#include <common.h>
#include <radtest.h>

#define YYERROR_VERBOSE 1

extern int yylex();

/* Local variables */
static int current_nesting_level; /* Nesting level of WHILE/DO statements */
static int error_count;           /* In interactive mode: number of errors
				     in the statement being compiled. Gets
				     reset to 0 after each statement.
				     In batch mode: total number of errors
				     encountered so far */

static struct {                   /* Definition of the function currently
				     being compiled */
	char *function;           /* Function name */
	grad_locus_t locus;       /* Location of the beginning of
				     the definition */
} defn;

/* Context stack support. This is used for improving diagnostic
   messages and after-error synchronization */

struct context_stack {
	struct context_stack *next;
	enum context ctx;
};

static struct context_stack *context_stack;

static enum context
push_ctx(enum context ctx)
{
	struct context_stack *p = grad_emalloc(sizeof(*p));
	p->ctx = ctx;
	p->next = context_stack;
	context_stack = p;
	return ctx;
}

enum context
pop_ctx(void)
{
	enum context ctx;
	struct context_stack *p = context_stack;

	if (!context_stack)
		return ctx_none;
	ctx = p->ctx;
	context_stack = p->next;
	free(p);
	return ctx;
}

enum context
peek_ctx(void)
{
	return context_stack ? context_stack->ctx : ctx_none;
}

/* Forward declarations */
static void run_statement(radtest_node_t *node);
static void errsync();
void yyerror(char const *s);


#line 179 "gram.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "gram.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_EOL = 3,                        /* EOL  */
  YYSYMBOL_AUTH = 4,                       /* AUTH  */
  YYSYMBOL_ACCT = 5,                       /* ACCT  */
  YYSYMBOL_SEND = 6,                       /* SEND  */
  YYSYMBOL_EXPECT = 7,                     /* EXPECT  */
  YYSYMBOL_T_BEGIN = 8,                    /* T_BEGIN  */
  YYSYMBOL_T_END = 9,                      /* T_END  */
  YYSYMBOL_IF = 10,                        /* IF  */
  YYSYMBOL_ELSE = 11,                      /* ELSE  */
  YYSYMBOL_WHILE = 12,                     /* WHILE  */
  YYSYMBOL_DO = 13,                        /* DO  */
  YYSYMBOL_BREAK = 14,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 15,                  /* CONTINUE  */
  YYSYMBOL_INPUT = 16,                     /* INPUT  */
  YYSYMBOL_SHIFT = 17,                     /* SHIFT  */
  YYSYMBOL_GETOPT = 18,                    /* GETOPT  */
  YYSYMBOL_CASE = 19,                      /* CASE  */
  YYSYMBOL_IN = 20,                        /* IN  */
  YYSYMBOL_T_RETURN = 21,                  /* T_RETURN  */
  YYSYMBOL_SET = 22,                       /* SET  */
  YYSYMBOL_PRINT = 23,                     /* PRINT  */
  YYSYMBOL_EXIT = 24,                      /* EXIT  */
  YYSYMBOL_T_BOGUS = 25,                   /* T_BOGUS  */
  YYSYMBOL_ARGCOUNT = 26,                  /* ARGCOUNT  */
  YYSYMBOL_IDENT = 27,                     /* IDENT  */
  YYSYMBOL_PARM = 28,                      /* PARM  */
  YYSYMBOL_NAME = 29,                      /* NAME  */
  YYSYMBOL_NUMBER = 30,                    /* NUMBER  */
  YYSYMBOL_QUOTE = 31,                     /* QUOTE  */
  YYSYMBOL_BSTRING = 32,                   /* BSTRING  */
  YYSYMBOL_IPADDRESS = 33,                 /* IPADDRESS  */
  YYSYMBOL_PRITEM = 34,                    /* PRITEM  */
  YYSYMBOL_OR = 35,                        /* OR  */
  YYSYMBOL_AND = 36,                       /* AND  */
  YYSYMBOL_EQ = 37,                        /* EQ  */
  YYSYMBOL_NE = 38,                        /* NE  */
  YYSYMBOL_LT = 39,                        /* LT  */
  YYSYMBOL_LE = 40,                        /* LE  */
  YYSYMBOL_GT = 41,                        /* GT  */
  YYSYMBOL_GE = 42,                        /* GE  */
  YYSYMBOL_43_ = 43,                       /* '+'  */
  YYSYMBOL_44_ = 44,                       /* '-'  */
  YYSYMBOL_45_ = 45,                       /* '*'  */
  YYSYMBOL_46_ = 46,                       /* '/'  */
  YYSYMBOL_47_ = 47,                       /* '%'  */
  YYSYMBOL_UMINUS = 48,                    /* UMINUS  */
  YYSYMBOL_NOT = 49,                       /* NOT  */
  YYSYMBOL_50_ = 50,                       /* '['  */
  YYSYMBOL_51_ = 51,                       /* '('  */
  YYSYMBOL_52_ = 52,                       /* ')'  */
  YYSYMBOL_53_ = 53,                       /* ']'  */
  YYSYMBOL_54_ = 54,                       /* ','  */
  YYSYMBOL_YYACCEPT = 55,                  /* $accept  */
  YYSYMBOL_program = 56,                   /* program  */
  YYSYMBOL_input = 57,                     /* input  */
  YYSYMBOL_list = 58,                      /* list  */
  YYSYMBOL_lstmt = 59,                     /* lstmt  */
  YYSYMBOL_maybe_eol = 60,                 /* maybe_eol  */
  YYSYMBOL_stmt = 61,                      /* stmt  */
  YYSYMBOL_62_1 = 62,                      /* $@1  */
  YYSYMBOL_63_2 = 63,                      /* $@2  */
  YYSYMBOL_64_3 = 64,                      /* $@3  */
  YYSYMBOL_function_def = 65,              /* function_def  */
  YYSYMBOL_if = 66,                        /* if  */
  YYSYMBOL_case = 67,                      /* case  */
  YYSYMBOL_do = 68,                        /* do  */
  YYSYMBOL_while = 69,                     /* while  */
  YYSYMBOL_else = 70,                      /* else  */
  YYSYMBOL_in = 71,                        /* in  */
  YYSYMBOL_caselist = 72,                  /* caselist  */
  YYSYMBOL_casecond = 73,                  /* casecond  */
  YYSYMBOL_nls = 74,                       /* nls  */
  YYSYMBOL_name = 75,                      /* name  */
  YYSYMBOL_string = 76,                    /* string  */
  YYSYMBOL_nesting_level = 77,             /* nesting_level  */
  YYSYMBOL_port_type = 78,                 /* port_type  */
  YYSYMBOL_req_code = 79,                  /* req_code  */
  YYSYMBOL_send_flags = 80,                /* send_flags  */
  YYSYMBOL_send_flag_list = 81,            /* send_flag_list  */
  YYSYMBOL_send_flag = 82,                 /* send_flag  */
  YYSYMBOL_expr_or_pair_list = 83,         /* expr_or_pair_list  */
  YYSYMBOL_cond = 84,                      /* cond  */
  YYSYMBOL_bool = 85,                      /* bool  */
  YYSYMBOL_expr = 86,                      /* expr  */
  YYSYMBOL_maybe_expr = 87,                /* maybe_expr  */
  YYSYMBOL_value = 88,                     /* value  */
  YYSYMBOL_closure = 89,                   /* closure  */
  YYSYMBOL_imm_value = 90,                 /* imm_value  */
  YYSYMBOL_maybe_prlist = 91,              /* maybe_prlist  */
  YYSYMBOL_maybe_pair_list = 92,           /* maybe_pair_list  */
  YYSYMBOL_pair_list = 93,                 /* pair_list  */
  YYSYMBOL_pair = 94,                      /* pair  */
  YYSYMBOL_op = 95,                        /* op  */
  YYSYMBOL_prlist = 96,                    /* prlist  */
  YYSYMBOL_pritem = 97                     /* pritem  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  66
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   513

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  124
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  199

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   299


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    47,     2,     2,
      51,    52,    45,    43,    54,    44,     2,    46,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    50,     2,    53,     2,     2,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    48,    49
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   173,   173,   174,   175,   178,   182,   188,   194,   202,
     206,   217,   225,   226,   229,   234,   242,   250,   258,   258,
     267,   267,   267,   275,   280,   286,   294,   300,   305,   313,
     321,   330,   339,   345,   350,   357,   365,   380,   409,   415,
     421,   427,   433,   439,   442,   447,   454,   463,   464,   468,
     471,   474,   475,   476,   484,   487,   493,   497,   503,   507,
     516,   519,   522,   531,   540,   549,   552,   559,   562,   563,
     569,   573,   580,   587,   594,   601,   608,   615,   622,   631,
     634,   635,   653,   657,   673,   680,   687,   694,   701,   708,
     714,   721,   724,   727,   732,   737,   742,   746,   764,   767,
     773,   778,   783,   788,   793,   798,   806,   809,   813,   816,
     819,   824,   829,   834,   837,   850,   854,   858,   862,   866,
     870,   876,   881,   886,   893
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "EOL", "AUTH", "ACCT",
  "SEND", "EXPECT", "T_BEGIN", "T_END", "IF", "ELSE", "WHILE", "DO",
  "BREAK", "CONTINUE", "INPUT", "SHIFT", "GETOPT", "CASE", "IN",
  "T_RETURN", "SET", "PRINT", "EXIT", "T_BOGUS", "ARGCOUNT", "IDENT",
  "PARM", "NAME", "NUMBER", "QUOTE", "BSTRING", "IPADDRESS", "PRITEM",
  "OR", "AND", "EQ", "NE", "LT", "LE", "GT", "GE", "'+'", "'-'", "'*'",
  "'/'", "'%'", "UMINUS", "NOT", "'['", "'('", "')'", "']'", "','",
  "$accept", "program", "input", "list", "lstmt", "maybe_eol", "stmt",
  "$@1", "$@2", "$@3", "function_def", "if", "case", "do", "while", "else",
  "in", "caselist", "casecond", "nls", "name", "string", "nesting_level",
  "port_type", "req_code", "send_flags", "send_flag_list", "send_flag",
  "expr_or_pair_list", "cond", "bool", "expr", "maybe_expr", "value",
  "closure", "imm_value", "maybe_prlist", "maybe_pair_list", "pair_list",
  "pair", "op", "prlist", "pritem", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-126)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-110)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     326,    21,  -126,    -3,   -16,   423,  -126,  -126,  -126,    13,
      13,   253,   253,  -126,   253,  -126,   253,   253,    36,    58,
     351,  -126,    56,   423,   199,   253,    57,  -126,  -126,    39,
      25,    -3,  -126,  -126,  -126,   280,   375,  -126,  -126,  -126,
    -126,    16,  -126,  -126,  -126,    26,  -126,  -126,  -126,  -126,
     253,   253,   280,   161,  -126,  -126,   244,  -126,  -126,   244,
     170,  -126,  -126,    71,   253,   253,  -126,  -126,    56,  -126,
     399,   199,   226,   137,  -126,   244,   190,  -126,   199,    59,
    -126,  -126,   -16,  -126,    -5,  -126,   244,     9,  -126,  -126,
    -126,  -126,  -126,  -126,    61,   253,    38,    38,   439,    34,
      20,  -126,   253,   253,   253,   253,   253,    62,   253,  -126,
      94,   244,    47,   170,  -126,  -126,   438,   439,  -126,   199,
     199,   199,   199,   199,   199,   199,   199,   443,    97,   253,
     443,   145,  -126,   280,  -126,  -126,  -126,  -126,  -126,  -126,
     253,  -126,   105,    72,  -126,  -126,    61,    50,  -126,  -126,
       6,     6,    38,    38,    38,    60,  -126,  -126,  -126,  -126,
     278,   465,   471,   471,    42,    42,    42,    42,    93,  -126,
     106,  -126,   449,   103,   443,  -126,   244,  -126,  -126,  -126,
    -126,    55,    97,   443,  -126,  -126,   443,    99,  -126,  -126,
    -126,  -126,   109,  -126,  -126,   110,   199,  -126,   225
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     9,    60,     0,     0,    38,    41,    40,    54,
      54,    30,    91,    39,    91,    32,     0,    91,     0,     0,
       0,     5,     0,     0,     0,     0,     0,    18,    11,     0,
       0,    61,    62,    59,    58,    65,     0,     7,    55,    28,
      29,     0,    96,    94,    95,   104,   100,   102,   103,   101,
       0,     0,   108,     0,    80,    93,    92,    33,    35,   124,
      23,   121,    27,     0,     0,   106,     1,     6,     4,    10,
       0,     0,   108,    12,    68,    79,     0,    20,     0,     0,
      56,    57,     0,    63,   104,    26,    67,     0,   110,    14,
       8,    51,    52,    53,    49,   106,    90,    89,     0,     0,
       0,    31,     0,     0,     0,     0,     0,     0,     0,   123,
       0,    24,     0,   107,    34,    69,     0,    79,    13,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
       0,     0,    64,    65,   115,   118,   116,   119,   117,   120,
       0,   113,     0,     0,   111,    50,    49,     0,    82,   105,
      84,    85,    86,    87,    88,    98,   122,    37,    36,    70,
      72,    71,    73,    76,    74,    77,    75,    78,    15,    43,
       0,    44,     0,     0,     0,    25,   114,   112,    81,    97,
      99,     0,    12,     0,    17,    45,     0,     0,    19,    83,
      42,    16,     0,    21,    47,    46,     0,    48,    22
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -126,  -126,  -126,    86,     8,  -125,   -20,  -126,  -126,  -126,
    -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,   -56,  -126,
     -30,  -126,   107,  -126,    40,  -126,  -126,    87,   -13,   -55,
    -126,   -10,     5,  -126,  -126,  -126,    30,  -126,   -41,   -82,
    -126,   111,   -33
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    19,    20,    36,    37,   127,    22,    78,   130,   196,
      23,    24,    25,    26,    27,   183,   129,   170,   171,   195,
     146,    94,    39,    82,    35,    30,    31,    32,    85,    73,
      74,    75,    57,    54,   181,    55,   112,    99,    87,    88,
     140,   113,    61
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      68,    53,    56,   169,    56,   144,    59,    56,    21,   -66,
     141,   100,   -66,    33,    34,    76,   115,   116,   144,    58,
     -66,   141,    62,   131,    28,    86,    29,   109,    67,    80,
      81,   100,   134,   135,   136,   137,   138,   139,   142,    63,
      96,    97,    98,    38,    90,    91,    95,    92,    93,   142,
      59,   104,   105,   106,   111,    59,   107,   190,    66,    69,
      77,   177,   117,   143,   160,   161,   162,   163,   164,   165,
     166,   167,  -109,    64,   143,   156,    79,    95,    90,   110,
     109,  -110,  -110,  -110,  -110,    59,   149,    65,   107,   132,
     145,   155,   150,   151,   152,   153,   154,   157,    59,   158,
     118,   142,   179,    59,   182,   180,   187,   168,   189,    70,
     173,   193,   194,   197,   185,   184,   178,    40,    83,   172,
     175,     0,   133,    86,    41,   147,     0,    60,     0,     0,
     176,     0,    42,    43,    44,    45,    46,    47,    48,    49,
     118,   198,   134,   135,   136,   137,   138,   139,   174,    50,
      51,     0,     0,     0,   188,     0,     0,    52,     0,     0,
     172,     0,     0,   191,     0,     0,   192,     0,     0,     0,
       0,     0,   119,   120,   121,   122,   123,   124,   125,   126,
     119,   120,   121,   122,   123,   124,   125,   126,    41,     0,
     101,     0,     0,     0,     0,     0,    42,    43,    44,    45,
      46,    47,    48,    49,   102,   103,   104,   105,   106,     0,
     128,   107,     0,    50,    51,     0,     0,    41,     0,     0,
       0,    52,     0,     0,   108,    42,    43,    44,    45,    46,
      47,    48,    49,   102,   103,   104,   105,   106,     0,     0,
     107,     0,    50,    51,    41,     0,     0,     0,    71,     0,
      72,     0,    42,    43,    44,    84,    46,    47,    48,    49,
     119,   120,   121,   122,   123,   124,   125,   126,     0,    50,
      51,    41,     0,     0,     0,    71,     0,    72,     0,    42,
      43,    44,    45,    46,    47,    48,    49,   102,   103,   104,
     105,   106,     0,     0,   107,     0,    50,    51,    41,     0,
       0,     0,     0,     0,    52,     0,    42,    43,    44,    84,
      46,    47,    48,    49,   120,   121,   122,   123,   124,   125,
     126,     0,     0,    50,    51,     0,    -2,     1,     0,     2,
       0,    52,     3,     4,     5,     0,     6,     0,     7,     8,
       9,    10,    11,    12,     0,    13,     0,    14,    15,    16,
      17,    -3,     1,     0,     2,    18,     0,     3,     4,     5,
       0,     6,     0,     7,     8,     9,    10,    11,    12,     0,
      13,     0,    14,    15,    16,    17,     1,     0,     2,     0,
      18,     3,     4,     5,    89,     6,     0,     7,     8,     9,
      10,    11,    12,     0,    13,     0,    14,    15,    16,    17,
       1,     0,     2,     0,    18,     3,     4,     5,   114,     6,
       0,     7,     8,     9,    10,    11,    12,     0,    13,     0,
      14,    15,    16,    17,     1,     0,     2,     0,    18,     3,
       4,     5,     0,     6,     0,     7,     8,     9,    10,    11,
      12,     0,    13,     0,    14,    15,    16,    17,     0,     3,
       4,     5,    18,     6,     0,     7,     8,     9,    10,    11,
      12,     0,    13,     0,    14,    15,    16,    17,     0,     0,
       0,     0,    18,   119,   120,   121,   122,   123,   124,   125,
     126,     0,   102,   103,   104,   105,   106,     0,     0,   107,
     159,   148,   102,   103,   104,   105,   106,     0,     0,   107,
       0,   186,   121,   122,   123,   124,   125,   126,  -110,  -110,
     123,   124,   125,   126
};

static const yytype_int16 yycheck[] =
{
      20,    11,    12,   128,    14,    87,    16,    17,     0,     0,
       1,    52,     3,    29,    30,    25,    71,    72,   100,    14,
      11,     1,    17,    78,     3,    35,    29,    60,    20,     4,
       5,    72,    37,    38,    39,    40,    41,    42,    29,     3,
      50,    51,    52,    30,    36,    29,    51,    31,    32,    29,
      60,    45,    46,    47,    64,    65,    50,   182,     0,     3,
       3,   143,    72,    54,   119,   120,   121,   122,   123,   124,
     125,   126,    52,    37,    54,   108,    37,    51,    70,     8,
     113,    39,    40,    41,    42,    95,    52,    51,    50,    30,
      29,    29,   102,   103,   104,   105,   106,     3,   108,    52,
       3,    29,    52,   113,    11,    45,     3,   127,    53,    23,
     130,    12,     3,     3,   170,     9,   146,    10,    31,   129,
     133,    -1,    82,   133,    18,    95,    -1,    16,    -1,    -1,
     140,    -1,    26,    27,    28,    29,    30,    31,    32,    33,
       3,   196,    37,    38,    39,    40,    41,    42,     3,    43,
      44,    -1,    -1,    -1,   174,    -1,    -1,    51,    -1,    -1,
     170,    -1,    -1,   183,    -1,    -1,   186,    -1,    -1,    -1,
      -1,    -1,    35,    36,    37,    38,    39,    40,    41,    42,
      35,    36,    37,    38,    39,    40,    41,    42,    18,    -1,
      29,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,    29,
      30,    31,    32,    33,    43,    44,    45,    46,    47,    -1,
      20,    50,    -1,    43,    44,    -1,    -1,    18,    -1,    -1,
      -1,    51,    -1,    -1,    54,    26,    27,    28,    29,    30,
      31,    32,    33,    43,    44,    45,    46,    47,    -1,    -1,
      50,    -1,    43,    44,    18,    -1,    -1,    -1,    49,    -1,
      51,    -1,    26,    27,    28,    29,    30,    31,    32,    33,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    43,
      44,    18,    -1,    -1,    -1,    49,    -1,    51,    -1,    26,
      27,    28,    29,    30,    31,    32,    33,    43,    44,    45,
      46,    47,    -1,    -1,    50,    -1,    43,    44,    18,    -1,
      -1,    -1,    -1,    -1,    51,    -1,    26,    27,    28,    29,
      30,    31,    32,    33,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    43,    44,    -1,     0,     1,    -1,     3,
      -1,    51,     6,     7,     8,    -1,    10,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,     0,     1,    -1,     3,    29,    -1,     6,     7,     8,
      -1,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,     1,    -1,     3,    -1,
      29,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
       1,    -1,     3,    -1,    29,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,     1,    -1,     3,    -1,    29,     6,
       7,     8,    -1,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    -1,     6,
       7,     8,    29,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    29,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    43,    44,    45,    46,    47,    -1,    -1,    50,
      52,    52,    43,    44,    45,    46,    47,    -1,    -1,    50,
      -1,    52,    37,    38,    39,    40,    41,    42,    37,    38,
      39,    40,    41,    42
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     6,     7,     8,    10,    12,    13,    14,
      15,    16,    17,    19,    21,    22,    23,    24,    29,    56,
      57,    59,    61,    65,    66,    67,    68,    69,     3,    29,
      80,    81,    82,    29,    30,    79,    58,    59,    30,    77,
      77,    18,    26,    27,    28,    29,    30,    31,    32,    33,
      43,    44,    51,    86,    88,    90,    86,    87,    87,    86,
      96,    97,    87,     3,    37,    51,     0,    59,    61,     3,
      58,    49,    51,    84,    85,    86,    86,     3,    62,    37,
       4,     5,    78,    82,    29,    83,    86,    93,    94,     9,
      59,    29,    31,    32,    76,    51,    86,    86,    86,    92,
      93,    29,    43,    44,    45,    46,    47,    50,    54,    97,
       8,    86,    91,    96,     9,    84,    84,    86,     3,    35,
      36,    37,    38,    39,    40,    41,    42,    60,    20,    71,
      63,    84,    30,    79,    37,    38,    39,    40,    41,    42,
      95,     1,    29,    54,    94,    29,    75,    91,    52,    52,
      86,    86,    86,    86,    86,    29,    97,     3,    52,    52,
      84,    84,    84,    84,    84,    84,    84,    84,    61,    60,
      72,    73,    86,    61,     3,    83,    86,    94,    75,    52,
      45,    89,    11,    70,     9,    73,    52,     3,    61,    53,
      60,    61,    61,    12,     3,    74,    64,     3,    84
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    55,    56,    56,    56,    57,    57,    58,    58,    59,
      59,    59,    60,    60,    61,    61,    61,    61,    62,    61,
      63,    64,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    65,    66,    67,
      68,    69,    70,    71,    72,    72,    73,    74,    74,    75,
      75,    76,    76,    76,    77,    77,    78,    78,    79,    79,
      80,    80,    81,    81,    82,    83,    83,    83,    84,    84,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    85,
      86,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    87,    87,    88,    88,    88,    88,    88,    89,    89,
      90,    90,    90,    90,    90,    90,    91,    91,    92,    92,
      93,    93,    93,    93,    94,    95,    95,    95,    95,    95,
      95,    96,    96,    96,    97
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     2,     1,     2,     1,     2,     1,
       2,     2,     0,     1,     3,     4,     6,     5,     0,     5,
       0,     0,     8,     2,     3,     5,     3,     2,     2,     2,
       1,     3,     1,     2,     3,     2,     4,     4,     1,     1,
       1,     1,     2,     2,     1,     2,     4,     1,     2,     0,
       1,     1,     1,     1,     0,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     3,     0,     1,     1,     1,     2,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     1,
       1,     4,     3,     5,     3,     3,     3,     3,     3,     2,
       2,     0,     1,     1,     1,     1,     1,     4,     0,     1,
       1,     1,     1,     1,     1,     3,     0,     1,     0,     1,
       1,     2,     3,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 5: /* input: lstmt  */
#line 179 "gram.y"
                {
			run_statement((yyvsp[0].node));
		}
#line 1470 "gram.c"
    break;

  case 6: /* input: input lstmt  */
#line 183 "gram.y"
                {
			run_statement((yyvsp[0].node));
		}
#line 1478 "gram.c"
    break;

  case 7: /* list: lstmt  */
#line 189 "gram.y"
                {
			(yyval.list) = grad_list_create();
			if ((yyvsp[0].node))
				grad_list_append((yyval.list), (yyvsp[0].node));
		}
#line 1488 "gram.c"
    break;

  case 8: /* list: list lstmt  */
#line 195 "gram.y"
                {
			if ((yyvsp[0].node))
				grad_list_append((yyvsp[-1].list), (yyvsp[0].node));
			(yyval.list) = (yyvsp[-1].list);
		}
#line 1498 "gram.c"
    break;

  case 9: /* lstmt: EOL  */
#line 203 "gram.y"
                {
			(yyval.node) = NULL;
		}
#line 1506 "gram.c"
    break;

  case 10: /* lstmt: stmt EOL  */
#line 207 "gram.y"
                {
			switch (peek_ctx()) {
			case ctx_iferr:
			case ctx_doerr:
				pop_ctx();
				break;
			default:
				break;
			}
		}
#line 1521 "gram.c"
    break;

  case 11: /* lstmt: error EOL  */
#line 218 "gram.y"
                {
			errsync();
			yyclearin;
			yyerrok;
		}
#line 1531 "gram.c"
    break;

  case 14: /* stmt: T_BEGIN list T_END  */
#line 230 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_stmt);
			(yyval.node)->v.list = (yyvsp[-1].list);
		}
#line 1540 "gram.c"
    break;

  case 15: /* stmt: if cond maybe_eol stmt  */
#line 235 "gram.y"
                {
			pop_ctx();
			(yyval.node) = radtest_node_alloc(radtest_node_cond);
			(yyval.node)->v.cond.cond = (yyvsp[-2].node);
			(yyval.node)->v.cond.iftrue = (yyvsp[0].node);
			(yyval.node)->v.cond.iffalse = NULL;
		}
#line 1552 "gram.c"
    break;

  case 16: /* stmt: if cond maybe_eol stmt else stmt  */
#line 243 "gram.y"
                {
			pop_ctx();
			(yyval.node) = radtest_node_alloc(radtest_node_cond);
			(yyval.node)->v.cond.cond = (yyvsp[-4].node);
			(yyval.node)->v.cond.iftrue = (yyvsp[-2].node);
			(yyval.node)->v.cond.iffalse = (yyvsp[0].node);
		}
#line 1564 "gram.c"
    break;

  case 17: /* stmt: case expr in caselist T_END  */
#line 251 "gram.y"
                {
			pop_ctx();
			(yyval.node) = radtest_node_alloc(radtest_node_case);
			(yyval.node)->locus = (yyvsp[-3].node)->locus;
			(yyval.node)->v.branch.expr = (yyvsp[-3].node);
			(yyval.node)->v.branch.branchlist = (yyvsp[-1].list);
		}
#line 1576 "gram.c"
    break;

  case 18: /* $@1: %empty  */
#line 258 "gram.y"
                      { current_nesting_level++; }
#line 1582 "gram.c"
    break;

  case 19: /* stmt: while $@1 cond EOL stmt  */
#line 259 "gram.y"
                {
			pop_ctx();
			current_nesting_level--;
			(yyval.node) = radtest_node_alloc(radtest_node_loop);
			(yyval.node)->v.loop.cond = (yyvsp[-2].node);
			(yyval.node)->v.loop.body = (yyvsp[0].node);
			(yyval.node)->v.loop.first_pass = 0;
		}
#line 1595 "gram.c"
    break;

  case 20: /* $@2: %empty  */
#line 267 "gram.y"
                       { current_nesting_level++; }
#line 1601 "gram.c"
    break;

  case 21: /* $@3: %empty  */
#line 267 "gram.y"
                                                                   { current_nesting_level--; }
#line 1607 "gram.c"
    break;

  case 22: /* stmt: do EOL $@2 stmt EOL WHILE $@3 cond  */
#line 268 "gram.y"
                {
			pop_ctx();
			(yyval.node) = radtest_node_alloc(radtest_node_loop);
			(yyval.node)->v.loop.cond = (yyvsp[0].node);
			(yyval.node)->v.loop.body = (yyvsp[-4].node);
			(yyval.node)->v.loop.first_pass = 1;
		}
#line 1619 "gram.c"
    break;

  case 23: /* stmt: PRINT prlist  */
#line 276 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_print);
			(yyval.node)->v.list = (yyvsp[0].list);
		}
#line 1628 "gram.c"
    break;

  case 24: /* stmt: NAME EQ expr  */
#line 281 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_asgn);
			(yyval.node)->v.asgn.name = (yyvsp[-2].string);
			(yyval.node)->v.asgn.expr = (yyvsp[0].node);
		}
#line 1638 "gram.c"
    break;

  case 25: /* stmt: SEND send_flags port_type req_code expr_or_pair_list  */
#line 287 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_send);
			(yyval.node)->v.send.cntl = (yyvsp[-3].symtab);
			(yyval.node)->v.send.port_type = (yyvsp[-2].i);
			(yyval.node)->v.send.code = (yyvsp[-1].i);
			(yyval.node)->v.send.expr = (yyvsp[0].node);
		}
#line 1650 "gram.c"
    break;

  case 26: /* stmt: EXPECT req_code expr_or_pair_list  */
#line 295 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_expect);
			(yyval.node)->v.expect.code = (yyvsp[-1].i);
			(yyval.node)->v.expect.expr = (yyvsp[0].node);
		}
#line 1660 "gram.c"
    break;

  case 27: /* stmt: EXIT maybe_expr  */
#line 301 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_exit);
			(yyval.node)->v.expr = (yyvsp[0].node);
		}
#line 1669 "gram.c"
    break;

  case 28: /* stmt: BREAK nesting_level  */
#line 306 "gram.y"
                {
			if ((yyvsp[0].i) > current_nesting_level) {
				parse_error(_("not enough 'while's to break from"));
			}
			(yyval.node) = radtest_node_alloc(radtest_node_break);
			(yyval.node)->v.level = (yyvsp[0].i);
		}
#line 1681 "gram.c"
    break;

  case 29: /* stmt: CONTINUE nesting_level  */
#line 314 "gram.y"
                {
			if ((yyvsp[0].i) > current_nesting_level) {
				parse_error(_("not enough 'while's to continue"));
			}
			(yyval.node) = radtest_node_alloc(radtest_node_continue);
			(yyval.node)->v.level = (yyvsp[0].i);
		}
#line 1693 "gram.c"
    break;

  case 30: /* stmt: INPUT  */
#line 322 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_input);
			(yyval.node)->v.input.expr = NULL;
			(yyval.node)->v.input.var = (radtest_variable_t*)
				grad_sym_lookup_or_install(vartab,
							   "INPUT",
							   1);
		}
#line 1706 "gram.c"
    break;

  case 31: /* stmt: INPUT expr NAME  */
#line 331 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_input);
			(yyval.node)->v.input.expr = (yyvsp[-1].node);
			(yyval.node)->v.input.var = (radtest_variable_t*)
				grad_sym_lookup_or_install(vartab,
							   (yyvsp[0].string),
							   1);
		}
#line 1719 "gram.c"
    break;

  case 32: /* stmt: SET  */
#line 340 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_set);
			(yyval.node)->v.set.argc = (yyvsp[0].set).argc;
			(yyval.node)->v.set.argv = (yyvsp[0].set).argv;
		}
#line 1729 "gram.c"
    break;

  case 33: /* stmt: SHIFT maybe_expr  */
#line 346 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_shift);
			(yyval.node)->v.expr = (yyvsp[0].node);
		}
#line 1738 "gram.c"
    break;

  case 34: /* stmt: function_def list T_END  */
#line 351 "gram.y"
                {
			(yyvsp[-2].fun)->body = (yyvsp[-1].list);
			radtest_fix_mem();
			(yyval.node) = NULL;
			defn.function = NULL;
		}
#line 1749 "gram.c"
    break;

  case 35: /* stmt: T_RETURN maybe_expr  */
#line 358 "gram.y"
                {
			if (!defn.function) {
				parse_error(_("return outside of a function definition"));
			}
			(yyval.node) = radtest_node_alloc(radtest_node_return);
			(yyval.node)->v.expr = (yyvsp[0].node);
		}
#line 1761 "gram.c"
    break;

  case 36: /* stmt: NAME '(' maybe_prlist ')'  */
#line 366 "gram.y"
                {
			radtest_function_t *fun;

			fun = (radtest_function_t*)
				grad_sym_lookup(functab, (yyvsp[-3].string));
			if (!fun) {
				parse_error(_("undefined function `%s'"), (yyvsp[-3].string));
			}
			(yyval.node) = radtest_node_alloc(radtest_node_call);
			(yyval.node)->v.call.fun = fun;
			(yyval.node)->v.call.args = (yyvsp[-1].list);
		}
#line 1778 "gram.c"
    break;

  case 37: /* function_def: NAME EOL T_BEGIN EOL  */
#line 381 "gram.y"
                {
			radtest_function_t *fun;

			if (defn.function) {
				parse_error(_("nested function definitions "
					      "are not allowed"));
				parse_error_loc(&defn.locus,
						_("the current function "
						  "definition begins here"));
				YYERROR; /* FIXME */
			}
			defn.function = (yyvsp[-3].string);
			defn.locus = source_locus;
			fun = (radtest_function_t*)
				grad_sym_lookup_or_install(functab, (yyvsp[-3].string), 1);
			if (fun->body) {
				parse_error(_("redefinition of function `%s'"), (yyvsp[-3].string));
				parse_error_loc(&fun->locus,
					     _("`%s' previously defined here"),
						(yyvsp[-3].string));

				YYERROR; /* FIXME */
			}
			fun->locus = source_locus;
			(yyval.fun) = fun;
		}
#line 1809 "gram.c"
    break;

  case 38: /* if: IF  */
#line 410 "gram.y"
                {
			push_ctx(ctx_if);
		}
#line 1817 "gram.c"
    break;

  case 39: /* case: CASE  */
#line 416 "gram.y"
                {
			push_ctx(ctx_case);
		}
#line 1825 "gram.c"
    break;

  case 40: /* do: DO  */
#line 422 "gram.y"
                {
			push_ctx(ctx_do);
		}
#line 1833 "gram.c"
    break;

  case 41: /* while: WHILE  */
#line 428 "gram.y"
                {
			push_ctx(ctx_while);
		}
#line 1841 "gram.c"
    break;

  case 42: /* else: ELSE maybe_eol  */
#line 434 "gram.y"
                {
			pop_ctx();
		}
#line 1849 "gram.c"
    break;

  case 44: /* caselist: casecond  */
#line 443 "gram.y"
                {
			(yyval.list) = grad_list_create();
			grad_list_append((yyval.list), (yyvsp[0].case_branch));
		}
#line 1858 "gram.c"
    break;

  case 45: /* caselist: caselist casecond  */
#line 448 "gram.y"
                {
			grad_list_append((yyvsp[-1].list), (yyvsp[0].case_branch));
			(yyval.list) = (yyvsp[-1].list);
		}
#line 1867 "gram.c"
    break;

  case 46: /* casecond: expr ')' stmt nls  */
#line 455 "gram.y"
                {
			radtest_case_branch_t *p = radtest_branch_alloc();
			p->cond = (yyvsp[-3].node);
			p->node = (yyvsp[-1].node);
			(yyval.case_branch) = p;
		}
#line 1878 "gram.c"
    break;

  case 49: /* name: %empty  */
#line 468 "gram.y"
                {
			(yyval.string) = NULL;
		}
#line 1886 "gram.c"
    break;

  case 53: /* string: BSTRING  */
#line 477 "gram.y"
                {
			parse_error(_("warning: truncating binary string"));
			(yyval.string) = (yyvsp[0].bstring).ptr;
		}
#line 1895 "gram.c"
    break;

  case 54: /* nesting_level: %empty  */
#line 484 "gram.y"
                {
			(yyval.i) = 1;
		}
#line 1903 "gram.c"
    break;

  case 55: /* nesting_level: NUMBER  */
#line 488 "gram.y"
                {
			(yyval.i) = (yyvsp[0].number);
		}
#line 1911 "gram.c"
    break;

  case 56: /* port_type: AUTH  */
#line 494 "gram.y"
                {
			(yyval.i) = GRAD_PORT_AUTH;
		}
#line 1919 "gram.c"
    break;

  case 57: /* port_type: ACCT  */
#line 498 "gram.y"
                {
			(yyval.i) = GRAD_PORT_ACCT;
		}
#line 1927 "gram.c"
    break;

  case 58: /* req_code: NUMBER  */
#line 504 "gram.y"
                {
			(yyval.i) = (yyvsp[0].number);
		}
#line 1935 "gram.c"
    break;

  case 59: /* req_code: NAME  */
#line 508 "gram.y"
                {
			(yyval.i) = grad_request_name_to_code((yyvsp[0].string));
			if ((yyval.i) == 0)
				parse_error(_("expected integer value or request code name"));
		}
#line 1945 "gram.c"
    break;

  case 60: /* send_flags: %empty  */
#line 516 "gram.y"
                {
			(yyval.symtab) = NULL;
		}
#line 1953 "gram.c"
    break;

  case 62: /* send_flag_list: send_flag  */
#line 523 "gram.y"
                {
			radtest_variable_t *var;

			(yyval.symtab) = grad_symtab_create(sizeof(*var), var_free);
			var = (radtest_variable_t*) grad_sym_install((yyval.symtab),
								     (yyvsp[0].var)->name);
			radtest_var_copy (var, (yyvsp[0].var));
		}
#line 1966 "gram.c"
    break;

  case 63: /* send_flag_list: send_flag_list send_flag  */
#line 532 "gram.y"
                {
			radtest_variable_t *var;
			var = (radtest_variable_t*) grad_sym_install((yyvsp[-1].symtab),
								     (yyvsp[0].var)->name);
			radtest_var_copy (var, (yyvsp[0].var)); /* FIXME: check this */
		}
#line 1977 "gram.c"
    break;

  case 64: /* send_flag: NAME EQ NUMBER  */
#line 541 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_integer);
			(yyval.var)->name = (yyvsp[-2].string);
			(yyval.var)->datum.number = (yyvsp[0].number);
		}
#line 1987 "gram.c"
    break;

  case 65: /* expr_or_pair_list: %empty  */
#line 549 "gram.y"
                {
			(yyval.node) = NULL;
		}
#line 1995 "gram.c"
    break;

  case 66: /* expr_or_pair_list: pair_list  */
#line 553 "gram.y"
                {
			radtest_variable_t *var = radtest_var_alloc(rtv_pairlist);
			var->datum.list = (yyvsp[0].list);
			(yyval.node) = radtest_node_alloc(radtest_node_value);
			(yyval.node)->v.var = var;
		}
#line 2006 "gram.c"
    break;

  case 69: /* cond: NOT cond  */
#line 564 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_unary);
			(yyval.node)->v.unary.op = radtest_op_not;
			(yyval.node)->v.unary.operand = (yyvsp[0].node);
		}
#line 2016 "gram.c"
    break;

  case 70: /* cond: '(' cond ')'  */
#line 570 "gram.y"
                {
			(yyval.node) = (yyvsp[-1].node);
		}
#line 2024 "gram.c"
    break;

  case 71: /* cond: cond AND cond  */
#line 574 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_and;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2035 "gram.c"
    break;

  case 72: /* cond: cond OR cond  */
#line 581 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_or;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2046 "gram.c"
    break;

  case 73: /* cond: cond EQ cond  */
#line 588 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_eq;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2057 "gram.c"
    break;

  case 74: /* cond: cond LT cond  */
#line 595 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_lt;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2068 "gram.c"
    break;

  case 75: /* cond: cond GT cond  */
#line 602 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_gt;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2079 "gram.c"
    break;

  case 76: /* cond: cond NE cond  */
#line 609 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_ne;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2090 "gram.c"
    break;

  case 77: /* cond: cond LE cond  */
#line 616 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_le;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2101 "gram.c"
    break;

  case 78: /* cond: cond GE cond  */
#line 623 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_ge;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2112 "gram.c"
    break;

  case 81: /* expr: GETOPT string name name  */
#line 636 "gram.y"
                {
			char *name = (yyvsp[-1].string) ? (yyvsp[-1].string) : "OPTVAR";
			(yyval.node) = radtest_node_alloc(radtest_node_getopt);
			(yyval.node)->v.gopt.last = 0;
			(yyval.node)->v.gopt.optstr = (yyvsp[-2].string);
			(yyval.node)->v.gopt.var = (radtest_variable_t*)
				grad_sym_lookup_or_install(vartab, name, 1);

			name = (yyvsp[0].string) ? (yyvsp[0].string) : "OPTARG";
			(yyval.node)->v.gopt.arg = (radtest_variable_t*)
				grad_sym_lookup_or_install(vartab, name, 1);

			name = (yyvsp[0].string) ? (yyvsp[0].string) : "OPTIND";
			(yyval.node)->v.gopt.ind = (radtest_variable_t*)
				grad_sym_lookup_or_install(vartab, name, 1);

		}
#line 2134 "gram.c"
    break;

  case 82: /* expr: '(' expr ')'  */
#line 654 "gram.y"
                {
			(yyval.node) = (yyvsp[-1].node);
		}
#line 2142 "gram.c"
    break;

  case 83: /* expr: expr '[' NAME closure ']'  */
#line 658 "gram.y"
                {
			grad_dict_attr_t *dict = grad_attr_name_to_dict((yyvsp[-2].string));
			if (!dict) {
				parse_error(_("unknown attribute `%s'"), (yyvsp[-2].string));
				(yyval.node) = NULL;
			} else {
				(yyval.node) = radtest_node_alloc(radtest_node_attr);
				(yyval.node)->v.attr.node = (yyvsp[-4].node);
				(yyval.node)->v.attr.dict = dict;
				(yyval.node)->v.attr.all = (yyvsp[-1].i);
				if ((yyvsp[-1].i) && dict->type != GRAD_TYPE_STRING)
					parse_error(
		     _("warning: '*' is meaningless for this attribute type"));
			}
		}
#line 2162 "gram.c"
    break;

  case 84: /* expr: expr '+' expr  */
#line 674 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_add;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2173 "gram.c"
    break;

  case 85: /* expr: expr '-' expr  */
#line 681 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_sub;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2184 "gram.c"
    break;

  case 86: /* expr: expr '*' expr  */
#line 688 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_mul;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2195 "gram.c"
    break;

  case 87: /* expr: expr '/' expr  */
#line 695 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_div;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2206 "gram.c"
    break;

  case 88: /* expr: expr '%' expr  */
#line 702 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_bin);
			(yyval.node)->v.bin.op = radtest_op_mod;
			(yyval.node)->v.bin.left = (yyvsp[-2].node);
			(yyval.node)->v.bin.right = (yyvsp[0].node);
		}
#line 2217 "gram.c"
    break;

  case 89: /* expr: '-' expr  */
#line 709 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_unary);
			(yyval.node)->v.unary.op = radtest_op_neg;
			(yyval.node)->v.unary.operand = (yyvsp[0].node);
		}
#line 2227 "gram.c"
    break;

  case 90: /* expr: '+' expr  */
#line 715 "gram.y"
                {
			(yyval.node) = (yyvsp[0].node);
		}
#line 2235 "gram.c"
    break;

  case 91: /* maybe_expr: %empty  */
#line 721 "gram.y"
                {
			(yyval.node) = NULL;
		}
#line 2243 "gram.c"
    break;

  case 93: /* value: imm_value  */
#line 728 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_value);
			(yyval.node)->v.var = (yyvsp[0].var);
		}
#line 2252 "gram.c"
    break;

  case 94: /* value: IDENT  */
#line 733 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_deref);
			(yyval.node)->v.deref = (yyvsp[0].deref);
		}
#line 2261 "gram.c"
    break;

  case 95: /* value: PARM  */
#line 738 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_parm);
			(yyval.node)->v.parm = (yyvsp[0].parm);
		}
#line 2270 "gram.c"
    break;

  case 96: /* value: ARGCOUNT  */
#line 743 "gram.y"
                {
			(yyval.node) = radtest_node_alloc(radtest_node_argcount);
		}
#line 2278 "gram.c"
    break;

  case 97: /* value: NAME '(' maybe_prlist ')'  */
#line 747 "gram.y"
                {
			radtest_function_t *fun;

			fun = (radtest_function_t*)
				grad_sym_lookup(functab, (yyvsp[-3].string));
			if (!fun) {
				parse_error(_("undefined function `%s'"), (yyvsp[-3].string));
				(yyval.node) = NULL;
			} else {
				(yyval.node) = radtest_node_alloc(radtest_node_call);
				(yyval.node)->v.call.fun = fun;
				(yyval.node)->v.call.args = (yyvsp[-1].list);
			}
		}
#line 2297 "gram.c"
    break;

  case 98: /* closure: %empty  */
#line 764 "gram.y"
                {
			(yyval.i) = 0;
		}
#line 2305 "gram.c"
    break;

  case 99: /* closure: '*'  */
#line 768 "gram.y"
                {
			(yyval.i) = 1;
		}
#line 2313 "gram.c"
    break;

  case 100: /* imm_value: NUMBER  */
#line 774 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_integer);
			(yyval.var)->datum.number = (yyvsp[0].number);
		}
#line 2322 "gram.c"
    break;

  case 101: /* imm_value: IPADDRESS  */
#line 779 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_ipaddress);
			(yyval.var)->datum.ipaddr = (yyvsp[0].ipaddr);
		}
#line 2331 "gram.c"
    break;

  case 102: /* imm_value: QUOTE  */
#line 784 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_string);
			(yyval.var)->datum.string = (yyvsp[0].string);
		}
#line 2340 "gram.c"
    break;

  case 103: /* imm_value: BSTRING  */
#line 789 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_bstring);
			(yyval.var)->datum.bstring = (yyvsp[0].bstring);
		}
#line 2349 "gram.c"
    break;

  case 104: /* imm_value: NAME  */
#line 794 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_string);
			(yyval.var)->datum.string = (yyvsp[0].string);
		}
#line 2358 "gram.c"
    break;

  case 105: /* imm_value: '(' maybe_pair_list ')'  */
#line 799 "gram.y"
                {
			(yyval.var) = radtest_var_alloc(rtv_pairlist);
			(yyval.var)->datum.list = (yyvsp[-1].list);
		}
#line 2367 "gram.c"
    break;

  case 106: /* maybe_prlist: %empty  */
#line 806 "gram.y"
                {
			(yyval.list) = NULL;
		}
#line 2375 "gram.c"
    break;

  case 108: /* maybe_pair_list: %empty  */
#line 813 "gram.y"
                {
			(yyval.list) = NULL;
		}
#line 2383 "gram.c"
    break;

  case 110: /* pair_list: pair  */
#line 820 "gram.y"
                {
			(yyval.list) = grad_list_create();
			grad_list_append((yyval.list), (yyvsp[0].pair));
		}
#line 2392 "gram.c"
    break;

  case 111: /* pair_list: pair_list pair  */
#line 825 "gram.y"
                {
			grad_list_append((yyvsp[-1].list), (yyvsp[0].pair));
			(yyval.list) = (yyvsp[-1].list);
		}
#line 2401 "gram.c"
    break;

  case 112: /* pair_list: pair_list ',' pair  */
#line 830 "gram.y"
                {
			grad_list_append((yyvsp[-2].list), (yyvsp[0].pair));
			(yyval.list) = (yyvsp[-2].list);
		}
#line 2410 "gram.c"
    break;

  case 114: /* pair: NAME op expr  */
#line 838 "gram.y"
                {
			grad_dict_attr_t *attr = grad_attr_name_to_dict((yyvsp[-2].string));
			if (!attr)
				parse_error(_("unknown attribute `%s'"), (yyvsp[-2].string));

			(yyval.pair) = radtest_pair_alloc();
			(yyval.pair)->attr = attr;
			(yyval.pair)->op = (yyvsp[-1].op);
			(yyval.pair)->node = (yyvsp[0].node);
		}
#line 2425 "gram.c"
    break;

  case 115: /* op: EQ  */
#line 851 "gram.y"
                {
			(yyval.op) = grad_operator_equal;
		}
#line 2433 "gram.c"
    break;

  case 116: /* op: LT  */
#line 855 "gram.y"
                {
			(yyval.op) = grad_operator_less_than;
		}
#line 2441 "gram.c"
    break;

  case 117: /* op: GT  */
#line 859 "gram.y"
                {
			(yyval.op) = grad_operator_greater_than;
		}
#line 2449 "gram.c"
    break;

  case 118: /* op: NE  */
#line 863 "gram.y"
                {
			(yyval.op) = grad_operator_not_equal;
		}
#line 2457 "gram.c"
    break;

  case 119: /* op: LE  */
#line 867 "gram.y"
                {
			(yyval.op) = grad_operator_less_equal;
		}
#line 2465 "gram.c"
    break;

  case 120: /* op: GE  */
#line 871 "gram.y"
                {
			(yyval.op) = grad_operator_greater_equal;
		}
#line 2473 "gram.c"
    break;

  case 121: /* prlist: pritem  */
#line 877 "gram.y"
                {
			(yyval.list) = grad_list_create();
			grad_list_append((yyval.list), (yyvsp[0].node));
		}
#line 2482 "gram.c"
    break;

  case 122: /* prlist: prlist ',' pritem  */
#line 882 "gram.y"
                {
			grad_list_append((yyvsp[-2].list), (yyvsp[0].node));
			(yyval.list) = (yyvsp[-2].list);
		}
#line 2491 "gram.c"
    break;

  case 123: /* prlist: prlist pritem  */
#line 887 "gram.y"
                {
			grad_list_append((yyvsp[-1].list), (yyvsp[0].node));
			(yyval.list) = (yyvsp[-1].list);
		}
#line 2500 "gram.c"
    break;


#line 2504 "gram.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 896 "gram.y"


void
yyerror(char const *s)
{
	if (strcmp(s, "parse error") == 0
	    || strcmp(s, "syntax error") == 0) {
		if (yychar == T_END)
			parse_error(_("Misplaced `end'"));
		else if (yychar == EOL) {
			grad_locus_t loc = source_locus;
			loc.line--;
			parse_error_loc(&loc, _("Unexpected end of line"));
		} else if (peek_ctx() == ctx_doerr)
			;
		else
			parse_error(s);
	} else if (yychar == EOL) {
		grad_locus_t loc = source_locus;
		loc.line--;
		parse_error_loc(&loc, s);
	} else
		parse_error(s);
}

static char *funcname_displayed = NULL;

static int
namecmp(char const *a, char const *b)
{
	if (!a || !b)
		return a != b;
	return strcmp(a, b);
}

static void
print_function_name(void)
{
	if (namecmp(funcname_displayed, defn.function)) {
		if (defn.function)
			fprintf(stderr, _("In function `%s':\n"),
				defn.function);
		else
			fprintf(stderr, _("At top level:\n"));
		funcname_displayed = defn.function;
	}
}

void
parse_error(const char *fmt, ...)
{
	va_list ap;

	print_function_name();
	va_start(ap, fmt);
	fprintf(stderr, "%s:%lu: ",
		source_locus.file,
		(unsigned long) source_locus.line);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	error_count++;
}

void
parse_error_loc(grad_locus_t *locus, const char *fmt, ...)
{
	va_list ap;

	print_function_name();
	va_start(ap, fmt);
	fprintf(stderr, "%s:%lu: ",
		locus->file, (unsigned long) locus->line);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	error_count++;
}

void
set_yydebug(void)
{
	extern int yydebug;
	if (GRAD_DEBUG_LEVEL(1)) {
		yydebug = 1;
	}
}

static void
run_statement(radtest_node_t *node)
{
	if (!dry_run) {
		if (!error_count)
			radtest_eval(node, toplevel_env);
		/* Clear error_count only if we are in interactive
		   mode. There is no use continuing executing of a script
		   after an erroneous statement. */
		if (interactive)
			error_count = 0;
	}
	radtest_free_mem();
}

int
read_and_eval(char *filename)
{
	if (open_input(filename))
		return 1;
	return (yyparse() || error_count) ? 1 : 0;
}

static void
errsync(void)
{
	enum context ctx = pop_ctx();
	switch (ctx) {
	case ctx_none:
		break;

	case ctx_if:
		push_ctx(ctx_iferr);
		break;

	case ctx_do:
		current_nesting_level--;
		push_ctx(ctx_doerr);
		break;

	case ctx_while:
		current_nesting_level--;
		break;

	default:
		break;
	}
}
