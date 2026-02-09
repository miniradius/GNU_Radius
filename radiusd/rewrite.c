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

/* Substitute the type names.  */
#define YYSTYPE         RW_YYSTYPE
/* Substitute the variable and function names.  */
#define yyparse         rw_yyparse
#define yylex           rw_yylex
#define yyerror         rw_yyerror
#define yydebug         rw_yydebug
#define yynerrs         rw_yynerrs
#define yylval          rw_yylval
#define yychar          rw_yychar

/* First part of user prologue.  */
#line 1 "rewrite.y"

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
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>

#include <radiusd.h>
#include <setjmp.h>
#include <rewrite.h>
#ifdef USE_SERVER_GUILE
# include <libguile.h>
# include <radius/radscm.h>
#endif

#define RW_MIN(a,b) ((a)<(b)) ? (a) : (b)

/*
 * Generalized object
 */
typedef struct objhdr {
	SLIST_ENTRY(objhdr) link;
} OBJHDR;

static void obj_free_all(void);

#define OBJ(type) \
	OBJHDR  header; \
	DLIST_ENTRY(type) link;


/* ************************************************************
 * Basic data types
 */

typedef int stkoff_t;             /* Offset on stack */
typedef unsigned int pctr_t;      /* Program counter */
typedef void (*INSTR)(void);      /* program instruction */

typedef union rw_code_cell {
	void     *v_ptr;
	char     *v_str;
	int      v_int;
	long     v_long;
	u_int    v_u_int;
	stkoff_t v_off;
	pctr_t   v_pc;
	struct comp_regex *v_rx;
	size_t   v_size;
} RWSTYPE;

#define rw_cat(a,b) a ## b
#define rw_c_val(x,t) ((x).rw_cat(v_,t))
#define rw_c_cast(x,t) ((RWSTYPE)(t)(x))

typedef union {
	INSTR   c_instr;
	RWSTYPE c_value;
} RWCODE;

#define rw_code_instr(p) ((p).c_instr)
#define rw_code_value(p) ((p).c_value)

#define RW_REG ('z'-'a'+1)

typedef struct {
	RWSTYPE    reg[RW_REG];       /* Registers */
	#define rA reg[0]
	char       *sA;               /* String accumulator */
	pctr_t     pc;                /* Program counter */

	RWSTYPE    *stack;            /* Stack+heap space */
	int        stacksize;         /* Size of stack */
	int        st;                /* Top of stack */
	int        sb;                /* Stack base */
	int        ht;                /* Top of heap */

	int        nmatch;
	regmatch_t *pmatch;

	grad_request_t *req;

	jmp_buf    jmp;
} RWMACH;

/* Compiled regular expression
 */
typedef struct comp_regex COMP_REGEX;
struct comp_regex {
	OBJ(comp_regex);
	regex_t      regex;    /* compiled regex itself */
	int          nmatch;   /* number of \( ... \) groups */
};

typedef DLIST_HEAD(, comp_regex) COMP_REGEX_HEAD;
/*
 * Binary Operations
 */
typedef enum {
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
	BAnd,
	BXor,
	BOr,
	And,
	Or,
	Shl,
	Shr,
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	Max_opcode
} Bopcode;

/*
 * Unary operations
 */
typedef enum {
	Neg,
	Not,
	Max_unary
} Uopcode;

/*
 * Matrix types
 */
typedef enum {
	Generic,
	Nop,
	Enter,
	Leave,
	Stop,
	Constant,
	Matchref,
	Variable,
	Unary,
	Binary,
	Cond,
	Asgn,
	Match,
	Coercion,
	Expression,
	Return,
	Jump,
	Branch,
	Target,
	Call,
	Builtin,
	Pop,
	Pusha,
	Popa,
	Attr,
	Attr_asgn,
	Attr_check,
	Attr_delete,
	Max_mtxtype
} Mtxtype;

/*
 * Function parameter
 */
typedef struct parm_t PARAMETER;
struct parm_t {
	PARAMETER   *prev;     /* Previous parameter */
	PARAMETER   *next;     /* Next parameter */
	grad_data_type_t    datatype;  /* type */
	stkoff_t    offset;    /* Offset on stack */
};

/*
 * Local variable
 */
typedef struct variable VAR;
struct variable {
	OBJ(variable);
	VAR       *dcllink;  /* Link to the next variable vithin the
			      * same declaration
			      */
	char      *name;     /* name of the variable */
	int       level;     /* nesting level */
	int       offset;    /* offset on stack */
	grad_data_type_t  datatype;  /* type */
	int       constant;  /* true if assigned a constant value */
	grad_datum_t     datum;     /* constant value itself */
};

/*
 * Function definition
 */
typedef struct function_def {
	struct function_def *next;
	char       *name;        /* Function name */
	grad_data_type_t   rettype;      /* Return type */
	pctr_t     entry;        /* Code entry */
	COMP_REGEX_HEAD rx_head; /* List of compiled regexps */
	int        nparm;        /* Number of parameters */
	PARAMETER  *parm;        /* List of parameters */
	stkoff_t   stack_alloc;  /* required stack allocation */
	grad_locus_t      loc;   /* source location where the function
				  * was declared
				  */
} FUNCTION;

#define STACK_BASE 2

/*
 * Built-in function
 */
typedef struct  {
	INSTR    handler;        /* Function itself */
	char     *name;          /* Function name */
	grad_data_type_t rettype;        /* Return type */
	char     *parms;         /* coded parameter types */
} builtin_t;

/*
 * Operation matrices
 */
typedef struct mtx MTX;
/*
 * All matrices contain the following common fields:
 *    alloc- link to the previously allocated matrix.
 *           It is used at the end of code generation
 *           pass to free all allocated matrices.
 *    next - link to the next matrix in the subexpression
 *    prev - link to the previous matrix in the subexpression
 * Additionally, all expression matrices contain the field
 * `datatype' which contains the data type for this matrix.
 */

#define COMMON_EXPR_MTX \
	grad_data_type_t datatype;\
	MTX      *uplink;\
	MTX      *arglink;

/*
 * Generic matrix: nothing special
 * Type: Generic
 */
typedef struct {
	COMMON_EXPR_MTX
} GEN_MTX;
/*
 * Constant matrix
 * Type: Constant
 */
typedef struct {
	COMMON_EXPR_MTX
	grad_datum_t    datum;     /* Constant value */
} CONST_MTX;
/*
 * Reference to a previous regexp: corresponds to a \N construct
 * Type: Matchref
 */
typedef struct {
	COMMON_EXPR_MTX
	int      num;       /* Number of \( ... \) to be referenced */
} MATCHREF_MTX;
/*
 * Reference to a variable
 * Type: Variable
 */
typedef struct {
	COMMON_EXPR_MTX
	VAR      *var;      /* Variable being referenced */
} VAR_MTX;
/*
 * Unary operation matrix
 * Type: Unary
 */
typedef struct {
	COMMON_EXPR_MTX
	Uopcode  opcode;    /* Operation code */
	MTX      *arg;      /* Argument */
} UN_MTX;
/*
 * Binary operation matrix
 * Type: Binary
 */
typedef struct {
	COMMON_EXPR_MTX
	Bopcode   opcode;   /* Operation code */
	MTX      *arg[2];   /* Arguments */
} BIN_MTX;
/*
 * Assignment matrix
 * Type: Asgn
 */
typedef struct {
	COMMON_EXPR_MTX
	VAR      *lval;     /* Lvalue */
	MTX      *arg;      /* Rvalue */
} ASGN_MTX;
/*
 * Conditional expression matrix
 * Type: Cond
 */
typedef struct {
	MTX      *expr;     /* Conditional expression */
	MTX      *if_true;  /* Branch if true */
	MTX      *if_false; /* Branch if false */
} COND_MTX;
/*
 * Regexp match
 * Type: Match
 */
typedef struct {
	COMMON_EXPR_MTX
	int        negated; /* Is the match negated ? */
	MTX        *arg;    /* Argument (lhs) */
	COMP_REGEX *rx;     /* Regexp (rhs) */
} MATCH_MTX;
/*
 * Type coercion
 * Type: Coerce
 */
typedef struct {
	COMMON_EXPR_MTX
	MTX      *arg;      /* Argument of the coercion */
} COERCE_MTX;
/*
 * Expression
 * Type: Expression
 */
typedef struct {
	COMMON_EXPR_MTX
	MTX      *expr;
} EXPR_MTX;
/*
 * Return from function
 * Type: Return
 */
typedef struct {
	COMMON_EXPR_MTX
	MTX      *expr;     /* Return value */
} RET_MTX;
/*
 * Unconditional branch (jump)
 * Type: Jump
 */
typedef struct {
	MTX *link;          /* Link to the next jump matrix
			     * (for break and continue matrices)
			     */
	MTX      *dest;     /* Jump destination (usually a NOP matrix) */
} JUMP_MTX;
/*
 * Conditional branch
 * Type: Branch
 */
typedef struct {
	int      cond;      /* Condition: 1 - equal, 0 - not equal */
	MTX      *dest;     /* Jump destination (usually a NOP matrix) */
} BRANCH_MTX;
/*
 * Stack frame matrix
 * Type: Enter, Leave
 */
typedef struct {
	stkoff_t  stacksize;/* Required stack size */
} FRAME_MTX;
/*
 * Jump target
 * Type: Target
 */
typedef struct {
	pctr_t  pc;         /* Target's program counter */
} TGT_MTX;
/*
 * No-op matrix. It is always inserted at the branch destination
 * points. Its purpose is to keep a singly-linked list of jump
 * locations for fixing up jump statements.
 * Type: Nop
 */
typedef struct {
	MTX       *tgt;     /* Target list.
			       FIXME: This should be DLIST_HEAD.
			     */
	pctr_t     pc;      /* Program counter for backward
			       references */
} NOP_MTX;
/*
 * Function call
 * Type: Call
 */
typedef struct {
	COMMON_EXPR_MTX
	FUNCTION  *fun;     /* Called function */
	int       nargs;    /* Number of arguments */
	MTX       *args;    /* Arguments */
} CALL_MTX;
/*
 * Builtin function call
 * Type: Builtin
 */
typedef struct {
	COMMON_EXPR_MTX
	INSTR     fun;      /* Handler function */
	int       nargs;    /* Number of arguments */
	MTX       *args;    /* Arguments */
} BTIN_MTX;
/*
 * Attribute matrix
 * Type: Attr, Attr_asgn, Attr_check
 */
typedef struct {
	COMMON_EXPR_MTX
	int       attrno;   /* Attribute number */
	MTX       *index;   /* Index expression */
	MTX       *rval;    /* Rvalue */
} ATTR_MTX;

struct mtx {
	OBJ(mtx);
	int             id;
	grad_locus_t    loc;
	Mtxtype         type;

	union {
		GEN_MTX    gen;
		NOP_MTX    nop;
		FRAME_MTX  frame;
		CONST_MTX  cnst;
		MATCHREF_MTX    ref;
		VAR_MTX    var;
		UN_MTX     un;
		BIN_MTX    bin;
		COND_MTX   cond;
		ASGN_MTX   asgn;
		MATCH_MTX  match;
		COERCE_MTX coerce;
		RET_MTX    ret;
		JUMP_MTX   jump;
		BRANCH_MTX branch;
		TGT_MTX    tgt;
		CALL_MTX   call;
		BTIN_MTX   btin;
		ATTR_MTX   attr;
	};
};

/*
 * Stack frame
 */
typedef struct frame_t FRAME;

struct frame_t {
	OBJ(frame_t);
	int       level;        /* nesting level */
	stkoff_t  stack_offset; /* offset in the stack */
};


/* *****************************************************************
 * Static data
 */
/*
 * Stack Frame list
 */
static DLIST_HEAD(, frame_t) frame_head = DLIST_HEAD_INITIALIZER(frame_head);

static inline FRAME *
frame_cur(void)
{
	return DLIST_LAST(&frame_head);
}

static int errcnt;         /* Number of errors detected */
static FUNCTION *function; /* Function being compiled */
static grad_symtab_t *rewrite_tab;/* Function table */

static DLIST_HEAD(, mtx) mtx_head = DLIST_HEAD_INITIALIZER(mtx_head);
static DLIST_HEAD(, variable) var_head = DLIST_HEAD_INITIALIZER(var_head);

/*
 * Loops
 */
typedef struct loop_t LOOP;
struct loop_t {
	OBJ(loop_t);
	MTX *lp_break;
	MTX *lp_cont;
};
static DLIST_HEAD(, loop_t) loop_head = DLIST_HEAD_INITIALIZER(loop_head);

static inline int
in_loop(void)
{
	return !DLIST_EMPTY(&loop_head);
}

static inline LOOP *
loop_last(void)
{
	return DLIST_LAST(&loop_head);
}

static void loop_push(MTX *mtx);
static void loop_pop(void);
static void loop_fixup(MTX *list, MTX *target);
static void loop_init(void);
static void loop_unwind_all(void);

/*
 * Lexical analyzer stuff
 */
static FILE *infile;               /* Input file */
static grad_locus_t locus;         /* Input location */

static char *inbuf;                /* Input string */
static char *curp;                 /* Current pointer */

static int   yyeof;                /* rised when EOF is encountered */
static grad_strbuf_t tokbuf;       /* token buffer */

static grad_data_type_t return_type = Undefined;
				   /* Data type of the topmost expression. */

static int regcomp_flags = 0;      /* Flags to be used with regcomps */

#define regex_init() regcomp_flags = 0

/* Runtime */
static size_t rewrite_stack_size = 4096;  /* Size of stack+heap */
static RWSTYPE *runtime_stack;
static RWMACH mach;

/* Default domain for gettext functions. It is initialized to PACKAGE
   by default */
static char *default_gettext_domain;


/* ***************************************************************
 * Function declarations
 */

/*
 * Lexical analyzer
 */
static int yylex(void);
static void yysync(void);
static int yyerror(char const *s);

/*
 * Frames
 */
static void frame_init(void);
static void frame_push(void);
static void frame_pop(void);
static void frame_unwind_all(void);
/*
 * Variables
 */
static void var_init(void);
static VAR * var_alloc(grad_data_type_t type, char *name, int grow);
static void var_unwind_level(void);
static void var_unwind_all(void);
static void var_type(grad_data_type_t type, VAR *var);
static VAR *var_lookup(char *name);
/*
 * Matrices
 */
static void mtx_init(void);
static void mtx_unwind_all(void);
static MTX * mtx_cur(void);
static MTX * mtx_nop(void);
static MTX * mtx_jump(void);
static MTX * mtx_frame(Mtxtype type, stkoff_t stksize);
static MTX * mtx_stop(void);
static MTX * mtx_pop(void);
static MTX * mtx_return(MTX *arg);
static MTX * mtx_alloc(Mtxtype type);
static MTX * mtx_const(grad_value_t *val);
static MTX * mtx_ref(int num);
static MTX * mtx_var(VAR *var);
static MTX * mtx_asgn(VAR *var, MTX *arg);
static MTX * mtx_bin(Bopcode opcode, MTX *arg1, MTX *arg2);
static MTX * mtx_un(Uopcode opcode, MTX *arg);
static MTX * mtx_match(int negated, MTX *mtx, COMP_REGEX *);
static MTX * mtx_cond(MTX *cond, MTX *if_true, MTX *if_false);
static MTX * mtx_coerce(grad_data_type_t type, MTX *arg);
static MTX * mtx_call(FUNCTION *fun, MTX *args);
static MTX * mtx_builtin(builtin_t *bin, MTX *args);
static MTX * mtx_attr(grad_dict_attr_t *attr, MTX *index);
static MTX * mtx_attr_asgn(grad_dict_attr_t *attr, MTX *index, MTX *rval);
static MTX * mtx_attr_check(grad_dict_attr_t *attr, MTX *index);
static MTX * mtx_attr_delete(grad_dict_attr_t *attr, MTX *index);

static MTX * coerce(MTX  *arg, grad_data_type_t type);
/*
 * Regular expressions
 */
static COMP_REGEX * rx_alloc(regex_t  *regex, int nmatch);
static void rx_head_free(COMP_REGEX_HEAD *rx);
static COMP_REGEX * compile_regexp(char *str);
/*
 * Functions
 */
static FUNCTION * function_install(FUNCTION *fun);
static void function_free(void *);
static void function_delete(void);
static void function_cleanup(void);
/*
 * Built-in functions
 */
static builtin_t * builtin_lookup(char *name);

/*
 * Code optimizer and generator
 */
static int optimize(void);
static pctr_t codegen(void);
static void code_init(void);
static void code_check(void);

/*
 * Auxiliary and debugging functions
 */
static const char * datatype_str_nom(grad_data_type_t type);
static const char * datatype_str_acc(grad_data_type_t type);
static const char * datatype_str_abl(grad_data_type_t type);
static grad_data_type_t attr_datatype(grad_dict_attr_t *);

/*
 * Run-Time
 */
static void gc(void);
static void run(pctr_t pc);
static int run_init(pctr_t pc, grad_request_t *req);
static int rw_error(const char *msg);
static int rw_error_free(char *msg);

/* These used to lock/unlock access to rw_code array. Now this is
   not needed. However, I left the placeholders for a while... */
#define rw_code_lock()
#define rw_code_unlock()

#define AVPLIST(m) ((m)->req ? (m)->req->avlist : NULL)

static FUNCTION fmain;

#line 749 "rewrite.c"

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


/* Debug traces.  */
#ifndef RW_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define RW_YYDEBUG 1
#  else
#   define RW_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define RW_YYDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined RW_YYDEBUG */
#if RW_YYDEBUG
extern int rw_yydebug;
#endif

/* Token kinds.  */
#ifndef RW_YYTOKENTYPE
# define RW_YYTOKENTYPE
  enum rw_yytokentype
  {
    RW_YYEMPTY = -2,
    RW_YYEOF = 0,                  /* "end of file"  */
    RW_YYerror = 256,              /* error  */
    RW_YYUNDEF = 257,              /* "invalid token"  */
    TYPE = 258,                    /* TYPE  */
    IF = 259,                      /* IF  */
    ELSE = 260,                    /* ELSE  */
    RETURN = 261,                  /* RETURN  */
    WHILE = 262,                   /* WHILE  */
    FOR = 263,                     /* FOR  */
    DO = 264,                      /* DO  */
    BREAK = 265,                   /* BREAK  */
    CONTINUE = 266,                /* CONTINUE  */
    DELETE = 267,                  /* DELETE  */
    STRING = 268,                  /* STRING  */
    IDENT = 269,                   /* IDENT  */
    NUMBER = 270,                  /* NUMBER  */
    REFERENCE = 271,               /* REFERENCE  */
    VARIABLE = 272,                /* VARIABLE  */
    FUN = 273,                     /* FUN  */
    BUILTIN = 274,                 /* BUILTIN  */
    ATTR = 275,                    /* ATTR  */
    BOGUS = 276,                   /* BOGUS  */
    OR = 277,                      /* OR  */
    AND = 278,                     /* AND  */
    MT = 279,                      /* MT  */
    NM = 280,                      /* NM  */
    EQ = 281,                      /* EQ  */
    NE = 282,                      /* NE  */
    LT = 283,                      /* LT  */
    LE = 284,                      /* LE  */
    GT = 285,                      /* GT  */
    GE = 286,                      /* GE  */
    SHL = 287,                     /* SHL  */
    SHR = 288,                     /* SHR  */
    UMINUS = 289,                  /* UMINUS  */
    NOT = 290,                     /* NOT  */
    TYPECAST = 291                 /* TYPECAST  */
  };
  typedef enum rw_yytokentype rw_yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined RW_YYSTYPE && ! defined RW_YYSTYPE_IS_DECLARED
union RW_YYSTYPE
{
#line 676 "rewrite.y"

	int   number;
	int   type;
	VAR   *var;
	MTX   *mtx;
	FUNCTION  *fun;
	builtin_t *btin;
	grad_dict_attr_t *attr;
	struct {
		MTX *arg_first;
		MTX *arg_last;
	} arg;
	struct {
		int nmatch;
		regex_t regex;
	} rx;
	char  *string;

#line 859 "rewrite.c"

};
typedef union RW_YYSTYPE RW_YYSTYPE;
# define RW_YYSTYPE_IS_TRIVIAL 1
# define RW_YYSTYPE_IS_DECLARED 1
#endif


extern RW_YYSTYPE rw_yylval;


int rw_yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TYPE = 3,                       /* TYPE  */
  YYSYMBOL_IF = 4,                         /* IF  */
  YYSYMBOL_ELSE = 5,                       /* ELSE  */
  YYSYMBOL_RETURN = 6,                     /* RETURN  */
  YYSYMBOL_WHILE = 7,                      /* WHILE  */
  YYSYMBOL_FOR = 8,                        /* FOR  */
  YYSYMBOL_DO = 9,                         /* DO  */
  YYSYMBOL_BREAK = 10,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 11,                  /* CONTINUE  */
  YYSYMBOL_DELETE = 12,                    /* DELETE  */
  YYSYMBOL_STRING = 13,                    /* STRING  */
  YYSYMBOL_IDENT = 14,                     /* IDENT  */
  YYSYMBOL_NUMBER = 15,                    /* NUMBER  */
  YYSYMBOL_REFERENCE = 16,                 /* REFERENCE  */
  YYSYMBOL_VARIABLE = 17,                  /* VARIABLE  */
  YYSYMBOL_FUN = 18,                       /* FUN  */
  YYSYMBOL_BUILTIN = 19,                   /* BUILTIN  */
  YYSYMBOL_ATTR = 20,                      /* ATTR  */
  YYSYMBOL_BOGUS = 21,                     /* BOGUS  */
  YYSYMBOL_22_ = 22,                       /* '='  */
  YYSYMBOL_OR = 23,                        /* OR  */
  YYSYMBOL_AND = 24,                       /* AND  */
  YYSYMBOL_MT = 25,                        /* MT  */
  YYSYMBOL_NM = 26,                        /* NM  */
  YYSYMBOL_27_ = 27,                       /* '|'  */
  YYSYMBOL_28_ = 28,                       /* '^'  */
  YYSYMBOL_29_ = 29,                       /* '&'  */
  YYSYMBOL_EQ = 30,                        /* EQ  */
  YYSYMBOL_NE = 31,                        /* NE  */
  YYSYMBOL_LT = 32,                        /* LT  */
  YYSYMBOL_LE = 33,                        /* LE  */
  YYSYMBOL_GT = 34,                        /* GT  */
  YYSYMBOL_GE = 35,                        /* GE  */
  YYSYMBOL_SHL = 36,                       /* SHL  */
  YYSYMBOL_SHR = 37,                       /* SHR  */
  YYSYMBOL_38_ = 38,                       /* '+'  */
  YYSYMBOL_39_ = 39,                       /* '-'  */
  YYSYMBOL_40_ = 40,                       /* '*'  */
  YYSYMBOL_41_ = 41,                       /* '/'  */
  YYSYMBOL_42_ = 42,                       /* '%'  */
  YYSYMBOL_UMINUS = 43,                    /* UMINUS  */
  YYSYMBOL_NOT = 44,                       /* NOT  */
  YYSYMBOL_TYPECAST = 45,                  /* TYPECAST  */
  YYSYMBOL_46_ = 46,                       /* '{'  */
  YYSYMBOL_47_ = 47,                       /* '}'  */
  YYSYMBOL_48_ = 48,                       /* ';'  */
  YYSYMBOL_49_ = 49,                       /* ','  */
  YYSYMBOL_50_ = 50,                       /* '('  */
  YYSYMBOL_51_ = 51,                       /* ')'  */
  YYSYMBOL_YYACCEPT = 52,                  /* $accept  */
  YYSYMBOL_program = 53,                   /* program  */
  YYSYMBOL_input = 54,                     /* input  */
  YYSYMBOL_dcllist = 55,                   /* dcllist  */
  YYSYMBOL_decl = 56,                      /* decl  */
  YYSYMBOL_fundecl = 57,                   /* fundecl  */
  YYSYMBOL_begin = 58,                     /* begin  */
  YYSYMBOL_end = 59,                       /* end  */
  YYSYMBOL_obrace = 60,                    /* obrace  */
  YYSYMBOL_cbrace = 61,                    /* cbrace  */
  YYSYMBOL_autodcl = 62,                   /* autodcl  */
  YYSYMBOL_autovar = 63,                   /* autovar  */
  YYSYMBOL_varlist = 64,                   /* varlist  */
  YYSYMBOL_dclparm = 65,                   /* dclparm  */
  YYSYMBOL_parmlist = 66,                  /* parmlist  */
  YYSYMBOL_parm = 67,                      /* parm  */
  YYSYMBOL_args = 68,                      /* args  */
  YYSYMBOL_arglist = 69,                   /* arglist  */
  YYSYMBOL_arg = 70,                       /* arg  */
  YYSYMBOL_list = 71,                      /* list  */
  YYSYMBOL_stmt = 72,                      /* stmt  */
  YYSYMBOL_73_1 = 73,                      /* @1  */
  YYSYMBOL_while = 74,                     /* while  */
  YYSYMBOL_do = 75,                        /* do  */
  YYSYMBOL_else = 76,                      /* else  */
  YYSYMBOL_cond = 77,                      /* cond  */
  YYSYMBOL_expr = 78                       /* expr  */
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

#if 1

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
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined RW_YYSTYPE_IS_TRIVIAL && RW_YYSTYPE_IS_TRIVIAL)))

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
#define YYFINAL  34
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   535

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  27
/* YYNRULES -- Number of rules.  */
#define YYNRULES  87
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  166

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   291


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
       2,     2,     2,     2,     2,     2,     2,    42,    29,     2,
      50,    51,    40,    38,    49,    39,     2,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    48,
       2,    22,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    28,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    46,    27,    47,     2,     2,     2,     2,
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
      15,    16,    17,    18,    19,    20,    21,    23,    24,    25,
      26,    30,    31,    32,    33,    34,    35,    36,    37,    43,
      44,    45
};

#if RW_YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   726,   726,   732,   736,   758,   759,   760,   777,   803,
     842,   853,   854,   857,   860,   866,   877,   878,   881,   887,
     892,   903,   907,   913,   918,   925,   935,   938,   944,   949,
     957,   963,   964,   967,   971,   976,   981,   988,   993,  1009,
    1009,  1028,  1042,  1055,  1059,  1065,  1072,  1079,  1087,  1098,
    1105,  1113,  1117,  1121,  1127,  1131,  1135,  1139,  1143,  1147,
    1151,  1155,  1159,  1163,  1167,  1171,  1175,  1179,  1183,  1187,
    1191,  1195,  1199,  1203,  1207,  1211,  1215,  1219,  1223,  1227,
    1231,  1235,  1239,  1243,  1247,  1251,  1255,  1264
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TYPE", "IF", "ELSE",
  "RETURN", "WHILE", "FOR", "DO", "BREAK", "CONTINUE", "DELETE", "STRING",
  "IDENT", "NUMBER", "REFERENCE", "VARIABLE", "FUN", "BUILTIN", "ATTR",
  "BOGUS", "'='", "OR", "AND", "MT", "NM", "'|'", "'^'", "'&'", "EQ", "NE",
  "LT", "LE", "GT", "GE", "SHL", "SHR", "'+'", "'-'", "'*'", "'/'", "'%'",
  "UMINUS", "NOT", "TYPECAST", "'{'", "'}'", "';'", "','", "'('", "')'",
  "$accept", "program", "input", "dcllist", "decl", "fundecl", "begin",
  "end", "obrace", "cbrace", "autodcl", "autovar", "varlist", "dclparm",
  "parmlist", "parm", "args", "arglist", "arg", "list", "stmt", "@1",
  "while", "do", "else", "cond", "expr", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-84)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-4)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      67,    17,   -84,   -84,   -84,   -84,    -1,   -22,    12,   -20,
     207,   207,    44,   207,   110,    65,   -84,    33,   -84,    20,
     432,    19,    19,   207,   207,   207,   207,   207,   -84,   -84,
      24,   -84,    16,   235,   -84,   -84,   -84,   -84,   169,    74,
     207,   207,    66,    90,   207,   207,   207,   207,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   207,   207,
       9,   -84,   -84,   432,    57,    61,   -84,   432,    58,   432,
     264,   207,   207,   -84,    62,   207,   -84,   -84,    68,    70,
      94,   169,   152,   -84,    62,   169,   380,   105,    74,   -84,
     451,   469,   -84,   -84,    60,   199,   482,   493,   493,   -33,
     -33,   -33,   -33,   -23,   -23,    -3,    -3,   -84,   -84,   -84,
     107,   -84,   -29,   -84,   -84,   207,   -84,   100,   293,   -84,
     207,   169,   406,   -84,   -84,    13,   152,   -84,   -84,   -84,
     -84,   169,   -84,   -84,   -84,    -6,   -84,   -84,   128,   -84,
     -84,   207,   -84,   322,   127,   -84,   -84,   207,   -84,   -84,
     126,   -84,   120,   -84,   432,   -84,   -84,   169,   351,    62,
     -84,   -84,    87,    88,   -84,   -84
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,    50,    53,    49,    51,    52,     0,     0,    55,
       0,     0,     0,     0,     0,     0,     2,     0,     5,     0,
       4,     0,     0,     0,    26,    26,     0,     0,    76,    75,
      57,    77,     0,     0,     1,     7,     6,    14,     0,    11,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     9,    10,    54,     0,    27,    28,    30,     0,    59,
       0,     0,     0,    78,     0,     0,    45,    46,     0,     0,
       0,     0,     0,    31,     0,     0,     0,     0,    12,    16,
      74,    73,    86,    87,    68,    70,    69,    80,    81,    82,
      83,    84,    85,    71,    72,    63,    64,    65,    66,    67,
       0,    21,     0,    23,    61,     0,    62,    56,     0,    79,
       0,     0,     0,    41,    42,     0,     0,    15,     8,    13,
      32,     0,    39,    34,    19,     0,    17,    25,     0,    22,
      29,     0,    58,     0,    35,    37,    43,     0,    33,    38,
       0,    18,     0,    24,    60,    48,    47,     0,     0,     0,
      20,    36,     0,     0,    44,    40
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -84,   -84,   -84,   -84,   121,   -84,   118,    14,   -84,   -84,
     -84,    51,   -84,   122,   -84,     4,   130,   -84,    28,    64,
     -53,   -84,   -84,   -84,   -84,   -83,     0
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    15,    16,    17,    18,    19,    81,   128,    39,   129,
      88,    89,   135,    61,   112,   113,    64,    65,    66,    82,
      83,   150,    84,    85,   157,   121,    86
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      20,   131,    26,    53,    54,    55,    56,    57,    58,    59,
      28,    29,   110,    31,    33,    55,    56,    57,    58,    59,
     138,    23,   139,    63,    67,    67,    69,    70,    24,   130,
      27,    21,   132,    -3,    35,    22,     1,    57,    58,    59,
      90,    91,   151,   152,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     111,   146,    25,   147,    30,    34,    37,    72,   144,    60,
       1,   118,   119,   130,    71,   122,   163,    87,   149,    92,
       2,     3,     4,     5,     6,     7,     8,     9,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    93,   161,    10,    11,    12,   114,   116,
     115,    13,   120,    32,   125,    67,   123,    14,   124,   134,
     143,   137,   141,     2,     3,     4,     5,     6,     7,     8,
       9,   110,   156,   159,   160,   164,   165,    38,    36,   136,
     148,   154,   153,   140,    62,   126,     0,   158,    10,    11,
      12,     0,     0,     0,    13,    68,    74,     0,    75,    76,
      14,    77,    78,    79,    80,     2,     3,     4,     5,     6,
       7,     8,     9,    74,     0,    75,    76,     0,    77,    78,
      79,    80,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,    13,     0,    37,   127,
       0,     0,    14,     0,     0,     0,     0,    10,    11,    12,
       0,     0,     0,    13,     0,    37,     0,     0,     0,    14,
       2,     3,     4,     5,     6,     7,     8,     9,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,     0,     0,     0,    10,    11,    12,     0,     0,
       0,    13,     0,     0,     0,     0,     0,    14,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,   117,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,     0,     0,     0,     0,
       0,     0,     0,     0,   142,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,     0,     0,     0,     0,     0,
       0,     0,     0,   155,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,     0,     0,     0,     0,     0,     0,
       0,     0,   162,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59,     0,     0,     0,     0,     0,   133,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,     0,
       0,     0,     0,     0,   145,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59
};

static const yytype_int16 yycheck[] =
{
       0,    84,    22,    36,    37,    38,    39,    40,    41,    42,
      10,    11,     3,    13,    14,    38,    39,    40,    41,    42,
      49,    22,    51,    23,    24,    25,    26,    27,    50,    82,
      50,    14,    85,     0,     1,    18,     3,    40,    41,    42,
      40,    41,    48,    49,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      51,    48,    50,    50,    20,     0,    46,    51,   121,    50,
       3,    71,    72,   126,    50,    75,   159,     3,   131,    13,
      13,    14,    15,    16,    17,    18,    19,    20,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    13,   157,    38,    39,    40,    51,    51,
      49,    44,    50,     3,    20,   115,    48,    50,    48,    14,
     120,    14,    22,    13,    14,    15,    16,    17,    18,    19,
      20,     3,     5,     7,    14,    48,    48,    19,    17,    88,
     126,   141,   138,   115,    22,    81,    -1,   147,    38,    39,
      40,    -1,    -1,    -1,    44,    25,     4,    -1,     6,     7,
      50,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,     4,    -1,     6,     7,    -1,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      38,    39,    40,    -1,    -1,    -1,    44,    -1,    46,    47,
      -1,    -1,    50,    -1,    -1,    -1,    -1,    38,    39,    40,
      -1,    -1,    -1,    44,    -1,    46,    -1,    -1,    -1,    50,
      13,    14,    15,    16,    17,    18,    19,    20,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    38,    39,    40,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    50,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    51,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    -1,    -1,    -1,    -1,    -1,    48,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    -1,    48,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,    13,    14,    15,    16,    17,    18,    19,    20,
      38,    39,    40,    44,    50,    53,    54,    55,    56,    57,
      78,    14,    18,    22,    50,    50,    22,    50,    78,    78,
      20,    78,     3,    78,     0,     1,    56,    46,    58,    60,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      50,    65,    65,    78,    68,    69,    70,    78,    68,    78,
      78,    50,    51,    51,     4,     6,     7,     9,    10,    11,
      12,    58,    71,    72,    74,    75,    78,     3,    62,    63,
      78,    78,    13,    13,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
       3,    51,    66,    67,    51,    49,    51,    51,    78,    78,
      50,    77,    78,    48,    48,    20,    71,    47,    59,    61,
      72,    77,    72,    48,    14,    64,    63,    14,    49,    51,
      70,    22,    51,    78,    72,    48,    48,    50,    59,    72,
      73,    48,    49,    67,    78,    51,     5,    76,    78,     7,
      14,    72,    51,    77,    48,    48
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    52,    53,    54,    54,    55,    55,    55,    56,    57,
      57,    58,    58,    59,    60,    61,    62,    62,    63,    64,
      64,    65,    65,    66,    66,    67,    68,    68,    69,    69,
      70,    71,    71,    72,    72,    72,    72,    72,    72,    73,
      72,    72,    72,    72,    72,    74,    75,    76,    77,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     2,     2,     4,     3,
       3,     1,     2,     1,     1,     1,     1,     2,     3,     1,
       3,     2,     3,     1,     3,     2,     0,     1,     1,     3,
       1,     1,     2,     3,     2,     3,     5,     3,     3,     0,
       6,     2,     2,     3,     6,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     3,     1,     4,     2,     5,     3,
       6,     4,     4,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     2,     2,     2,     3,     4,
       3,     3,     3,     3,     3,     3,     3,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = RW_YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == RW_YYEMPTY)                                        \
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
   Use RW_YYerror or RW_YYUNDEF. */
#define YYERRCODE RW_YYUNDEF


/* Enable debugging if requested.  */
#if RW_YYDEBUG

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
#else /* !RW_YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !RW_YYDEBUG */


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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


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

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = RW_YYEMPTY; /* Cause a token to be read.  */

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
  if (yychar == RW_YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= RW_YYEOF)
    {
      yychar = RW_YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == RW_YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = RW_YYUNDEF;
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
  yychar = RW_YYEMPTY;
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
  case 2: /* program: input  */
#line 727 "rewrite.y"
          {
		  obj_free_all();
	  }
#line 2368 "rewrite.c"
    break;

  case 3: /* input: dcllist  */
#line 733 "rewrite.y"
          {
		  return_type = Undefined;
	  }
#line 2376 "rewrite.c"
    break;

  case 4: /* input: expr  */
#line 737 "rewrite.y"
          {
		  if (errcnt) {
			  YYERROR;
		  }

		  mtx_return((yyvsp[0].mtx));

		  memset(&fmain, 0, sizeof(fmain));
		  fmain.name = "main";
		  fmain.rettype = return_type = (yyvsp[0].mtx)->gen.datatype;
		  function = &fmain;

		  if (optimize() == 0) {
			  codegen();
			  if (errcnt) {
				  YYERROR;
			  }
		  }
	  }
#line 2400 "rewrite.c"
    break;

  case 7: /* dcllist: dcllist error  */
#line 761 "rewrite.y"
          {
		  /* Roll back all changes done so far */
		  var_unwind_all();
		  loop_unwind_all();
		  frame_unwind_all();
		  mtx_unwind_all();
		  function_delete();
		  /* Synchronize input after error */
		  yysync();
		  /* Clear input and error condition */
		  yyclearin;
		  yyerrok;
		  errcnt = 0;
	  }
#line 2419 "rewrite.c"
    break;

  case 8: /* decl: fundecl begin list end  */
#line 778 "rewrite.y"
          {
		  if (errcnt) {
			  function_delete();
			  YYERROR;
		  }

		  if (optimize() == 0) {
			  codegen();
			  if (errcnt) {
				  function_delete();
				  YYERROR;
			  }
		  } else {
			  function_delete();
		  }

		  /* clean up things */
		  var_unwind_all();
		  loop_unwind_all();
		  frame_unwind_all();
		  mtx_unwind_all();
		  function_cleanup();
	  }
#line 2447 "rewrite.c"
    break;

  case 9: /* fundecl: TYPE IDENT dclparm  */
#line 804 "rewrite.y"
          {
		  VAR *var;
		  PARAMETER *last, *parm;
		  FUNCTION f;

		  if (errcnt)
			  YYERROR;

		  memset(&f, 0, sizeof(f));
		  f.name    = (yyvsp[-1].string);
		  f.rettype = (yyvsp[-2].type);
		  f.entry   = 0;
		  f.loc     = locus;

		  f.nparm   = 0;
		  f.parm    = NULL;

		  /* Count number of parameters */
		  for (var = (yyvsp[0].var); var; var = DLIST_NEXT(var, link))
			  f.nparm++;

		  f.parm = last = NULL;
		  for (var = (yyvsp[0].var); var; var = DLIST_NEXT(var, link)) {
			  parm = grad_emalloc(sizeof(*parm));
			  parm->datatype = var->datatype;
			  var->offset = -(STACK_BASE+
					  f.nparm - var->offset);
			  parm->offset   = var->offset;
			  parm->prev     = last;
			  parm->next     = NULL;
			  if (f.parm == NULL)
				  f.parm = parm;
			  else
				  last->next = parm;
			  last = parm;
		  }
		  function = function_install(&f);
	  }
#line 2490 "rewrite.c"
    break;

  case 10: /* fundecl: TYPE FUN dclparm  */
#line 843 "rewrite.y"
          {
		  grad_log_loc(GRAD_LOG_ERR, &locus,
			       _("redefinition of function `%s'"), (yyvsp[-1].fun)->name);
		  grad_log_loc(GRAD_LOG_ERR, &(yyvsp[-1].fun)->loc,
			       _("previously defined here"));
		  errcnt++;
		  YYERROR;
	  }
#line 2503 "rewrite.c"
    break;

  case 14: /* obrace: '{'  */
#line 861 "rewrite.y"
          {
		  frame_push();
	  }
#line 2511 "rewrite.c"
    break;

  case 15: /* cbrace: '}'  */
#line 867 "rewrite.y"
          {
		  var_unwind_level();
		  frame_pop();
	  }
#line 2520 "rewrite.c"
    break;

  case 18: /* autovar: TYPE varlist ';'  */
#line 882 "rewrite.y"
          {
		  var_type((yyvsp[-2].type), (yyvsp[-1].var));
	  }
#line 2528 "rewrite.c"
    break;

  case 19: /* varlist: IDENT  */
#line 888 "rewrite.y"
          {
		  (yyval.var) = var_alloc(Undefined, (yyvsp[0].string), +1);
		  (yyval.var)->dcllink = NULL;
	  }
#line 2537 "rewrite.c"
    break;

  case 20: /* varlist: varlist ',' IDENT  */
#line 893 "rewrite.y"
          {
		  VAR *var = var_alloc(Undefined, (yyvsp[0].string), +1);
		  var->dcllink = (yyvsp[-2].var);
		  (yyval.var) = var;
	  }
#line 2547 "rewrite.c"
    break;

  case 21: /* dclparm: '(' ')'  */
#line 904 "rewrite.y"
          {
		  (yyval.var) = NULL;
	  }
#line 2555 "rewrite.c"
    break;

  case 22: /* dclparm: '(' parmlist ')'  */
#line 908 "rewrite.y"
          {
		  (yyval.var) = (yyvsp[-1].var);
	  }
#line 2563 "rewrite.c"
    break;

  case 23: /* parmlist: parm  */
#line 914 "rewrite.y"
          {
		  /*FIXME*/
		  /*$$->dcllink = NULL;*/
	  }
#line 2572 "rewrite.c"
    break;

  case 24: /* parmlist: parmlist ',' parm  */
#line 919 "rewrite.y"
          {
		  /*$1->dcllink = $3;*/
		  (yyval.var) = (yyvsp[-2].var);
	  }
#line 2581 "rewrite.c"
    break;

  case 25: /* parm: TYPE IDENT  */
#line 926 "rewrite.y"
          {
		  (yyval.var) = var_alloc((yyvsp[-1].type), (yyvsp[0].string), +1);
	  }
#line 2589 "rewrite.c"
    break;

  case 26: /* args: %empty  */
#line 935 "rewrite.y"
          {
		  (yyval.mtx) = NULL;
	  }
#line 2597 "rewrite.c"
    break;

  case 27: /* args: arglist  */
#line 939 "rewrite.y"
          {
		  (yyval.mtx) = (yyvsp[0].arg).arg_first;
	  }
#line 2605 "rewrite.c"
    break;

  case 28: /* arglist: arg  */
#line 945 "rewrite.y"
          {
		  (yyvsp[0].mtx)->gen.arglink = NULL;
		  (yyval.arg).arg_first = (yyval.arg).arg_last = (yyvsp[0].mtx);
	  }
#line 2614 "rewrite.c"
    break;

  case 29: /* arglist: arglist ',' arg  */
#line 950 "rewrite.y"
          {
		  (yyvsp[-2].arg).arg_last->gen.arglink = (yyvsp[0].mtx);
		  (yyvsp[-2].arg).arg_last = (yyvsp[0].mtx);
		  (yyval.arg) = (yyvsp[-2].arg);
	  }
#line 2624 "rewrite.c"
    break;

  case 33: /* stmt: begin list end  */
#line 968 "rewrite.y"
          {
		  (yyval.mtx) = (yyvsp[-1].mtx);
	  }
#line 2632 "rewrite.c"
    break;

  case 34: /* stmt: expr ';'  */
#line 972 "rewrite.y"
          {
		  mtx_stop();
		  mtx_pop();
	  }
#line 2641 "rewrite.c"
    break;

  case 35: /* stmt: IF cond stmt  */
#line 977 "rewrite.y"
          {
		  (yyvsp[-1].mtx)->cond.if_false = mtx_nop();
		  (yyval.mtx) = mtx_cur();
	  }
#line 2650 "rewrite.c"
    break;

  case 36: /* stmt: IF cond stmt else stmt  */
#line 982 "rewrite.y"
          {
		  mtx_stop();
		  (yyvsp[-3].mtx)->cond.if_false = (yyvsp[-1].mtx);
		  DLIST_PREV((yyvsp[-1].mtx), link)->jump.dest = mtx_nop();
		  (yyval.mtx) = mtx_cur();
	  }
#line 2661 "rewrite.c"
    break;

  case 37: /* stmt: RETURN expr ';'  */
#line 989 "rewrite.y"
          {
		  /*mtx_stop();*/
		  (yyval.mtx) = mtx_return((yyvsp[-1].mtx));
	  }
#line 2670 "rewrite.c"
    break;

  case 38: /* stmt: while cond stmt  */
#line 994 "rewrite.y"
          {
		  MTX *mtx;

		  mtx_stop();
		  mtx = mtx_jump();
		  mtx->jump.dest = (yyvsp[-2].mtx);
		  (yyvsp[-1].mtx)->cond.if_false = mtx_nop();
		  (yyval.mtx) = mtx_cur();

		  /* Fixup possible breaks */
		  loop_fixup(loop_last()->lp_break, (yyval.mtx));
		  /* Fixup possible continues */
		  loop_fixup(loop_last()->lp_cont, (yyvsp[-2].mtx));
		  loop_pop();
	  }
#line 2690 "rewrite.c"
    break;

  case 39: /* @1: %empty  */
#line 1009 "rewrite.y"
                  { (yyval.mtx) = mtx_nop(); }
#line 2696 "rewrite.c"
    break;

  case 40: /* stmt: do stmt @1 WHILE cond ';'  */
#line 1010 "rewrite.y"
          {
		  /* Default cond rule sets if_true to the next NOP matrix
		   * Invert this behaviour.
		   */
		  (yyvsp[-1].mtx)->cond.if_false = (yyvsp[-1].mtx)->cond.if_true;
		  (yyvsp[-1].mtx)->cond.if_true = (yyvsp[-5].mtx);
		  (yyval.mtx) = mtx_cur();

		  /* Fixup possible breaks */
		  loop_fixup(loop_last()->lp_break, (yyval.mtx));
		  /* Fixup possible continues */
		  loop_fixup(loop_last()->lp_cont, (yyvsp[-3].mtx));
		  loop_pop();
	  }
#line 2715 "rewrite.c"
    break;

  case 41: /* stmt: BREAK ';'  */
#line 1029 "rewrite.y"
          {
		  if (!in_loop()) {
			  grad_log_loc(GRAD_LOG_ERR, &locus,
				       "%s",
				       _("nothing to break from"));
			  errcnt++;
			  YYERROR;
		  }

		  (yyval.mtx) = mtx_jump();
		  (yyval.mtx)->jump.link = loop_last()->lp_break;
		  loop_last()->lp_break = (yyval.mtx);
	  }
#line 2733 "rewrite.c"
    break;

  case 42: /* stmt: CONTINUE ';'  */
#line 1043 "rewrite.y"
          {
		  if (!in_loop()) {
			  grad_log_loc(GRAD_LOG_ERR, &locus,
				       "%s",
				       _("nothing to continue"));
			  errcnt++;
			  YYERROR;
		  }
		  (yyval.mtx) = mtx_jump();
		  (yyval.mtx)->jump.link = loop_last()->lp_cont;
		  loop_last()->lp_cont = (yyval.mtx);
	  }
#line 2750 "rewrite.c"
    break;

  case 43: /* stmt: DELETE ATTR ';'  */
#line 1056 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_delete((yyvsp[-1].attr), NULL);
	  }
#line 2758 "rewrite.c"
    break;

  case 44: /* stmt: DELETE ATTR '(' expr ')' ';'  */
#line 1060 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_delete((yyvsp[-4].attr), (yyvsp[-2].mtx));
	  }
#line 2766 "rewrite.c"
    break;

  case 45: /* while: WHILE  */
#line 1066 "rewrite.y"
          {
		  (yyval.mtx) = mtx_nop();
		  loop_push((yyval.mtx));
	  }
#line 2775 "rewrite.c"
    break;

  case 46: /* do: DO  */
#line 1073 "rewrite.y"
          {
		  (yyval.mtx) = mtx_nop();
		  loop_push((yyval.mtx));
	  }
#line 2784 "rewrite.c"
    break;

  case 47: /* else: ELSE  */
#line 1080 "rewrite.y"
          {
		  mtx_stop();
		  mtx_jump();
		  (yyval.mtx) = mtx_nop();
	  }
#line 2794 "rewrite.c"
    break;

  case 48: /* cond: '(' expr ')'  */
#line 1088 "rewrite.y"
          {
		  mtx_stop();
		  (yyval.mtx) = mtx_cond((yyvsp[-1].mtx), NULL, NULL);
		  (yyval.mtx)->cond.if_true = mtx_nop();
	  }
#line 2804 "rewrite.c"
    break;

  case 49: /* expr: NUMBER  */
#line 1099 "rewrite.y"
          {
		  grad_value_t val;
		  val.type = Integer;
		  val.datum.ival = (yyvsp[0].number);
		  (yyval.mtx) = mtx_const(&val);
	  }
#line 2815 "rewrite.c"
    break;

  case 50: /* expr: STRING  */
#line 1106 "rewrite.y"
          {
		  grad_value_t val;
		  val.type = String;
		  val.datum.sval.size = strlen((yyvsp[0].string));
		  val.datum.sval.data = (yyvsp[0].string);
		  (yyval.mtx) = mtx_const(&val);
	  }
#line 2827 "rewrite.c"
    break;

  case 51: /* expr: REFERENCE  */
#line 1114 "rewrite.y"
          {
		  (yyval.mtx) = mtx_ref((yyvsp[0].number));
	  }
#line 2835 "rewrite.c"
    break;

  case 52: /* expr: VARIABLE  */
#line 1118 "rewrite.y"
          {
		  (yyval.mtx) = mtx_var((yyvsp[0].var));
	  }
#line 2843 "rewrite.c"
    break;

  case 53: /* expr: IDENT  */
#line 1122 "rewrite.y"
          {
		  grad_log_loc(GRAD_LOG_ERR, &locus, _("undefined variable: %s"), (yyvsp[0].string));
		  errcnt++;
		  YYERROR;
	  }
#line 2853 "rewrite.c"
    break;

  case 54: /* expr: VARIABLE '=' expr  */
#line 1128 "rewrite.y"
          {
		  (yyval.mtx) = mtx_asgn((yyvsp[-2].var), (yyvsp[0].mtx));
	  }
#line 2861 "rewrite.c"
    break;

  case 55: /* expr: ATTR  */
#line 1132 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr((yyvsp[0].attr), NULL);
	  }
#line 2869 "rewrite.c"
    break;

  case 56: /* expr: ATTR '(' expr ')'  */
#line 1136 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr((yyvsp[-3].attr), (yyvsp[-1].mtx));
	  }
#line 2877 "rewrite.c"
    break;

  case 57: /* expr: '*' ATTR  */
#line 1140 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_check((yyvsp[0].attr), NULL);
	  }
#line 2885 "rewrite.c"
    break;

  case 58: /* expr: '*' ATTR '(' expr ')'  */
#line 1144 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_check((yyvsp[-3].attr), (yyvsp[-1].mtx));
	  }
#line 2893 "rewrite.c"
    break;

  case 59: /* expr: ATTR '=' expr  */
#line 1148 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_asgn((yyvsp[-2].attr), NULL, (yyvsp[0].mtx));
	  }
#line 2901 "rewrite.c"
    break;

  case 60: /* expr: ATTR '(' expr ')' '=' expr  */
#line 1152 "rewrite.y"
          {
		  (yyval.mtx) = mtx_attr_asgn((yyvsp[-5].attr), (yyvsp[-3].mtx), (yyvsp[0].mtx));
	  }
#line 2909 "rewrite.c"
    break;

  case 61: /* expr: FUN '(' args ')'  */
#line 1156 "rewrite.y"
          {
		  (yyval.mtx) = mtx_call((yyvsp[-3].fun), (yyvsp[-1].mtx));
	  }
#line 2917 "rewrite.c"
    break;

  case 62: /* expr: BUILTIN '(' args ')'  */
#line 1160 "rewrite.y"
          {
		  (yyval.mtx) = mtx_builtin((yyvsp[-3].btin), (yyvsp[-1].mtx));
	  }
#line 2925 "rewrite.c"
    break;

  case 63: /* expr: expr '+' expr  */
#line 1164 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Add, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2933 "rewrite.c"
    break;

  case 64: /* expr: expr '-' expr  */
#line 1168 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Sub, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2941 "rewrite.c"
    break;

  case 65: /* expr: expr '*' expr  */
#line 1172 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Mul, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2949 "rewrite.c"
    break;

  case 66: /* expr: expr '/' expr  */
#line 1176 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Div, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2957 "rewrite.c"
    break;

  case 67: /* expr: expr '%' expr  */
#line 1180 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Rem, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2965 "rewrite.c"
    break;

  case 68: /* expr: expr '|' expr  */
#line 1184 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(BOr, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2973 "rewrite.c"
    break;

  case 69: /* expr: expr '&' expr  */
#line 1188 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(BAnd, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2981 "rewrite.c"
    break;

  case 70: /* expr: expr '^' expr  */
#line 1192 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(BXor, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2989 "rewrite.c"
    break;

  case 71: /* expr: expr SHL expr  */
#line 1196 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Shl, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 2997 "rewrite.c"
    break;

  case 72: /* expr: expr SHR expr  */
#line 1200 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Shr, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3005 "rewrite.c"
    break;

  case 73: /* expr: expr AND expr  */
#line 1204 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(And, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3013 "rewrite.c"
    break;

  case 74: /* expr: expr OR expr  */
#line 1208 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Or, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3021 "rewrite.c"
    break;

  case 75: /* expr: '-' expr  */
#line 1212 "rewrite.y"
          {
		  (yyval.mtx) = mtx_un(Neg, (yyvsp[0].mtx));
	  }
#line 3029 "rewrite.c"
    break;

  case 76: /* expr: '+' expr  */
#line 1216 "rewrite.y"
          {
		  (yyval.mtx) = (yyvsp[0].mtx);
	  }
#line 3037 "rewrite.c"
    break;

  case 77: /* expr: NOT expr  */
#line 1220 "rewrite.y"
          {
		  (yyval.mtx) = mtx_un(Not, (yyvsp[0].mtx));
	  }
#line 3045 "rewrite.c"
    break;

  case 78: /* expr: '(' expr ')'  */
#line 1224 "rewrite.y"
          {
		  (yyval.mtx) = (yyvsp[-1].mtx);
	  }
#line 3053 "rewrite.c"
    break;

  case 79: /* expr: '(' TYPE ')' expr  */
#line 1228 "rewrite.y"
          {
		  (yyval.mtx) = mtx_coerce((yyvsp[-2].type), (yyvsp[0].mtx));
	  }
#line 3061 "rewrite.c"
    break;

  case 80: /* expr: expr EQ expr  */
#line 1232 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Eq, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3069 "rewrite.c"
    break;

  case 81: /* expr: expr NE expr  */
#line 1236 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Ne, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3077 "rewrite.c"
    break;

  case 82: /* expr: expr LT expr  */
#line 1240 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Lt, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3085 "rewrite.c"
    break;

  case 83: /* expr: expr LE expr  */
#line 1244 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Le, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3093 "rewrite.c"
    break;

  case 84: /* expr: expr GT expr  */
#line 1248 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Gt, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3101 "rewrite.c"
    break;

  case 85: /* expr: expr GE expr  */
#line 1252 "rewrite.y"
          {
		  (yyval.mtx) = mtx_bin(Ge, (yyvsp[-2].mtx), (yyvsp[0].mtx));
	  }
#line 3109 "rewrite.c"
    break;

  case 86: /* expr: expr MT STRING  */
#line 1256 "rewrite.y"
          {
		  COMP_REGEX *rx;
		  if ((rx = compile_regexp((yyvsp[0].string))) == NULL) {
			  errcnt++;
			  YYERROR;
		  }
		  (yyval.mtx) = mtx_match(0, (yyvsp[-2].mtx), rx);
	  }
#line 3122 "rewrite.c"
    break;

  case 87: /* expr: expr NM STRING  */
#line 1265 "rewrite.y"
          {
		  COMP_REGEX *rx;
		  if ((rx = compile_regexp((yyvsp[0].string))) == NULL) {
			  errcnt++;
			  YYERROR;
		  }
		  (yyval.mtx) = mtx_match(1, (yyvsp[-2].mtx), rx);
	  }
#line 3135 "rewrite.c"
    break;


#line 3139 "rewrite.c"

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
  yytoken = yychar == RW_YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= RW_YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == RW_YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = RW_YYEMPTY;
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
  if (yychar != RW_YYEMPTY)
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
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 1275 "rewrite.y"


int
yyerror(char const *s)
{
	grad_log_loc(GRAD_LOG_ERR, &locus, "%s", s);
	errcnt++;
	return 0;
}


/* **************************************************************************
 * Interface functions
 */
int
parse_rewrite(char *path)
{
	locus.file = path;
	infile = fopen(locus.file, "r");
	if (!infile) {
		if (errno != ENOENT) {
			grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR,
				 _("can't open file `%s'"),
				 locus.file);
			return -1;
		}
		return -2;
	}

	GRAD_DEBUG(1, "Loading file %s", locus.file);
	rw_code_lock();
	yyeof = 0;
	locus.line = 1;
	errcnt = 0;
	regex_init();
	tokbuf = grad_strbuf_create();

	mtx_init();
	var_init();
	loop_init();
	frame_init();

	frame_push();

	yyparse();

	obj_free_all();

	fclose(infile);
	grad_strbuf_free(tokbuf);
	rw_code_unlock();
	return errcnt;
}

static void debug_dump_code(void);

static int
parse_rewrite_string(char const *str)
{
	rw_code_lock();
	code_check();
	yyeof = 0;
	locus.file = "<string>";
	locus.line = 1;
	errcnt = 0;
	regex_init();
	tokbuf = grad_strbuf_create();

	mtx_init();
	var_init();
	loop_init();
	frame_init();

	frame_push();

	if (GRAD_DEBUG_LEVEL(50))
		yydebug++;

	infile = 0;
	inbuf = curp = grad_estrdup(str);

	yyparse();

	if (GRAD_DEBUG_LEVEL(100))
		debug_dump_code();

	obj_free_all();

	free(inbuf);
	grad_strbuf_free(tokbuf);
	rw_code_unlock();
	return errcnt;
}


/* **************************************************************************
 * Lexical analyzer stuff: too simple to be written in lex.
 */
static int
unput(int c)
{
	if (!c)
		return 0;
	if (infile)
		ungetc(c, infile);
	else if (curp > inbuf)
		*--curp = c;
	return c;
}

static int
input(void)
{
	if (yyeof)
		yychar = 0;
	else if (infile) {
		if ((yychar = getc(infile)) <= 0) {
			yyeof++;
			yychar = 0;
		}
	} else if (curp) {
		yychar = *curp++;
		if (!yychar)
			yyeof++;
	}
	return yychar;
}

static int  rw_backslash(void);
static int  c2d(int c);
static int  read_number(void);
static int  read_num(int n, int base);
static char *read_string(void);
static char *read_ident(int c);
static char *read_to_delim(int c);
static int  skip_to_nl(void);
static int c_comment(void);

/*
 * Convert a character to digit. Only octal, decimal and hex digits are
 * allowed. If any other character is input, c2d() returns 100, which is
 * greater than any number base allowed.
 */
int
c2d(int c)
{
	switch (c) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return c - '0';
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
		return c - 'A' + 16;
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
		return c - 'a' + 10;
	}
	return 100;
}

/*
 * Read a number. Usual C conventions apply.
 */
int
read_number(void)
{
	int c;
	int base;
	int res;

	c = yychar;
	if (c == '0') {
		if (input() == 'x' || yychar == 'X') {
			base = 16;
		} else {
			base = 8;
			unput(yychar);
		}
	} else
		base = 10;

	res = read_num(c2d(c), base);
	if (base == 10 && yychar == '.') {
		int n;

		for (n = 0; n < 3 && yychar == '.'; n++) {
			int val;

			input();
			val = read_num(0, base);
			res = (res << 8) + val;
		}
		if (n != 3)
			res <<= 8 * (3-n);
	}
	return res;
}

int
read_num(int n, int base)
{
	int d;

	while (input() && (d = c2d(yychar)) < 16)
		n = n*base + d;
	unput(yychar);
	return n;
}

int
rw_backslash(void)
{
	switch (input()) {
	case '\\':
		return '\\';
	case 'a':
		return '\a';
	case 'b':
		return '\b';
	case 'f':
		return '\f';
	case 'n':
		return '\n';
	case 'r':
		return '\r';
	case 't':
		return '\t';
	case 'e':
		return '\033';
	case '0':
		return read_number();
	case 'x':
	case 'X':
		return read_num(0, 16);
	case '(':
	case ')':
		/* Preserve regular expressions */
		unput(yychar);
		yychar = '\\';
	}
	return yychar;
}

/*
 * Read a string up to the closing doublequote
 */
char *
read_string(void)
{
	while (input() && yychar != '"') {
		if (yychar == '\\')
			yychar = rw_backslash();
		grad_strbuf_grow_char(tokbuf, yychar);
	}
	return grad_strbuf_finish(tokbuf, 0);
}

/*
 * Read everything up to the given delimiter
 */
char *
read_to_delim(int c)
{
	while (input() && yychar != c)
		grad_strbuf_grow_char(tokbuf, yychar);
	return grad_strbuf_finish(tokbuf, 0);
}

/*
 * Is `c' a part of the word?
 */
#define isword(c) (isalnum(c) || c == '_' || c == '$')

/*
 * Is `c' a whitespace character?
 */
#define isws(c) ((c) == ' ' || (c) == '\t')

/*
 * Read identifier
 */
char *
read_ident(int c)
{
	grad_strbuf_grow_char(tokbuf, c);
	while (input() && isword(yychar))
		grad_strbuf_grow_char(tokbuf, yychar);
	unput(yychar);
	return grad_strbuf_finish(tokbuf, 0);
}

/*
 * Skip input up to the next newline
 */
int
skip_to_nl(void)
{
	while (input() && yychar != '\n')
		;
	return unput(yychar);
}

/*
 * Skip a C-style comment
 */
int
c_comment(void)
{
	if (yychar != '/')
		return 0;
	if (input() == '*') {
		size_t keep_line = locus.line;

		do {
			while (input() != '*') {
				if (yychar == 0) {
					grad_log_loc(GRAD_LOG_ERR, &locus,
		       _("unexpected EOF in comment started at line %lu"),
						     (unsigned long) keep_line);
					return 0;
				} else if (yychar == '\n')
					locus.line++;
			}
		} while (input() != '/');
		return 1;
	}
	unput(yychar);
	yychar = '/';
	return 0;
}


/* Pragmatic comments */
enum pragma_handler_phase {
	pragma_begin,
	pragma_cont,
	pragma_error,
	pragma_end
};

typedef int (*pragma_handler_fp) (enum pragma_handler_phase);

static int
regex_pragma (enum pragma_handler_phase phase)
{
	int disable = 0;
	int bit;
	char *s;
	static int regexp_accum;

	switch (phase) {
	case pragma_begin:
		regexp_accum = 0;
		return 0;

	case pragma_end:
		regcomp_flags = regexp_accum;
		return 0;

	case pragma_error:
		return 0;

	case pragma_cont:
		break;
	}

	switch (yychar) {
	case '+':
		disable = 0;
		input();
		break;

	case '-':
		disable = 1;
		input();
		break;
	}
	if (!isword(yychar)) {
		grad_log_loc(GRAD_LOG_ERR, &locus, _("Malformed pragma"));
		return 1;
	}

	s = read_ident(yychar);

	if (strcmp (s, "extended") == 0)
		bit = REG_EXTENDED;
	else if (strcmp (s, "icase") == 0)
		bit = REG_ICASE;
	else if (strcmp (s, "newline") == 0)
		bit = REG_NEWLINE;
	else {
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("Unknown regexp flag: %s"), s);
		return 1;
	}

	if (disable)
		regexp_accum &= ~bit;
	else
		regexp_accum |= bit;
	return 0;
}

static pragma_handler_fp
find_pragma_handler(char *s)
{
	if (strcmp(s, "regex") == 0)
		return regex_pragma;
	return NULL;
}

static void
handle_pragma(void)
{
	int rc;
	pragma_handler_fp pragma_handler;

	while (input() && isws(yychar))
		;
	if (yychar == 0)
		return;

	pragma_handler = find_pragma_handler (read_ident(yychar));

	if (pragma_handler) {
		pragma_handler(pragma_begin);

		do {
			while (input() && isws(yychar))
				;
			if (yychar == 0 || yychar == '\n')
				break;
			rc = pragma_handler(pragma_cont);
		} while (rc == 0 && yychar != '\n' && yychar != 0);

		pragma_handler(rc ? pragma_error : pragma_end);
	}
}




/* Parse a 'sharp' (single-line) comment */
void
sharp_comment(void)
{
	while (input() && isws(yychar))
		;
	if (yychar == 0)
		return;
	else if (yychar == '\n') {
		locus.line++;
		return;
	} else if (isword(yychar)) {
		if (strcmp(read_ident(yychar), "pragma") == 0)
			handle_pragma();
	}

	skip_to_nl();
}


#define DEBUG_LEX1(s) if (GRAD_DEBUG_LEVEL(60)) printf("yylex: " s "\n")
#define DEBUG_LEX2(s,v) if (GRAD_DEBUG_LEVEL(60)) printf("yylex: " s "\n", v)

static grad_keyword_t rw_kw[] = {
	{ "if",       IF },
	{ "else",     ELSE },
	{ "return",   RETURN },
	{ "for",      FOR },
	{ "do",       DO },
	{ "while",    WHILE },
	{ "break",    BREAK },
	{ "continue", CONTINUE },
	{ "delete",   DELETE },
	{ NULL }
};

int
yylex(void)
{
	int nl;
	int c;
	VAR *var;
	FUNCTION *fun;
	builtin_t *btin;

	/* Skip whitespace and comment lines */
	do {
		nl = 0;
		while (input() && isspace(yychar))
			if (yychar == '\n')
				locus.line++;

		if (!yychar)
			return 0;

		if (yychar == '#') {
			sharp_comment();
			nl = 1;
		}
	} while (nl || c_comment());

	/*
	 * A regexp reference
	 */
	if (yychar == '\\') {
		input();
		yylval.number = read_number();
		DEBUG_LEX2("REFERENCE %d", yylval.number);
		return REFERENCE;
	}

	/*
	 * A character
	 */
	if (yychar == '\'') {
		if (input() == '\\')
			c = rw_backslash();
		else
			c = yychar;
		if (input() != '\'') {
			grad_log_loc(GRAD_LOG_ERR, &locus,
				     "%s",
				     _("unterminated character constant"));
			errcnt++;
		}
		yylval.number = c;
		DEBUG_LEX2("CHAR %d", c);
		return NUMBER;
	}

	/*
	 * A number
	 */
	if (isdigit(yychar)) {
		yylval.number = read_number();
		DEBUG_LEX2("NUMBER %d", yylval.number);
		return NUMBER;
	}

	/*
	 * Quoted string
	 */
	if (yychar == '"') {
		yylval.string = read_string();
		DEBUG_LEX2("STRING %s", yylval.string);
		return STRING;
	}

	/* A/V  pair reference.
	   We do not allow %<number> sequences, since it would result
	   in conflict with binary '%' operator.
	   Thanks to Clement Gerouville for noticing.  */
	if (yychar == '%') {
		grad_dict_attr_t *attr = 0;
		char *attr_name;

		input();
		if (yychar == '[' || yychar == '{') {
			attr_name = read_to_delim(yychar == '[' ? ']' : '}');
			attr = grad_attr_name_to_dict(attr_name);
		} else {
			unput(yychar);
			return '%';
		}
		if (!attr) {
			grad_log_loc(GRAD_LOG_ERR, &locus,
				     _("unknown attribute `%s'"),
				     attr_name);
			errcnt++;
			return BOGUS;
		}
		yylval.attr = attr;
		DEBUG_LEX2("ATTR: %s", attr->name);
		return ATTR;
	}


	/*
	 * Data type or identifier
	 */
	if (isword(yychar)) {
		yylval.string = read_ident(yychar);

		if (strcmp(yylval.string, "integer") == 0) {
			DEBUG_LEX1("TYPE(Integer)");
			yylval.type = Integer;
			return TYPE;
		} else if (strcmp(yylval.string, "string") == 0) {
			DEBUG_LEX1("TYPE(String)");
			yylval.type = String;
			return TYPE;
		}

		if ((c = grad_xlat_keyword(rw_kw, yylval.string, 0)) != 0) {
			DEBUG_LEX2("KW: %s", yylval.string);
			return c;
		}

		if ((var = var_lookup(yylval.string))) {
			DEBUG_LEX2("VARIABLE: %s", yylval.string);
			yylval.var = var;
			return VARIABLE;
		}

		if ((fun = (FUNCTION*) grad_sym_lookup(rewrite_tab, yylval.string))) {
			DEBUG_LEX2("FUN %s", yylval.string);
			yylval.fun = fun;
			return FUN;
		}

		if ((btin = builtin_lookup(yylval.string))) {
			DEBUG_LEX2("BUILTIN %s", yylval.string);
			yylval.btin = btin;
			return BUILTIN;
		}
		DEBUG_LEX2("IDENT: %s", yylval.string);
		return IDENT;
	}

	/*
	 * Boolean expressions
	 */
	if (yychar == '&' || yychar == '|') {
		int c = yychar;

		if (input() == c) {
			DEBUG_LEX2("%s", yychar == '&' ? "AND" : "OR");
			return yychar == '&' ? AND : OR;
		}
		unput(yychar);

		DEBUG_LEX2("%c", c);
		return c;
	}

	/*
	 * Comparison operator
	 */
	if (strchr("<>=!", yychar)) {
		int c = yychar;
		if (input() == '=') {
			switch (c) {
			case '<':
				DEBUG_LEX1("LE");
				return LE;
			case '>':
				DEBUG_LEX1("GE");
				return GE;
			case '=':
				DEBUG_LEX1("EQ");
				return EQ;
			case '!':
				DEBUG_LEX1("NE");
				return NE;
			}
		} else if (c == yychar) {
			if (c == '<') {
				DEBUG_LEX1("SHL");
				return SHL;
			}
			if (c == '>') {
				DEBUG_LEX1("SHR");
				return SHR;
			}
			unput(yychar);
			DEBUG_LEX2("%c", yychar);
			return yychar;
		} else if (yychar == '~') {
			if (c == '=') {
				DEBUG_LEX1("MT");
				return MT;
			}
			if (c == '!') {
				DEBUG_LEX1("NM");
				return NM;
			}
		}
		unput(yychar);
		switch (c) {
		case '<':
			DEBUG_LEX1("LT");
			return LT;
		case '>':
			DEBUG_LEX1("GT");
			return GT;
		case '!':
			DEBUG_LEX1("NOT");
			return NOT;
		default:
			return c;
		}
	}

	DEBUG_LEX2("%c", yychar);
	return yychar;
}

void
yysync(void)
{
	while (skip_to_nl() == '\n' && !isalpha(input()))
		locus.line++;
	unput(yychar);
}


/* ****************************************************************************
 * Generalized object handling
 */

static SLIST_HEAD(, objhdr) alloc_head = SLIST_HEAD_INITIALIZER(alloc_head);

static void *
obj_alloc(size_t size)
{
	OBJHDR *p = grad_emalloc(size);
	SLIST_INSERT_HEAD(p, &alloc_head, link);
	return p;
}

#define OBJ_ALLOC(p) p = obj_alloc(sizeof(*p))

static void
obj_free_all(void)
{
	SLIST_FREE(&alloc_head, link, objhdr, free);
}


/* **************************************************************************
 * Frames
 */

void
frame_init(void)
{
	DLIST_INIT(&frame_head);
}

void
frame_push(void)
{
	FRAME *this_frame, *curframe = frame_cur();

	OBJ_ALLOC(this_frame);

	if (!curframe) {
		this_frame->level = 0;
		this_frame->stack_offset = 0;
	} else {
		if (curframe->level == 0)
			this_frame->stack_offset = 1;
		else
			this_frame->stack_offset = curframe->stack_offset;
		this_frame->level = curframe->level + 1;
	}
	DLIST_INSERT_TAIL(&frame_head, this_frame, link);
}

void
frame_pop(void)
{
	DLIST_REMOVE(&frame_head, DLIST_LAST(&frame_head), link);
}

void
frame_update_alloc(void)
{
	FRAME *this_frame = DLIST_LAST(&frame_head);

	if (this_frame->stack_offset > function->stack_alloc)
		function->stack_alloc = this_frame->stack_offset;
}

void
frame_unwind_all(void)
{
	while (!DLIST_EMPTY(&frame_head))
		DLIST_REMOVE_HEAD(&frame_head, link);
	frame_push();
}


/* **************************************************************************
 * Loops
 */

void
loop_init(void)
{
	DLIST_INIT(&loop_head);
}

void
loop_unwind_all(void)
{
	loop_init(); // FIXME:?
}

/*ARGSUSED*/
void
loop_push(MTX *mtx)
{
	LOOP *this_loop;
	OBJ_ALLOC(this_loop);
	DLIST_INSERT_TAIL(&loop_head, this_loop, link);
}

void
loop_pop(void)
{
	DLIST_REMOVE(&loop_head, DLIST_LAST(&loop_head), link);
}

void
loop_fixup(MTX *list, MTX *target)
{
	MTX *jp;

	for (jp = list; jp; jp = jp->jump.link)
		jp->jump.dest = target;
}


/* **************************************************************************
 * Variables
 */
void
var_init(void)
{
	DLIST_INIT(&var_head);
}

VAR *
var_alloc(grad_data_type_t type, char *name, int grow)
{
	VAR *var;

	OBJ_ALLOC(var);
	DLIST_INSERT_TAIL(&var_head, var, link);

	/* Initialize fields */
	var->name     = name;
	var->datatype = type;
	var->level    = frame_cur()->level;
	var->offset   = frame_cur()->stack_offset;
	frame_cur()->stack_offset += grow;

	return var;
}

void
var_unwind_level(void)
{
	int cnt = 0;
	VAR *last;

	while (!DLIST_EMPTY(&var_head) &&
	       (last = DLIST_LAST(&var_head))->level == frame_cur()->level) {
		DLIST_REMOVE(&var_head, last, link);
		cnt++;
	}

	if (cnt)
		frame_update_alloc();
}

void
var_unwind_all(void)
{
	while (!DLIST_EMPTY(&var_head))
		DLIST_REMOVE_HEAD(&var_head, link);
}

void
var_type(grad_data_type_t type, VAR *var)
{
	for (; var; var = var->dcllink)
		var->datatype = type;
}

VAR *
var_lookup(char *name)
{
	VAR *var;

	DLIST_FOREACH_REVERSE(var, &var_head, link)
		if (strcmp(var->name, name) == 0)
			break;
	return var;
}


/* **************************************************************************
 * Matrix generation
 */
static int mtx_current_id;

/*
 * Insert a matrix into list
 */
static void
mtx_append(MTX *mtx)
{
	DLIST_INSERT_TAIL(&mtx_head, mtx, link);
}

static void
mtx_remove(MTX *mtx)
{
	DLIST_REMOVE(&mtx_head, mtx, link);
}

void
mtx_insert(MTX *prev, MTX *mtx)
{
	MTX *up;

	DLIST_INSERT_AFTER(&mtx_head, prev, mtx, link);
	if ((up = prev->gen.uplink)) {
		switch (up->type) {
		case Unary:
			up->un.arg = mtx;
			break;
		case Binary:
			if (up->bin.arg[0] == prev)
				up->bin.arg[0] = mtx;
			else
				up->bin.arg[1] = mtx;
			break;
		case Return:
			up->ret.expr = mtx;
			break;
		default:
			/*should not happen*/
			break;
		}
	}
}

void
mtx_init(void)
{
	DLIST_INIT(&mtx_head);
}

void
mtx_unwind_all(void)
{
	while (!DLIST_EMPTY(&mtx_head))
		DLIST_REMOVE_HEAD(&mtx_head, link);
}

MTX *
mtx_cur(void)
{
	return DLIST_LAST(&mtx_head);
}

MTX *
mtx_frame(Mtxtype type, stkoff_t stksize)
{
	MTX *mtx = mtx_alloc(type);
	mtx_append(mtx);
	mtx->frame.stacksize = stksize;
	return mtx;
}

MTX *
mtx_nop(void)
{
	MTX *mtx = mtx_alloc(Nop);
	mtx_append(mtx);
	return mtx;
}

MTX *
mtx_jump(void)
{
	MTX *mtx = mtx_alloc(Jump);
	mtx_append(mtx);
	return mtx;
}

MTX *
mtx_stop(void)
{
	MTX *mtx = mtx_alloc(Stop);
	mtx_append(mtx);
	return mtx;
}

MTX *
mtx_pop(void)
{
	MTX *mtx = mtx_alloc(Pop);
	mtx_append(mtx);
	return mtx;
}


MTX *
mtx_return(MTX *arg)
{
	MTX *mtx = mtx_alloc(Return);

	mtx_append(mtx);
	mtx->ret.expr = arg;
	arg->gen.uplink = (MTX*)mtx;
	return (MTX*)mtx;
}

/*
 * Allocate a matrix of given type and append it to the list
 */
MTX *
mtx_alloc(Mtxtype type)
{
	MTX *mtx;

	OBJ_ALLOC(mtx);

	mtx->type  = type;
	mtx->loc   = locus;
	mtx->id    = mtx_current_id++;
	return mtx;
}

/*
 * Create a Constant matrix
 */
MTX *
mtx_const(grad_value_t *val)
{
	MTX *mtx = mtx_alloc(Constant);

	mtx_append(mtx);
	mtx->cnst.datatype = val->type;
	mtx->cnst.datum = val->datum;
	return mtx;
}

/*
 * Create a Reference matrix
 */
MTX *
mtx_ref(int num)
{
	MTX *mtx = mtx_alloc(Matchref);
	mtx_append(mtx);
	mtx->ref.datatype = String;
	mtx->ref.num = num;
	return mtx;
}

MTX *
mtx_var(VAR *var)
{
	MTX *mtx = mtx_alloc(Variable);
	mtx_append(mtx);
	mtx->var.datatype = var->datatype;
	mtx->var.var = var;
	return mtx;
}

MTX *
mtx_asgn(VAR *var, MTX *arg)
{
	MTX *mtx = mtx_alloc(Asgn);

	mtx_append(mtx);
	if (var->datatype != arg->gen.datatype)
		coerce(arg, var->datatype);
	mtx->asgn.datatype = var->datatype;
	mtx->asgn.lval = var;
	mtx->asgn.arg  = arg;
	return mtx;
}


grad_data_type_t
attr_datatype(grad_dict_attr_t *attr)
{
	switch (attr->type) {
	case GRAD_TYPE_STRING:
		/* FIXME: It could be a nice move to do

		     (attr->prop & GRAD_AP_BINARY_STRING) ? Binstr : String;

		   instead... */
		return String;
	case GRAD_TYPE_DATE:
		return String;
	case GRAD_TYPE_INTEGER:
	case GRAD_TYPE_IPADDR:
		return Integer;
	}
	grad_insist_fail("unknown attribute type");
	/*NOTREACHED*/
	return -1;
}

MTX *
mtx_attr(grad_dict_attr_t *attr, MTX *index)
{
	MTX *mtx = mtx_alloc(Attr);
	mtx_append(mtx);
	mtx->attr.attrno   = attr->value;
	mtx->attr.datatype = attr_datatype(attr);
	mtx->attr.index = index;
	return mtx;
}

MTX *
mtx_attr_check(grad_dict_attr_t *attr,	MTX *index)
{
	MTX *mtx = mtx_alloc(Attr_check);
	mtx_append(mtx);
	mtx->attr.attrno   = attr->value;
	mtx->attr.datatype = Integer;
	mtx->attr.index = index;
	return mtx;
}


void
rw_coercion_warning(grad_data_type_t from, grad_data_type_t to, char *pref)
{
	grad_log_loc(GRAD_LOG_WARN, &locus,
		     _("%s implicit coercion %s %s"),
		     pref ? pref : "",
		     datatype_str_abl(from),
		     datatype_str_acc(to));
}


MTX *
mtx_attr_asgn(grad_dict_attr_t *attr, MTX *index, MTX *rval)
{
	MTX *mtx = mtx_alloc(Attr_asgn);
	mtx_append(mtx);
	mtx->attr.attrno   = attr->value;
	mtx->attr.datatype = attr_datatype(attr);
	if (rval->gen.datatype != mtx->attr.datatype) {
		rw_coercion_warning(rval->gen.datatype, mtx->attr.datatype, NULL);
		rval = coerce(rval, mtx->attr.datatype);
	}
	mtx->attr.index = index;
	mtx->attr.rval = rval;
	return mtx;
}

MTX *
mtx_attr_delete(grad_dict_attr_t *attr, MTX *index)
{
	MTX *mtx = mtx_alloc(Attr_delete);
	mtx_append(mtx);
	mtx->attr.attrno   = attr->value;
	mtx->attr.datatype = attr_datatype(attr);
	mtx->attr.index = index;
	return mtx;
}

MTX *
mtx_bin(Bopcode opcode, MTX *arg1, MTX *arg2)
{
	MTX *mtx = mtx_alloc(Binary);

	mtx_append(mtx);
	if (arg1->gen.datatype != arg2->gen.datatype) {
		rw_coercion_warning(String, Integer, NULL);
		if (arg1->gen.datatype == String)
			arg1 = coerce(arg1, Integer);
		else
			arg2 = coerce(arg2, Integer);
	}

	switch (arg1->gen.datatype) {
	case String:
		switch (opcode) {
		case Add:
			mtx->bin.datatype = String;
			break;
		case Eq:
		case Ne:
		case Lt:
		case Le:
		case Gt:
		case Ge:
			mtx->bin.datatype = Integer;
			break;
		default:
			grad_log_loc(GRAD_LOG_ERR, &locus,
				     "%s",
				     _("operation not applicable to strings"));
			errcnt++;
			return mtx;
		}
		break;

	case Integer:
		mtx->bin.datatype = Integer;
		break;

	default:
		grad_insist_fail("unknown data type");
	}

	mtx->bin.opcode = opcode;
	mtx->bin.arg[0] = arg1;
	mtx->bin.arg[1] = arg2;
	arg1->gen.uplink = arg2->gen.uplink = mtx;
	return mtx;
}

MTX *
mtx_un(Uopcode opcode, MTX *arg)
{
	MTX *mtx = mtx_alloc(Unary);

	mtx_append(mtx);
	if (arg->gen.datatype != Integer) {
		rw_coercion_warning(String, Integer, NULL);
		coerce(arg, Integer);
	}
	mtx->un.datatype = Integer;
	mtx->un.opcode = opcode;
	mtx->un.arg = arg;
	arg->gen.uplink = mtx;
	return mtx;
}

MTX *
mtx_match(int negated, MTX *arg, COMP_REGEX *rx)
{
	MTX *mtx = mtx_alloc(Match);

	mtx_append(mtx);
	if (arg->gen.datatype != String) {
		rw_coercion_warning(Integer, String, NULL);
		coerce(arg, String);
	}
	mtx->match.datatype = Integer;
	mtx->match.negated = negated;
	mtx->match.arg = arg;
	mtx->match.rx  = rx;
	return mtx;
}

MTX *
mtx_cond(MTX *cond, MTX *if_true, MTX *if_false)
{
	MTX *mtx = mtx_alloc(Cond);

	mtx_append(mtx);
	mtx->cond.expr = cond;
	mtx->cond.if_true   = if_true;
	mtx->cond.if_false  = if_false;
	return mtx;
}

MTX *
mtx_coerce(grad_data_type_t type, MTX *arg)
{
	if (type == arg->gen.datatype)
		return mtx_cur();
	return coerce(arg, type);
}

MTX *
coerce(MTX *arg, grad_data_type_t type)
{
	MTX *mtx = mtx_alloc(Coercion);

	mtx_insert(arg, mtx);
	mtx->coerce.datatype = type;
	mtx->coerce.arg = arg;
	return mtx;
}

MTX *
mtx_call(FUNCTION *fun, MTX *args)
{
	MTX       *argp;
	MTX  *call;
	PARAMETER *parmp;
	int       argn;

	/*
	 * Test the number and types of arguments. Insert reasonable
	 * typecasts.
	 */
	argn = 0;
	argp = args;
	parmp = fun->parm;
	while (argp && parmp) {
		if (argp->gen.datatype != parmp->datatype) {
			char buf[24];
			snprintf(buf, sizeof buf, _("(argument %d)"), argn);
			rw_coercion_warning(argp->gen.datatype,
					    parmp->datatype, buf);
			coerce(argp, parmp->datatype);
		}
		argn++;
		argp  = argp->gen.arglink;
		parmp = parmp->next;
	}

	/*
	 * Note that the argument count mismatch is not an error!
	 */
	if (argp) {
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("too many arguments in call to %s"),
			     fun->name);
		errcnt++;
	} else if (parmp) {
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("too few arguments in call to %s"),
			     fun->name);
		errcnt++;
	}

	call = mtx_alloc(Call);
	mtx_append(call);

	call->call.datatype = fun->rettype;
	call->call.fun  = fun;
	call->call.args = args;
	call->call.nargs = argn;

	return call;
}

MTX *
mtx_builtin(builtin_t *bin, MTX *args)
{
	MTX          *argp;
	MTX          *call;
	int          argn;
	char         *parmp;
	grad_data_type_t     type = Integer;
	/*
	 * Test the number and types of arguments. Insert reasonable
	 * typecasts.
	 */
	argn = 0;
	argp = args;
	parmp = bin->parms;

	while (argp && parmp) {
		switch (parmp[0]) {
		case 'i':
			type = Integer;
			break;
		case 's':
			type = String;
			break;
		default:
			grad_insist_fail("malformed builtin");
		}

		if (argp->gen.datatype != type) {
			char buf[24];
			snprintf(buf, sizeof buf, _("(argument %d)"), argn);
			rw_coercion_warning(argp->gen.datatype, type, buf);
			coerce(argp, type);
		}
		argn++;
		argp  = argp->gen.arglink;
		parmp++;
	}

	if (argp) {
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("too many arguments in call to %s"),
			     bin->name);
		errcnt++;
	} else if (*parmp) {
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("too few arguments in call to %s"),
			     bin->name);
		errcnt++;
	}

	call = mtx_alloc(Builtin);
	mtx_append(call);

	call->btin.datatype = bin->rettype;
	call->btin.fun  = bin->handler;
	call->btin.args = args;
	call->btin.nargs = argn;

	return call;
}


/* ****************************************************************************
 * Code optimizer (rudimentary)
 */
static const char *
datatype_str_nom(grad_data_type_t type)
{
	switch (type) {
	case Undefined:
		return _("Undefined");
	case Integer:
		return _("integer");
	case String:
		return _("string");
	default:
		return _("UNKNOWN");
	}
}

static const char *
datatype_str_abl(grad_data_type_t type)
{
	switch (type) {
	case Undefined:
		return _("from Undefined");
	case Integer:
		return _("from integer");
	case String:
		return _("from string");
	default:
		return _("from UNKNOWN");
	}
}

static const char *
datatype_str_acc(grad_data_type_t type)
{
	switch (type) {
	case Undefined:
		return _("to Undefined");
	case Integer:
		return _("to integer");
	case String:
		return _("to string");
	default:
		return _("to UNKNOWN");
	}
}

static FILE *
debug_open_file(void)
{
	FILE *fp;
	char *path;

	path = grad_mkfilename(grad_log_dir, "radius.mtx");
	if ((fp = fopen(path, "a")) == NULL) {
		grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR,
			 _("can't open file `%s'"),
			 path);
	}
	free(path);
	return fp;
}

static void debug_print_datum(FILE *fp, grad_data_type_t type,  grad_datum_t *datum);
static void debug_print_var(FILE *fp, VAR *var);
static void debug_print_unary(FILE *fp, MTX *mtx);
static void debug_print_binary(FILE *fp, MTX *mtx);
static void debug_print_mtxlist(char *s);

static char *b_opstr[] = {
	"Eq",
	"Ne",
	"Lt",
	"Le",
	"Gt",
	"Ge",
	"&",
	"^",
	"|",
	"And",
	"Or",
	"Shl",
	"Shr",
	"Add",
	"Sub",
	"Mul",
	"Div",
	"Rem",
};

static char *u_opstr[] = {
	"Neg",
	"Not"
};

#define LINK(m) (m ? m->id : 0)

void
debug_print_datum(FILE *fp, grad_data_type_t type, grad_datum_t *datum)
{
	fprintf(fp, "%3.3s ", datatype_str_nom(type));
	switch (type) {
	case Integer:
		fprintf(fp, "%d", datum->ival);
		break;

	case String:
		fprintf(fp, "%s", datum->sval.data);//FIXME: if binary?
		break;

	default:
		grad_insist_fail("unknown data type");
	}
}

void
debug_print_var(FILE *fp, VAR *var)
{
	fprintf(fp, "%3.3s %s L:%d S:%d",
		datatype_str_nom(var->datatype),
		var->name,
		var->level,
		var->offset);
	if (var->constant) {
		fprintf(fp, "CONST ");
		debug_print_datum(fp, var->datatype, &var->datum);
	}
}

void
debug_print_unary(FILE *fp, MTX *mtx)
{
	fprintf(fp, "OP:%s M:%d",
		u_opstr[mtx->un.opcode], LINK(mtx->un.arg));
}

void
debug_print_binary(FILE *fp, MTX *mtx)
{
	fprintf(fp, "OP:%s M1:%d M2:%d",
		b_opstr[mtx->bin.opcode],
		LINK(mtx->bin.arg[0]),
		LINK(mtx->bin.arg[1]));
}


void
debug_print_mtxlist(char *s)
{
	FILE *fp;
	MTX  *mtx, *tmp;

	if ((fp = debug_open_file()) == NULL)
		return;

	#define CASE(c) case c: fprintf(fp, "%-10.10s", #c);

	fprintf(fp, "%s\n", s);
	DLIST_FOREACH(mtx, &mtx_head, link) {
		fprintf(fp, "%4d: %4d %4d ",
			mtx->id,
			LINK(DLIST_PREV(mtx, link)),
			LINK(DLIST_NEXT(mtx, link)));
		switch (mtx->type) {
		CASE (Generic)
			break;
		CASE (Nop)
			break;
		CASE (Enter)
			fprintf(fp, "%3.3s %d",
				"",
				mtx->frame.stacksize);
			break;
		CASE (Leave)
			fprintf(fp, "%3.3s %d",
				"",
				mtx->frame.stacksize);
			break;
		CASE (Stop)
			break;
		CASE (Constant)
			debug_print_datum(fp, mtx->cnst.datatype,
					  &mtx->cnst.datum);
			break;
		CASE (Matchref)
			fprintf(fp, "%3.3s %d",
				datatype_str_nom(String),
				mtx->ref.num);
			break;
		CASE (Variable)
			debug_print_var(fp, mtx->var.var);
			break;
		CASE (Unary)
			fprintf(fp, "%3.3s ",
				datatype_str_nom(mtx->un.datatype));
			debug_print_unary(fp, mtx);
			break;
		CASE (Binary)
			fprintf(fp, "%3.3s ",
				datatype_str_nom(mtx->gen.datatype));
			debug_print_binary(fp, mtx);
			break;
		CASE (Cond)
			fprintf(fp, "%3.3s ", "");
			fprintf(fp, "C:%4d T:%4d F:%4d",
				LINK(mtx->cond.expr),
				LINK(mtx->cond.if_true),
				LINK(mtx->cond.if_false));
			break;
		CASE (Asgn)
			fprintf(fp, "%3.3s ",
				datatype_str_nom(mtx->asgn.datatype));
			fprintf(fp, "V:%s,%d,%d M:%4d",
				mtx->asgn.lval->name,
				mtx->asgn.lval->level,
				mtx->asgn.lval->offset,
				LINK(mtx->asgn.arg));
				break;
		CASE (Match)
			fprintf(fp, "    N:%1d M:%4d RX:%p",
				mtx->match.negated,
				LINK(mtx->match.arg),
				mtx->match.rx);
			break;
		CASE (Coercion)
			fprintf(fp, "%3.3s M:%4d",
				datatype_str_nom(mtx->coerce.datatype),
				LINK(mtx->coerce.arg));
			break;
		CASE (Return)
			fprintf(fp, "%3.3s M:%4d",
				datatype_str_nom(mtx->ret.expr->gen.datatype),
				LINK(mtx->ret.expr));
			break;
		CASE (Jump)
			fprintf(fp, "%3.3s M:%4d",
				"",
				LINK(mtx->jump.dest));
			break;
		CASE (Branch)
			fprintf(fp, "%3.3s M:%4d",
				mtx->branch.cond ? "NE" : "EQ",
				LINK(mtx->branch.dest));
			break;
		CASE (Call)
			fprintf(fp, "%3.3s F:%s, A:%d:",
				datatype_str_nom(mtx->call.datatype),
				mtx->call.fun->name,
				mtx->call.fun->nparm);
			for (tmp = mtx->call.args; tmp; tmp = tmp->gen.arglink)
				fprintf(fp, "%d,", tmp->id);
			break;

		CASE(Builtin)
			fprintf(fp, "%3.3s F:%p, A:%d:",
				datatype_str_nom(mtx->btin.datatype),
				mtx->btin.fun,
				mtx->btin.nargs);
			for (tmp = mtx->btin.args; tmp; tmp = tmp->gen.arglink)
				fprintf(fp, "%d,", tmp->id);
			break;

		CASE (Pop)
			break;

		CASE (Pusha)
			break;

		CASE (Popa)
			break;

		CASE (Attr)
			fprintf(fp, "%3.3s A:%d I:%d",
				datatype_str_nom(mtx->attr.datatype),
				mtx->attr.attrno,
				mtx->attr.index ? mtx->attr.index->id : 0);
			break;

		CASE (Attr_check)
			fprintf(fp, "%3.3s A:%d I:%d",
				datatype_str_nom(mtx->attr.datatype),
				mtx->attr.attrno,
				mtx->attr.index ? mtx->attr.index->id : 0);
			break;

		CASE (Attr_asgn)
			fprintf(fp, "%3.3s A:%d I:%d M:%d",
				datatype_str_nom(mtx->attr.datatype),
				mtx->attr.attrno,
				mtx->attr.index ? mtx->attr.index->id : 0,
				LINK(mtx->attr.rval));
			break;

		CASE (Attr_delete)
			fprintf(fp, "%3.3s A:%d I:%d",
				datatype_str_nom(mtx->attr.datatype),
				mtx->attr.attrno,
				mtx->attr.index ? mtx->attr.index->id : 0);
			break;

		default:
			fprintf(fp, "UNKNOWN: %d", mtx->type);
		}
		fprintf(fp, "\n");
	}

	fclose(fp);
}

void
debug_print_function(void)
{
	FILE      *fp;
	PARAMETER *parm;
	int        n;

	if ((fp = debug_open_file()) == NULL)
		return;

	fprintf(fp, "FUNCTION: %s\n", function->name);
	fprintf(fp, "RETURNS : %s\n", datatype_str_nom(function->rettype));
	fprintf(fp, "NPARMS  : %d\n", function->nparm);
	fprintf(fp, "PARMS   :\n");

	for (parm = function->parm, n = 0; parm; parm = parm->next, n++)
		fprintf(fp, "    %4d: %s at %4d\n",
			n, datatype_str_nom(parm->datatype),
			parm->offset);

	fclose(fp);
}

#define DEBUG_MTX(c) if (GRAD_DEBUG_LEVEL(30)) debug_print_mtxlist(c);
#define DEBUG_FUN()  if (GRAD_DEBUG_LEVEL(25)) debug_print_function();

static int pass2_unary(MTX *mtx);
static int pass2_binary(MTX *mtx);

void
pass1(void)
{
	MTX *mtx;
	MTX *end;
	MTX *last;

	/*
	 * Create an entry matrix
	 */
	mtx = mtx_alloc(Enter);
	DLIST_INSERT_HEAD(&mtx_head, mtx, link);
	mtx->frame.stacksize = function->stack_alloc;

	last = DLIST_LAST(&mtx_head);
	/*
	 * Provide a default return statement if necessary
	 */
	if (last->type != Return) {
		grad_value_t val;
		grad_log_loc(GRAD_LOG_WARN, &last->loc,
			     _("missing return statement"));

		val.type = function->rettype;
		switch (function->rettype) {
		case Integer:
			val.datum.ival = 0;
			break;

		case String:
			val.datum.sval.data = "";
			val.datum.sval.size = 0;
			break;

		default:
			grad_insist_fail("Unknown data type");
		}
		mtx_const(&val);
		mtx_frame(Leave, function->stack_alloc);
	} else {
		last->type = Leave;
		last->frame.stacksize = function->stack_alloc;
	}

	/*
	 * Insert a no-op matrix before the `leave' one
	 */
	end = mtx_alloc(Nop);
	DLIST_INSERT_BEFORE(&mtx_head, DLIST_LAST(&mtx_head), end, link);

	DLIST_FOREACH(mtx, &mtx_head, link) {
		if (mtx->type == Return) {
			if (mtx->ret.expr->gen.datatype != function->rettype) {
				rw_coercion_warning(
					mtx->ret.expr->gen.datatype,
					function->rettype, NULL);
				coerce(mtx->ret.expr, function->rettype);
			}
			mtx->type = Jump;
			mtx->jump.dest = end;
		}
	}
}

/*
 * Second pass: elimination of constant sub-expressions
 */

/*
 * Perform immediate unary calculations
 */
int
pass2_unary(MTX *mtx)
{
	MTX *arg = mtx->un.arg;

	switch (mtx->un.opcode) {
	case Not:
		arg->cnst.datum.ival = !arg->cnst.datum.ival;
		break;

	case Neg:
		arg->cnst.datum.ival = -arg->cnst.datum.ival;
		break;

	default:
		grad_insist_fail("Unexpected opcode");
	}
	mtx->type = Constant;
	mtx->cnst.datum = arg->cnst.datum;
	mtx_remove(arg);
	return 0;
}

/*
 * Perform immediate binary computations
 */
int
pass2_binary(MTX *mtx)
{
	MTX *arg0 = mtx->bin.arg[0];
	MTX *arg1 = mtx->bin.arg[1];
	grad_datum_t dat;

	switch (mtx->bin.opcode) {
	case Eq:
		dat.ival = arg0->cnst.datum.ival == arg1->cnst.datum.ival;
		break;

	case Ne:
		dat.ival = arg0->cnst.datum.ival != arg1->cnst.datum.ival;
		break;

	case Lt:
		dat.ival = arg0->cnst.datum.ival < arg1->cnst.datum.ival;
		break;

	case Le:
		dat.ival = arg0->cnst.datum.ival <= arg1->cnst.datum.ival;
		break;

	case Gt:
		dat.ival = arg0->cnst.datum.ival > arg1->cnst.datum.ival;
		break;

	case Ge:
		dat.ival = arg0->cnst.datum.ival >= arg1->cnst.datum.ival;
		break;

	case BAnd:
		dat.ival = arg0->cnst.datum.ival & arg1->cnst.datum.ival;
		break;

	case BOr:
		dat.ival = arg0->cnst.datum.ival | arg1->cnst.datum.ival;
		break;

	case BXor:
		dat.ival = arg0->cnst.datum.ival ^ arg1->cnst.datum.ival;
		break;

	case And:
		dat.ival = arg0->cnst.datum.ival && arg1->cnst.datum.ival;
		break;

	case Or:
		dat.ival = arg0->cnst.datum.ival || arg1->cnst.datum.ival;
		break;

	case Shl:
		dat.ival = arg0->cnst.datum.ival << arg1->cnst.datum.ival;
		break;

	case Shr:
		dat.ival = arg0->cnst.datum.ival >> arg1->cnst.datum.ival;
		break;

	case Add:
		dat.ival = arg0->cnst.datum.ival + arg1->cnst.datum.ival;
		break;

	case Sub:
		dat.ival = arg0->cnst.datum.ival - arg1->cnst.datum.ival;
		break;

	case Mul:
		dat.ival = arg0->cnst.datum.ival * arg1->cnst.datum.ival;
		break;

	case Div:
		if (arg1->cnst.datum.ival == 0) {
			grad_log_loc(GRAD_LOG_ERR, &arg1->loc,
				     _("divide by zero"));
			errcnt++;
		} else
			dat.ival =
				arg0->cnst.datum.ival / arg1->cnst.datum.ival;
		break;

	case Rem:
		if (arg1->cnst.datum.ival == 0) {
			grad_log_loc(GRAD_LOG_ERR, &arg1->loc,
				     _("divide by zero"));
			errcnt++;
		} else
			dat.ival =
				arg0->cnst.datum.ival % arg1->cnst.datum.ival;
		break;

	default:
		grad_insist_fail("Unexpected opcode");
	}
	mtx->type = Constant;
	mtx->cnst.datum = dat;
	mtx_remove(arg0);
	mtx_remove(arg1);
	return 0;
}

MTX *
mtx_branch(int cond, MTX *target)
{
	MTX *nop = mtx_alloc(Nop);
	MTX *mtx = mtx_alloc(Branch);
	mtx_insert(target, nop);
	mtx->branch.cond = cond;
	mtx->branch.dest = nop;
	return mtx;
}

void
mtx_bool(MTX *mtx)
{
	MTX *j_mtx, *p, *p1;

	/* Insert after first operand:
	   popa
	   pusha
	   pusha      ;; Duplicate tos value
	   j?e   L10
	   popa       ;; Pop up the unneded value */

	p = mtx_alloc(Popa);
	mtx_insert(mtx->bin.arg[0], p);
	p1 = mtx_alloc(Pusha);
	mtx_insert(p, p1);
	p = mtx_alloc(Pusha);
	mtx_insert(p1, p);
	j_mtx = mtx_branch(mtx->bin.opcode == Or, mtx);
	mtx_insert(p, j_mtx);
	p1 = mtx_alloc(Popa);
	mtx_insert(j_mtx, p1);
	/* Remove the op matrix
	   Note that the mtx->cond.expr is not correct after this
	   operation, but this does not affect the functionality */
	mtx_remove(mtx);
}

/*
 * Second optimization pass: immediate computations
 */
int
pass2(void)
{
	MTX *mtx, *next;
	int optcnt;
	int errcnt = 0;

	do {
		optcnt = 0;
		mtx = DLIST_FIRST(&mtx_head);
		while (mtx) {
			next = DLIST_NEXT(mtx, link);
			switch (mtx->type) {
			case Unary:
				if (mtx->un.arg->type != Constant)
					break;
				if (pass2_unary(mtx))
					errcnt++;
				else
					optcnt++;
				break;

			case Binary:
				if (mtx->bin.arg[0]->type == Constant
				    && mtx->bin.arg[1]->type == Constant) {
					switch (mtx->bin.datatype) {
					case Integer:
						if (pass2_binary(mtx))
							errcnt++;
						else
							optcnt++;
						break;

					case String:
						/*NO STRING OPS SO FAR */;
						break;

					default:
						grad_insist_fail("Unknown data type");
					}
				} else if (mtx->bin.opcode == And
					   || mtx->bin.opcode == Or) {
					mtx_bool(mtx);
				}
				break;
				/*FIXME: ADD `if (1)'/`if 0' evaluation */
			case Jump:
				if (mtx->jump.dest == DLIST_NEXT(mtx, link))
					mtx_remove(mtx);
				break;

			case Attr:
			case Attr_asgn:
			case Attr_check:
			case Attr_delete:
				/*FIXME: the rw_attr.0 functions should
				  expect an immediate value after the
				  attribute number */
				break;

			default:
				break;
			}
			mtx = next;
		}
	} while (errcnt == 0 && optcnt > 0);
	return errcnt;
}

int
optimize(void)
{
	DEBUG_FUN();
	DEBUG_MTX("on entry to optimize");
	pass1();
	DEBUG_MTX("after first pass");
	if (pass2())
		return -1;
	DEBUG_MTX("after second pass (immediate computations)");
	return 0;
}


/* ****************************************************************************
 * Code generator
 */


static RWCODE *rw_code;         /* Code segment */
static pctr_t rw_pc;            /* PC when compiling the code */
static size_t rw_codesize;      /* Length of code segment */

void
code_check(void)
{
	if (rw_code == NULL) {
		rw_codesize  = 4096;
		rw_code  = grad_ecalloc(rw_codesize, sizeof(rw_code[0]));
	}
}

void
code_init(void)
{
	code_check();
	/* code cell #0 is the default return address */
	rw_c_val(rw_code_value(rw_code[0]),pc) = 0;
	rw_pc = 1;
}

static void
debug_dump_code(void)
{
	FILE    *fp;
	pctr_t  pc;
	int     i;

	if ((fp = debug_open_file()) == NULL)
		return;
	fprintf(fp, "Code size: %zu\n", rw_codesize);
	fprintf(fp, "Code dump:\n");

	pc = 0;
	do {
		fprintf(fp, "%4d:", pc);
		for (i = 0; i < 8 && pc < rw_codesize; i++, pc++)
			fprintf(fp, " %8x",
				rw_c_val(rw_code_value(rw_code[pc]), u_int));
		fprintf(fp, "\n");
	} while (pc < rw_codesize);

	fclose(fp);
}

/*
 * Runtime function prototypes
 */
static void pushn(RWSTYPE n);
static int cpopn(RWSTYPE *np);
static RWSTYPE popn(void);
static void checkpop(int cnt);
static void pushref(char *str, int from, int to);
static RWSTYPE *heap_reserve(int size);
static void pushs(RWSTYPE *sptr, size_t size, int len);
static void pushstr(const char *str, size_t len);

static void pushint(int);
static int popint(void);

static void rw_pushn(void);
static void rw_pushs(void);
static void rw_pushref(void);
static void rw_pushv(void);
static void rw_i2s(void);
static void rw_s2i(void);
static void rw_eq(void);
static void rw_ne(void);
static void rw_lt(void);
static void rw_le(void);
static void rw_gt(void);
static void rw_ge(void);
static void rw_eqs(void);
static void rw_nes(void);
static void rw_lts(void);
static void rw_les(void);
static void rw_gts(void);
static void rw_ges(void);
static void rw_b_xor(void);
static void rw_b_and(void);
static void rw_b_or(void);
static void rw_shl(void);
static void rw_shr(void);
static void rw_add(void);
static void rw_sub(void);
static void rw_mul(void);
static void rw_div(void);
static void rw_rem(void);
static void rw_not(void);
static void rw_neg(void);
static void rw_asgn(void);
static void rw_enter(void);
static void rw_leave(void);
static void rw_match(void);
static void rw_jmp(void);
static void rw_jne(void);
static void rw_je(void);
static void rw_adds(void);
static void rw_adjstk(void);
static void rw_popn(void);
static void rw_pusha(void);
static void rw_popa(void);
static void rw_call(void);
static void rw_builtin(void);
static void rw_attrs(void);
static void rw_attrs0(void);
static void rw_attrn(void);
static void rw_attrn0(void);
static void rw_attrcheck(void);
static void rw_attrcheck0(void);
static void rw_attrasgn(void);
static void rw_attrasgn0(void);
static void rw_attr_delete(void);
static void rw_attr_delete0(void);

INSTR bin_codetab[] = {
	rw_eq,
	rw_ne,
	rw_lt,
	rw_le,
	rw_gt,
	rw_ge,
	rw_b_and,
	rw_b_xor,
	rw_b_or,
	NULL,
	NULL,
	rw_shl,
	rw_shr,
	rw_add,
	rw_sub,
	rw_mul,
	rw_div,
	rw_rem,
};

INSTR bin_string_codetab[] = {
	rw_eqs,
	rw_nes,
	rw_lts,
	rw_les,
	rw_gts,
	rw_ges,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	rw_adds,
	NULL,
	NULL,
	NULL,
	NULL
};

INSTR coerce_tab[Max_datatype][Max_datatype] = {
/*                Undefined  Integer  String */
/* Undefined */ {  NULL,      NULL,    NULL   },
/* Integer */   {  NULL,      NULL,    rw_i2s },
/* String */    {  NULL,      rw_s2i,  NULL   },
};

static void check_codesize(int delta);
static pctr_t code_cell(RWCODE cell);
static pctr_t code_cell(RWCODE cell);
static pctr_t code_instr(INSTR instr);
static pctr_t code_value(RWSTYPE val);
static pctr_t data(int val);
static pctr_t data_str(char *ptr);
static void add_target(NOP_MTX *mtx, pctr_t pc);

void
add_target(NOP_MTX *mtx, pctr_t pc)
{
	MTX *tgt = mtx_alloc(Target);
	DLIST_NEXT(tgt, link) = mtx->tgt;
	mtx->tgt = tgt;
	tgt->tgt.pc = pc;
}

void
fixup_target(NOP_MTX *mtx, pctr_t pc)
{
	MTX   *tgt;

	for (tgt = mtx->tgt; tgt; tgt = DLIST_NEXT(tgt, link))
		rw_c_val(rw_code_value(rw_code[tgt->tgt.pc]), pc) = pc;
	mtx->tgt = NULL;
}

pctr_t
codegen(void)
{
	MTX       *mtx;

	function->entry = rw_pc;
	DLIST_FOREACH(mtx, &mtx_head, link) {
		switch (mtx->type) {
		case Generic:
		case Return:
		default:
			grad_log(GRAD_LOG_CRIT,
				 "INTERNAL ERROR: codegen stumbled accross generic matrix!");
			errcnt++;
			return 0;
		case Nop:
			/* Fix-up the references */
			fixup_target(&mtx->nop, rw_pc);
			mtx->nop.pc = rw_pc;
			break;
		case Stop:
			break;
		case Enter:
			code_instr(rw_enter);
			data(mtx->frame.stacksize);
			break;
		case Leave:
			code_instr(rw_leave);
			break;
		case Constant:
			switch (mtx->cnst.datatype) {
			case Integer:
				code_instr(rw_pushn);
				data(mtx->cnst.datum.ival);
				break;

			case String:
				code_instr(rw_pushs);
				data_str(mtx->cnst.datum.sval.data);
				break;

			default:
				grad_insist_fail("Unknown data type");
			}
			break;
		case Matchref:
			code_instr(rw_pushref);
			data(mtx->ref.num);
			break;
		case Variable:
			/* Variable dereference.
			 */
			code_instr(rw_pushv);
			data(mtx->var.var->offset);
			break;
		case Unary:
			switch (mtx->un.opcode) {
			case Not:
				code_instr(rw_not);
				break;

			case Neg:
				code_instr(rw_neg);
				break;

			default:
				grad_insist_fail("Unexpected opcode");
			}
			break;
		case Binary:
			if (mtx->bin.arg[0]->gen.datatype == String)
				code_instr(bin_string_codetab[mtx->bin.opcode]);
			else
				code_instr(bin_codetab[mtx->bin.opcode]);
			break;
		case Cond:
			/*FIXME: this needs optimization */
			code_instr(rw_jne);
			add_target(&mtx->cond.if_true->nop, rw_pc);
			code_instr(NULL);
			if (mtx->cond.if_false) {
				code_instr(rw_jmp);
				add_target(&mtx->cond.if_false->nop, rw_pc);
				code_instr(NULL);
			}
			break;

		case Asgn:
			code_instr(rw_asgn);
			data(mtx->asgn.lval->offset);
			break;

		case Match:
			code_instr(rw_match);
			code_value((RWSTYPE)mtx->match.rx);
			if (mtx->match.negated)
				code_instr(rw_not);
			break;

		case Coercion:
			code_instr(coerce_tab[mtx->coerce.arg->gen.datatype][mtx->coerce.datatype]);
			break;

		case Jump:
			code_instr(rw_jmp);
			add_target(&mtx->jump.dest->nop, rw_pc);
			code_instr(NULL);
			break;

		case Branch:
			code_instr(mtx->branch.cond ? rw_jne : rw_je);
			add_target(&mtx->branch.dest->nop, rw_pc);
			code_instr(NULL);
			break;

		case Call:
			code_instr(rw_call);
			code_value((RWSTYPE) mtx->call.fun->entry);
			code_instr(rw_adjstk);
			data(mtx->call.nargs);
			break;

		case Builtin:
			code_instr(rw_builtin);
			code_instr(mtx->btin.fun);
			code_instr(rw_adjstk);
			data(mtx->btin.nargs);
			break;

		case Pop:
			code_instr(rw_popn);
			break;

		case Popa:
			code_instr(rw_popa);
			break;

		case Pusha:
			code_instr(rw_pusha);
			break;

		case Attr:
			switch (mtx->attr.datatype) {
			case Integer:
				if (mtx->attr.index)
					code_instr(rw_attrn);
				else
					code_instr(rw_attrn0);
				break;

			case String:
				if (mtx->attr.index)
					code_instr(rw_attrs);
				else
					code_instr(rw_attrs0);
				break;

			default:
				grad_insist_fail("Unknown data type");
			}
			data(mtx->attr.attrno);
			break;

		case Attr_check:
			if (mtx->attr.index)
				code_instr(rw_attrcheck);
			else
				code_instr(rw_attrcheck0);
			data(mtx->attr.attrno);
			break;

		case Attr_asgn:
			if (mtx->attr.index)
				code_instr(rw_attrasgn);
			else
				code_instr(rw_attrasgn0);
			data(mtx->attr.attrno);
			break;

		case Attr_delete:
			if (mtx->attr.index)
				code_instr(rw_attr_delete);
			else
				code_instr(rw_attr_delete0);
			data(mtx->attr.attrno);
			break;
		}
	}

	/*
	 * Second pass: fixup backward references
	 */
	DLIST_FOREACH(mtx, &mtx_head, link) {
		if (mtx->type == Nop)
			fixup_target(&mtx->nop, mtx->nop.pc);
	}

	if (GRAD_DEBUG_LEVEL(25)) {
		FILE *fp = debug_open_file();
		if (fp) {
			fprintf(fp, "entry: %d, size %d\n",
				function->entry, rw_pc - function->entry);
			fclose(fp);
		}
	}

	return function->entry;
}

void
check_codesize(int delta)
{
	if (rw_pc + delta >= rw_codesize) {
		RWCODE *p = grad_ecalloc(rw_codesize + 4096,
					 sizeof(rw_code[0]));
		memcpy(p, rw_code, rw_codesize * sizeof(rw_code[0]));
		free(rw_code);
		rw_code = p;
		rw_codesize += 4096;
	}
}

pctr_t
code_cell(RWCODE cell)
{
	check_codesize(1);
	rw_code[rw_pc] = cell;
	return rw_pc++;
}

pctr_t
code_instr(INSTR instr)
{
	RWCODE c;
	rw_code_instr(c) = instr;
	return code_cell(c);
}

pctr_t
code_value(RWSTYPE val)
{
	RWCODE c;
	rw_code_value(c) = val;
	return code_cell(c);
}

pctr_t
data(int val)
{
	return code_value((RWSTYPE)val);
}

pctr_t
data_str(char *ptr)
{
	int  len   = strlen(ptr) + 1;
	u_int delta = (len + sizeof(rw_code[0])) / sizeof(rw_code[0]);

	check_codesize(delta+1);
	rw_c_val(rw_code_value(rw_code[rw_pc]), u_int) = delta;
	rw_pc++;
	memcpy(rw_code + rw_pc, ptr, len);
	rw_pc += delta;
	return rw_pc;
}


/* ****************************************************************************
 * Regular expressions
 */

COMP_REGEX *
rx_alloc(regex_t *regex, int nmatch)
{
	COMP_REGEX *rx;

	rx = grad_emalloc(sizeof(*rx));
	rx->regex  = *regex;
	rx->nmatch = nmatch;
	DLIST_INSERT_HEAD(&function->rx_head, rx, link);
	return rx;
}

static void
rx_free(COMP_REGEX *rx)
{
	regfree(&rx->regex);
	free(rx);
}

void
rx_head_free(COMP_REGEX_HEAD *rxhead)
{
	DLIST_FREE(rxhead, link, comp_regex, rx_free);
}

COMP_REGEX *
compile_regexp(char *str)
{
	char     *p;
	regex_t  regex;
	int      nmatch;

	int rc = regcomp(&regex, str, regcomp_flags);
	if (rc) {
		char errbuf[512];
		regerror(rc, &regex, errbuf, sizeof(errbuf));
		grad_log_loc(GRAD_LOG_ERR, &locus,
			     _("regexp error: %s"),
			     errbuf);
		return NULL;
	}
	/* count the number of matches */
	nmatch = 0;
	for (p = str; *p; ) {
		if (*p == '\\')
			if (p[1] == '(') {
				nmatch++;
				p += 2;
				continue;
			}
		p++;
	}

	return rx_alloc(&regex, nmatch);
}

void
function_delete(void)
{
	if (function) {
		grad_symtab_delete(rewrite_tab, (grad_symbol_t*)function);
		function_cleanup();
	}
}

void
function_cleanup(void)
{
	function = NULL;
}


/* ****************************************************************************
 * Runtime functions
 */

/*
 * Push a number on stack
 */
void
pushn(RWSTYPE n)
{
	if (mach.st >= mach.ht) {
		/*FIXME: gc();*/
		GRAD_DEBUG(1, "st=%d, ht=%d", mach.st, mach.ht);
		rw_error(_("out of pushdown space"));
	}
	mach.stack[mach.st++] = n;
}

/*
 * Push a string on stack
 */
void
pushs(RWSTYPE *sptr, size_t size, int len)
{
	if (mach.ht - len - 1 <= mach.st) {
		/* Heap overrun: */
		/*gc(); */
		rw_error(_("heap overrun"));
	}

	while (len)
		mach.stack[mach.ht--] = sptr[--len];
	rw_c_val(mach.stack[mach.ht--], size) = size;
	pushn(rw_c_cast(mach.stack + mach.ht + 1, void*));
}

void
pushstr(const char *str, size_t len)
{
	RWSTYPE *p = heap_reserve(sizeof(RWSTYPE) + len + 1);
	char *s = (char*)(p + 1);
	memcpy(s, str, len);
	s[len] = 0;
	rw_c_val(p[0], size) = len;
	pushn(rw_c_cast(p,void*));
}

#define B2RW(s) (s + sizeof(mach.stack[0]) - 1) / sizeof(mach.stack[0])

RWSTYPE *
heap_reserve(int size)
{
	size_t words = B2RW(size);

	if (mach.ht - words <= mach.st) {
		/* Heap overrun: */
		gc();
		if (mach.ht - words <= mach.st)
			rw_error(_("heap overrun"));
	}
	mach.ht -= words;
	return mach.stack + mach.ht--;
}


/* Temporary space functions */
char *
temp_space_create(void)
{
	return (char*)(mach.stack + mach.st);
}

size_t
temp_space_size(void)
{
	return (mach.ht - mach.st)*sizeof(mach.stack[0]);
}

void
temp_space_copy(char **baseptr, char *text, size_t size)
{
	size_t len = (size + sizeof(mach.stack[0])) / sizeof(mach.stack[0]);
	if (*baseptr + len >= (char*)(mach.stack + mach.ht))
		rw_error(_("out of heap space"));
	memcpy(*baseptr, text, size);
	*baseptr += size;
}

RWSTYPE *
temp_space_fix(char *end)
{
	size_t len, size;
	char *base = (char*)(mach.stack + mach.st);

	temp_space_copy(&end, "", 0);
	size = end - base;
	len = B2RW(size);
	mach.ht -= len;
	memmove(mach.stack + mach.ht, base, size);
	rw_c_val(mach.stack[--mach.ht], size) = strlen(base);
	return mach.stack + mach.ht--;
}


/*
 * Pop number from stack and store into NP.
 */
int
cpopn(RWSTYPE *np)
{
	if (mach.st <= 0) {
		rw_error(_("out of popup"));
	}
	*np = mach.stack[--mach.st];
	return 0;
}

/*
 * Pop the number from stack without error checking. checkpop() function
 * should be called before calling this one.
 */
RWSTYPE
popn(void)
{
	return mach.stack[--mach.st];
}

void
mem2string(grad_string_t *p, RWSTYPE *loc)
{
	p->size = rw_c_val(loc[0], size);
	p->data = (char*) (loc + 1);
}

void
poparr(grad_string_t *p)
{
	RWSTYPE v = popn();
	mem2string(p, (RWSTYPE*) rw_c_val(v, ptr));
}

RWSTYPE
tos(void)
{
	return mach.stack[mach.st-1];
}

/*
 * Check if the stack contains at list CNT elements.
 */
void
checkpop(int cnt)
{
	if (mach.st < cnt)
		rw_error(_("out of popup"));
}

/*
 * Push a backreference value on stack.
 * Arguments: str     --    input string
 *            from    --    start of reference in string
 *            to      --    end of reference in string
 */
void
pushref(char *str, int from, int to)
{
	pushstr(str + from, to - from);
}

static void
pushint(int v)
{
	pushn((RWSTYPE)v);
}

static int
popint(void)
{
	RWSTYPE t = popn();
	return rw_c_val(t, int);
}

/*
 * Create a stack frame and enter the function
 */
void
enter(int n)
{
	pushn((RWSTYPE)mach.sb);
	mach.sb = mach.st;
	mach.st += n;
}

/*
 * Destroy the stack frame and leave the function
 */
void
leave(void)
{
	/* Save return value */
	mach.rA = popn();
	/* Restore stack frame */
	mach.st = mach.sb;
	mach.sb = rw_c_val(popn(), int);
	mach.pc = rw_c_val(popn(), pc);
}

RWSTYPE
getarg(int num)
{
	return mach.stack[mach.sb - (STACK_BASE + num)];
}


/* ****************************************************************************
 * Instructions
 */

static int
rw_error(const char *msg)
{
	grad_log(GRAD_LOG_ERR,
		 "%s: %s",
		 _("rewrite runtime error"), msg);
	longjmp(mach.jmp, 1);
	/*NOTREACHED*/
}

static int
rw_error_free(char *msg)
{
	grad_log(GRAD_LOG_ERR,
		 "%s: %s",
		 _("rewrite runtime error"), msg);
	free(msg);
	longjmp(mach.jmp, 1);
	/*NOTREACHED*/
}

void
rw_call(void)
{
	pctr_t  pc = rw_c_val(rw_code_value(rw_code[mach.pc]), pc);
	pushn((RWSTYPE)(mach.pc + 1)); /* save return address */
	mach.pc = pc;
}

void
rw_adjstk(void)
{
	u_int delta = rw_c_val(rw_code_value(rw_code[mach.pc]), u_int);
	mach.pc++;
	mach.st -= delta;
	pushn(mach.rA);   /* Push the return back on stack */
}

void
rw_enter(void)
{
	/*FIXME: runtime checking */
	int n = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	enter(n);
}

void
rw_leave(void)
{
	leave();
}

/*
 * Push a number on stack
 */
void
rw_pushn(void)
{
	RWSTYPE n = rw_code_value(rw_code[mach.pc++]);
	pushn(n);
}

/*
 * Push a reference value on stack
 */
void
rw_pushref(void)
{
	int i = rw_c_val(rw_code_value(rw_code[mach.pc]), int);
	mach.pc++;
	pushref(mach.sA, mach.pmatch[i].rm_so, mach.pmatch[i].rm_eo);
}

/*
 * Push a variable on stack
 */
void
rw_pushv(void)
{
	stkoff_t n = rw_c_val(rw_code_value(rw_code[mach.pc]), off);
	mach.pc++;
	pushn(mach.stack[mach.sb + n]);
}

void
rw_pushs(void)
{
	int   len = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	RWSTYPE *sptr = (RWSTYPE*) (rw_code + mach.pc);

	mach.pc += len;
	pushs(sptr, strlen((char*)sptr), len);
}

/*
 * Assign a value to a variable
 */
void
rw_asgn(void)
{
	stkoff_t off = rw_c_val(rw_code_value(rw_code[mach.pc++]), off);
	RWSTYPE n;

	cpopn(&n);

	mach.stack[mach.sb + off] = n;
	pushn(n);
}

void
assert_request_presence(void)
{
	if (!mach.req)
		rw_error(_("no request supplied"));
}

/* Check if the A/V pair is supplied in the request
 */
void
rw_attrcheck0(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);

	pushn(rw_c_cast(grad_avl_find(AVPLIST(&mach), attr) != NULL, int));
}

void
rw_attrcheck(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	RWSTYPE index;
	int res;

	cpopn(&index);
	res = grad_avl_find_n(AVPLIST(&mach), attr, rw_c_val(index, int)) != NULL;
	pushn((RWSTYPE)res);
}

/*
 * Assign a value to an A/V pair
 */
void
attrasgn_internal(int attr, grad_avp_t *pair, RWSTYPE val)
{
	grad_string_t str;

	assert_request_presence();
	if (!pair) {
		 pair = grad_avp_create(attr);
		 if (!pair)
			rw_error(_("can't create A/V pair"));
		 grad_avl_add_pair(&mach.req->avlist, pair);
	 }

	switch (pair->type) {
	case GRAD_TYPE_STRING:
	case GRAD_TYPE_DATE:
		mem2string(&str, (RWSTYPE*)rw_c_val(val, ptr));
		free(pair->avp_strvalue);
		pair->avp_strvalue = grad_emalloc(str.size+1);
		memcpy(pair->avp_strvalue, str.data, str.size);
		pair->avp_strvalue[str.size] = 0;
		pair->avp_strlength = str.size;
		break;

	case GRAD_TYPE_INTEGER:
	case GRAD_TYPE_IPADDR:
		pair->avp_lvalue = rw_c_val(val, u_int);
		break;
	}

	pushn(val);
}

void
rw_attrasgn0(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	RWSTYPE val;

	cpopn(&val);
	attrasgn_internal(attr, grad_avl_find(AVPLIST(&mach), attr), val);
}

void
rw_attrasgn(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	RWSTYPE val;
	RWSTYPE index;

	cpopn(&val);
	cpopn(&index);
	attrasgn_internal(attr,
			  grad_avl_find_n(AVPLIST(&mach), attr,
					  rw_c_val(index, int)),
			  val);
}

void
rw_attrs0(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	grad_avp_t *pair;

	if ((pair = grad_avl_find(AVPLIST(&mach), attr)) == NULL)
		pushstr("", 0);
	else if (pair->prop & GRAD_AP_ENCRYPT) {
		char string[GRAD_STRING_LENGTH+1];
		int len;
		req_decrypt_password(string, mach.req, pair);
		len = strlen(string);
		pushstr(string, len);
	} else
		pushstr(pair->avp_strvalue, pair->avp_strlength);
}

void
rw_attrn0(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	grad_avp_t *pair;

	if ((pair = grad_avl_find(AVPLIST(&mach), attr)) == NULL)
		pushn(rw_c_cast(0, int));
	else
		pushn(rw_c_cast(pair->avp_lvalue, u_int));
}

void
rw_attrs(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	grad_avp_t *pair;
	RWSTYPE index;

	cpopn(&index);
	pair = grad_avl_find_n(AVPLIST(&mach), attr, rw_c_val(index, int));
	if (pair == NULL)
		pushstr("", 0);
	else
		pushstr(pair->avp_strvalue, pair->avp_strlength);
}

void
rw_attrn(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	grad_avp_t *pair;
	RWSTYPE index;

	cpopn(&index);
	pair = grad_avl_find_n(AVPLIST(&mach), attr, rw_c_val(index, int));
	if (pair == NULL)
		pushn(rw_c_cast(0, int));
	else
		pushn(rw_c_cast(pair->avp_lvalue, u_int));
}

void
rw_attr_delete0(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	grad_avl_delete(&mach.req->avlist, attr);
}

void
rw_attr_delete(void)
{
	int attr = rw_c_val(rw_code_value(rw_code[mach.pc++]), int);
	RWSTYPE index;

	assert_request_presence();
	cpopn(&index);
	grad_avl_delete_n(&mach.req->avlist, attr, rw_c_val(index, int));
}

/*
 * Pop (and discard) a value from stack
 */
void
rw_popn(void)
{
	RWSTYPE n;
	cpopn(&n);
}

/*
 * Pop a value from stack into the accumulator
 */
void
rw_popa(void)
{
	cpopn(&mach.rA);
}

/*
 * Push accumulator on stack
 */
void
rw_pusha(void)
{
	pushn(mach.rA);
}

/*
 * String concatenation
 */
void
rw_adds(void)
{
	grad_string_t s1, s2;
	RWSTYPE *p;
	char *s;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);
	p = heap_reserve(sizeof(RWSTYPE) + s1.size + s2.size + 1);
	s = (char*)(p + 1);
	memcpy(s, s1.data, s1.size);
	s += s1.size;
	memcpy(s, s2.data, s2.size);
	s += s2.size;
	*s = 0;
	rw_c_val(p[0], size) = s1.size + s2.size;
	pushn(rw_c_cast(p, void*));
}

/*
 * Unary negation
 */
void
rw_neg(void)
{
	checkpop(1);
	pushint(-popint());
}

/*
 * Bitwise operations
 */
void
rw_b_and(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 & n2);
}

void
rw_b_or(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 | n2);
}

void
rw_b_xor(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 ^ n2);
}

void
rw_shl(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 << n2);
}

void
rw_shr(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 >> n2);
}

/*
 * Addition
 */
void
rw_add(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1+n2);
}

/*
 * Subtraction
 */
void
rw_sub(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1-n2);
}

/*
 * Multiplication
 */
void
rw_mul(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1*n2);
}

/*
 * Division
 */
void
rw_div(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	if (n2 == 0)
		rw_error(_("division by zero!"));
	pushint(n1/n2);
}

/*
 * Remainder
 */
void
rw_rem(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	if (n2 == 0)
		rw_error(_("division by zero!"));
	pushint(n1%n2);
}


/* Type conversion */
void
rw_i2s(void)
{
	int n = popint();
	char buf[64];

	snprintf(buf, sizeof(buf), "%d", n);
	pushstr(buf, strlen(buf));
}

void
rw_s2i(void)
{
	grad_string_t s;
	mem2string(&s, (RWSTYPE *)rw_c_val(popn(), ptr));
	pushint(strtol(s.data, NULL, 0));
}



void
rw_eq(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 == n2);
}

void
rw_ne(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 != n2);
}

void
rw_lt(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 < n2);
}

void
rw_le(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 <= n2);
}

void
rw_gt(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 > n2);
}

void
rw_ge(void)
{
	int n1, n2;

	checkpop(2);
	n2 = popint();
	n1 = popint();
	pushint(n1 >= n2);
}

void
rw_eqs(void)
{
	grad_string_t s1, s2;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);

	pushint(s1.size == s2.size && memcmp(s1.data, s2.data, s1.size) == 0);
}

void
rw_nes(void)
{
	grad_string_t s1, s2;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);

	pushint(!(s1.size == s2.size && memcmp(s1.data, s2.data, s1.size) == 0));
}

void
rw_lts(void)
{
	grad_string_t s1, s2;
	size_t size;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);
	size = RW_MIN(s1.size, s2.size);
	pushint(memcmp(s1.data, s2.data, size < 0) || s1.size < s2.size);
}

void
rw_les(void)
{
	grad_string_t s1, s2;
	size_t size;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);
	size = RW_MIN(s1.size, s2.size);
	pushint(memcmp(s1.data, s2.data, size <= 0) || s1.size <= s2.size);
}

void
rw_gts(void)
{
	grad_string_t s1, s2;
	size_t size;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);
	size = RW_MIN(s1.size, s2.size);
	pushint(memcmp(s1.data, s2.data, size > 0) || s1.size > s2.size);
}

void
rw_ges(void)
{
	grad_string_t s1, s2;
	size_t size;

	checkpop(2);
	poparr(&s2);
	poparr(&s1);
	size = RW_MIN(s1.size, s2.size);
	pushint(memcmp(s1.data, s2.data, size >= 0) || s1.size >= s2.size);
}

void
rw_not(void)
{
	int n;

	checkpop(1);
	n = popint();
	pushint(!n);
}

static void
need_pmatch(size_t n)
{
	n++;
	if (mach.nmatch < n) {
		free(mach.pmatch);
		mach.nmatch = n;
		mach.pmatch = grad_ecalloc(n, sizeof(mach.pmatch[0]));
	}
}

void
rw_match(void)
{
	COMP_REGEX *rx = rw_c_val(rw_code_value(rw_code[mach.pc++]), rx);
	grad_string_t s;
	int rc;

	poparr(&s);
	need_pmatch(rx->nmatch);
	mach.sA = s.data;

	rc = regexec(&rx->regex, mach.sA,
		     rx->nmatch + 1, mach.pmatch, 0);
	if (rc && GRAD_DEBUG_LEVEL(1)) {
		char errbuf[512];
		regerror(rc, &rx->regex,
			 errbuf, sizeof(errbuf));
		grad_log(GRAD_LOG_DEBUG,
			 _("rewrite regex failure: %s. Input: %s"),
			 errbuf, (char*)rw_c_val(mach.rA, ptr));
	}
	pushint(rc == 0);
}

void
rw_jmp(void)
{
	pctr_t pc = rw_c_val(rw_code_value(rw_code[mach.pc++]), pc);
	mach.pc = pc;
}

void
rw_jne(void)
{
	int n;
	pctr_t pc = rw_c_val(rw_code_value(rw_code[mach.pc++]), pc);

	n = popint();
	if (n != 0)
		mach.pc = pc;
}

void
rw_je(void)
{
	int n;
	pctr_t pc = rw_c_val(rw_code_value(rw_code[mach.pc++]), pc);

	n = popint();
	if (n == 0)
		mach.pc = pc;
}

void
rw_builtin(void)
{
	INSTR fun = rw_code_instr(rw_code[mach.pc++]);
	pushn((RWSTYPE)mach.pc);
	enter(0);
	fun();
	leave();
}

void
run(pctr_t pc)
{
	INSTR ip;

	mach.pc = pc;
	while ((ip = rw_code_instr(rw_code[mach.pc]))) {
		if (mach.pc >= rw_codesize)
			rw_error(_("pc out of range"));
//                (*(rw_code[mach.pc++]))();
		mach.pc++;
		(*ip)();
	}
}


/* ****************************************************************************
 * A placeholder for the garbage collector
 */

void
gc(void)
{
}


/* ****************************************************************************
 * Built-in functions
 */

/*
 * integer length(string s)
 */
static void
bi_length(void)
{
	grad_string_t s;
	mem2string(&s, (RWSTYPE*)rw_c_val(getarg(1), ptr));
	pushn((RWSTYPE)s.size);
}

/*
 * integer index(string s, integer a)
 */
static void
bi_index(void)
{
	grad_string_t s;
	char *p;
	int   c;

	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	c = rw_c_val(getarg(1), int);
	p = memchr(s.data, c, s.size);
	pushint(p ? p - s.data : -1);
}

/*
 * integer rindex(string s, integer a)
 */
static void
bi_rindex(void)
{
	grad_string_t s;
	int i;
	int c;

	mem2string(&s, (RWSTYPE*)rw_c_val(getarg(2), ptr));
	c = rw_c_val(getarg(1), int);
	for (i = s.size - 1; i >= 0; i--)
		if (s.data[i] == c)
			break;
	pushint(i);
}

/*
 * string substr(string s, int start, int length)
 */
static void
bi_substr(void)
{
	grad_string_t src;
	RWSTYPE *p;
	char *dest;
	int   start, length;

	mem2string(&src, (RWSTYPE*)rw_c_val(getarg(3), ptr));
	start  = rw_c_val(getarg(2), int);
	length = rw_c_val(getarg(1), int);
	if (length < 0)
		length = src.size - start;

	p = heap_reserve(sizeof(RWSTYPE) + length + 1);
	dest = (char *)(p + 1);
	if (length > 0)
		memcpy(dest, src.data + start, length);
	dest[length] = 0;
	rw_c_val(p[0], size) = length;
	pushn(rw_c_cast(p, void*));
}

static void
bi_field(void)
{
	grad_string_t str;
	char *p, *endp;
	int fn = rw_c_val(getarg(1), int);
	char *s = "";
	int len = 1;

	mem2string(&str, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	endp = str.data + str.size;
	for (p = str.data; p < endp && fn--; ) {
		/* skip initial whitespace */
		while (p < endp && isspace(*p))
			p++;

		s = p;
		len = 0;
		while (p < endp && !isspace(*p)) {
			p++;
			len++;
		}
	}

	if (p == endp && fn)
		pushstr("", 0);
	else
		pushstr(s, len);
}

static void
bi_logit(void)
{
	grad_string_t msg;
	mem2string(&msg, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	grad_log(GRAD_LOG_INFO, "%s", msg.data);
	pushint(0);
}

static void
bi_htonl(void)
{
	uint32_t val = rw_c_val(getarg(1), u_int);
	pushn(rw_c_cast(htonl(val), u_int));
}

static void
bi_ntohl(void)
{
	uint32_t val = rw_c_val(getarg(1), u_int);
	pushn(rw_c_cast(ntohl(val), u_int));
}

static void
bi_htons(void)
{
	uint16_t val = rw_c_val(getarg(1), u_int); /* FIXME: range checking */
	pushn(rw_c_cast(htons(val), u_int));
}

static void
bi_ntohs(void)
{
	uint16_t val = rw_c_val(getarg(1), u_int); /* FIXME: range checking */
	pushn(rw_c_cast(ntohs(val & 0xffff), u_int));
}

static void
bi_inet_ntoa(void)
{
	char buffer[GRAD_IPV4_STRING_LENGTH];
	char *s = grad_ip_iptostr(rw_c_val(getarg(1), u_int), buffer);
	pushstr(s, strlen(s));
}

static void
bi_inet_aton(void)
{
	grad_string_t s;
	mem2string(&s, (RWSTYPE*)rw_c_val(getarg(1), ptr));
	/* Note: inet_aton is not always present. See lib/iputils.c */
	pushn(rw_c_cast(grad_ip_strtoip(s.data), u_int));
}

static void
bi_tolower(void)
{
	grad_string_t src;
	grad_string_t dest;
	int i;

	mem2string(&src, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	pushstr(src.data, src.size);
	mem2string(&dest, (RWSTYPE*) rw_c_val(tos(), ptr));
	for (i = 0; i < dest.size; i++)
		dest.data[i] = tolower(dest.data[i]);
}

static void
bi_toupper(void)
{
	grad_string_t src;
	grad_string_t dest;
	int i;

	mem2string(&src, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	pushstr(src.data, src.size);
	mem2string(&dest, (RWSTYPE*) rw_c_val(tos(), ptr));
	for (i = 0; i < dest.size; i++)
		dest.data[i] = toupper(dest.data[i]);
}

static void
bi_request_code_string(void)
{
	int code = rw_c_val(getarg(1), int);
	const char *s = grad_request_code_to_name(code);
	pushstr(s, strlen(s));
}

static void
bi_request_source_ip(void)
{
	assert_request_presence();
	pushn(rw_c_cast(mach.req->ipaddr, u_int));
}

static void
bi_request_source_port(void)
{
	assert_request_presence();
	pushn(rw_c_cast(mach.req->udp_port, u_int));
}

static void
bi_request_id(void)
{
	assert_request_presence();
	pushn(rw_c_cast(mach.req->id, u_int));
}

static void
bi_request_code(void)
{
	assert_request_presence();
	pushn(rw_c_cast(mach.req->code, u_int));
}

static void
bi_nas_name(void)
{
	grad_nas_t *nas;
	uint32_t ip = (uint32_t) rw_c_val(getarg(1), u_int);

	if ((nas = grad_nas_lookup_ip(ip)) != NULL) {
		char *s = nas->shortname[0] ? nas->shortname : nas->longname;
		pushstr(s, strlen(s));
	} else {
		char nasname[GRAD_MAX_LONGNAME];

		grad_ip_gethostname(ip, nasname, sizeof(nasname));
		pushstr(nasname, strlen(nasname));
	}
}

static void
bi_nas_short_name(void)
{
	grad_nas_t *nas;
	uint32_t ip = (uint32_t) rw_c_val(getarg(1), u_int);

	if ((nas = grad_nas_lookup_ip(ip)) && nas->shortname[0]) {
		pushstr(nas->shortname, strlen(nas->shortname));
	} else {
		char nasname[GRAD_MAX_LONGNAME];

		grad_ip_gethostname(ip, nasname, sizeof(nasname));
		pushstr(nasname, strlen(nasname));
	}
}

static void
bi_nas_full_name(void)
{
	grad_nas_t *nas;
	uint32_t ip = (uint32_t) rw_c_val(getarg(1), u_int);

	if ((nas = grad_nas_lookup_ip(ip)) != NULL) {
		pushstr(nas->longname, strlen(nas->longname));
	} else {
		char nasname[GRAD_MAX_LONGNAME];

		grad_ip_gethostname(ip, nasname, sizeof(nasname));
		pushstr(nasname, strlen(nasname));
	}
}

static void
bi_gethostbyaddr(void)
{
	uint32_t ip = (uint32_t) rw_c_val(getarg(1), u_int);
	char nasname[GRAD_MAX_LONGNAME];

	grad_ip_gethostname(ip, nasname, sizeof(nasname));
	pushstr(nasname, strlen(nasname));
}

static void
bi_gethostbyname(void)
{
	grad_string_t s;
	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	pushn(rw_c_cast(grad_ip_gethostaddr(s.data), u_int));
}

static void
bi_time(void)
{
	pushn(rw_c_cast(time(NULL), u_int));
}

static void
bi_strftime(void)
{
	struct tm *tm;
	char *base;
	time_t t = (time_t) rw_c_val(getarg(1), u_int);
	grad_string_t fmt;
	size_t n;

	mem2string(&fmt, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	tm = localtime(&t);
	base = temp_space_create();
	n = strftime(base, temp_space_size(), fmt.data, tm);
	pushstr(base, n);
}

static void
rw_regerror(const char *prefix, regex_t *rx, int rc)
{
	size_t sz = regerror(rc, rx, NULL, 0);
	char *errbuf = malloc(sz + strlen (prefix) + 1);
	if (!errbuf)
		rw_error(prefix);
	else {
		strcpy(errbuf, prefix);
		regerror(rc, rx, errbuf + strlen(prefix), sz);
		rw_error_free(errbuf);
	}
}

enum subst_segment_type {
	subst_text,       /* pure text */
	subst_ref,        /* back reference (\NN) */
	subst_match       /* substitute whole match (&) */
};

struct subst_segment {
	enum subst_segment_type type;
	SLIST_ENTRY(subst_segment) link;
	union {
		struct {
			char *ptr;
			size_t len;
		} text;      /* type == subst_text */
		size_t ref;  /* type == subst_ref */
	} v;
};

typedef SLIST_HEAD(,subst_segment) SUBST_HEAD;

static void
add_text_segment(SUBST_HEAD *head, char *ptr, char *end)
{
	struct subst_segment *seg;
	if (ptr >= end)
		return;
	seg = grad_emalloc(sizeof(*seg));
	seg->type = subst_text;
	seg->v.text.ptr = ptr;
	seg->v.text.len = end - ptr;
	SLIST_PUSH(head, seg, link);
}

static void
add_match_segment(SUBST_HEAD *head)
{
	struct subst_segment *seg = grad_emalloc(sizeof(*seg));
	seg->type = subst_match;
	SLIST_PUSH(head, seg, link);
}

static void
add_ref_segment(SUBST_HEAD *head, size_t ref)
{
	struct subst_segment *seg = grad_emalloc(sizeof(*seg));
	seg->type = subst_ref;
	seg->v.ref = ref;
	SLIST_PUSH(head, seg, link);
}

void
subst_create(char *text, SUBST_HEAD *head)
{
	char *p;

	SLIST_INIT(head);
	p = text;
	while (*p) {
		if (*p == '\\' && p[1]) {
			if (p[1] == '&') {
				add_text_segment(head, text, p);
				text = ++p;
				p++;
			} else if (p[1] == '\\') {
				add_text_segment(head, text, p+1);
				p += 2;
				text = p;
			} else if (isdigit(p[1])) {
				size_t ref;
				char *q;

				add_text_segment(head, text, p);
				ref = strtoul(p+1, &q, 10);
				add_ref_segment(head, ref);
				text = p = q;
			} else {
				add_text_segment(head, text, p);
				text = ++p;
			}
		} else if (*p == '&') {
			add_text_segment(head, text, p);
			add_match_segment(head);
			text = ++p;
		} else
			p++;
	}
	add_text_segment(head, text, p);
}

void
subst_destroy(SUBST_HEAD *head)
{
	SLIST_FREE(head, link, subst_segment, free);
}

void
subst_run(SUBST_HEAD *head, size_t nsub, char **baseptr, char *arg)
{
	struct subst_segment *seg;

	SLIST_FOREACH(seg, head, link) {
		switch (seg->type) {
		case subst_text:
			temp_space_copy(baseptr,
					seg->v.text.ptr, seg->v.text.len);
			break;

		case subst_ref:
			if (seg->v.ref >= nsub)
				rw_error(_("Invalid backreference"));
			temp_space_copy(baseptr,
					arg + mach.pmatch[seg->v.ref].rm_so,
					mach.pmatch[seg->v.ref].rm_eo -
					  mach.pmatch[seg->v.ref].rm_so);
			break;

		case subst_match:
			temp_space_copy(baseptr,
					arg + mach.pmatch[0].rm_so,
					mach.pmatch[0].rm_eo -
					  mach.pmatch[0].rm_so);
		}
	}
}

static void
bi_gsub(void)
{
	grad_string_t re_str;
	grad_string_t repl;
	grad_string_t arg;
	char *p;
	char *base;
	regex_t rx;
	SUBST_HEAD subst;
	int rc;

	mem2string(&re_str, (RWSTYPE*) rw_c_val(getarg(3), ptr));
	mem2string(&repl, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	mem2string(&arg, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	p = arg.data;

	rc = regcomp(&rx, re_str.data, regcomp_flags);
	if (rc)
		rw_regerror(_("regexp compile error: "), &rx, rc);

	need_pmatch(rx.re_nsub);

	subst_create(repl.data, &subst);

	base = temp_space_create();
	while (*p
	       && regexec(&rx, p, rx.re_nsub + 1, mach.pmatch, 0) == 0) {
		temp_space_copy(&base, p, mach.pmatch[0].rm_so);
		subst_run(&subst, rx.re_nsub + 1, &base, p);
		p += mach.pmatch[0].rm_eo;
		if (mach.pmatch[0].rm_eo == 0)
			p++;
	}
	temp_space_copy(&base, p, strlen(p) + 1);
	subst_destroy(&subst);
	regfree(&rx);

	pushn(rw_c_cast(temp_space_fix(base), void *));
}

static void
bi_sub(void)
{
	grad_string_t re_str;
	grad_string_t repl;
	grad_string_t arg;
	char *p;
	char *base;
	regex_t rx;
	SUBST_HEAD subst;
	int rc;

	mem2string(&re_str, (RWSTYPE*) rw_c_val(getarg(3), ptr));
	mem2string(&repl, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	mem2string(&arg, (RWSTYPE*) rw_c_val(getarg(1), ptr));

	rc = regcomp(&rx, re_str.data, regcomp_flags);
	if (rc)
		rw_regerror(_("regexp compile error: "), &rx, rc);

	need_pmatch(rx.re_nsub);

	subst_create(repl.data, &subst);

	base = temp_space_create();
	p = arg.data;
	if (regexec(&rx, p, rx.re_nsub + 1, mach.pmatch, 0) == 0) {
		temp_space_copy(&base, p, mach.pmatch[0].rm_so);
		subst_run(&subst, rx.re_nsub + 1, &base, p);
		p += mach.pmatch[0].rm_eo;
	}
	temp_space_copy(&base, p, strlen(p) + 1);
	subst_destroy(&subst);
	regfree(&rx);

	pushn(rw_c_cast(temp_space_fix(base), void*));
}

#define ISPRINT(c) (((unsigned char)c) < 128 && (isalnum(c) || c == '-'))

static void
bi_qprn(void)
{
	grad_string_t arg;
	char *p, *s, *end;
	size_t count;
	RWSTYPE *sp;

	mem2string(&arg, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	end = arg.data + arg.size;
	for (count = 0, p = arg.data; p < end; p++)
		if (!ISPRINT(*p))
			count++;

	/* Each encoded character takes 3 bytes. */
	sp = heap_reserve(sizeof(RWSTYPE) + arg.size + 2*count + 1);
	rw_c_val(sp[0], size) = arg.size + 2*count;
	pushn(rw_c_cast(sp, void*));

	for (p = (char*)(sp + 1), s = arg.data; s < end; s++) {
		if (ISPRINT(*s))
			*p++ = *s;
		else {
			char buf[3];
			snprintf(buf, sizeof buf, "%02X", *(unsigned char*)s);
			*p++ = '%';
			*p++ = buf[0];
			*p++ = buf[1];
		}
	}
	*p = 0;
}

static void
bi_quote_string(void)
{
	int quote;
	grad_string_t arg;
	RWSTYPE *sp;
	char *p;
	size_t size;

	mem2string(&arg, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	size = grad_argcv_quoted_length_n(arg.data, arg.size, &quote);
	sp = heap_reserve(sizeof(RWSTYPE) + size + 1);
	rw_c_val(sp[0], size) = size;
	pushn(rw_c_cast(sp, void*));
	p = (char*)(sp + 1);
	grad_argcv_quote_copy_n(p, arg.data, arg.size);
}

static void
bi_unquote_string(void)
{
	grad_string_t arg;
	RWSTYPE *sp;
	char *p;

	mem2string(&arg, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	sp = heap_reserve(sizeof(RWSTYPE) +  arg.size + 1);
	p = (char*)(sp + 1);
	grad_argcv_unquote_copy(p, arg.data, arg.size);
	rw_c_val(sp[0], u_int) = strlen(p);
	pushn(rw_c_cast(sp, void*));
}

static void
bi_textdomain(void)
{
	grad_string_t s;
	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	pushstr(default_gettext_domain, strlen (default_gettext_domain));
	grad_string_replace(&default_gettext_domain, s.data);
}

static void
bi_gettext(void)
{
	grad_string_t s;
	const char *p;

	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	p = dgettext(default_gettext_domain, s.data);
	pushstr(p, strlen(p));
}

static void
bi_dgettext(void)
{
	grad_string_t domain;
	grad_string_t text;
	const char *p;

	mem2string(&domain, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	mem2string(&text, (RWSTYPE*) rw_c_val(getarg(1), ptr));
	p = dgettext(domain.data, text.data);
	pushstr(p, strlen(p));
}


static void
bi_ngettext(void)
{
	grad_string_t s;
	grad_string_t pl;
	unsigned long n;
	const char *p;

	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(3), ptr));
	mem2string(&pl, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	n = (unsigned long) rw_c_val(getarg(1), u_int);
	p = dngettext(default_gettext_domain,
		      s.data,
		      pl.data,
		      n);
	pushstr(p, strlen(p));
}

static void
bi_dngettext(void)
{
	grad_string_t domain;
	grad_string_t s;
	grad_string_t pl;
	unsigned long n;
	const char *p;

	mem2string(&domain, (RWSTYPE*) rw_c_val(getarg(4), ptr));
	mem2string(&s, (RWSTYPE*) rw_c_val(getarg(3), ptr));
	mem2string(&pl, (RWSTYPE*) rw_c_val(getarg(2), ptr));
	n = (unsigned long) rw_c_val(getarg(1), u_int);

	p = dngettext(domain.data, s.data, pl.data, n);
	pushstr(p, strlen(p));
}

static builtin_t builtin[] = {
	{ bi_length,  "length", Integer, "s" },
	{ bi_index,   "index",  Integer, "si" },
	{ bi_rindex,  "rindex", Integer, "si" },
	{ bi_substr,  "substr", String,  "sii" },
	{ bi_logit,   "logit",  Integer, "s" },
	{ bi_field,   "field",  String,  "si" },
	{ bi_ntohl, "ntohl", Integer, "i" },
	{ bi_htonl, "htonl", Integer, "i" },
	{ bi_ntohs, "ntohs", Integer, "i" },
	{ bi_htons, "htons", Integer, "i" },
	{ bi_inet_ntoa, "inet_ntoa", String, "i" },
	{ bi_inet_aton, "inet_aton", Integer, "s" },
	{ bi_sub, "sub", String, "sss" },
	{ bi_gsub, "gsub", String, "sss" },
	{ bi_qprn, "qprn", String, "s" },
	{ bi_tolower, "tolower", String, "s" },
	{ bi_toupper, "toupper", String, "s" },
	{ bi_unquote_string, "unquote_string", String, "s" },
	{ bi_quote_string, "quote_string", String, "s" },
	{ bi_request_code_string, "request_code_string", String, "i" },
	/* i18n support */
	{ bi_gettext, "gettext", String, "s" },
	{ bi_gettext, "_", String, "s" },
	{ bi_dgettext, "dgettext", String, "ss" },
	{ bi_ngettext, "ngettext", String, "ssi" },
	{ bi_dngettext, "dngettext", String, "sssi" },
	{ bi_textdomain, "textdomain", String, "s" },
	/* Request internals */
	{ bi_request_source_ip,   "request_source_ip", Integer, "" },
	{ bi_request_source_port, "request_source_port", Integer, "" },
	{ bi_request_id, "request_id", Integer, "" },
	{ bi_request_code, "request_code", Integer, "" },
	/* Radius database */
	{ bi_nas_name, "nas_name", String, "i" },
	{ bi_nas_short_name, "nas_short_name", String, "i" },
	{ bi_nas_full_name, "nas_full_name", String, "i" },
	/* DNS lookups */
	{ bi_gethostbyaddr, "gethostbyaddr", Integer, "s" },
	{ bi_gethostbyname, "gethostbyname", String, "i" },
	/* Time functions */
	{ bi_time, "time", Integer, "" },
	{ bi_strftime, "strftime", String, "si" },
	{ NULL }
};

builtin_t *
builtin_lookup(char *name)
{
	int i;

	for (i = 0; builtin[i].handler; i++)
		if (strcmp(name, builtin[i].name) == 0)
			return &builtin[i];
	return NULL;
}


/* ****************************************************************************
 * Function registering/unregistering
 */

void
function_free(void *p)
{
	FUNCTION *f = p;
	PARAMETER *parm, *next;

	rx_head_free(&f->rx_head);
	parm = f->parm;
	while (parm) {
		next = parm->next;
		free(parm);
		parm = next;
	}
}

FUNCTION *
function_install(FUNCTION *fun)
{
	FUNCTION *fp;

	if ((fp = (FUNCTION *)grad_sym_lookup(rewrite_tab, fun->name))) {
		grad_log_loc(GRAD_LOG_ERR, &fun->loc,
			     _("redefinition of function %s"));
		grad_log_loc(GRAD_LOG_ERR, &fp->loc,
			     _("previously defined here"));
		errcnt++;
		return fp;
	}
	fp = (FUNCTION*)grad_sym_install(rewrite_tab, fun->name);

	fp->rettype = fun->rettype;
	fp->entry   = fun->entry;
	fp->rx_head = fun->rx_head;
	fp->nparm   = fun->nparm;
	fp->parm    = fun->parm;
	fp->stack_alloc = fun->stack_alloc;
	fp->loc     = fun->loc;
	return fp;
}


/* ****************************************************************************
 * Runtime functions
 */

static char pair_print_prefix[] = "    ";

static void
rw_mach_init(void)
{
	memset(&mach, 0, sizeof(mach));

	if (!runtime_stack)
		runtime_stack = grad_ecalloc(rewrite_stack_size,
					     sizeof(runtime_stack[0]));

	mach.stack = runtime_stack;
	mach.st = 0;                      /* Stack top */
	mach.ht = rewrite_stack_size - 1; /* Heap top */

	grad_string_replace(&default_gettext_domain, PACKAGE);
}

static void
rw_mach_destroy(void)
{
	free(mach.pmatch);
	mach.pmatch = NULL;
}

FUNCTION *
rewrite_check_function(const char *name, grad_data_type_t rettype, char *typestr)
{
	int i;
	PARAMETER *p;

	FUNCTION *fun = (FUNCTION*) grad_sym_lookup(rewrite_tab, name);
	if (!fun) {
		grad_log(GRAD_LOG_ERR, _("function %s not defined"), name);
		return NULL;
	}
	if (fun->rettype != rettype) {
		grad_log(GRAD_LOG_ERR, _("function %s returns wrong data type"), name);
		return NULL;
	}

	for (i = 0, p = fun->parm; i < fun->nparm; i++, p = p->next) {
		switch (typestr[i]) {
		case 0:
			grad_log(GRAD_LOG_ERR,
				 _("function %s takes too many arguments"),
				 name);
			return NULL;

		case 'i':
			if (p->datatype != Integer) {
				grad_log(GRAD_LOG_ERR,
					 _("function %s: argument %d must be integer"),
					 name, i+1);
				return NULL;
			}
			break;

		case 's':
			if (p->datatype != String) {
				grad_log(GRAD_LOG_ERR,
					 _("function %s: argument %d must be string"),
					 name, i+1);
				return NULL;
			}
			break;

		default:
			grad_insist_fail("bad datatype");
		}
	}

	if (typestr[i]) {
		grad_log(GRAD_LOG_ERR,
			 _("function %s takes too few arguments"),
			 name);
		return NULL;
	}

	return fun;
}

int
run_init(pctr_t pc, grad_request_t *request)
{
	FILE *fp;

	rw_mach_init();
	if (setjmp(mach.jmp)) {
		rw_mach_destroy();
		return -1;
	}

	mach.req = request;
	if (GRAD_DEBUG_LEVEL(2)) {
		if ((fp = debug_open_file()) != NULL) {
			fprintf(fp, "Before rewriting:\n");
			grad_avl_fprint(fp, pair_print_prefix, 1,
					AVPLIST(&mach));
			fclose(fp);
		}
	}

	/* Imitate a function call */
	pushn(rw_c_cast(0, pctr_t)); /* Return address */
	run(pc);                     /* call function */
	if (GRAD_DEBUG_LEVEL(2)) {
		if ((fp = debug_open_file()) != NULL) {
			fprintf(fp, "After rewriting\n");
			grad_avl_fprint(fp, pair_print_prefix, 1,
					AVPLIST(&mach));
			fclose(fp);
		}
	}
	rw_mach_destroy();
	return 0;
}

static void
return_value(grad_value_t *val)
{
	switch (val->type) {
	case Integer:
		val->datum.ival = rw_c_val(mach.rA, u_int);
		break;

	case String:
		mem2string(&val->datum.sval, (RWSTYPE*) rw_c_val(mach.rA, ptr));
		val->datum.sval.data = grad_ebstrndup(val->datum.sval.data,
						      val->datum.sval.size);
		break;

	default:
		abort();
	}
}

static int
evaluate(pctr_t pc, grad_request_t *req, grad_value_t *val)
{
	if (run_init(pc, req))
		return -1;
	if (val)
		return_value(val);
	return 0;
}

int
rewrite_invoke(grad_data_type_t rettype, grad_value_t *val,
	       const char *name,
	       grad_request_t *request, char *typestr, ...)
{
	FILE *fp;
	va_list ap;
	FUNCTION *fun;
	int nargs;
	char *s;

	fun = rewrite_check_function(name, rettype, typestr);
	if (!fun)
		return -1;

	rw_mach_init();
	if (setjmp(mach.jmp)) {
		rw_mach_destroy();
		return -1;
	}

	mach.req = request;
	if (GRAD_DEBUG_LEVEL(2)) {
		if ((fp = debug_open_file()) != NULL) {
			fprintf(fp, "Before rewriting:\n");
			grad_avl_fprint(fp, pair_print_prefix, 1,
					AVPLIST(&mach));
			fclose(fp);
		}
	}

	/* Pass arguments */
	nargs = 0;

	va_start(ap, typestr);
	while (*typestr) {
		nargs++;
		switch (*typestr++) {
		case 'i':
			pushint(va_arg(ap, int));
			break;
		case 's':
			s = va_arg(ap, char*);
			pushstr(s, strlen(s));
			break;
		default:
			grad_insist_fail("bad datatype");
		}
	}
	va_end(ap);

	/* Imitate a function call */
	pushn(rw_c_cast(0, pctr_t));   /* Return address */
	run(fun->entry);               /* call function */
	if (GRAD_DEBUG_LEVEL(2)) {
		if ((fp = debug_open_file()) != NULL) {
			fprintf(fp, "After rewriting\n");
			grad_avl_fprint(fp, pair_print_prefix, 1,
					AVPLIST(&mach));
			fclose(fp);
		}
	}
	val->type = fun->rettype;
	return_value(val);
	rw_mach_destroy();
	return 0;
}

char *
rewrite_compile(char *expr)
{
	int rc;
	FUNCTION *fun;
	char *name = grad_emalloc(strlen(expr) + 3);

	sprintf(name, "$%s$", expr);
	fun = (FUNCTION*) grad_sym_lookup(rewrite_tab, name);
	if (!fun) {
		rc = parse_rewrite_string(expr);
		if (rc) {
			free(name);
			return NULL;
		}
		function->name = name;
		function_install(function);
	}
	return name;
}

int
rewrite_interpret(char const *expr, grad_request_t *req, grad_value_t *val)
{
	pctr_t save_pc = rw_pc;
	int rc;

	rc = parse_rewrite_string(expr);
	rw_pc = save_pc;
	if (rc)
		return rc;

	val->type = return_type;
	if (return_type == Undefined)
		return -1;

	return evaluate(rw_pc, req, val);
}

int
rewrite_eval(char *symname, grad_request_t *req, grad_value_t *val)
{
	FUNCTION *fun;

	fun = (FUNCTION*) grad_sym_lookup(rewrite_tab, symname);
	if (!fun)
		return -1;

	if (fun->nparm) {
		grad_log(GRAD_LOG_ERR,
			 ngettext("function %s() requires %d parameter",
				  "function %s() requires %d parameters",
				  fun->nparm),
			 fun->name, fun->nparm);
		return -1;
	}

	if (val)
		val->type = fun->rettype;
	return evaluate(fun->entry, req, val);
}


/* ****************************************************************************
 * Configuration
 */

static grad_list_t *source_list;        /* List of loaded source files */
static grad_list_t *rewrite_load_path;  /* Load path list */

/* Add a path to load path */
static void
rewrite_add_load_path(const char *str)
{
	if (!rewrite_load_path)
		rewrite_load_path = grad_list_create();
	grad_list_append(rewrite_load_path, grad_estrdup(str));
}

void
register_source_name(char *path)
{
	if (!source_list)
		source_list = grad_list_create();
	grad_list_append(source_list, path);
}

struct load_data {
	int rc;
	char *name;
};

/* Try to load a source file.
   ITEM is a directory name, DATA is struct load_data.
   Return 1 if the file was found (no matter was it loaded or not) */
static int
try_load(void *item, void *data)
{
	int rc = 0;
	struct load_data *lp = data;
	char *path = grad_mkfilename((char*)item, lp->name);

	lp->rc = parse_rewrite(path);
	if (lp->rc >= 0) {
		register_source_name(path);
		rc = 1;
	} else
		free(path);
	return rc;
}

/* Load the given rewrite module. */
int
rewrite_load_module(char *name)
{
	int rc;
	if (name[0] == '/') {
		register_source_name(grad_estrdup(name));
		rc = parse_rewrite(name);
	} else {
		struct load_data ld;
		ld.rc = 1;
		ld.name = name;
		grad_list_iterate(rewrite_load_path, try_load, &ld);
		rc = ld.rc;
	}
	return rc;
}

static int
free_path(void *item, void *data ARG_UNUSED)
{
	free(item);
	return 0;
}

static grad_list_t *source_candidate_list; /* List of modules that are to
					      be loaded */

int
rewrite_stmt_term(int finish, void *block_data, void *handler_data)
{
	if (!finish) {
		grad_symtab_clear(rewrite_tab);

		yydebug = GRAD_DEBUG_LEVEL(50);
		grad_list_destroy(&source_list, free_path, NULL);
		grad_list_destroy(&rewrite_load_path, free_path, NULL);
		rewrite_add_load_path(grad_config_dir);
		rewrite_add_load_path(RADIUS_DATADIR "/rewrite");

		free(runtime_stack);
		runtime_stack = NULL;
	}
	return 0;
}

static int
rewrite_cfg_add_load_path(int argc, cfg_value_t *argv,
			  void *block_data, void *handler_data)
{
	if (argc > 2) {
		cfg_argc_error(0);
		return 0;
	}

	if (argv[1].type != CFG_STRING) {
		cfg_type_error(CFG_STRING);
		return 0;
	}

	rewrite_add_load_path(argv[1].v.string);
	return 0;
}

static int
rewrite_cfg_load(int argc, cfg_value_t *argv,
		 void *block_data, void *handler_data)
{
	if (argc > 2) {
		cfg_argc_error(0);
		return 0;
	}

	if (argv[1].type != CFG_STRING) {
		cfg_type_error(CFG_STRING);
		return 0;
	}

	grad_list_append(source_candidate_list, grad_estrdup(argv[1].v.string));
	return 0;
}

/* Configuration hooks and initialization */

static void
rewrite_before_config_hook(void *a ARG_UNUSED, void *b ARG_UNUSED)
{
	grad_list_destroy(&source_candidate_list, free_path, NULL);
	source_candidate_list = grad_list_create();
	code_init();
}

static int
_load_module(void *item, void *data ARG_UNUSED)
{
	if (rewrite_load_module(item) == -2)
		grad_log(GRAD_LOG_ERR, _("file not found: %s"), item);
	return 0;
}

void
rewrite_load_all(void)
{
	if (!source_candidate_list)
		return;

	/* For compatibility with previous versions load the
	   file $grad_config_dir/rewrite, if no explicit "load" statements
	   were given */
	if (grad_list_count(source_candidate_list) == 0)
		rewrite_load_module("rewrite");

	grad_list_iterate(source_candidate_list, _load_module, NULL);
	if (GRAD_DEBUG_LEVEL(100))
		debug_dump_code();
}

void
rewrite_init(void)
{
	rewrite_tab = grad_symtab_create(sizeof(FUNCTION), function_free);
	radiusd_set_preconfig_hook(rewrite_before_config_hook, NULL, 0);
}


struct cfg_stmt rewrite_stmt[] = {
	{ "stack-size", CS_STMT, NULL, cfg_get_number, &rewrite_stack_size,
	  NULL, NULL },
	{ "load-path", CS_STMT, NULL, rewrite_cfg_add_load_path, NULL, NULL, NULL },
	{ "load", CS_STMT, NULL, rewrite_cfg_load, NULL, NULL, NULL },
	{ NULL, }
};

size_t
rewrite_get_stack_size(void)
{
	return rewrite_stack_size;
}

void
rewrite_set_stack_size(size_t s)
{
	if (s == rewrite_stack_size)
		return;
	rewrite_stack_size = s;
	free(runtime_stack);
	runtime_stack = NULL;
}

void
grad_value_free(grad_value_t *val)
{
	if (val->type == String)
		free(val->datum.sval.data);
}


/* ****************************************************************************
 * Guile interface
 */
#ifdef USE_SERVER_GUILE

SCM
radscm_datum_to_scm(grad_value_t *val)
{
	switch (val->type) {
	case Integer:
		return scm_from_long(val->datum.ival);

	case String:
		/* FIXME! */
		return scm_from_locale_string(val->datum.sval.data);

	default:
		grad_insist_fail("Unknown data type");
	}
	return SCM_UNSPECIFIED;
}

int
radscm_scm_to_ival(SCM cell, int *val)
{
	if (scm_is_number(cell))
		*val = scm_to_int(cell);
	else if (SCM_CHARP(cell))
		*val = SCM_CHAR(cell);
	else if (scm_is_bool(cell))
		*val = scm_to_bool(cell);
	else if (scm_is_null(cell))
		*val = 0;
	else if (scm_is_string(cell)) {
		char *p, *str = scm_to_locale_string(cell);
		long x;

		errno = 0;
		x = strtol(str, &p, 0);
		free(str);
		if (errno || *p)
			return -1;
		if (x < INT_MIN || x > INT_MAX)
			return -1;

		*val = x;
	} else
		return -1;
	return 0;
}

SCM
radscm_rewrite_execute(const char *func_name, SCM ARGS)
{
	char *name;
	FUNCTION *fun;
	PARAMETER *parm;
	int nargs;
	int n, rc;
	grad_value_t value;
	SCM cell;
	SCM FNAME;
	SCM retval;

	FNAME = SCM_CAR(ARGS);
	ARGS  = SCM_CDR(ARGS);
	SCM_ASSERT(scm_is_string(FNAME), FNAME, SCM_ARG1, func_name);

	name = scm_to_locale_string(FNAME);
	fun = (FUNCTION*) grad_sym_lookup(rewrite_tab, name);
	free(name);
	if (!fun)
		scm_misc_error(func_name,
			       _("function ~S not defined"),
			       scm_list_1(FNAME));

	rw_mach_init();

	/* Pass arguments */
	nargs = 0;
	parm = fun->parm;

	for (cell = ARGS; !scm_is_null(cell);
	     cell = SCM_CDR(cell), parm = parm->next) {
		SCM car = SCM_CAR(cell);

		if (++nargs > fun->nparm) {
			rw_code_unlock();
			scm_misc_error(func_name,
				       _("too many arguments for ~S"),
				       scm_list_1(FNAME));
		}

		switch (parm->datatype) {
		case Integer:
			rc = radscm_scm_to_ival(car, &n);
			if (!rc)
				pushint(n);
			break;

		case String:
			if (scm_is_string(car)) {
				char *p = scm_to_locale_string(car);
				pushstr(p, strlen(p));
				free(p);
				rc = 0;
			} else
				rc = 1;
			break;

		default:
			grad_insist_fail("Unknown data type");
		}

		if (rc) {
			rw_mach_destroy();
			scm_misc_error(func_name,
				       _("type mismatch in argument ~S(~S) in call to ~S"),
				       scm_list_3(scm_from_int(nargs),
						  car,
						  FNAME));
		}
	}

	if (fun->nparm != nargs) {
		rw_mach_destroy();
		scm_misc_error(func_name,
			       _("too few arguments for ~S"),
			       scm_list_1(FNAME));
	}

	/* Imitate a function call */
	if (setjmp(mach.jmp)) {
		rw_mach_destroy();
		return SCM_BOOL_F;
	}

	pushn(rw_c_cast(0, pctr_t));      /* Return address */
	run(fun->entry);                  /* call function */

	value.type = fun->rettype;
	return_value(&value);
	retval = radscm_datum_to_scm(&value);
	grad_value_free(&value);
	rw_mach_destroy();
	return retval;
}


#endif
