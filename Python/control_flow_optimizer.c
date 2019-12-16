/* Peephole optimizations for bytecode compiler. */

#include "Python.h"

#include "Python-ast.h"
#include "node.h"
#include "ast.h"
#include "code.h"
#include "symtable.h"
#include "opcode.h"
#include "wordcode_helpers.h"

#define UNCONDITIONAL_JUMP(op)  (op==JUMP_ABSOLUTE || op==JUMP_FORWARD)
#define CONDITIONAL_JUMP(op) (op==POP_JUMP_IF_FALSE || op==POP_JUMP_IF_TRUE \
    || op==JUMP_IF_FALSE_OR_POP || op==JUMP_IF_TRUE_OR_POP)
#define ABSOLUTE_JUMP(op) (op==JUMP_ABSOLUTE \
    || op==POP_JUMP_IF_FALSE || op==POP_JUMP_IF_TRUE \
    || op==JUMP_IF_FALSE_OR_POP || op==JUMP_IF_TRUE_OR_POP)
#define JUMPS_ON_TRUE(op) (op==POP_JUMP_IF_TRUE || op==JUMP_IF_TRUE_OR_POP)
#define GETJUMPTGT(arr, i) (get_arg(arr, i) / sizeof(_Py_CODEUNIT) + \
        (ABSOLUTE_JUMP(_Py_OPCODE(arr[i])) ? 0 : i+1))
#define ISBASICBLOCK(blocks, start, end) \
    (blocks[start]==blocks[end])


/* Scans back N consecutive LOAD_CONST instructions, skipping NOPs,
   returns index of the Nth last's LOAD_CONST's EXTENDED_ARG prefix.
   Callers are responsible to check CONST_STACK_LEN beforehand.
*/
static Py_ssize_t
lastn_const_start(const _Py_CODEUNIT *codestr, Py_ssize_t i, Py_ssize_t n)
{
    assert(n > 0);
    for (;;) {
        i--;
        assert(i >= 0);
        if (_Py_OPCODE(codestr[i]) == LOAD_CONST) {
            if (!--n) {
                while (i > 0 && _Py_OPCODE(codestr[i-1]) == EXTENDED_ARG) {
                    i--;
                }
                return i;
            }
        }
        else {
            assert(_Py_OPCODE(codestr[i]) == EXTENDED_ARG);
        }
    }
}

/* Scans through EXTENDED ARGs, seeking the index of the effective opcode */
static Py_ssize_t
find_op(const _Py_CODEUNIT *codestr, Py_ssize_t codelen, Py_ssize_t i)
{
    while (i < codelen && _Py_OPCODE(codestr[i]) == EXTENDED_ARG) {
        i++;
    }
    return i;
}

/* Given the index of the effective opcode,
   scan back to construct the oparg with EXTENDED_ARG */
static unsigned int
get_arg(const _Py_CODEUNIT *codestr, Py_ssize_t i)
{
    _Py_CODEUNIT word;
    unsigned int oparg = _Py_OPARG(codestr[i]);
    if (i >= 1 && _Py_OPCODE(word = codestr[i-1]) == EXTENDED_ARG) {
        oparg |= _Py_OPARG(word) << 8;
        if (i >= 2 && _Py_OPCODE(word = codestr[i-2]) == EXTENDED_ARG) {
            oparg |= _Py_OPARG(word) << 16;
            if (i >= 3 && _Py_OPCODE(word = codestr[i-3]) == EXTENDED_ARG) {
                oparg |= _Py_OPARG(word) << 24;
            }
        }
    }
    return oparg;
}

/* Fill the region with NOPs. */
static void
fill_nops(_Py_CODEUNIT *codestr, Py_ssize_t start, Py_ssize_t end)
{
    memset(codestr + start, NOP, (end - start) * sizeof(_Py_CODEUNIT));
}

/* Given the index of the effective opcode,
   attempt to replace the argument, taking into account EXTENDED_ARG.
   Returns -1 on failure, or the new op index on success */
static Py_ssize_t
set_arg(_Py_CODEUNIT *codestr, Py_ssize_t i, unsigned int oparg)
{
    unsigned int curarg = get_arg(codestr, i);
    int curilen, newilen;
    if (curarg == oparg)
        return i;
    curilen = instrsize(curarg);
    newilen = instrsize(oparg);
    if (curilen < newilen) {
        return -1;
    }

    write_op_arg(codestr + i + 1 - curilen, _Py_OPCODE(codestr[i]), oparg, newilen);
    fill_nops(codestr, i + 1 - curilen + newilen, i + 1);
    return i-curilen+newilen;
}

/* Attempt to write op/arg at end of specified region of memory.
   Preceding memory in the region is overwritten with NOPs.
   Returns -1 on failure, op index on success */
static Py_ssize_t
copy_op_arg(_Py_CODEUNIT *codestr, Py_ssize_t i, unsigned char op,
            unsigned int oparg, Py_ssize_t maxi)
{
    int ilen = instrsize(oparg);
    if (i + ilen > maxi) {
        return -1;
    }
    write_op_arg(codestr + maxi - ilen, op, oparg, ilen);
    fill_nops(codestr, i, maxi - ilen);
    return maxi - 1;
}

/* Replace LOAD_CONST c1, LOAD_CONST c2 ... LOAD_CONST cn, BUILD_TUPLE n
   with    LOAD_CONST (c1, c2, ... cn).
   The consts table must still be in list form so that the
   new constant (c1, c2, ... cn) can be appended.
   Called with codestr pointing to the first LOAD_CONST.
*/
static Py_ssize_t
fold_tuple_on_constants(_Py_CODEUNIT *codestr, Py_ssize_t codelen,
                        Py_ssize_t c_start, Py_ssize_t opcode_end,
                        PyObject *consts, int n)
{
    /* Pre-conditions */
    assert(PyList_CheckExact(consts));

    /* Buildup new tuple of constants */
    PyObject *newconst = PyTuple_New(n);
    if (newconst == NULL) {
        return -1;
    }

    for (Py_ssize_t i = 0, pos = c_start; i < n; i++, pos++) {
        assert(pos < opcode_end);
        pos = find_op(codestr, codelen, pos);
        assert(_Py_OPCODE(codestr[pos]) == LOAD_CONST);

        unsigned int arg = get_arg(codestr, pos);
        PyObject *constant = PyList_GET_ITEM(consts, arg);
        Py_INCREF(constant);
        PyTuple_SET_ITEM(newconst, i, constant);
    }

    Py_ssize_t index = PyList_GET_SIZE(consts);
#if SIZEOF_SIZE_T > SIZEOF_INT
    if ((size_t)index >= UINT_MAX - 1) {
        Py_DECREF(newconst);
        PyErr_SetString(PyExc_OverflowError, "too many constants");
        return -1;
    }
#endif

    /* Append folded constant onto consts */
    if (PyList_Append(consts, newconst)) {
        Py_DECREF(newconst);
        return -1;
    }
    Py_DECREF(newconst);

    return copy_op_arg(codestr, c_start, LOAD_CONST,
                       (unsigned int)index, opcode_end);
}

static unsigned int *
markblocks(struct compiler_unit *u, Py_ssize_t len)
{
    unsigned int *blocks = PyMem_New(unsigned int, len);
    int i, j, opcode, blockcnt = 0;
    basicblock *block;

    if (blocks == NULL) {
        PyErr_NoMemory();
        return NULL;
    }
    memset(blocks, 0, len*sizeof(int));

    for (block = u->u_blocks; block != NULL; block = block->b_list) {
        if (block->b_instr != NULL) {
            opcode = block->b_instr->i_opcode;
            switch (opcode) {
                case FOR_ITER:
                case JUMP_FORWARD:
                case JUMP_IF_FALSE_OR_POP:
                case JUMP_IF_TRUE_OR_POP:
                case POP_JUMP_IF_FALSE:
                case POP_JUMP_IF_TRUE:
                case JUMP_ABSOLUTE:
                case SETUP_FINALLY:
                case SETUP_WITH:
                case SETUP_ASYNC_WITH:
                    break;
            }
        }
        else {
            assert (block->b_iused == 0);
            assert (block->b_ialloc == 0);
        }
    }

    /* Build block numbers in the second pass */
    for (i = 0; i < len; i++) {
        blockcnt += blocks[i];          /* increment blockcnt over labels */
        blocks[i] = blockcnt;
    }
    return blocks;
}

struct result {
    struct compiler *c;
    struct assembler *a;
};

typedef struct result Struct;

Struct getResult(struct compiler *c, struct assembler *a)
{
    Struct s;

    s.compiler = c;
    s.assembler = a;

    return s;
}


struct compiler *c
PyCFG_Optimize(struct compiler *c, struct assembler *a, PyObject* consts, PyObject *names)
{
    basicblock *b, *entryblock = NULL;
    basicblock **stack, **sp;
    int nblocks = 0, maxdepth = 0;
    for (b = c->u->u_blocks; b != NULL; b = b->b_list) {
        b->b_startdepth = INT_MIN;
        entryblock = b;
        nblocks++;
    }

    for (int i = 0; i < nblocks; i++) {
            struct instr *instr = &b->b_instr[i];
            opecode = instr->i_opcode;

            switch (opcode) {
                    /* Skip over LOAD_CONST trueconst
                    POP_JUMP_IF_FALSE xx.  This improves
                    "while 1" performance.  */
                case LOAD_CONST:
                    cumlc = lastlc + 1;
                    if (nextop != POP_JUMP_IF_FALSE  ||
                        !ISBASICBLOCK(blocks, op_start, i + 1)) {
                        break;
                    }
                    PyObject* cnt = PyList_GET_ITEM(consts, get_arg(codestr, i));
                    int is_true = PyObject_IsTrue(cnt);
                    if (is_true == -1) {
                        goto exitError;
                    }
                    if (is_true == 1) {
                        fill_nops(codestr, op_start, nexti + 1);
                        cumlc = 0;
                    }
                    break;
            }
    }
                            
}
