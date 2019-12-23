/* Basic Block Optimizations. */

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

static int
get_arg(_instr *instr)
{
    return instr->i_oparg;
} 

static void
fill_nops(_instr *instr)
{
    instr->i_opcode = NOP;
    instr->i_oparg  = NULL;
}

_basicblock
PyCFG_Optimize(_compiler *c, _assembler *a, PyObject* consts, PyObject *names)
{
    Py_ssize_t h, i, nexti, op_start, tgt;
    unsigned char opcode, nextop;
    _basicblock *b, *entryblock = NULL;
    _basicblock **stack, **sp;
    int nblocks = 0, maxdepth = 0;

    unsigned int cumlc = 0, lastlc = 0;

    Py_ssize_t codesize = PyBytes_GET_SIZE(a->a_bytecode);
    assert(codesize % sizeof(_Py_CODEUNIT) == 0);
    Py_ssize_t codelen = codesize / sizeof(_Py_CODEUNIT);

    b = c->u->u_blocks;
    for (i = 0; i < b->b_iused; i++) {
        struct instr *instr = &b->b_instr[i];
        opcode = instr->i_opcode;

        Py_ssize_t nexti = i + 1;
        struct instr * nextinst = &b->b_instr[nexti];
        nextop = nextinst->i_opcode;

        switch (opcode) {

            /*..........................................
                Skip over LOAD_CONST trueconst
                POP_JUMP_IF_FALSE xx.  This improves
                "while 1" performance.  
            ..........................................*/

            case LOAD_CONST:
                cumlc = lastlc + 1;
                if (nextop != POP_JUMP_IF_FALSE) {
                    break;
                }
                PyObject* cnt = PyList_GET_ITEM(consts, get_arg(instr));
                int is_true = PyObject_IsTrue(cnt);
                if (is_true == -1) {
                    goto exitError;
                }
                if (is_true == 1) {
                    fill_nops(instr);
                    cumlc = 0;
                }
                break;
                
            /*......................................
                Remove unreachable ops after RETURN 
            ......................................*/

        }
    }

  exitError:
    code = NULL;

  exitUnchanged:
    Py_XINCREF(b);
    PyMem_Free(blocks);
    PyMem_Free(codestr);
    return b;                       
}
