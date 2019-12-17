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
get_arg(struct instr *instr)
{
    return instr->i_oparg;
}


static void
fill_nops(struct instr *instr)
{
    instr->i_opcode = NOP;
    instr->i_oparg  = NULL;
}

PyObject*
PyCFG_Optimize(struct compiler *c, struct assembler *a, PyObject* consts, PyObject *names)
{
    basicblock *b, *entryblock = NULL;
    basicblock **stack, **sp;
    int nblocks = 0, maxdepth = 0;

    Py_ssize_t codesize = PyBytes_GET_SIZE(a->a_bytecode);
    assert(codesize % sizeof(_Py_CODEUNIT) == 0);
    Py_ssize_t codelen = codesize / sizeof(_Py_CODEUNIT);

    for (b = c->u->u_blocks; b != NULL; b = b->b_list) {
        b->b_startdepth = INT_MIN;
        entryblock = b;
        nblocks++;
    }

    b = c->u->u_blocks;
    for (i = 0; i < b->b_iused; i++) {
        struct instr *instr = &b->b_instr[i];
        opecode = instr->i_opcode;

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
        }
    }
    return b;                       
}
