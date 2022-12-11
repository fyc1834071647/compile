#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include "Type.h"

// 相较于lab5的Ast.cpp(主要实现了各个节点的output()函数)
// 这里主要增加了实现了各个节点的typeCheck()和genCode()函数

extern FILE *yyout;
int Node::counter = 0;
IRBuilder *Node::builder = nullptr;

Node::Node()
{
    seq = counter++;
}

void Node::backPatch(std::vector<Instruction *> &list, BasicBlock *bb)
{
    for (auto &inst : list) // 遍历指令的list
    {
        if (inst->isCond()) // 如果指令是COND
        {
            // COND指令所属基础块放在bb前
            bb->addPred(dynamic_cast<CondBrInstruction *>(inst)->getParent());
            // bb放在COND指令所属基础块后
            dynamic_cast<CondBrInstruction *>(inst)->getParent()->addSucc(bb);
            // 为真时运行bb
            dynamic_cast<CondBrInstruction *>(inst)->setTrueBranch(bb);
        }
        else if (inst->isUncond())
        {
            bb->addPred(dynamic_cast<CondBrInstruction *>(inst)->getParent());
            dynamic_cast<CondBrInstruction *>(inst)->getParent()->addSucc(bb);
            dynamic_cast<UncondBrInstruction *>(inst)->setBranch(bb);
        }
    }
}

void Node::backPatchFalse(std::vector<Instruction *> &list, BasicBlock *bb)
{
    for (auto &inst : list)
    {
        if (inst->isCond())
        {
            bb->addPred(dynamic_cast<CondBrInstruction *>(inst)->getParent());
            dynamic_cast<CondBrInstruction *>(inst)->getParent()->addSucc(bb);
            dynamic_cast<CondBrInstruction *>(inst)->setFalseBranch(bb);
        }
        else if (inst->isUncond())
        {
            bb->addPred(dynamic_cast<CondBrInstruction *>(inst)->getParent());
            dynamic_cast<CondBrInstruction *>(inst)->getParent()->addSucc(bb);
            dynamic_cast<UncondBrInstruction *>(inst)->setBranch(bb);
        }
    }
}

std::vector<Instruction *> Node::merge(std::vector<Instruction *> &list1, std::vector<Instruction *> &list2)
{
    std::vector<Instruction *> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit *unit)
{
    // std::cout  << "start" << std::endl;
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    // 这里直接添加了四条中间代码，用于输入输出流
    fprintf(yyout, "declare i32 @getint()\ndeclare void @putint(i32)\ndeclare i32 @getch()\ndeclare void @putch(i32)\n"); //
    root->genCode();
    // std::cout  << "end" << std::endl;
}

// gencode

void FunctionDef::genCode()
{
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);

    if (FPs != nullptr)
    {
        FPs->genCode();
    }

    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
     */
}

void BinaryExpr::genCode()
{
    // 得到后续生成的指令要插入的基本块
    BasicBlock *bb = builder->getInsertBB();
    // 得到指令所在的函数
    Function *func = bb->getParent();
    // 根据操作码生成中间代码
    if (op == AND)
    {
        BasicBlock *trueBB = new BasicBlock(func); // 第二个子表达式生成的指令需要插入的位置
        // 第一个子表达式生成中间代码
        // 过程中，生成的跳转指令的目标基本块尚不能确定
        // 因此会将其插入到子表达式结点的true_list 和false_list中
        expr1->genCode();
        // 由于expr1为真，接着运行expr2
        // 能确定expr1的true_list中跳转指令的目的块为trueBB
        backPatch(expr1->trueList(), trueBB); // 回填
        builder->setInsertBB(trueBB);         // 设置要插入的基本块，确保expr2产生中间代码时插入的为trueBB
        expr2->genCode();                     // expr2的中间代码
        // 当前表达式为真，expr2一定为真
        // 为假，两个式子都为假
        true_list = expr2->trueList();
        false_list = merge(expr1->falseList(), expr2->falseList());
        // 结果的类型设置
        dst->getType()->kind = 4;
    }
    else if (op == OR)
    {
        // TODO
        // 仿照AND
        BasicBlock *falseBB = new BasicBlock(func);
        expr1->genCode();
        backPatchFalse(expr1->falseList(), falseBB);
        builder->setInsertBB(falseBB);
        expr2->genCode();
        false_list = expr2->falseList();
        true_list = merge(expr1->trueList(), expr2->trueList());
        dst->getType()->kind = 4;
    }
    else if (op >= LESS && op <= MORE)
    {
        // TODO
        expr1->genCode();
        expr2->genCode();
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case MORE:
            opcode = CmpInstruction::G;
            break;
        case MOREEQUAL:
            opcode = CmpInstruction::GE;
            break;
        case LESS:
            opcode = CmpInstruction::L;
            break;
        case LESSEQUAL:
            opcode = CmpInstruction::LE;
            break;
        case EQUAL:
            opcode = CmpInstruction::E;
            break;
        case NOEQUAL:
            opcode = CmpInstruction::NE;
            break;
        default:
            break;
        }

        new CmpInstruction(opcode, dst, src1, src2, bb);
        // 比较运算，直接合并
        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = merge(expr1->falseList(), expr2->falseList());
        Instruction *temp = new CondBrInstruction(nullptr, nullptr, dst, bb); // 真假分支均为空
        this->trueList().push_back(temp);
        this->falseList().push_back(temp);
        dst->getType()->kind = 4;
    }
    else if (op >= ADD && op <= SUB)
    {
        expr1->genCode();
        expr2->genCode();
        Operand *src1 = expr1->getOperand();
        Operand *src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        // TODO
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case PERC:
            opcode = BinaryInstruction::MOD;
            break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void Constant::genCode()
{
    //
}

void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getAddr();
    new LoadInstruction(dst, addr, bb);
}

void IfStmt::genCode() // if语句
{
    Function *func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    // 设置块的位置关系
    // builder - then_bb - end_bb
    //        \___________/
    then_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(then_bb);
    end_bb->addPred(then_bb);
    then_bb->addSucc(end_bb);
    end_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(end_bb);

    if (cond != nullptr)
        cond->genCode();
    // TODO
    // 条件表达式不为bool，增加一步和0比较的操作
    if (!cond->getOperand()->getType()->isBool())
    {
        BasicBlock *bb = cond->builder->getInsertBB();
        // 操作数
        Operand *src = cond->getOperand();
        // 0
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        Constant *digit = new Constant(se);
        Operand *t = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
        // 比较指令
        CmpInstruction *temp = new CmpInstruction(CmpInstruction::EXCLAMATION, t, src, digit->getOperand(), bb);
        src = t;
        cond->trueList().push_back(temp);
        cond->falseList().push_back(temp);
        // 条件分支指令
        Instruction *m = new CondBrInstruction(nullptr, nullptr, t, bb);
        cond->trueList().push_back(m);
        cond->falseList().push_back(m);
    }
    // END

    // 回填
    backPatch(cond->trueList(), then_bb);
    backPatchFalse(cond->falseList(), end_bb);

    // then的中间代码放到then_bb
    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    // 非条件分支
    new UncondBrInstruction(end_bb, then_bb);
    // 插入的基本块更新为end_bb
    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode() // if-else语句
{
    // TODO
    // 类似IfStmt
    Function *func;
    BasicBlock *then_bb, *else_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);

    // 设置块的位置关系
    then_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(then_bb);
    else_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(else_bb);
    end_bb->addPred(then_bb);
    then_bb->addSucc(end_bb);
    end_bb->addPred(else_bb);
    else_bb->addSucc(end_bb);

    cond->genCode();
    // 条件表达式不为bool，增加一步和0比较的操作
    if (!cond->getOperand()->getType()->isBool())
    {
        BasicBlock *bb = cond->builder->getInsertBB();
        Operand *src = cond->getOperand();
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        Constant *digit = new Constant(se);
        Operand *t = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
        CmpInstruction *temp = new CmpInstruction(CmpInstruction::EXCLAMATION, t, src, digit->getOperand(), bb);
        src = t;
        cond->trueList().push_back(temp);
        cond->falseList().push_back(temp);
        Instruction *m = new CondBrInstruction(nullptr, nullptr, t, bb);
        cond->trueList().push_back(m);
        cond->falseList().push_back(m);
    }
    // 回填
    backPatch(cond->trueList(), then_bb);
    backPatchFalse(cond->falseList(), else_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    // 相较IfStmt不同，这里增加了else中间代码的生成
    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);
    // 要插入的基本块更新为end_bb
    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode()
{
    // TODO
    stmt->genCode();
}

void SeqNode::genCode()
{
    // TODO
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode()
{
    // 声明
    for (auto iter = ids->Ids.rbegin(); iter != ids->Ids.rend(); iter++)
    {
        IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>((*iter)->getSymPtr());
        if (se->isGlobal())
        {
            Operand *addr;
            SymbolEntry *addr_se;
            addr_se = new IdentifierSymbolEntry(*se);
            addr_se->setType(new PointerType(se->getType()));
            addr = new Operand(addr_se);
            se->setAddr(addr);
            //
            bool temp = false;
            Operand *src;
            for (long unsigned int i = 0; i < ids->Assigns.size(); i++)
            {
                if (ids->Assigns[i]->lval->symbolEntry == se)
                {
                    ids->Assigns[i]->genCode();
                    src = ids->Assigns[i]->expr->getOperand();
                    temp = true;
                    break;
                }
            }
            if (temp == false)
            {
                SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
                Constant *digit = new Constant(se);
                src = digit->getOperand();
            }
            Instruction *alloca = new AllocaInstruction2(src, addr, se);
            alloca->output();
            //
        }
        else if (se->isLocal())
        {
            Function *func = builder->getInsertBB()->getParent();
            BasicBlock *entry = func->getEntry();
            Instruction *alloca;
            Operand *addr;
            SymbolEntry *addr_se;
            Type *type;
            type = new PointerType(se->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, se); // allocate space for local id in function stack.
            entry->insertFront(alloca);               // allocate instructions should be inserted into the begin of the entry block.
            se->setAddr(addr);                        // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        }
    }
    for (long unsigned int i = 0; i < ids->Assigns.size(); i++)
    {
        IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(ids->Assigns[i]->lval->getSymPtr());
        if (se->isGlobal())
        {
            continue;
        }
        else if (se->isLocal())
        {
            Operand *addr = dynamic_cast<IdentifierSymbolEntry *>(ids->Assigns[i]->lval->getSymPtr())->getAddr();
            se->setAddr(addr);
            ids->Assigns[i]->genCode();
        }
    }
}

void ReturnStmt::genCode()
{
    // TODO
    BasicBlock *bb = builder->getInsertBB();
    retValue->genCode();
    Operand *src = retValue->getOperand();
    // 生成ret指令
    new RetInstruction(src, bb);
}

void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry *>(lval->getSymPtr())->getAddr();
    Operand *src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

void SignleStmt::genCode()
{
    expr->genCode();
}

void Empty::genCode()
{
}

void FuncRParams::genCode()
{
}

void FuncFParam::genCode() // 函数形参
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getAddr();
    new LoadInstruction(dst, addr, bb);
}

void FuncFParams::genCode() // 函数形参列表 (int a=1, bool b=0)
{
    Function *func = builder->getInsertBB()->getParent();
    for (long unsigned int i = 0; i < FPs.size(); i++)
    {
        IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(FPs[i]->getSymPtr());
        Type *type1 = new PointerType(se->getType());
        Type *type2 = new IntType(32);
        SymbolEntry *addr_se = new TemporarySymbolEntry(type2, SymbolTable::getLabel());
        Operand *addr = new Operand(addr_se);
        // 创建新的操作数
        SymbolTable ::counter++;
        SymbolEntry *addr_se2 = new TemporarySymbolEntry(type1, SymbolTable::getLabel());
        Operand *addr2 = new Operand(addr_se2);

        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        // alloca指令分配新空间
        alloca = new AllocaInstruction(addr2, se);                   // allocate space for local id in function stack.
        entry->insertBack(alloca);                                   // allocate instructions should be inserted into the begin of the entry block.
        StoreInstruction *store = new StoreInstruction(addr2, addr); // 复制值
        entry->insertBack(store);
        se->setAddr(addr2);
        func->params.push_back(addr);
    }
}

void FunctionCall::genCode()
{
    // 先处理函数参数
    std::vector<Operand *> params;
    if (RPs != nullptr)
        for (unsigned i = 0; i < RPs->Exprs.size(); i++)
        {
            if (RPs->Exprs[i] != nullptr)
                RPs->Exprs[i]->genCode(); //
            params.push_back(RPs->Exprs[i]->getOperand());
        }
    BasicBlock *entry = builder->getInsertBB();
    Type *type2 = new IntType(32);
    SymbolTable ::counter++;
    SymbolEntry *addr_se2 = new TemporarySymbolEntry(type2, SymbolTable::getLabel());
    dst = new Operand(addr_se2);

    // call指令
    FunctioncallInstruction *temp = new FunctioncallInstruction(dst, symbolEntry, params);
    entry->insertBack(temp);
}

void ConstIdList::genCode()
{
}

void IdList::genCode()
{
}

void WhileStmt::genCode()
{
    Function *func;
    BasicBlock *loop_bb, *end_bb, *cond_bb;

    func = builder->getInsertBB()->getParent();
    loop_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    cond_bb = new BasicBlock(func);

    UncondBrInstruction *temp = new UncondBrInstruction(cond_bb, builder->getInsertBB());
    temp->output();
    // 设置前后
    cond_bb->addPred(builder->getInsertBB());
    builder->getInsertBB()->addSucc(cond_bb);
    loop_bb->addPred(cond_bb);
    cond_bb->addSucc(loop_bb);
    end_bb->addPred(loop_bb);
    loop_bb->addSucc(end_bb);
    end_bb->addPred(cond_bb);
    cond_bb->addSucc(end_bb);
    builder->setInsertBB(cond_bb);

    cond->genCode();
    if (!cond->getOperand()->getType()->isBool())
    {
        BasicBlock *bb = cond->builder->getInsertBB();
        Operand *src = cond->getOperand();
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        Constant *digit = new Constant(se);
        Operand *t = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
        CmpInstruction *temp = new CmpInstruction(CmpInstruction::EXCLAMATION, t, src, digit->getOperand(), bb);
        src = t;
        cond->trueList().push_back(temp);
        cond->falseList().push_back(temp);
        Instruction *m = new CondBrInstruction(nullptr, nullptr, t, bb);
        cond->trueList().push_back(m);
        cond->falseList().push_back(m);
    }
    backPatch(cond->trueList(), loop_bb);
    backPatchFalse(cond->falseList(), end_bb);

    builder->setInsertBB(loop_bb);
    loop->genCode();
    loop_bb = builder->getInsertBB();
    new CondBrInstruction(cond_bb, end_bb, cond->getOperand(), loop_bb);

    builder->setInsertBB(end_bb);
}

void ConstDeclStmt::genCode()
{
    for (long unsigned int i = 0; i < Cids->CIds.size(); i++)
    {
        IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(Cids->CIds[i]->getSymPtr());
        if (se->isGlobal())
        {
            Operand *addr;
            SymbolEntry *addr_se;
            addr_se = new IdentifierSymbolEntry(*se);
            addr_se->setType(new PointerType(se->getType()));
            addr = new Operand(addr_se);
            se->setAddr(addr);
            Cids->Assigns[i]->genCode();
            Operand *src = Cids->Assigns[i]->expr->getOperand();
            Instruction *alloca = new AllocaInstruction2(src, addr, se);
            alloca->output();
        }
        else if (se->isLocal())
        {
            Function *func = builder->getInsertBB()->getParent();
            BasicBlock *entry = func->getEntry();
            Instruction *alloca;
            Operand *addr;
            SymbolEntry *addr_se;
            Type *type;
            type = new PointerType(se->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, se); // allocate space for local id in function stack.
            entry->insertFront(alloca);               // allocate instructions should be inserted into the begin of the entry block.
            se->setAddr(addr);

            Cids->Assigns[i]->expr->genCode();
            Operand *addr1 = dynamic_cast<IdentifierSymbolEntry *>(Cids->Assigns[i]->lval->getSymPtr())->getAddr();
            se->setAddr(addr1);
            Operand *src = Cids->Assigns[i]->expr->getOperand();
            BasicBlock *ttt = builder->getInsertBB();
            new StoreInstruction(addr1, src, ttt); // set the addr operand in symbol entry so that we can use it in subsequent code generation.
        }
    }
}

void ContinueStmt::genCode()
{
}

void BreakStmt::genCode()
{
}

void ConstId::genCode()
{
}

void SignleExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    if (op == EXCLAMATION)
    {
        Operand *src = expr->getOperand();
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        Constant *digit = new Constant(se);
        expr->genCode();
        if (!expr->getOperand()->getType()->isBool())
        {
            Operand *t = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
            new CmpInstruction(CmpInstruction::EXCLAMATION, t, src, digit->getOperand(), bb);
            src = t;
        }
        new XorInstruction(dst, src, bb);
        dst->getType()->kind = 4;
        isCond = true;
    }
    if (op >= SUB && op <= ADD)
    {
        expr->genCode();
        Operand *src = expr->getOperand();
        if (src->getType()->isBool()) // 是bool值，使用zext指令转为i32
        {
            Operand *t = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(t, expr->dst, bb);
            expr->dst = t;
            src = t;
        }
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        default:
            break;
        }
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, 0);
        Constant *digit = new Constant(se);
        new BinaryInstruction(opcode, dst, digit->getOperand(), src, bb);
        isCond = expr->isCond;
    }
}

// typecheck

void Ast::typeCheck()
{
    fprintf(yyout, ";TypeCheck Begin!\n");
    if (root != nullptr)
        root->typeCheck(); // 递归
}

void FunctionDef::typeCheck()
{
    fprintf(yyout, ";FunctionDef %s TypeCheck Begin!\n", se->toStr().c_str());
    stmt->typeCheck(); // 递归
}

void BinaryExpr::typeCheck()
{
    Type *type1 = expr1->getSymPtr()->getType();
    Type *type2 = expr2->getSymPtr()->getType();
    if (type1 != type2) // 类型不相同
    {
        fprintf(stderr, "type %s and %s mismatch",
                type1->toStr().c_str(), type2->toStr().c_str());
        exit(EXIT_FAILURE);
    }
    fprintf(yyout, ";BinaryExpr TypeCheck OVER!\n");
    symbolEntry->setType(type1);
    expr1->typeCheck(); // 递归
    expr2->typeCheck();
}

void SignleExpr::typeCheck()
{
    Type *type = expr->getSymPtr()->getType();
    if (type->isVoid()) // 不应该为void
    {
        fprintf(stderr, "type can't be void");
        exit(EXIT_FAILURE);
    }
    symbolEntry->setType(type);
    expr->typeCheck();
}

void ReturnStmt::typeCheck()
{
    fprintf(yyout, ";Return Statement TypeCheck begin!\n");
    if (retValue != nullptr) // 返回不为空指针
    {
        retValue->typeCheck(); // 递归
        type = retValue->getOperand()->getType();
    }
    else
    {
        type = new VoidType(); // 空指针转为void
    }
    fprintf(yyout, ";Return Statement TypeCheck OVER!\n");
}

void IfStmt::typeCheck()
{
    cond->typeCheck();
    thenStmt->typeCheck();
}

void Constant::typeCheck()
{
    // Todo
}

void Id::typeCheck()
{
    // Todo
}

void IfElseStmt::typeCheck()
{
    // Todo
}

void CompoundStmt::typeCheck()
{
    // Todo
}

void SeqNode::typeCheck()
{
    // Todo
}

void DeclStmt::typeCheck()
{
}

void AssignStmt::typeCheck()
{
}

void SignleStmt::typeCheck()
{
}

void FuncRParams::typeCheck()
{
}

void Empty::typeCheck()
{
}

void FuncFParam::typeCheck()
{
}

void FuncFParams::typeCheck()
{
}

void ConstIdList::typeCheck()
{
}

void IdList::typeCheck()
{
}

void WhileStmt::typeCheck()
{
}

void FunctionCall::typeCheck()
{
}

void ConstDeclStmt::typeCheck()
{
}

void ContinueStmt::typeCheck()
{
}

void BreakStmt::typeCheck()
{
}

void ConstId::typeCheck()
{
}

// output

void Ast::output()
{
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch (op)
    {
    case ADD:
        op_str = "add";
        break;
    case SUB:
        op_str = "sub";
        break;
    case AND:
        op_str = "and";
        break;
    case OR:
        op_str = "or";
        break;
    case LESS:
        op_str = "less";
        break;
    case MORE:
        op_str = "more";
        break;
    case MOREEQUAL:
        op_str = "moreequal";
        break;
    case LESSEQUAL:
        op_str = "lessequal";
        break;
    case EQUAL:
        op_str = "equal";
        break;
    case NOEQUAL:
        op_str = "noequal";
        break;
    case DIV:
        op_str = "div";
        break;
    case MUL:
        op_str = "mul";
        break;
    case PERC:
        op_str = "mod";
        break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

void SignleExpr::output(int level)
{
    std::string op_str;
    switch (op)
    {
    case SUB:
        op_str = "negative";
        break;
    case ADD:
        op_str = "positive";
        break;
    case EXCLAMATION:
        op_str = "anti";
        break;
    }
    fprintf(yyout, "%*cSignleExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}

void Constant::output(int level)
{
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

void ConstId::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getScope();
    fprintf(yyout, "%*cConstId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
}

void Id::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
}

void FuncFParam::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry *>(symbolEntry)->getScope();
    fprintf(yyout, "%*cFuncFParam\tname:%s\tscope:%d\ttype:%s\n", level, ' ',
            name.c_str(), scope, type.c_str());
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level + 4);
    stmt2->output(level + 4);
}

void BreakStmt::output(int level)
{
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level)
{
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    ids->output(level + 4);
}

void ConstDeclStmt::output(int level)
{
    fprintf(yyout, "%*cConstDeclStmt\n", level, ' ');
    Cids->output(level + 4);
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void FunctionDef::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ',
            name.c_str(), type.c_str());
    if (FPs != nullptr)
    {
        FPs->output(level + 4);
    }
    stmt->output(level + 4);
}

void FunctionCall::output(int level)
{
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cFuncCall\tname: %s\ttype: %s\n", level, ' ',
            name.c_str(), type.c_str());
    if (RPs != nullptr)
    {
        RPs->output(level + 4);
    }
}

void WhileStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    loop->output(level + 4);
}

void IdList::output(int level)
{
    fprintf(yyout, "%*cIdList\n", level, ' ');
    for (long unsigned int i = 0; i < Ids.size(); i++)
    {
        Ids[i]->output(level + 4);
    }
    for (long unsigned int i = 0; i < Assigns.size(); i++)
    {
        Assigns[i]->output(level + 4);
    }
}
void ConstIdList::output(int level)
{
    fprintf(yyout, "%*cConstIdList\n", level, ' ');
    for (long unsigned int i = 0; i < CIds.size(); i++)
    {
        CIds[i]->output(level + 4);
        Assigns[i]->output(level + 4);
    }
}

void FuncFParams::output(int level)
{
    fprintf(yyout, "%*cFuncFParams\n", level, ' ');
    for (long unsigned int i = 0; i < FPs.size(); i++)
    {
        FPs[i]->output(level + 4);
    }
    for (long unsigned int i = 0; i < Assigns.size(); i++)
    {
        Assigns[i]->output(level + 4);
    }
}

void FuncRParams::output(int level)
{
    fprintf(yyout, "%*cFuncRParams\n", level, ' ');
    for (long unsigned int i = 0; i < Exprs.size(); i++)
    {
        Exprs[i]->output(level + 4);
    }
}

void Empty::output(int level)
{
    fprintf(yyout, "%*cEmpty Statement\n", level, ' ');
}

void SignleStmt::output(int level)
{
    fprintf(yyout, "%*cSignle Statement\n", level, ' ');
    expr->output(level + 4);
}