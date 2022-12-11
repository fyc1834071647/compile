#ifndef __IRBUILDER_H__
#define __IRBUILDER_H__

class Unit;
class Function;
class BasicBlock;

class IRBuilder
{
private:
    Unit *unit;
    // 所属的节点生成的中间代码插入该基础块
    BasicBlock *insertBB; // The current basicblock that instructions should be inserted into.

public:
    IRBuilder(Unit *unit) : unit(unit){};
    void setInsertBB(BasicBlock *bb) { insertBB = bb; };
    Unit *getUnit() { return unit; };
    BasicBlock *getInsertBB() { return insertBB; }; // 得到后续生成的指令要插入的基本块
};

#endif