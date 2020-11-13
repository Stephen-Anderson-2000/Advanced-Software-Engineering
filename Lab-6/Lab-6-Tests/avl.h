#ifndef AVL_H
#define AVL_H

#include "../../BinaryTrees/bst.h"

class AVL : public BST
{
    public:
    AVL();
private:
    bool rebalance(Node* &);
    int calcBalanceFactor(Node* &, Node* &);
    int findBranchHeight(Node* &, int &, int = 0);
};

#endif // AVL_H
