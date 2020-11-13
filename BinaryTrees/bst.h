#ifndef BST_H
#define BST_H

#include <string>

class BST
{
public:
    using KeyType = int;
    using ItemType = std::string;
    BST() = default;  
    ~BST();
    BST(const BST &);
    BST & operator=(const BST &);
    BST(BST &&);
    BST & operator=(BST &&);
    ItemType* lookup(KeyType);
    void insert(KeyType, ItemType);
    void remove(KeyType);
    void displayEntries();
    void displayTree();
    //void removeIf(std::function <bool(KeyType)>);

private:
    struct Node;
    Node* root = leaf();
    static Node* leaf();
    static bool isLeaf(Node*);
    ItemType* lookupRec(KeyType, Node*);
    int calcBalanceFactor(Node* &, Node* &);
    int findBranchHeight(Node* &, int &, int = 0);
    void insertRec(KeyType, ItemType, Node* &);
    void inOrderTraversal(Node* &);
    void preOrderDisplay(Node* &, std::string);
    void removeRec(Node* &, KeyType);
    Node* detachMinimumNode(Node* &);
    void deepDelete(Node*);
    Node* deepCopy(Node*);
    void rotateRight(Node* &); // Should only be called when the node being passed in has a valid left child node
    void rotateLeft(Node* &); // Should only be called when the node being passed in has a valid right child node
    bool rebalance(Node* &);
};

#endif // BST_H
