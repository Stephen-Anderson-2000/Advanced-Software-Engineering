#include "node.h"

Node::Node(KeyType key, ItemType item)
{
    this->key = key;
    this->item = item;
    this->leftChild = nullptr;
    this->rightChild = nullptr;
}
