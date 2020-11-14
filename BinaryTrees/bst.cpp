#include "bst.h"
#include <cassert>
#include <iostream>

struct BST::Node
{
    Node(KeyType, ItemType);
    BST::KeyType key;
    BST::ItemType item;
    Node* leftChild;
    Node* rightChild;
    int balanceFactor;
}; // A struct to store the key, item and the pointers to the next nodes

BST::Node::Node(KeyType key, ItemType item)
{
    this->key = key;
    this->item = item;
    // Errors can occur if the children are not explicitly set as leaves when initialised
    this->leftChild = BST::leaf();
    this->rightChild = BST::leaf();
    // Initialises the balance factor that is used for AVL balancing
    this->balanceFactor = 0;
} // Sets up the Node type and makes sure that the children are nullptrs

BST::BST(const BST & originalTree)
{
    this->root = deepCopy(originalTree.root);
}

BST::~BST()
{
    deepDelete(root);
}

BST & BST::operator=(const BST & original)
{
    if (this != &original)
    {
        deepDelete(this->root);
    }
    this->root = deepCopy(original.root);
    return *this;
}

BST::BST(BST && originalTree)
{
    this->root = originalTree.root;
    originalTree.root = leaf();
}

BST & BST::operator=(BST && originalTree)
{
    this->root = originalTree.root;
    if (this != &originalTree)
    {
        originalTree.root = leaf();
    }
    return *this;
}

BST::Node* BST::leaf()
{
    return nullptr;
} // Just used as a way of describing when a node has become null

bool BST::isLeaf(Node* n)
{
    return (n == nullptr);
} // Returns true if the node is a nullptr

BST::ItemType* BST::lookup(KeyType soughtKey)
{
    return lookupRec(soughtKey, this->root);
} // Essentially used like an API to interact with the recursive function

BST::ItemType* BST::lookupRec(KeyType soughtKey, Node* currentNode)
{
    if(!isLeaf(currentNode))
    {
        if (soughtKey == currentNode->key)
        {
            return &currentNode->item;
        }
        else if (soughtKey < currentNode->key)
        {
            return lookupRec(soughtKey, currentNode->leftChild);
        }
        else if (soughtKey > currentNode->key)
        {
            return lookupRec(soughtKey, currentNode->rightChild);
        }
    }
    return nullptr;
} // Recursively searches the BST to return the location in memory of the item

void BST::insert(KeyType newKey, ItemType newItem)
{
    insertRec(newKey, newItem, this->root);
} // The wrapper used to insert a new item into a sorted BST

void BST::insertRec(KeyType newKey, ItemType newItem, Node* &currentNode)
{
    // If the current node is a nullptr then the new item can be inserted
    if (isLeaf(currentNode))
    {
        Node* newNode = new Node(newKey, newItem);
        currentNode = newNode;
    }
    // This implementation overwrites existing items if a matching key is found
    else if (newKey == currentNode->key)
    {
        currentNode->item = newItem;
    }
    else if (newKey < currentNode->key)
    {
        insertRec(newKey, newItem, currentNode->leftChild);
        currentNode->balanceFactor = calcBalanceFactor(currentNode->leftChild, currentNode->rightChild);
    }
    else if (newKey > currentNode->key)
    {
        insertRec(newKey, newItem, currentNode->rightChild);
        currentNode->balanceFactor = calcBalanceFactor(currentNode->leftChild, currentNode->rightChild);
    }
} // Recursively iterates until it reaches the correct insertion point

int BST::calcBalanceFactor(Node* &leftChild, Node* &rightChild)
{
    if (!isLeaf(leftChild) && !isLeaf(rightChild))
    {
        int rightHeight = 0, leftHeight = 0;
        findBranchHeight(rightChild, rightHeight);
        findBranchHeight(leftChild, leftHeight);
        return (rightHeight - leftHeight);
    }
    else if (!isLeaf(leftChild))
    {
        return (leftChild->balanceFactor - 1);
    }
    else if (!isLeaf(rightChild))
    {
        return (rightChild->balanceFactor + 1);
    }
    else
    {
        return 0;
    }
}


// Look for alternative solution
int BST::findBranchHeight(Node* &currentNode, int &maxLevel, int level)
{
    if (!isLeaf(currentNode))
    {
        findBranchHeight(currentNode->leftChild, maxLevel, ++level);

        if (level > maxLevel)
        {
            maxLevel = level;
        }

        findBranchHeight(currentNode->rightChild, maxLevel, level);
    }
    return maxLevel;
}

void BST::remove(KeyType soughtKey)
{
    // Needs this extra if statement to prevent memory access violations
    // when using this->root->attribute
    if (!isLeaf(this->root))
    {
        // Only runs the recursive function if the BST is not a single entry
        if (!isLeaf(this->root->leftChild) || !isLeaf(this->root->rightChild))
        {
            removeRec(this->root, soughtKey);
        }
        else if (this->root->key == soughtKey)
        {
            this->root = leaf();
        }
    }
} // The wrapper used for removing a node by its key

void BST::removeRec(Node * & currentNode, KeyType soughtKey)
{
    // Prevents memory access violations when the current node is a nullptr
    if (!isLeaf(currentNode))
    {
        if (soughtKey == currentNode->key)
        {
            if (!isLeaf(currentNode->leftChild) && !isLeaf(currentNode->rightChild))
            {
                // Makes a copy of the next minimum node and sets the current node
                // to have the same values
                Node* minimumNode = detachMinimumNode(currentNode->rightChild);
                currentNode->key = minimumNode->key;
                currentNode->item = minimumNode->item;
            }
            else if(isLeaf(currentNode->leftChild) && isLeaf(currentNode->rightChild))
            {
                // The current node does not have any children so it can be set as a nullptr
                currentNode = leaf();
            }
            // Points at the next node in line
            else if(!isLeaf(currentNode->leftChild))
            {
                // Need to manually deallocate the current node
                Node* tempNode = currentNode;
                currentNode = currentNode->leftChild;
                delete(tempNode);
            }
            else if(!isLeaf(currentNode->rightChild))
            {
                // Need to manually deallocate the current node
                Node* tempNode = currentNode;
                currentNode = currentNode->rightChild;
                delete(tempNode);
            }
        }
        else if (soughtKey < currentNode->key)
        {
            removeRec(currentNode->leftChild, soughtKey);
        }
        else if (soughtKey > currentNode->key)
        {
            removeRec(currentNode->rightChild, soughtKey);
        }
    }
} // The recursive function to remove a desired node from the BST

BST::Node* BST::detachMinimumNode(Node * & currentNode)
{
    // Stops the recursion if the left child is a leaf as the minimum has been found
    if(isLeaf(currentNode->leftChild))
    {
        // Copies the minimum node and then deletes it
        Node* minimumNode = new Node(currentNode->key, currentNode->item);
        currentNode = leaf();
        return minimumNode;
    }
    else
    {
        return detachMinimumNode(currentNode->leftChild);
    }
} // Returns a copy of the minimum node

void BST::displayEntries()
{
    inOrderTraversal(this->root);
} // Used as a wrapper to display the BST in key order

void BST::inOrderTraversal(Node * & currentNode)
{
    // Prevents memory access violations when the current node is a nullptr
    if (!isLeaf(currentNode))
    {
        // Traverses the left branch
        if (!isLeaf(currentNode->leftChild))
        {
            inOrderTraversal(currentNode->leftChild);
        }

        int height = 0;
        height = findBranchHeight(currentNode, height);
        std::cout << currentNode->key << '\t' << currentNode->item << '\t' << height << std::endl;

        // Traverses the right branch
        if (!isLeaf(currentNode->rightChild))
        {
            inOrderTraversal(currentNode->rightChild);
        }
    }
} // Prints the key/item pairs in a sorted list

void BST::displayTree()
{
    if (!isLeaf(root))
    {
        preOrderDisplay(this->root, "");
    }
    else
    {
        std::cout << '*' << std::endl;
    }
} // Used to print the tree graphically(ish)

void BST::preOrderDisplay(Node * &currentNode, std::string whiteSpace)
{
    // 'whiteSpace' is used to help with the display formatting

    std::cout << whiteSpace << currentNode->key << ' ' << currentNode->balanceFactor << std::endl;

    // Effectively increments the whitespace
    whiteSpace += "\t";

    // Traverses the left branch or prints '*' if the branch stops there
    if (!isLeaf(currentNode->leftChild))
    {
        preOrderDisplay(currentNode->leftChild, whiteSpace);
    }
    else
    {
       std::cout << whiteSpace << "*" << std::endl;
    }

    // Traverses the right branch or prints '*' if the branch stops there
    if (!isLeaf(currentNode->rightChild))
    {
        preOrderDisplay(currentNode->rightChild, whiteSpace);
    }
    else
    {
        std::cout << whiteSpace << "*" << std::endl;
    }
} // Prints the tree in its graphical form

void BST::deepDelete(Node * current)
{
    if (!isLeaf(current))
    {
        deepDelete(current->leftChild);

        deepDelete(current->rightChild);

        delete(current);
    }
}

BST::Node* BST::deepCopy(Node * originalNode)
{
    if (!isLeaf(originalNode))
    {
        Node* newNode = new Node(originalNode->key, originalNode->item);
        newNode->leftChild = deepCopy(originalNode->leftChild);
        newNode->rightChild = deepCopy(originalNode->rightChild);
        newNode->balanceFactor = originalNode->balanceFactor;
        return newNode;
    }
    return originalNode;
}

void BST::rotateRight(Node* & localRoot)
{
    /*
    Node* b = localRoot;
    Node* a = b->leftChild;
    //Node* alpha = a->leftChild;
    Node* beta = a->rightChild;
    //Node* gamma = b->rightChild;

    localRoot = a;
    b->leftChild = beta;
    a->rightChild = b;
    */

    Node* oldRoot = localRoot;
    assert(!isLeaf(oldRoot));
    Node* newRoot = localRoot->leftChild;
    assert(!isLeaf(newRoot));
    Node* newChild = newRoot->rightChild;

    oldRoot->balanceFactor = oldRoot->balanceFactor + 1 + std::max(-(newRoot->balanceFactor), 0);
    newRoot->balanceFactor = newRoot->balanceFactor + 1 + std::max(oldRoot->balanceFactor, 0);

    localRoot = newRoot;
    oldRoot->leftChild = newChild;
    newRoot->rightChild = oldRoot;
}

void BST::rotateLeft(Node* & localRoot)
{
    Node* oldRoot = localRoot;
    assert(!isLeaf(oldRoot));
    Node* newRoot = oldRoot->rightChild;
    assert(!isLeaf(newRoot));
    Node* newChild = newRoot->leftChild;

    oldRoot->balanceFactor = oldRoot->balanceFactor - 1 - std::max(newRoot->balanceFactor, 0);
    newRoot->balanceFactor = newRoot->balanceFactor - 1 - std::max(oldRoot->balanceFactor, 0);

    localRoot = newRoot;
    oldRoot->rightChild = newChild;
    newRoot->leftChild = oldRoot;
}

bool BST::rebalance(Node* &localRoot)
{
    return false;
    // returns true if sub-tree height decreased
}
