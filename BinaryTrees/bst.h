#ifndef BST_H
#define BST_H

#include <string>

template<typename K, typename I>
class BST
{
public:
    //using KeyType = int;
    //using ItemType = std::string;
    using KeyType = K;
    using ItemType = I;
    BST() = default;
    ~BST();
    BST(const BST<KeyType, ItemType> &);
    BST<KeyType, ItemType> & operator=(const BST &);
    BST(BST<KeyType, ItemType> &&);
    BST<KeyType, ItemType> & operator=(BST<KeyType, ItemType> &&);
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
    void insertRec(KeyType, ItemType, Node* &);
    void inOrderTraversal(Node* &);
    void preOrderDisplay(Node* &, std::string);
    void removeRec(Node* &, KeyType);
    Node* detachMinimumNode(Node* &);
    void deepDelete(Node*);
    Node* deepCopy(Node*);
    void rotateRight(Node* &); // Should only be called when the node being passed in has a valid left child node
    void rotateLeft(Node* &); // Should only be called when the node being passed in has a valid right child node
};

template<typename K, typename I>
struct BST<K, I>::Node
{
    Node(KeyType, ItemType);
    BST::KeyType key;
    BST::ItemType item;
    Node* leftChild;
    Node* rightChild;
}; // A struct to store the key, item and the pointers to the next nodes

template<typename K, typename I>
BST<K, I>::BST(const BST & originalTree)
{
    this->root = deepCopy(originalTree.root);
}

template<typename K, typename I>
BST<K, I>::~BST()
{
    deepDelete(root);
}

template<typename K, typename I>
BST<K, I> & BST<K, I>::operator=(const BST<K, I> & original)
{
    if (this != &original)
    {
        deepDelete(this->root);
    }
    this->root = deepCopy(original.root);
    return *this;
}

template<typename K, typename I>
BST<K, I>::BST(BST<K, I> && originalTree)
{
    this->root = originalTree.root;
    originalTree.root = leaf();
}

template<typename K, typename I>
BST<K, I> & BST<K, I>::operator=(BST<K, I> && originalTree)
{
    this->root = originalTree.root;
    if (this != &originalTree)
    {
        originalTree.root = leaf();
    }
    return *this;
}

template<typename K, typename I>
typename BST<K, I>::Node* BST<K, I>::leaf()
{
    return nullptr;
} // Just used as a way of describing when a node has become null

template<typename K, typename I>
bool BST<K, I>::isLeaf(Node* n)
{
    return (n == nullptr);
} // Returns true if the node is a nullptr

template<typename K, typename I>
BST<K, I>::Node::Node(KeyType key, ItemType item)
{
    this->key = key;
    this->item = item;
    // Errors can occur if the children are not explicitly set as leaves when initialised
    this->leftChild = BST::leaf();
    this->rightChild = BST::leaf();
} // Sets up the Node type and makes sure that the children are nullptrs

template<typename K, typename I>
typename BST<K, I>::ItemType* BST<K, I>::lookup(KeyType soughtKey)
{
    return lookupRec(soughtKey, this->root);
} // Essentially used like an API to interact with the recursive function

template<typename K, typename I>
typename BST<K, I>::ItemType* BST<K, I>::lookupRec(KeyType soughtKey, Node* currentNode)
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

template<typename K, typename I>
void BST<K, I>::insert(KeyType newKey, ItemType newItem)
{
    insertRec(newKey, newItem, this->root);
} // The wrapper used to insert a new item into a sorted BST

template<typename K, typename I>
void BST<K, I>::insertRec(KeyType newKey, ItemType newItem, Node* &currentNode)
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
    }
    else if (newKey > currentNode->key)
    {
        insertRec(newKey, newItem, currentNode->rightChild);
    }
} // Recursively iterates until it reaches the correct insertion point

template<typename K, typename I>
void BST<K, I>::remove(KeyType soughtKey)
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

template<typename K, typename I>
void BST<K, I>::removeRec(Node * & currentNode, KeyType soughtKey)
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

template<typename K, typename I>
typename BST<K, I>::Node* BST<K, I>::detachMinimumNode(Node * & currentNode)
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

template<typename K, typename I>
void BST<K, I>::displayEntries()
{
    inOrderTraversal(this->root);
} // Used as a wrapper to display the BST in key order

template<typename K, typename I>
void BST<K, I>::inOrderTraversal(Node * & currentNode)
{
    // Prevents memory access violations when the current node is a nullptr
    if (!isLeaf(currentNode))
    {
        // Traverses the left branch
        if (!isLeaf(currentNode->leftChild))
        {
            inOrderTraversal(currentNode->leftChild);
        }

        std::cout << currentNode->key << '\t' << currentNode->item << std::endl;

        // Traverses the right branch
        if (!isLeaf(currentNode->rightChild))
        {
            inOrderTraversal(currentNode->rightChild);
        }
    }
} // Prints the key/item pairs in a sorted list

template<typename K, typename I>
void BST<K, I>::displayTree()
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

template<typename K, typename I>
void BST<K, I>::preOrderDisplay(Node * &currentNode, std::string whiteSpace)
{
    // 'whiteSpace' is used to help with the display formatting

    std::cout << whiteSpace << currentNode->key << std::endl;

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

template<typename K, typename I>
void BST<K, I>::deepDelete(Node * current)
{
    if (!isLeaf(current))
    {
        deepDelete(current->leftChild);
        deepDelete(current->rightChild);

        delete(current);
    }
}

template<typename K, typename I>
typename BST<K, I>::Node* BST<K, I>::deepCopy(Node * originalNode)
{
    if (!isLeaf(originalNode))
    {
        Node* newNode = new Node(originalNode->key, originalNode->item);
        newNode->leftChild = deepCopy(originalNode->leftChild);
        newNode->rightChild = deepCopy(originalNode->rightChild);
        return newNode;
    }
    return originalNode;
}

template<typename K, typename I>
void BST<K, I>::rotateRight(Node* & localRoot)
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

    localRoot = newRoot;
    oldRoot->leftChild = newChild;
    newRoot->rightChild = oldRoot;
}

template<typename K, typename I>
void BST<K, I>::rotateLeft(Node* & localRoot)
{
    Node* oldRoot = localRoot;
    assert(!isLeaf(oldRoot));
    Node* newRoot = oldRoot->rightChild;
    assert(!isLeaf(newRoot));
    Node* newChild = newRoot->leftChild;

    localRoot = newRoot;
    oldRoot->rightChild = newChild;
    newRoot->leftChild = oldRoot;
}

#endif // BST_H
