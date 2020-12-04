#ifndef SINGLYLINKEDLIST_H
#define SINGLYLINKEDLIST_H

template<typename K, typename I>
struct SinglyLinkedList
{
    using KeyType = K;
    using ItemType = I;

    SinglyLinkedList();

    struct Node;
    Node* head;

    void insert(KeyType, ItemType);
    void insertRec(KeyType, ItemType, Node*);
    ItemType* lookup(KeyType);
    ItemType* lookupRec(KeyType, Node*);
};

template<typename K, typename I>
struct SinglyLinkedList<K, I>::Node {
    Node(K, I);
    K key;
    I item;
    struct Node* next = nullptr;
};

template<typename K, typename I>
SinglyLinkedList<K, I>::SinglyLinkedList()
{
    this->head = nullptr;
}

template<typename K, typename I>
void SinglyLinkedList<K, I>::insert(KeyType k, ItemType i)
{
    insertRec(k, i, this->head);
}

template<typename K, typename I>
void SinglyLinkedList<K, I>::insertRec(KeyType k, ItemType i, Node* currentNode)
{
    if (currentNode == nullptr)
    {
        Node* newNode = new Node(k, i);
        currentNode = newNode;
    }
    else if (currentNode->key == k)
    {
        currentNode->item = i;
    }
    else
    {
        insert(k, i, currentNode->next);
    }
}

template<typename K, typename I>
typename SinglyLinkedList<K, I>::ItemType* SinglyLinkedList<K, I>::lookup(KeyType k)
{
    lookupRec(k, this->head);
}

template<typename K, typename I>
typename SinglyLinkedList<K, I>::ItemType* SinglyLinkedList<K, I>::lookupRec(KeyType k, Node* currentNode)
{
    if (currentNode != nullptr)
    {
        if (currentNode->key == k)
        {
            return currentNode;
        }
        else
        {
            lookup(k, currentNode->next);
        }
    }
    else
    {
        return nullptr;
    }
}

#endif // SINGLYLINKEDLIST_H
