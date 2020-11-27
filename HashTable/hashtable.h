#ifndef HASHTABLE_H
#define HASHTABLE_H

//#include "singlylinkedlist.h"
#include <vector>

template<typename K, typename I>
class HashTable
{
public:
    //using KeyType = std::string;
    //using ItemType = std::string;
    using KeyType = K;
    using ItemType = I;

    HashTable(unsigned int size = 100);

    int getTableLength() { return buckets.size(); }

    void insert(KeyType, ItemType);
    ItemType* lookup(KeyType);
    void remove(KeyType);

private:
    struct Bucket;
    std::vector<Bucket> buckets;
    unsigned int hash(KeyType); // Only hashes the key from a string to integer, does not calculate the array index
};

template<typename K, typename I>
struct HashTable<K, I>::Bucket
{
    Bucket(K, I);
    K key;
    I item;
    //SinglyLinkedList<K, I>* entryList = new SinglyLinkedList<K, I>;
}; // A struct to store a key, item pair

template<typename K, typename I>
HashTable<K, I>::HashTable(unsigned int size)
{
    buckets.resize(size);
}

template<typename K, typename I>
typename HashTable<K, I>::ItemType* HashTable<K, I>::lookup(KeyType k)
{
    return nullptr;
}

template<typename K, typename I>
void HashTable<K, I>::insert(KeyType k, ItemType i)
{
    return;
}

template<typename K, typename I>
void HashTable<K, I>::remove(KeyType k)
{
    return;
}


template<typename K, typename I>
unsigned int HashTable<K, I>::hash(KeyType key)
{
    // The value 5381 is a specific one chosen by the creator
    unsigned long hashVal = 5381;
    for (auto character : key)
    {
        // 33 is chosen by the creator of the hashing function
        hashVal = hashVal * 33 + character;
    }
    return hashVal;
} // Uses the djb2 hashing algorithm

#endif // HASHTABLE_H
