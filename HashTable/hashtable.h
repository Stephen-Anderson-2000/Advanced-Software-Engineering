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
    unsigned int calculateIndex(KeyType); // Performs the other half of the hashing method to find the index
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
    unsigned int index = calculateIndex(k);
    if (buckets[index].key == k)
    {
        return &buckets[index].item;
    }
    else
    {
        return nullptr;
    }
}

template<typename K, typename I>
void HashTable<K, I>::insert(KeyType k, ItemType i)
{
    unsigned int index = calculateIndex(k);
    if(buckets[index].key == k)
    {
        buckets[index].item = i;
    }
    else if(buckets[index].key != k)
    {
        buckets[index].key = k;
        buckets[index].item = i;
    }
}

template<typename K, typename I>
void HashTable<K, I>::remove(KeyType k)
{
    return;
}


template<typename K, typename I>
unsigned int HashTable<K, I>::hash(KeyType k)
{

    // The value 5381 is a specific one chosen by the creator
    unsigned int hashVal = 5381;

    char c;
    /*
    for(int i = 0; i < k.size(); i++){
        c = k[i++];
        hashVal = hashVal * 33 + c;
    }
    */
    hashVal = k;

    return hashVal;
} // Uses the djb2 hashing algorithm

template<typename K, typename I>
unsigned int HashTable<K, I>::calculateIndex(KeyType k)
{
    //return (hash(k) % buckets.size());
    return (hash(k) % buckets.size());
}

#endif // HASHTABLE_H
