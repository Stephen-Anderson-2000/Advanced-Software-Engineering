#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <string>

class HashTable
{
public:
    using KeyType = std::string;
    using ItemType = std::string;

    HashTable();

    void insert(KeyType, ItemType);
    ItemType* lookup(KeyType);
    void remove(KeyType);

private:
    unsigned int hash(KeyType); // Only hashes the key from a string to integer, does not calculate the array index
};

#endif // HASHTABLE_H
