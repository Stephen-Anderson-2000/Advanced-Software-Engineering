#include "hashtable.h"

HashTable::HashTable()
{

}

unsigned int HashTable::hash(KeyType key)
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
