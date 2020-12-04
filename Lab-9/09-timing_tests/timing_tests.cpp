#include <iostream>
#include <chrono>
#include <utility>
#include <map>
#include <unordered_map>

#include "keyitemgenerator.h"
#include "../../BinaryTrees/bst.h"

using std::chrono::steady_clock;
// using std::chrono::milliseconds;
// using std::chrono::microseconds;
using std::chrono::nanoseconds;
using std::chrono::duration_cast;


/* timingTestMap(n) creates a std::map<int,string>, and then
 * performs mapSize random insertions followed by numberOfLookups random lookups.
 * The mean time taken (in nanoseconds) per lookup is returned.
 */
nanoseconds timingTest_map(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   //KeyItemGenerator gen = KeyItemGenerator();
   /*  If no seed value is given, each KeyItemGenerator object will
    *  produce the same sequence of keys/items.
    *  If we want different pseudo-random keys/items from each object
    *  (and each program run), then we should provide a seed value.
    *  For example:
    *
    *   unsigned int seed = steady_clock::now().time_since_epoch().count();
    *   KeyItemGenerator gen = KeyItemGenerator(seed);
   */

   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   std::map<int,std::string> dict = {};

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert( {gen.randomKey(),gen.randomItem()} );
       /* Note: std::map::insert() differs slightly from BST::insert(),
        * in that it does not overwrite the item if the key already exists.
        */
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.find(gen.randomKey());
       /* Note: std::map::find() differs slightly from BST::lookup(),
        * in that it returns an *iterator* to the key/item pair.
        */
   }

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   nanoseconds meanTimePerLookup = timeTaken / numberOfLookups;

   return meanTimePerLookup;
}

nanoseconds timingTest_unordered_map(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   //KeyItemGenerator gen = KeyItemGenerator();

   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   std::unordered_map<int,std::string> dict = {};

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert( {gen.randomKey(),gen.randomItem()} );
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.find(gen.randomKey());
   }

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   nanoseconds meanTimePerLookup = timeTaken / numberOfLookups;

   return meanTimePerLookup;
}

nanoseconds timingTest_bst(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   //KeyItemGenerator gen = KeyItemGenerator();

   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   BST<int, std::string> dict;

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert(gen.randomKey(),gen.randomItem());
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.lookup(gen.randomKey());
   }

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   nanoseconds meanTimePerLookup = timeTaken / numberOfLookups;

   return meanTimePerLookup;
}


void BenchmarkOrderedMap()
{
    // This is the parameter we're measuring against.
    // We're interested in how time performance scales against dictionary size.
    const unsigned long long int dictSize = 5000000;

    // Adjust the number of lookups as needed to ensure that the tests run in a reasonable amount of time.
    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_map(dictSize,numberOfLookups);

    std::cout << "Data structure: std::map (red-black tree)"                                    << std::endl;
    std::cout << "Dictionary size: " << dictSize << " random entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " random lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void BenchmarkUnorderedMap()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_unordered_map(dictSize, numberOfLookups);

    std::cout << "Data structure: std::unordered_map"                                           << std::endl;
    std::cout << "Dictionary size: " << dictSize << " random entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " random lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void BenchmarkBST()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_unordered_map(dictSize, numberOfLookups);

    std::cout << "Data structure: BST"                                                          << std::endl;
    std::cout << "Dictionary size: " << dictSize << " random entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " random lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}


int main()
{
    //BenchmarkOrderedMap();
    //std::cout << '\n' << std::endl;
    //BenchmarkUnorderedMap();
    //std::cout << '\n' << std::endl;
    BenchmarkBST();
    return 0;
}


