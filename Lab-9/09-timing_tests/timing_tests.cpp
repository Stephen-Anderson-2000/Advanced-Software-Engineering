#include <iostream>
#include <chrono>
#include <utility>
#include <map>
#include <unordered_map>

#include "keyitemgenerator.h"
#include "../../BinaryTrees/bst.h"
#include "../../BinaryTrees_Templated/bst.h"

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

nanoseconds timingTest_map_ordered_insert(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   std::map<unsigned long long int,std::string> dict = {};

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert( {i, gen.randomItem()} );
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.find(mapSize);
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

nanoseconds timingTest_unordered_map_ordered_insert(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   //KeyItemGenerator gen = KeyItemGenerator();

   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   std::unordered_map<unsigned long long int,std::string> dict = {};

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert( {i, gen.randomItem()} );
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.find(mapSize);
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

   BST/*_Templated<unsigned long long int, std::string>*/ dict;

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert(gen.randomKey(), gen.randomItem());
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

nanoseconds timingTest_bst_sorted_insert(unsigned long long int mapSize, unsigned long long int numberOfLookups)
{
   //KeyItemGenerator gen = KeyItemGenerator();

   unsigned int seed = steady_clock::now().time_since_epoch().count();
   KeyItemGenerator gen = KeyItemGenerator(seed);

   BST/*_Templated<usigned long long int, std::string>*/ dict;

   for (unsigned long long int i = 0; i < mapSize; ++i)
   {
       dict.insert(i, gen.randomItem());
   }

   steady_clock::time_point startTime = steady_clock::now();

   for (unsigned long long int i = 0; i < numberOfLookups; ++i)
   {
       dict.lookup(mapSize);
   }

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   nanoseconds meanTimePerLookup = timeTaken / numberOfLookups;

   return meanTimePerLookup;
}

void Benchmark_Ordered_Map()
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

void Benchmark_Ordered_Map_Sorted_Insert()
{
    // This is the parameter we're measuring against.
    // We're interested in how time performance scales against dictionary size.
    const unsigned long long int dictSize = 5;

    // Adjust the number of lookups as needed to ensure that the tests run in a reasonable amount of time.
    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_map_ordered_insert(dictSize, numberOfLookups);

    std::cout << "Data structure: std::map (red-black tree)"                                    << std::endl;
    std::cout << "Dictionary size: " << dictSize << " sorted entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " last entry lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void Benchmark_Unordered_Map()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_unordered_map(dictSize, numberOfLookups);

    std::cout << "Data structure: std::unordered_map"                                           << std::endl;
    std::cout << "Dictionary size: " << dictSize << " random entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " random lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void Benchmark_Unordered_Map_Sorted_Insert()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_unordered_map(dictSize, numberOfLookups);

    std::cout << "Data structure: std::unordered_map"                                           << std::endl;
    std::cout << "Dictionary size: " << dictSize << " sorted entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " last entry lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void Benchmark_BST()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_bst(dictSize, numberOfLookups);

    std::cout << "Data structure: BST"                                                          << std::endl;
    std::cout << "Dictionary size: " << dictSize << " random entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " random lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

void Benchmark_BST_Sorted_Insert()
{
    const unsigned long long int dictSize = 5000000;

    const unsigned long long int numberOfLookups = 10000;

    nanoseconds meanTimePerLookup = timingTest_bst(dictSize, numberOfLookups);

    std::cout << "Data structure: BST"                                                          << std::endl;
    std::cout << "Dictionary size: " << dictSize << " sorted entries inserted."                 << std::endl;
    std::cout << "Performing " << numberOfLookups << " last entry lookups."                         << std::endl;
    std::cout << "Mean time taken per lookup: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;
}

int main()
{
    // Random insertions and lookups
    //Benchmark_Ordered_Map();
    //Benchmark_Ordered_Map_Sorted_Insert();
    //std::cout << '\n' << std::endl;
    //std::cout << '\n' << std::endl;
    //Benchmark_Unordered_Map();
    //std::cout << '\n' << std::endl;
    //Benchmark_Unordered_Map_Sorted_Insert();
    //std::cout << '\n' << std::endl;
    //Benchmark_BST();
    //std::cout << '\n' << std::endl;
    //Benchmark_BST_Sorted_Insert();
    //std::cout << '\n' << std::endl;
    return 0;
}


