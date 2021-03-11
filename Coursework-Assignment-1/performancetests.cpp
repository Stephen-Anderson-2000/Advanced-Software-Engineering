#include <iostream>
#include <chrono>
#include <fstream>
#include <string>

#include "maplist.h"
#include "unorderedmaplist.h"

using std::chrono::steady_clock;
using std::chrono::nanoseconds;
using std::chrono::duration_cast;

const int NUMENTRIES = 12;

std::string fileAdditions[NUMENTRIES] = { "20", "50", "100", "200", "500", "1K", "2K",
                                          "5K", "10K", "20K", "50K", "100K"/*, "200K",
                                          "500K", "1M", "2M", "3M"*/};

nanoseconds timingTest_Unordered_Map_List(std::string fileName)
{
   UnorderedMapList *dict = new UnorderedMapList(fileName);

   steady_clock::time_point startTime = steady_clock::now();

   dict->Sort_Bricks();

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   return timeTaken;
}

nanoseconds timingTest_Map_List(std::string fileName)
{
   MapList *dict = new MapList(fileName);

   steady_clock::time_point startTime = steady_clock::now();

   dict->Sort_Bricks();

   steady_clock::time_point finishTime = steady_clock::now();

   nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);

   return timeTaken;
}

void run_Unordered_Map_List_Timing_Tests()
{
    std::ofstream resultsFile("Unordered Map List Results.csv");
    resultsFile << "Number of Entries,Time Taken to Sort,,,,,Average\n";
    resultsFile << ",1,2,3,4,5\n";
    for (int i = 0; i < NUMENTRIES; i++)
    {
        std::string fileName = "../Great_Wall_Problem-test_data/" + fileAdditions[i] + "/";
        fileName += "input-pairs-" + fileAdditions[i] + ".txt";
        std::cout << fileName << std::endl;
        resultsFile << fileAdditions[i];
        for (int j = 0; j < 5; j++)
        {
            nanoseconds timeToSort = timingTest_Unordered_Map_List(fileName);
            resultsFile << "," << timeToSort.count();
        }
        resultsFile << "\n";
    }
    resultsFile.close();
}

void run_Map_List_Timing_Tests()
{
    std::ofstream resultsFile("Map List Results.csv");
    resultsFile << "Number of Entries,Time Taken to Sort,\n";
    resultsFile << ",1,2,3,4,5\n";
    for (int i = 0; i < NUMENTRIES; i++)
    {
        std::string fileName = "../Great_Wall_Problem-test_data/" + fileAdditions[i] + "/";
        fileName += "input-pairs-" + fileAdditions[i] + ".txt";
        std::cout << fileName << std::endl;
        resultsFile << fileAdditions[i];
        for (int j = 0; j < 5; j++)
        {
            nanoseconds timeToSort = timingTest_Unordered_Map_List(fileName);
            resultsFile << "," << timeToSort.count() ;
        }
        resultsFile << "\n";
    }
    resultsFile.close();
}
