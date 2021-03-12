#include <iostream>
#include <chrono>
#include <fstream>
#include <future>
#include <string>
#include <thread>

#include "maplist.h"
#include "unorderedmaplist.h"

using std::chrono::steady_clock;
using std::chrono::nanoseconds;
using std::chrono::duration_cast;
using std::future;
using std::ofstream;
using std::string;
using std::thread;

const int NUMENTRIES = 10;

std::string fileAdditions[NUMENTRIES] = { "20", "50", "100", "200", "500", "1K", "2K",
                                          "5K", "10K", "20K"/*, "50K", "100K", "200K",
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

void Time_UnorderedMapList(nanoseconds timingResults[5], int threadNum, string fileName)
{
    nanoseconds timeToSort = timingTest_Unordered_Map_List(fileName);
    timingResults[threadNum] = timeToSort;
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
    ofstream resultsFile("Unordered Map -1 Umap - List Results.csv");
    resultsFile << "Number of Entries,Time Taken to Sort,,,,,Average\n";
    resultsFile << ",1,2,3,4,5\n";
    for (int i = 0; i < NUMENTRIES; i++)
    {
        string fileName = "../Great_Wall_Problem-test_data/" + fileAdditions[i] + "/";
        fileName += "input-pairs-" + fileAdditions[i] + ".txt";
        std::cout << fileName << std::endl;
        resultsFile << fileAdditions[i];

        nanoseconds timingResults[5];
        thread t1(Time_UnorderedMapList, timingResults, i, fileName);
        thread t2(Time_UnorderedMapList, timingResults, i, fileName);
        thread t3(Time_UnorderedMapList, timingResults, i, fileName);
        thread t4(Time_UnorderedMapList, timingResults, i, fileName);
        thread t5(Time_UnorderedMapList, timingResults, i, fileName);

        t1.join();
        t2.join();
        t3.join();
        t4.join();
        t5.join();

        for (int j = 0; j < 5; j++)
        {
            //nanoseconds timeToSort = timingTest_Unordered_Map_List(fileName);
            //resultsFile << "," << timeToSort.count();
            resultsFile << "," << timingResults[j].count();
        }

        resultsFile << "\n";
    }
    resultsFile.close();
}

void run_Map_List_Timing_Tests()
{
    ofstream resultsFile("Map List -1 Map - Results.csv");
    resultsFile << "Number of Entries,Time Taken to Sort,\n";
    resultsFile << ",1,2,3,4,5\n";
    for (int i = 0; i < NUMENTRIES; i++)
    {
        string fileName = "../Great_Wall_Problem-test_data/" + fileAdditions[i] + "/";
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
