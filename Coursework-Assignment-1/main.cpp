//#include "unorderedmaplist.h"
//#include "maplist.h"
#include "performancetests.cpp"

//#include <iostream>
//#include <string>

int main(int argc, char *argv[])
{
    //UnorderedMapList *myUnorderedMapList = new UnorderedMapList(argv[1]);
    //myUnorderedMapList->Sort_Bricks();
    //myUnorderedMapList->Print_Sorted_Bricks();

    //MapList *myMapList = new MapList(argv[1]);
    //myMapList->Sort_Bricks();
    //myMapList->Print_Sorted_Bricks();

    run_Unordered_Map_List_Timing_Tests();
    //run_Map_List_Timing_Tests();

    return 0;
}
