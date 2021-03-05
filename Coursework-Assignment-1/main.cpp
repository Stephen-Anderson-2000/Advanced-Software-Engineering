#include "unorderedmaplist.h"
#include "maplist.h"

#include <iostream>
#include <string>
#include <vector>

int main(int argc, char *argv[])
{
    //std::string fileName = argv[1];
    UnorderedMapList *myUnorderedMapList = new UnorderedMapList(argv[1]);
    myUnorderedMapList->Sort_Bricks();
    //myUnorderedMapList->Print_Sorted_Bricks();

    MapList *myMapList = new MapList(argv[1]);
    myMapList->Sort_Bricks();
    myMapList->Print_Sorted_Bricks();

    return 0;
}
