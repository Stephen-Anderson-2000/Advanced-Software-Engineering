#ifndef MAPLIST_H
#define MAPLIST_H

#include <list>
#include <map>
#include <string>

class MapList
{
public:
    MapList(std::string);
    void Change_File(std::string);
    void Sort_Bricks();
    void Print_Sorted_Bricks();
private:
    std::string fileName;
    std::map<std::string, std::string> unsortedData;
    std::list< std::string> sortedData;
    std::pair<std::string, std::string> firstBrick;
    void Load_Data();
    void Invert_Data();
    void Sort_Eastern();
    void Sort_Western();
    std::pair<std::string, std::string> Find_Next_Brick(std::string);
};

#endif // MAPLIST_H
