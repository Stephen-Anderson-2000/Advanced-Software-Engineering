#ifndef UNORDEREDMAPLIST_H
#define UNORDEREDMAPLIST_H

#include <list>
#include <string>
#include <unordered_map>

class UnorderedMapList
{
public:
    UnorderedMapList(std::string);
    void Change_File(std::string);
    void Sort_Bricks();
    void Print_Sorted_Bricks();
private:
    std::string fileName;
    std::unordered_map<std::string, std::string> unsortedData;
    std::list< std::string> sortedData;
    std::pair<std::string, std::string> firstBrick;
    void Load_Data();
    void Sort_Eastern();
    void Sort_Western();
    std::pair<std::string, std::string> Find_Next_Brick(std::string);
    std::pair<std::string, std::string> Find_Next_Brick_By_Value(std::string);
};

#endif // UNORDEREDMAPLIST_H
