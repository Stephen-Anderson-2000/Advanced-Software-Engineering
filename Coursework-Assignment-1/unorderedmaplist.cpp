#include "unorderedmaplist.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

UnorderedMapList::UnorderedMapList(std::string fileName)
{
    this->fileName = fileName;
    this->firstBrick.first = "";
    this->firstBrick.second = "";
    Load_Data();
}

void UnorderedMapList::Change_File(std::string newFileName)
{
    this->fileName = newFileName;
    Load_Data();
}

void UnorderedMapList::Load_Data()
{
    this->unsortedData.clear();
    std::string line;
    std::ifstream inputFile;

    inputFile.open(fileName);
    while (std::getline(inputFile, line))
    {
        std::istringstream ss(line);
        std::string south, north;
        std::getline(ss, north, ',');
        std::getline(ss, south, ',');

        this->unsortedData.insert(std::make_pair(north, south));

        if (firstBrick.first == "")
        {
            this->firstBrick.first = north;
            this->firstBrick.second = south;
        }
    }
    inputFile.close();
}

void UnorderedMapList::Print_Sorted_Bricks()
{
    for (auto brick : this->sortedData)
    {
        std::cout << brick << std::endl;
    }
}

void UnorderedMapList::Sort_Bricks()
{
    Sort_Eastern();
    Sort_Western();
}

void UnorderedMapList::Sort_Eastern()
{
    bool sorting = true;
    std::pair<std::string, std::string> currentBrick = this->firstBrick;

    this->sortedData.push_back(currentBrick.second);

    while (sorting)
    {
        currentBrick = Find_Next_Brick(currentBrick.second);
        if (currentBrick.first == "")
        {
            sorting = false;
        }
        else
        {
            this->sortedData.push_back(currentBrick.second);
        }
    }
}

void UnorderedMapList::Sort_Western()
{
    bool sorting = true;
    std::pair<std::string, std::string> currentBrick = this->firstBrick;

    this->sortedData.push_front(currentBrick.first);

    while (sorting)
    {
        currentBrick = Find_Next_Brick_By_Value(currentBrick.first);
        if (currentBrick.first == "" || this->sortedData.size() > this->unsortedData.size())
        {
            sorting = false;
        }
        else
        {
            this->sortedData.push_front(currentBrick.first);
        }
    }
}

std::pair<std::string, std::string> UnorderedMapList::Find_Next_Brick(std::string key)
{
    auto found = this->unsortedData.find(key);
    if (found != unsortedData.end())
    {
        std::pair<std::string, std::string> nextBrick;
        nextBrick.first = found->first;
        nextBrick.second = found->second;
        return nextBrick;
    }
    else
    {
        return std::make_pair("", "");
    }
}

std::pair<std::string, std::string> UnorderedMapList::Find_Next_Brick_By_Value(std::string value)
{
    auto found = unsortedData.begin();

    while (found != unsortedData.end())
    {
        if (found->second == value)
        {
            std::pair<std::string, std::string> nextBrick;
            nextBrick.first = found->first;
            nextBrick.second = found->second;
            return nextBrick;
        }
        found++;
    }
    return std::make_pair("", "");
}

