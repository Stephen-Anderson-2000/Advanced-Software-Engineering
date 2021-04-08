#include "maplist.h"

#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

MapList::MapList(std::string fileName)
{
    this->fileName = fileName;
    this->firstBrick.first = "";
    this->firstBrick.second = "";
    Load_Data();
}

void MapList::Change_File(std::string newFileName)
{
    this->fileName = newFileName;
    Load_Data();
}

void MapList::Load_Data()
{
    this->unsortedData.clear();

    std::ifstream inputFile;
    inputFile.open(fileName);

    std::string line;
    while (std::getline(inputFile, line))
    {
        std::istringstream myStrStream(line);
        std::string south, north;
        std::getline(myStrStream, north, ',');
        std::getline(myStrStream, south, ',');

        // Sets a starting point for later use
        // Had difficulty using begin() which is why this is done
        if (firstBrick.first == "")
        {
            this->firstBrick.first = north;
            this->firstBrick.second = south;
        }

        this->unsortedData.insert(std::make_pair(north, south));
    }
    inputFile.close();
}

void MapList::Invert_Data()
{
    std::map<std::string, std::string> invertedData;

    for (auto brick : this->unsortedData)
    {
        invertedData.insert(std::make_pair(brick.second, brick.first));
    }
    this->unsortedData = invertedData;
}

void MapList::Print_Sorted_Bricks()
{
    // Found this from a stackoverflow response:
    // https://stackoverflow.com/questions/16229729/printing-out-contents-of-a-list-from-the-c-list-library
    // It is fairly compact and doesn't require explicit use of iterators
    for (auto brick : this->sortedData)
    {
        std::cout << brick << std::endl;
    }
}

void MapList::Sort_Bricks()
{
    // Sorts the bricks going West to East
    Sort_Eastern();

    Invert_Data();

    // Sorts the bricks going East to West
    Sort_Western();
}

void MapList::Sort_Eastern()
{
    bool sorting = true;
    // Declared outside of the loop to allow for longer persisted values
    std::pair<std::string, std::string> currentBrick = this->firstBrick;

    this->sortedData.push_back(currentBrick.second);

    // Keeps looping until the Eastern most brick is reached
    while (sorting)
    {
        currentBrick = Find_Next_Brick(currentBrick.second);
        // Next brick was not found
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

void MapList::Sort_Western()
{
    bool sorting = true;
    // Declared outside of the loop to allow for longer persisted values
    std::pair<std::string, std::string> currentBrick = this->firstBrick;

    // Keeps looping until the Western most brick is reached
    while (sorting)
    {
        currentBrick = Find_Next_Brick(currentBrick.second);
        // Next brick was not found
        if (currentBrick.first == "")
        {
            sorting = false;
        }
        else
        {
            this->sortedData.push_front(currentBrick.second);
        }
    }
}

std::pair<std::string, std::string> MapList::Find_Next_Brick(std::string key)
{
    // Creates an iterator to the found location
    auto found = this->unsortedData.find(key);
    if (found != unsortedData.end())
    {
        // Creates a new pair
        std::pair<std::string, std::string> nextBrick;
        nextBrick.first = found->first;
        nextBrick.second = found->second;
        // Deletes the old pair from the structure
        this->unsortedData.erase(found);
        return nextBrick;
    }
    else
    {
        return std::make_pair("", "");
    }
}
