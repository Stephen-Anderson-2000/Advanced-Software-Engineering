#include <iostream>

struct Coordinate
{
    void display();
    Coordinate(int x, int y);

    int xCo;
    int yCo;
    static bool isEqual(Coordinate, Coordinate);
};
