#include "Coordinate.h"

Coordinate::Coordinate(int x, int y)
{
    this -> xCo = x;
    this -> yCo = y;
}

void Coordinate::display()
{
    std::cout << '(' << xCo << ',' << yCo << ')' << std::endl;
}

bool Coordinate::isEqual(Coordinate c1, Coordinate c2)
{
    return (c1.xCo == c2.xCo && c1.yCo == c2.yCo);
}
