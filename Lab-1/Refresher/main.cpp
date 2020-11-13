#include <iostream>
#include "Coordinate.h"

using std::cout;
using std::endl;

using Hours = int;
using Minutes = int;
using Seconds = int;

void f(int i);
void g(int &i);
int factorial(int i);
void swap(Coordinate* & p1, Coordinate* & p2);

int main()
{
    int x = 3;
    f(x);
    cout << x << endl;
    g(x);
    cout << x << endl;
    cout << factorial(x) << endl;

    Coordinate p1 = Coordinate(1, 2);
    Coordinate p2(3, 7);
    p1.xCo = 4;
    p2.yCo = p2.yCo + 5;
    p1.display();
    p2.display();

    Coordinate p3 = Coordinate(1, 2);
    Coordinate* coPtr = new Coordinate(7, 8);

    Coordinate* p4 = new Coordinate(1, 2);
    Coordinate* p5 = new Coordinate(3, 4);
    swap(p4, p5);
    p4->display();

    Coordinate p6 = *coPtr;
    (*coPtr).display();
    coPtr->display();

    Coordinate* coPtr2 = &p6;

    return 0;
}

void f(int i)
{
    i++;
}

void g(int & i)
{
    i++;
}

int factorial(int i)
{
    if (i < 2)
    {
        return 1;
    }
    else
    {
        return (i * factorial(i - 1));
    }
}

void swap(Coordinate* & p1, Coordinate* & p2)
{
    Coordinate* temp = p1;
    p1 = p2;
    p2 = temp;
}
