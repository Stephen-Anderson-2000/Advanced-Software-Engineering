#include <limits>

#include "keyitemgenerator.h"

// If no seed value provided, use 0.
KeyItemGenerator::KeyItemGenerator() : KeyItemGenerator(0) {}

KeyItemGenerator::KeyItemGenerator(unsigned int seed)
{
    eng = std::default_random_engine{seed};

    constexpr int maxInt = std::numeric_limits<int>::max();
    constexpr int minInt = std::numeric_limits<int>::min();

    intDist  = std::uniform_int_distribution<int>{minInt,maxInt};
    charDist = std::uniform_int_distribution<int>{'A','Z'};
}

int KeyItemGenerator::randomKey()
{
    return intDist(eng);
}

std::string KeyItemGenerator::randomItem()
{
    char c = static_cast<char>(charDist(eng));
    return std::string(1,c);
}
