#ifndef KEYITEMGENERATOR_H_121116
#define KEYITEMGENERATOR_H_121116

#include <string>
#include <random>

class KeyItemGenerator
{
  public:
    KeyItemGenerator();
    KeyItemGenerator(unsigned int seed);

    int randomKey();

    // Generates a random string consisting of a single uppercase Roman letter
    std::string randomItem();

  private:
    std::default_random_engine eng;
    std::uniform_int_distribution<int> intDist;
    std::uniform_int_distribution<int> charDist;
};

#endif
