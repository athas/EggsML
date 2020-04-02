#include <iostream>
#include <random>

using namespace std;

int main() {
  random_device rd;
  mt19937 gen(rd());
  uniform_int_distribution<> digit(0, 9);
  uniform_real_distribution<> dist(0.0, 1.0);

  char c;
  bool rum = false;

  if (dist(gen) < 0.1) {
    cout << "STJERNEDATO "
         << digit(gen) << digit(gen) << digit(gen) << digit(gen) << digit(gen)
         << "." << digit(gen) << digit(gen) << ":\n";
  }
  while (cin.read(&c, 1)) {
    if (rum) {
      cout << "rum";
      if (isdigit(c)) {
        cout << "-";
      }
      rum = false;
    }
    cout << c;
    if (c == ' ' && dist(gen) < 0.1) {
      rum = true;
    }
  }
  return EXIT_SUCCESS;
}
