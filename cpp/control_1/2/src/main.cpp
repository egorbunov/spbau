#include <iostream>
#include <fstream>
#include <string>
#include "mergesort.h"
#include "vector.h"

int main() {
    const std::string INPUT_FILENAME = "input.txt";
    const std::string OUTPUT_FILENAME = "output.txt";

    au::vector array;

    std::ifstream in(INPUT_FILENAME, std::ifstream::in);
    if (!in) {
        std::cout << "File ['" << INPUT_FILENAME << "'] not found!";
        return 1;
    }
    int x;
    while (in >> x) {
        array.add(x);
    }

    in.close();

    mergesort(array.get_raw_ptr(), array.get_size());

    std::ofstream out(OUTPUT_FILENAME, std::ofstream::out);

    for (size_t i = 0; i < array.get_size(); ++i) {
        out << array.at(i) << " ";
    }
    out << std::endl;

    out.close();
}
