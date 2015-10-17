#include <iostream>
#include "huffman.h"

using namespace std;

int main() {
    FILE* inFile = std::fopen("f.txt", "rb");
//
//    au::BitReader br(inFile);
//
//    for (int i = 0; i < 8; ++i) {
//        if (br.read()) {
//            cout << 1;
//        } else {
//            cout << 0;
//        }
//    }
//    cout << endl;
//
    FILE* outFile = std::fopen("t.txt", "w+b");
//    au::BitWriter bw(outFile);
//
//    char arr[] = "01100001";
//    for (int i = 0; i < 8; ++i) {
//        bw.write(arr[i] != '0');
//    }
//

    au::huffmanEncode(inFile, outFile);

    std::fclose(inFile);
    std::fclose(outFile);

    return 0;
}