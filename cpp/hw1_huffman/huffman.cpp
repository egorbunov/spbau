#include <iostream>
#include "huffman.h"


namespace {
    struct HuffmanTree {
        au::byte_t byte;   // byte label, valid only if this node is leaf (both zeroChild and one pointers are null)
        size_t cnt;        // if node is leaf: cnt = number of occurrences of byte in file, else cnt = cnt(zeroChild)+
                           // cnt(oneChild)...
        HuffmanTree *zeroChild; // child: (this, child) edge marked by '0'
        HuffmanTree *oneChild;  // child: (this, child) edge marked by '1'

        HuffmanTree() {
            cnt = 0;
            zeroChild = nullptr;
            oneChild = nullptr;
        }

        /**
         * Please, be sure, that every node in tree is distinct!
         */
        ~HuffmanTree() {
            if (zeroChild != nullptr) {
                delete zeroChild;
            }
            if (oneChild != nullptr) {
                delete oneChild;
            }
        }

        HuffmanTree(HuffmanTree* zero, HuffmanTree* one) : zeroChild(zero), oneChild(one) {
            cnt = zeroChild->cnt + oneChild->cnt;
        }

        HuffmanTree(au::byte_t c, size_t cnt) : zeroChild(nullptr), oneChild(nullptr), byte(c), cnt(cnt) {
        }

        bool isLeaf() {
            return zeroChild == nullptr && oneChild == nullptr;
        }

        void setZeroChild(HuffmanTree* x) {
            zeroChild = x;
            cnt = zeroChild->cnt + oneChild->cnt;
        }

        void setOneChild(HuffmanTree* x) {
            oneChild = x;
            cnt = zeroChild->cnt + oneChild->cnt;
        }

        bool operator<(const HuffmanTree* x) const {
            return cnt < x->cnt;
        }
    };

    /**
     * Writes huffman codes extracted from given tree to given output file in next fashion:
     * Every byte binary coding is written as:
     *      [  byte  ][  descriptor  ][  binary code..]
     *      <-1 byte-><----1 byte----><----k bytes---->
     * There in first 5 bits of [  descriptor  ] number k is stored, and in the last 3 bits of that descriptor
     * last significant position of binary code in k byte is stored.
     */
    int dumpTree(FILE* out, const HuffmanTree& tree) {
        return 0;
    }


    /**
     * Builds huffman tree for given binary file
     */
    HuffmanTree* buildTree(FILE *inFile) {
        size_t counts[au::BYTE_NUMBER] = {0};

        while (true) {
            au::byte_t byte;
            if (std::fread(&byte, sizeof(au::byte_t), 1, inFile) != 1) {
                if (std::feof(inFile))
                    break;
                return nullptr;
            }
            counts[byte] += 1;
        }

        // building huffman tree!

        std::priority_queue<HuffmanTree *> heap;
        for (int i = 0; i < au::BYTE_NUMBER; ++i) {
            if (counts[i] > 0) {
                heap.push(new HuffmanTree(static_cast<au::byte_t>(i), counts[i]));
            }
        }

        HuffmanTree *zeroChild = nullptr;
        HuffmanTree *oneChild = nullptr;
        while (!heap.empty()) {
            zeroChild = heap.top();
            heap.pop();
            if (heap.empty())
                break;
            oneChild = heap.top();
            heap.pop();
            heap.push(new HuffmanTree(zeroChild, oneChild));
        }

        return zeroChild;
    }

    /**
     * Reads tree from given binary file. Tree is stored in form, which described
     * in doc. comment for {@code dumpTree} function
     *
     * @param in - binary input file stream
     */
    int readTree(FILE* inFile, HuffmanTree* tree) {
        return 0;
    }
}


void au::huffmanEncode(FILE* inFile, FILE* outFile) {

    HuffmanTree *tree = buildTree(inFile);
    if (tree != nullptr) {
        std::cout << (char) tree->byte << "; size = " << tree->cnt << std::endl;

    }

    if (tree != nullptr) {
        delete tree;
    }
}

void au::huffmanDecode(FILE* inFile, FILE* outFile) {

}
