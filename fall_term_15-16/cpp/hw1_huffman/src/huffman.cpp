#include <iostream>
#include "huffman.h"
#include "Result.h"
#include "FixedCapacityArray.h"
#include "bit_io.h"


namespace {
    struct HuffmanTree {
        au::byte_t byte;   // byte label, valid only if this node is leaf (both zeroChild and one pointers are null)
        size_t cnt;        // if node is leaf: cnt = number of occurrences of byte in file, else cnt = cnt(zeroChild)+
                           // cnt(oneChild)...

        HuffmanTree *zeroChild; // child: (this, child) edge marked by '0'
        HuffmanTree *oneChild;  // child: (this, child) edge marked by '1'

        bool isUsed; // use it in euler path dfs
        HuffmanTree() : byte(0), cnt(0), zeroChild(nullptr), oneChild(nullptr), isUsed(false) {
        }

        HuffmanTree(HuffmanTree* zero, HuffmanTree* one) : zeroChild(zero), oneChild(one) {
            cnt = zeroChild->cnt + oneChild->cnt;
            byte = 0;
            isUsed = false;
        }

        HuffmanTree(au::byte_t c, size_t cnt) : byte(c), cnt(cnt), zeroChild(nullptr), oneChild(nullptr), isUsed(false) {
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

        bool isLeaf() const {
            return zeroChild == nullptr && oneChild == nullptr;
        }

        bool operator<(const HuffmanTree* x) const {
            return cnt < x->cnt;
        }
    };

    struct NodeComparator {
        bool operator () (const HuffmanTree* a, const HuffmanTree* b) const {
            return a->cnt >= b->cnt;
        }
    };

    /**
     *
     * Writes huffman codes extracted from given tree to given output file in next fashion:
     * Every byte binary coding is written as:
     *      [  byte  ][  descriptor  ][  binary code..]
     *      <-1 byte-><----1 byte----><----k bytes---->
     * There in first 5 bits of [  descriptor  ] number k is stored, and in the last 3 bits of that descriptor
     * number of not used bits in k-th byte stored
     *
     * Whole compressed file structure is:
     *      [  number of bits not used at end  ][  number of bytes in dict.  ][  {byte -> code} dict.][ codes ]
     *      <------------1 byte----------------><----------2 bytes-----------><------k records-------><-rest-->
     * Every dictionary record (byte binary coding) is stored as described above.
     *
     * @return Result object with integer, which is equal to size of compressed file
     */
    au::Result<int> dumpTreeAndEncode(FILE* in, FILE* out, HuffmanTree* tree) {
        au::FixedCapacityArray<bool> *codes[au::BYTE_NUMBER] = {nullptr};

        HuffmanTree* stack[2 * au::BYTE_NUMBER] = {nullptr};
        bool isOneChild[2 * au::BYTE_NUMBER] = {false};
        size_t stackSize = 0;
        stack[stackSize++] = tree;
        au::FixedCapacityArray<bool> code(au::BYTE_NUMBER); // actually the capacity better be equal to tree height , but...

        au::two_byte_t byteNum = 0;      // number of bytes which occurred in file
        size_t finalLenInBits = 0; // len in bits of whole file after encoding

        while (stackSize > 0) {
            HuffmanTree *cur = stack[stackSize - 1];
            if (cur->isUsed) {
                code.pop();
                stackSize -= 1; // pop
                continue;
            }
            cur->isUsed = true;
            code.push(isOneChild[stackSize - 1]);
            if (cur->isLeaf()) {
                codes[cur->byte] = new au::FixedCapacityArray<bool>(code.getSize());
                code.copyTo(*codes[cur->byte]);
                finalLenInBits += cur->cnt * code.getSize();
                assert(((int) byteNum) < (1 << au::BITS_IN_BYTE)); // overflow check...
                byteNum += 1;
            } else {
                isOneChild[stackSize] = true;
                stack[stackSize] = cur->oneChild;
                stackSize += 1;
                isOneChild[stackSize] = false;
                stack[stackSize] = cur->zeroChild;
                stackSize += 1;
            }
            if (cur == tree)
                code.pop();
        }

        // calculating number of bytes of given file after compression (without header with tree)
        size_t finalByteNum = (finalLenInBits) / au::BITS_IN_BYTE;
        if (finalLenInBits % au::BITS_IN_BYTE != 0)
            finalByteNum += 1;

        // calculating number of bits, which will not be used at end of the file,
        // so at decoding step we need to read bits till that last not used bits.
        au::byte_t notUsedAtEnd = static_cast<au::byte_t>(finalByteNum * au::BITS_IN_BYTE - finalLenInBits);

        size_t finalFileSize = 0; // in bytes

        // writing number of not used bits and number of bytes in dictionary
        if (std::fwrite(&notUsedAtEnd, sizeof(au::byte_t), 1, out) != 1) {
            return au::Result<int>::error("Cannot write to given file!", 0);
        }
        if (std::fwrite(&byteNum, sizeof(au::two_byte_t), 1, out) != 1) {
            return au::Result<int>::error("Cannot write to given file!", 0);
        }

        finalFileSize += finalByteNum + sizeof(au::byte_t) + sizeof(au::two_byte_t);

        au::BitWriter bitWriter(out);
        // writting dictionary
        for (size_t i = 0; i < au::BYTE_NUMBER; ++i) {
            if (codes[i] != nullptr) {
                au::FixedCapacityArray<bool> &curCode = *codes[i];
                au::byte_t byte = static_cast<au::byte_t>(i);

                // writing dictionary entry descriptor: [. . . . . || . . .]
                //                                      <--5 bits--><3 bits>
                // First 5 bits = number of bytes for bit code
                // Second 3 bits = number of unused bits in last byte of bit code
                size_t bytesCnt = (curCode.getSize() / au::BITS_IN_BYTE)
                                  + (curCode.getSize() % au::BITS_IN_BYTE != 0 ? 1 : 0);
                size_t notUsedBitsCnt = au::BITS_IN_BYTE * bytesCnt - curCode.getSize();
                assert((bytesCnt < (1 << 5)) && (notUsedBitsCnt < (1 << 3)));
                au::byte_t recordDescriptor = static_cast<au::byte_t >((bytesCnt << 3) | notUsedBitsCnt);

                // writting byte and code descriptor
                if (std::fwrite(&byte, sizeof(au::byte_t), 1, out) != 1) {
                    return au::Result<int>::error("Cannot write to given file!", 0);
                }
                if (std::fwrite(&recordDescriptor, sizeof(au::byte_t), 1, out) != 1) {
                    return au::Result<int>::error("Cannot write to given file!", 0);
                }
                // writing code
                for (size_t j = 0; j < curCode.getSize(); ++j) {
                    bitWriter.write(curCode[j]);
                }
                bitWriter.flush();

                finalFileSize += bytesCnt + 2;
            }
        }

        // writing compressed file
        while (true) {
            au::byte_t byte = 0;
            if (std::fread(&byte, sizeof(au::byte_t), 1, in) != 1) {
                if (std::feof(in))
                    break;
                return au::Result<int>::error("Cannot read bytes from given file!", 0);
            }
            assert(codes[byte] != nullptr);
            au::FixedCapacityArray<bool> &curCode = *codes[byte];
            for (size_t i = 0; i < curCode.getSize(); ++i) {
                bitWriter.write(curCode[i]);
            }
        }
        bitWriter.flush();

        // releasing allocated codes
        for (size_t i = 0; i < au::BYTE_NUMBER; ++i) {
            if (codes[i] != nullptr) {
                delete codes[i];
            }
        }
        return au::Result<int>::ok((int) finalFileSize);
    }

    /**
     * Builds huffman tree for given binary file
     * Warn: Remember to delete returned tree
     */
    au::Result<HuffmanTree*> buildTree(FILE *inFile) {
        size_t counts[au::BYTE_NUMBER] = {0};

        while (true) {
            au::byte_t byte = 0;
            if (std::fread(&byte, sizeof(au::byte_t), 1, inFile) != 1) {
                if (std::feof(inFile))
                    break;
                return au::Result<HuffmanTree*>::error("Cannot read bytes from given file!", nullptr);
            }
            counts[byte] += 1;
        }

        // building huffman tree!

        std::priority_queue<HuffmanTree*, std::vector<HuffmanTree*>, NodeComparator> heap;
        for (size_t i = 0; i < au::BYTE_NUMBER; ++i) {
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

        return au::Result<HuffmanTree*>::ok(zeroChild);
    }

    /**
     * Reads tree from given binary file. Tree is stored in form, which described
     * in doc. comment for {@code dumpTree} function
     *
     * @param in - binary input file stream
     */
    au::Result<HuffmanTree*> readTree(FILE* inFile) {
        au::two_byte_t dictSize;
        if (std::fread(&dictSize, sizeof(au::two_byte_t), 1, inFile) != 1) {
            return au::Result<HuffmanTree*>::error("Cannot read from given file!", nullptr);
        }
        au::BitReader bitReader(inFile);
        HuffmanTree* root = new HuffmanTree();
        for (size_t i = 0; i < dictSize; ++i) {
            au::byte_t byte;
            if (std::fread(&byte, sizeof(au::byte_t), 1, inFile) != 1) {
                return au::Result<HuffmanTree *>::error("Cannot read from given file!", nullptr);
            }

            au::byte_t descriptor;
            if (std::fread(&descriptor, sizeof(au::byte_t), 1, inFile) != 1) {
                return au::Result<HuffmanTree *>::error("Cannot read from given file!", nullptr);
            }

            size_t byteNum = descriptor >> 3;
            size_t unusedBitNum = static_cast<size_t>(descriptor - (byteNum << 3));

            HuffmanTree *cur = root;

            for (size_t j = 0; j < byteNum * au::BITS_IN_BYTE - unusedBitNum; ++j) {
                bool bit = bitReader.read();

                if (bit) {
                    if (cur->oneChild == nullptr)
                        cur->oneChild = new HuffmanTree();
                    cur = cur->oneChild;
                } else {
                    if (cur->zeroChild == nullptr)
                        cur->zeroChild = new HuffmanTree();
                    cur = cur->zeroChild;
                }
            }

            cur->byte = byte;
            cur->cnt = 0;
            bitReader.finishByte();
        }

        return au::Result<HuffmanTree*>::ok(root);
    }
}


au::Result<int> au::huffmanEncode(FILE* inFile, FILE* outFile) {
    // building Huffman tree firstly
    Result<HuffmanTree*> buildRes = buildTree(inFile);
    if (!buildRes.isOk())
        return Result<int>::error(buildRes.msg(), 0);
    HuffmanTree* tree = buildRes.getData();

    if (tree == nullptr) { // tree->cnt actually contains number of bytes in file
        return au::Result<int>::ok(0);
    }

    std::fseek(inFile, 0, SEEK_SET);
    Result<int> encodeRes = dumpTreeAndEncode(inFile, outFile, tree);

    std::cout << "Input file size = " << tree->cnt << std::endl;
    std::cout << "Compressed file size = " << encodeRes.getData() << std::endl;

    delete tree;

    if (!encodeRes.isOk()) {
        return au::Result<int>::error(encodeRes.msg(), 0);
    }
    return au::Result<int>::ok(0);
}

au::Result<int> au::huffmanDecode(FILE* inFile, FILE* outFile) {
    // get file size
    std::fseek(inFile, 0, SEEK_END);
    long fileSize = ftell(inFile); // may be O(fileSize)?
    if (fileSize < 0)
        return Result<int>::error("Cannot read from given input file!", 0);
    std::fseek(inFile, 0, SEEK_SET);

    if (fileSize == 0) {
        // just do nothing, because outFile is already created and already empty
        return au::Result<int>::ok(0);
    }

    // read unused at end of the file bit count
    byte_t unusedBitCnt = 0;
    if (std::fread(&unusedBitCnt, sizeof(byte_t), 1, inFile) != 1) {
        return Result<int>::error("Cannot read from given file!", 0);
    }
    // reading huffman tree
    au::Result<HuffmanTree*> treeReadRes = readTree(inFile);
    if (!treeReadRes.isOk()) {
        return Result<int>::error(treeReadRes.msg(), 0);
    }

    HuffmanTree *root = treeReadRes.getData();
    long bitsLeft = (fileSize - ftell(inFile)) * au::BITS_IN_BYTE - unusedBitCnt;

    BitReader bitReader(inFile);
    HuffmanTree *cur = root;
    for (int i = 0; i < bitsLeft; ++i) {
        bool bit = bitReader.read();
        if (bitReader.doesErrorOccurred())
            return Result<int>::error("Cannot read from input file!", 0);

        if (bit)
            cur = cur->oneChild;
        else
            cur = cur->zeroChild;

        if (cur->isLeaf()) {
            if (std::fwrite(&cur->byte, sizeof(au::byte_t), 1, outFile) != 1) {
                return au::Result<int>::error("Cannot write to given output file!", 0);
            }
            cur = root;
        }
    }
    delete root;
    return au::Result<int>::ok(0);
}
