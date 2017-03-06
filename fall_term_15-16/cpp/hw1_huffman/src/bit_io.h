#ifndef BIT_IO_H_INCLUDED__
#define BIT_IO_H_INCLUDED__

#include <iosfwd>
#include <bits/ios_base.h>
#include <iostream>
#include "common_defs.h"

namespace au {
    class MayFail {
    protected:
        bool isError;
        std::string msg;

        MayFail() {
            isError = false;
            msg = "";
        }

        void setFailed(std::string msg) {
            isError = true;
            this->msg = msg;
        }

    public:
        bool doesErrorOccurred() {
            return isError;
        }

        std::string getMessage() {
            return msg;
        }
    };

    class BitWriter : public MayFail {
    public:
        BitWriter(FILE* pFile) : outFile(pFile), byte(0), pos(0) {}
        /**
         * Writes bit to {@code ofstream}, which was set using {@code setOFStream} method
         *
         * WARNING: be sure not to change accidentally output stream write pointer between bit write operations...
         *
         * @return number of bits left in byte, where given bit was written
         */
        size_t write(bool bit);
        void flush();
    private:
        FILE* outFile;
        byte_t byte; // byte, which we filling
        size_t pos; // position in byte [0|2|..|7] where to write next bit (from left (0) to right (7))
    };

    class BitReader : public MayFail {
    public:
        BitReader(FILE* pFile) : inFile(pFile), byte(0), pos(BITS_IN_BYTE), isEOF(false) {}

        /**
         * Read bit from input file stream
         *
         * WARNING: be sure not to change accidentally input stream get pointer between bit read operations...
         */
        bool read();

        /**
         * returns number of bits left to read from byte
         */
        size_t bitsLeft() {
            if (pos == au::BITS_IN_BYTE)
                return au::BITS_IN_BYTE;
            return au::BITS_IN_BYTE - pos - 1;
        }

        bool eof() {
            return isEOF;
        }

        void finishByte();

    private:
        FILE* inFile;
        byte_t byte; // byte, which we reading
        size_t pos; // position in byte from where next bit will be read
        bool isEOF;
    };

    void printBinary(FILE* in);
}

#endif //BIT_IO_H_INCLUDED__
