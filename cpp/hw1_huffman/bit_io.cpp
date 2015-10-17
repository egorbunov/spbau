#include "bit_io.h"

size_t au::BitWriter::write(bool bit) {
    if (bit)
        byte |= 1 << (BITS_IN_BYTE - 1 - pos);
    pos += 1;

    if (pos == BITS_IN_BYTE) {
        if (std::fwrite(&byte, sizeof(byte_t), 1, outFile) != 1) {
            setFailed("Cannot write byte to given file!");
        }
    }
}

bool au::BitReader::read() {
    if (pos == BITS_IN_BYTE) {
        if (std::fread(&byte, sizeof(byte_t), 1, inFile) != 1) {
            setFailed("Cannot read next byte from given file!");
        }
        pos = 0;
    }

    bool ret =( (byte >> (BITS_IN_BYTE - 1 - pos) & 1)) != 0;
    pos += 1;
    return ret;

}
