#include <iostream>
#include <cstring>
#include "huffman.h"
#include "bit_io.h"

using namespace std;

namespace au {
    struct Job {
        bool doEncode;
        bool doDecode;
        std::string inFilename;
        std::string outFilename;

        Job() : doEncode(false), doDecode(false), inFilename(""), outFilename("") {
        }

        static au::Result<Job> parse(char *arguments[], int size) {
            Job job;
            for (int i = 0; i < size; ++i) {
                if (strcmp(arguments[i], "-c") == 0) {
                    job.doEncode = true;
                } else if (strcmp(arguments[i], "-u") == 0) {
                    job.doDecode = true;
                } else if (strcmp(arguments[i], "--file") == 0 || strcmp(arguments[i], "-f") == 0) {
                    if (i + 1 >= size)
                        return au::Result<Job>::error("No input file specified!", job);
                    job.inFilename = std::string(arguments[i + 1]);
                    i += 1;
                } else if (strcmp(arguments[i], "-o") == 0 || strcmp(arguments[i], "--output") == 0) {
                    if (i + 1 >= size)
                        return au::Result<Job>::error("No output file specified!", job);
                    job.outFilename = std::string(arguments[i + 1]);
                    i += 1;
                } else {
                    return au::Result<Job>::error("Can't understand that argument: " + string(arguments[i]), job);
                }
            }
            if (job.doDecode == job.doEncode) {
                return au::Result<Job>::error("Can't understand what to do with input file: "
                                                  "decode [-u] or encode [-c] ? =(", job);
            }
            if (job.inFilename == "") {
                return au::Result<Job>::error("Input file not specified!", job);
            }
            if (job.outFilename == "") {
                return au::Result<Job>::error("Output file not specified!", job);
            }
            return au::Result<Job>::ok(job);
        }
    };

    void printHelp() {
        std::cout << "========================= HELP =========================" << std::endl;
        std::cout << "Huffman (en/de)coder. vX.X" << std::endl;
        std::cout << "PURPOSE:" << std::endl;
        std::cout << "      Use it to compress and decompress binary files using huffman algorithm." << std::endl;
        std::cout << "USAGE: huffman (-c|-u) (-f|--file) PATH_TO_INPUT_FILE (-o|--output) PATH_TO_OUTPUT_FILE" << std::endl;
        std::cout << "  -c" << std::endl;
        std::cout << "      Key to encode input file and write compressed file in specified output file." << std::endl;
        std::cout << "  -u" << std::endl;
        std::cout << "      Key to decode (decompress) input file and write uncompressed "
                         "file in specified output file." << std::endl;
        std::cout << "  -f or --file" << std::endl;
        std::cout << "      Specify input file after that key" << std::endl;
        std::cout << "  -o or --output" << std::endl;
        std::cout << "      Specify output file after that key" << std::endl;
    }
}

int main(int argc, char* argv[]) {

    au::Result<au::Job> parseArgRes = au::Job::parse(argv + 1, argc - 1);

    if (!parseArgRes.isOk()) {
        std::cout << "ERROR: " << parseArgRes.msg() << std::endl;
        au::printHelp();
        return 1;
    }
    au::Job job = parseArgRes.getData();

    FILE* inFile = std::fopen(job.inFilename.c_str(), "rb");
    if (inFile == NULL) {
        std::cout << "Error: cannot open input file: [ " << job.inFilename << " ]" << std::endl;
        return 1;
    }
    FILE* outFile = std::fopen(job.outFilename.c_str(), "wb");
    if (outFile == NULL) {
        std::cout << "Error: cannot open input file: [ " << job.outFilename << " ]" << std::endl;
        return 1;
    }

    if (job.doEncode) {
        au::Result<int> encodeResult = au::huffmanEncode(inFile, outFile);
        if (!encodeResult.isOk()) {
            std::cout << "ERROR: " << encodeResult.msg() << std::endl;
        }
    } else if (job.doDecode) {
        au::Result<int> decodeResult = au::huffmanDecode(inFile, outFile);
        if (!decodeResult.isOk()) {
            std::cout << "ERROR: " << decodeResult.msg() << std::endl;
        }
    }

    std::fclose(inFile);
    std::fclose(outFile);
    return 0;
}