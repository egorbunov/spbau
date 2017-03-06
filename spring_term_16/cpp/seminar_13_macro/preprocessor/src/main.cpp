#include <cstdio>
#include <cassert>
#include <vector>
#include <utility>
#include <cstdlib>
#include <ctime>
#include <map>
#include <string>

#include "myassert.h"
#include "mylog.h"
#include "instructions.h"

const char* instruction_name(InstructionCode code)
{
    static const std::map<InstructionCode, const char*> names {
        #define MAP_ELEM(b, d, l) { InstructionCode::INST_##b, #b },
            FOR_INSTRUCTIONS(MAP_ELEM)
        #undef MAP_ELEM
            { InstructionCode::INST_LAST, "no name" }
    };
    return names.at(code);
}

const char* instruction_description(InstructionCode code)
{
    static const std::map<InstructionCode, const char*> descriptions {
        #define MAP_ELEM(b, d, l) { InstructionCode::INST_##b, d },
            FOR_INSTRUCTIONS(MAP_ELEM)
        #undef MAP_ELEM
            { InstructionCode::INST_LAST, "no description" }
    };
    return descriptions.at(code);
}

std::vector<unsigned char> gen_rnd_instrs(const size_t instr_cnt)
{
    std::vector<unsigned char> buffer;

    // TODO

    return std::move(buffer);
}

void log_instrs(const std::vector<unsigned char> &instrs)
{
    // TODO
    (void)instrs;
}

void exec_instrs(const std::vector<unsigned char> &instrs)
{
    (void)instrs;
    // TODO this function should interpret instrs stream.
    // instrs stream parsing is similar to the one done in log_instrs
}

int main()
{
    std::srand(std::time(0));

    // Part 1
    bool assertion1_evaluated = false;
    myassert(assertion1_evaluated = true);
    assert((CONFIG_DEBUG && assertion1_evaluated)
            || (!CONFIG_DEBUG && !assertion1_evaluated));

    // Part 2
    LOG(INFO, "==============================");
    LOG(DEBUG, "%d %d %d %s", 1, 2, 3, "debug");
    LOG(INFO, "%d %d %s", 1, 2, "info");
    LOG(WARN, "%d %s", 1, "warning");
    LOG(ERROR, "%s", "error!");
    // LOG(LOG_LEVELS::INFO, "!!!");

    // Part 3
    LOG(INFO, "==============================");
    LOG(INFO, "%s %s", instruction_name(INST_DADD),
            instruction_description(INST_DADD));
    LOG(INFO, "%s %s", instruction_name(INST_IPRINT),
            instruction_description(INST_IPRINT));

    // // Part 4
    // auto inst_stream = gen_rnd_instrs(20);
    // log_instrs(inst_stream);

    // // Part 5
    // LOG(INFO, "==============================");
    // std::vector<unsigned char> super_code = {
    //     INST_ILOAD, 0xff, 0, 0, 0, 0, 0, 0, 0,
    //     INST_ILOAD, 0, 0, 0xdd, 0, 0, 0, 0, 0,
    //     INST_IADD,
    //     INST_ILOAD, 0xff, 0, 0, 0, 0, 0, 0, 0,
    //     INST_IADD,
    //     INST_IPRINT
    // }; // 0xff + 0xdd0000 + 0xff = 0xdd01fe
    // log_instrs(super_code);
    // exec_instrs(super_code);

    return 0;
}
