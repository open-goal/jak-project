add_executable(decompiler
        util/LispPrint.cpp
        main.cpp
        ObjectFile/ObjectFileDB.cpp
        Disasm/Instruction.cpp
        Disasm/InstructionDecode.cpp
        Disasm/OpcodeInfo.cpp
        Disasm/Register.cpp
        ObjectFile/LinkedObjectFileCreation.cpp
        ObjectFile/LinkedObjectFile.cpp
        Function/Function.cpp
        util/FileIO.cpp
        config.cpp
        util/LispPrint.cpp
        util/Timer.cpp
        Function/BasicBlocks.cpp
        Disasm/InstructionMatching.cpp
        TypeSystem/GoalType.cpp
        TypeSystem/GoalFunction.cpp
        TypeSystem/GoalSymbol.cpp
        TypeSystem/TypeInfo.cpp
        TypeSystem/TypeSpec.cpp Function/CfgVtx.cpp Function/CfgVtx.h)

target_link_libraries(decompiler
        minilzo)