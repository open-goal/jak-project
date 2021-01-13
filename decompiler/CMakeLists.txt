add_library(
        decomp
        SHARED
        data/game_count.cpp
        data/game_text.cpp
        data/StrFileReader.cpp
        data/tpage.cpp

        Disasm/Instruction.cpp
        Disasm/InstructionDecode.cpp
        Disasm/InstructionMatching.cpp
        Disasm/InstructionParser.cpp
        Disasm/OpcodeInfo.cpp
        Disasm/Register.cpp

        Function/BasicBlocks.cpp
        Function/CfgVtx.cpp
        Function/ExpressionBuilder.cpp
        Function/ExpressionStack.cpp
        Function/Function.cpp
        Function/RegUsage.cpp
        Function/TypeAnalysis.cpp
        Function/TypeInspector.cpp

        IR/BasicOpBuilder.cpp
        IR/CfgBuilder.cpp
        IR/IR.cpp
        IR/IR_ExpressionStack.cpp
        IR/IR_TypeAnalysis.cpp

        IR2/AtomicOp.cpp
        IR2/AtomicOpBuilder.cpp
        IR2/AtomicOpTypeAnalysis.cpp
        IR2/Env.cpp
        IR2/reg_usage.cpp

        ObjectFile/LinkedObjectFile.cpp
        ObjectFile/LinkedObjectFileCreation.cpp
        ObjectFile/ObjectFileDB.cpp
        ObjectFile/ObjectFileDB_IR2.cpp

        util/DecompilerTypeSystem.cpp
        util/TP_Type.cpp

        config.cpp
)

target_link_libraries(decomp
        minilzo
        common
        fmt
        )

add_executable(decompiler
        main.cpp
        )

target_link_libraries(decompiler
        decomp
        common
        minilzo
        fmt)
