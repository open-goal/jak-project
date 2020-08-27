add_library(goal SHARED
        Goal.cpp
        GoalFunctionForms.cpp
        GoalConditionalCompilation.cpp
        GoalBlockForms.cpp
        GoalCompilerControl.cpp
        GoalType.cpp
        GoalEnv.cpp
        GoalListener.cpp
        IR.cpp
        GoalPlace.cpp
        GoalLambda.cpp
        GoalDefineForms.cpp
        StaticObject.cpp
        GoalIntegerMath.cpp
        GoalMacroForms.cpp
        GoalControlFlow.cpp
        GoalDefType.cpp
        GoalMethod.cpp
        GoalInspector.cpp
        GoalFieldAccess.cpp
        GoalTypeUtil.cpp
        GoalObject.cpp
        GoalCompileAtoms.cpp
        GoalConstProp.cpp
        GoalUtil.cpp
        GoalPair.cpp
        GoalStaticObject.cpp
        GoalNew.cpp
        GoalAsm.cpp
        GoalEnum.cpp
        GoalBitfieldAccess.cpp)

target_link_libraries(goal goos_old reader_old)