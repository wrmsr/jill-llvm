add_llvm_target(JasminBackendCodeGen
  Backend.cpp
  Block.cpp
  Branch.cpp
  Const.cpp
  Function.cpp
  Instruction.cpp
  LoadStore.cpp
  Name.cpp
  PrintInst.cpp
  Sections.cpp
  Types.cpp
  Util.cpp
  )

add_subdirectory(TargetInfo)
