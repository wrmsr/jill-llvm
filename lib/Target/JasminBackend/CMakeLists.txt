add_llvm_target(JasminBackendCodeGen
  JasminBackend.cpp
  Backend.cpp
  Block.cpp
  Branch.cpp
  Const.cpp
  Function.cpp
  Instruction.cpp
  LoadStore.cpp
  Main.cpp
  Name.cpp
  PrintInst.cpp
  Sections.cpp
  Types.cpp
  Util.cpp
  )

add_subdirectory(TargetInfo)
