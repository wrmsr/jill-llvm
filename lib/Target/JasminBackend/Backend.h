#ifndef LLVM_TARGET_JASMIN_BACKEND_H
#define LLVM_TARGET_JASMIN_BACKEND_H

#include "JasminTargetMachine.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/ConstantsScanner.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Config/config.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/Scalar.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <map>
#include <set>

namespace llvm {

/// JasminWriter - This class is the main chunk of code that converts an LLVM
/// module to a Jasmin translation unit.
class JasminWriter : public ModulePass {
  typedef std::vector<Type*> TypeList;
  typedef std::map<Type*,std::string> TypeMap;
  typedef std::map<const Value*,std::string> ValueMap;
  typedef std::set<std::string> NameSet;
  typedef std::set<Type*> TypeSet;
  typedef std::set<const Value*> ValueSet;
  typedef std::map<const Value*,std::string> ForwardRefMap;

  std::unique_ptr<formatted_raw_ostream> OutOwner;
  formatted_raw_ostream &Out;
  Module *TheModule;
  uint64_t uniqueNum;
  TypeMap TypeNames;
  ValueMap ValueNames;
  NameSet UsedNames;
  TypeSet DefinedTypes;
  ValueSet DefinedValues;
  ForwardRefMap ForwardRefs;
  bool is_inline;
  unsigned indent_level;

public:
  static char ID;
  explicit JasminWriter() : ModulePass(ID), Out(*((formatted_raw_ostream*)0)) {}
  explicit JasminWriter(std::unique_ptr<formatted_raw_ostream> o);

  const char *getPassName() const override { return "Jasmin backend"; }

  bool runOnModule(Module &M) override;

  void error(const std::string& msg);

  formatted_raw_ostream& nl(formatted_raw_ostream &Out, int delta = 0);
  inline void in() { indent_level++; }
  inline void out() { if (indent_level >0) indent_level--; }

private:
  void printCallingConv(CallingConv::ID cc);
  void printEscapedString(const std::string& str);
  void printCFP(const ConstantFP* CFP);

  std::string getJasminName(Type* val);
  inline void printJasminName(Type* val);

  std::string getJasminName(const Value* val);
  inline void printJasminName(const Value* val);

  void printAttributes(const AttributeSet &PAL, const std::string &name);
  void printType(Type* Ty);
  void printTypes(const Module* M);

  void printConstant(const Constant *CPV);
  void printConstants(const Module* M);

  void printVariableBody(const GlobalVariable *GV);

  std::string getOpName(const Value*);

private:
  std::string sourceName;
  std::string className;
  unsigned int debug;
  const DataLayout *dataLayout;
  static char id;

  DenseSet<const Value *> externRefs;
  DenseMap<const BasicBlock *, unsigned int> blockIDs;
  DenseMap<const Value *, unsigned int> localVars;
  unsigned int usedRegisters;
  unsigned int vaArgNum;
  unsigned int instNum;

public:
  bool runOnFunction(Function &F);
  virtual void getAnalysisUsage(AnalysisUsage &usage) const override;

  // block.cpp
  void printBasicBlock(const BasicBlock *block);
  void printInstruction(const Instruction *inst);

  // branch.cpp
  void printPHICopy(const BasicBlock *src, const BasicBlock *dest);
  void printBranchInstruction(const BasicBlock *curBlock,
                              const BasicBlock *destBlock);
  void printBranchInstruction(const BasicBlock *curBlock,
                              const BasicBlock *trueBlock,
                              const BasicBlock *falseBlock);
  void printBranchInstruction(const BranchInst *inst);
  void printSelectInstruction(const Value *cond,
                              const Value *trueVal,
                              const Value *falseVal);
  void printSwitchInstruction(const SwitchInst *inst);
  void printLoop(const LoopInfo &LI, const Loop *l);

  // const.cpp
  void printPtrLoad(uint64_t n);
  void printConstLoad(const APInt &i);
  void printConstLoad(float f);
  void printConstLoad(double d);
  void printConstLoad(const Constant *c);
  void printConstLoad(const std::string &str, bool cstring);
  void printStaticConstant(const Constant *c);
  void printConstantExpr(const ConstantExpr *ce);

  // function.cpp
  std::string getCallSignature(const FunctionType *ty);
  void printOperandPack(const Instruction *inst,
                        unsigned int minOperand,
                        unsigned int maxOperand);
  void printFunctionCall(const Value *functionVal, const Instruction *inst);
  void printIntrinsicCall(const IntrinsicInst *inst);
  void printCallInstruction(const Instruction *inst);
  void printInvokeInstruction(const InvokeInst *inst);
  void printLocalVariable(Function &f, const Instruction *inst);
  void printFunctionBody(Function &f);
  unsigned int getLocalVarNumber(const Value *v);
  void printCatchJump(unsigned int numJumps);
  void printFunction(Function &f);

  // instruction.cpp
  void printCmpInstruction(unsigned int predicate,
                           const Value *left,
                           const Value *right);
  void printArithmeticInstruction(unsigned int op,
                                  const Value *left,
                                  const Value *right);
  void printBitCastInstruction(const Type *ty, const Type *srcTy);
  void printCastInstruction(const std::string &typePrefix,
                            const std::string &srcTypePrefix);
  void printCastInstruction(unsigned int op, const Value *v,
                            const Type *ty, const Type *srcTy);
  void printGepInstruction(const Value *v,
                           gep_type_iterator i,
                           gep_type_iterator e);
  void printAllocaInstruction(const AllocaInst *inst);
  void printVAArgInstruction(const VAArgInst *inst);
  void printVAIntrinsic(const IntrinsicInst *inst);
  void printMemIntrinsic(const MemIntrinsic *inst);
  void printMathIntrinsic(const IntrinsicInst *inst);
  void printBitIntrinsic(const IntrinsicInst *inst);

  // loadstore.cpp
  void printValueLoad(const Value *v);
  void printValueStore(const Value *v);
  void printIndirectLoad(const Value *v);
  void printIndirectLoad(const Type *ty);
  void printIndirectStore(const Value *ptr, const Value *val);
  void printIndirectStore(const Type *ty);

  // name.cpp
  std::string sanitizeName(std::string name);
  std::string getValueName(const Value *v);
  std::string getLabelName(const BasicBlock *block);

  // printinst.cpp
  void printBinaryInstruction(const char *name,
                              const Value *left,
                              const Value *right);
  void printBinaryInstruction(const std::string &name,
                              const Value *left,
                              const Value *right);
  void printSimpleInstruction(const char *inst);
  void printSimpleInstruction(const char *inst, const char *operand);
  void printSimpleInstruction(const std::string &inst);
  void printSimpleInstruction(const std::string &inst,
                              const std::string &operand);
  void printVirtualInstruction(const char *sig);
  void printVirtualInstruction(const char *sig, const Value *operand);
  void printVirtualInstruction(const char *sig,
                               const Value *left,
                               const Value *right);
  void printVirtualInstruction(const std::string &sig);
  void printVirtualInstruction(const std::string &sig, const Value *operand);
  void printVirtualInstruction(const std::string &sig,
                               const Value *left,
                               const Value *right);
  void printLabel(const char *label);
  void printLabel(const std::string &label);

  // sections.cpp
  void printHeader();
  void printFields();
  void printExternalMethods();
  void printConstructor();
  void printClInit();
  void printMainMethod();

  // types.cpp
  unsigned int getBitWidth(const Type *ty, bool expand = false);
  char getTypeID(const Type *ty, bool expand = false);
  std::string getTypeName(const Type *ty, bool expand = false);
  std::string getTypeDescriptor(const Type *ty, bool expand = false);
  std::string getTypePostfix(const Type *ty, bool expand = false);
  std::string getTypePrefix(const Type *ty, bool expand = false);
};

}

std::string ftostr(const llvm::APFloat &V);
std::string ftostr(double V);

#endif
