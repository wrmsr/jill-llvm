/*
* Copyright (c) 2009-2010 David Roberts <d@vidr.cc>
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/

#include "backend.hpp"

/**
 * Return a unique ID.
 * 
 * @return  a unique ID
 */
static uint64_t getUID() {
  static uint64_t x = 0;
  return ++x;
}

/**
 * Return the call signature of the given function type. An empty string is
 * returned if the function type appears to be non-prototyped.
 * 
 * @param ty  the function type
 * @return    the call signature
 */
std::string JVMWriter::getCallSignature(const llvm::FunctionType *ty) {
  if (ty->isVarArg() && ty->getNumParams() == 0)
    // non-prototyped function
    return "";
  std::string sig;
  sig += '(';
  for (unsigned int i = 0, e = ty->getNumParams(); i < e; i++)
    sig += getTypeDescriptor(ty->getParamType(i));
  if (ty->isVarArg()) sig += "I";
  sig += ')';
  sig += getTypeDescriptor(ty->getReturnType());
  return sig;
}

/**
 * Pack the specified operands of the given instruction into memory. The
 * address of the packed values is left on the top of the stack.
 * 
 * @param inst        the given instruction
 * @param minOperand  the lower bound on the operands to pack (inclusive)
 * @param maxOperand  the upper bound on the operands to pack (exclusive)
 */
void JVMWriter::printOperandPack(const llvm::Instruction *inst,
                                 unsigned int minOperand,
                                 unsigned int maxOperand) {
  unsigned int size = 0;
  for (unsigned int i = minOperand; i < maxOperand; i++)
    size += dataLayout->getTypeAllocSize(
      inst->getOperand(i)->getType());

  printSimpleInstruction("bipush", llvm::utostr(size));
  printSimpleInstruction("invokestatic",
                         "lljvm/runtime/Memory/allocateStack(I)I");
  printSimpleInstruction("dup");

  for (unsigned int i = minOperand; i < maxOperand; i++) {
    const llvm::Value *v = inst->getOperand(i);
    printValueLoad(v);
    printSimpleInstruction("invokestatic",
                           "lljvm/runtime/Memory/pack(I"
                           + getTypeDescriptor(v->getType()) + ")I");
  }
  printSimpleInstruction("pop");
}

/**
 * Print a call/invoke instruction.
 * 
 * @param functionVal  the function to call
 * @param inst         the instruction
 */
void JVMWriter::printFunctionCall(const llvm::Value *functionVal,
                                  const llvm::Instruction *inst) {
  unsigned int origin = llvm::isa<llvm::InvokeInst>(inst) ? 3 : 1;
  if (const llvm::Function *f = llvm::dyn_cast<llvm::Function>(functionVal)) { // direct call
    const llvm::FunctionType *ty = f->getFunctionType();

    //for(unsigned int i = origin, e = inst->getNumOperands(); i < e; i++)
    //    printValueLoad(inst->getOperand(i));

    for (unsigned int i = 0, e = ty->getNumParams(); i < e; i++)
      printValueLoad(inst->getOperand(i + origin));
    if (ty->isVarArg() && inst)
      printOperandPack(inst, ty->getNumParams() + origin,
                       inst->getNumOperands());

    if (externRefs.count(f))
      printSimpleInstruction("invokestatic",
                             getValueName(f) + getCallSignature(ty));
    else
      printSimpleInstruction("invokestatic",
                             classname + "/" + getValueName(f) + getCallSignature(ty));

    if (getValueName(f) == "setjmp") {
      unsigned int varNum = usedRegisters++;
      printSimpleInstruction("istore", llvm::utostr(varNum));
      printSimpleInstruction("iconst_0");
      printLabel("setjmp$" + llvm::utostr(varNum));
    }
  } else { // indirect call
    printValueLoad(functionVal);
    const llvm::FunctionType *ty = llvm::cast<llvm::FunctionType>(
      llvm::cast<llvm::PointerType>(functionVal->getType())->getElementType());
    printOperandPack(inst, origin, inst->getNumOperands());
    printSimpleInstruction("invokestatic",
                           "lljvm/runtime/Function/invoke_"
                           + getTypePostfix(ty->getReturnType()) + "(II)"
                           + getTypeDescriptor(ty->getReturnType()));
  }
}

/**
 * Print a call to an intrinsic function.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printIntrinsicCall(const llvm::IntrinsicInst *inst) {
  switch (inst->getIntrinsicID()) {
    case llvm::Intrinsic::vastart:
    case llvm::Intrinsic::vacopy:
    case llvm::Intrinsic::vaend:
      printVAIntrinsic(inst);
      break;
    case llvm::Intrinsic::memcpy:
    case llvm::Intrinsic::memmove:
    case llvm::Intrinsic::memset:
      printMemIntrinsic(llvm::cast<llvm::MemIntrinsic>(inst));
      break;
    case llvm::Intrinsic::flt_rounds:
      printSimpleInstruction("iconst_m1");
      break;
    case llvm::Intrinsic::dbg_declare:
      // ignore debugging intrinsics
      break;
    case llvm::Intrinsic::pow:
    case llvm::Intrinsic::exp:
    case llvm::Intrinsic::log10:
    case llvm::Intrinsic::log:
    case llvm::Intrinsic::sqrt:
      printMathIntrinsic(inst);
      break;
    case llvm::Intrinsic::bswap:
      printBitIntrinsic(inst);
      break;
    default:
      llvm::errs() << "Intrinsic = " << *inst << '\n';
      llvm_unreachable("Invalid intrinsic function");
  }
}

/**
 * Print a call instruction.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printCallInstruction(const llvm::Instruction *inst) {
  if (llvm::isa<llvm::IntrinsicInst>(inst))
    printIntrinsicCall(llvm::cast<llvm::IntrinsicInst>(inst));
  else
    printFunctionCall(inst->getOperand(0), inst);
}

/**
 * Print an invoke instruction.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printInvokeInstruction(const llvm::InvokeInst *inst) {
  std::string labelname = llvm::utostr(getUID()) + "$invoke";
  printLabel(labelname + "_begin");
  printFunctionCall(inst->getOperand(0), inst);
  if (!inst->getType()->isVoidTy())
    printValueStore(inst); // save return value
  printLabel(labelname + "_end");
  printBranchInstruction(inst->getParent(), inst->getNormalDest());
  printLabel(labelname + "_catch");
  printSimpleInstruction("pop");
  printBranchInstruction(inst->getParent(), inst->getUnwindDest());
  printSimpleInstruction(".catch lljvm/runtime/System$Unwind",
                         "from " + labelname + "_begin "
                         + "to " + labelname + "_end "
                         + "using " + labelname + "_catch");
}

/**
 * Allocate a local variable for the given function. Variable initialisation
 * and any applicable debugging information is printed.
 * 
 * @param f     the parent function of the variable
 * @param inst  the instruction assigned to the variable
 */
void JVMWriter::printLocalVariable(const llvm::Function &f,
                                   const llvm::Instruction *inst) {
  const llvm::Type *ty;
  if (llvm::isa<llvm::AllocaInst>(inst) && !llvm::isa<llvm::GlobalVariable>(inst))
    // local variable allocation
    ty = llvm::PointerType::getUnqual(
      llvm::cast<llvm::AllocaInst>(inst)->getAllocatedType());
  else // operation result
    ty = inst->getType();
  // getLocalVarNumber must be called at least once in this method
  unsigned int varNum = getLocalVarNumber(inst);
  if (debug >= 2)
    printSimpleInstruction(".var " + llvm::utostr(varNum) + " is "
                           + getValueName(inst) + ' ' + getTypeDescriptor(ty)
                           + " from begin_method to end_method");
  // initialise variable to avoid class verification errors
  printSimpleInstruction(getTypePrefix(ty, true) + "const_0");
  printSimpleInstruction(getTypePrefix(ty, true) + "store", llvm::utostr(varNum));
}

/**
 * Print the body of the given function.
 * 
 * @param f  the function
 */
void JVMWriter::printFunctionBody(const llvm::Function &f) {
  for (llvm::Function::const_iterator i = f.begin(), e = f.end(); i != e; i++) {
    if (llvm::Loop *l = getAnalysis<llvm::LoopInfo>().getLoopFor(i)) {
      if (l->getHeader() == i && l->getParentLoop() == 0)
        printLoop(l);
    } else
      printBasicBlock(i);
  }
}

/**
 * Return the local variable number of the given value. Register/s are
 * allocated for the variable if necessary.
 * 
 * @param v  the value
 * @return   the local variable number
 */
unsigned int JVMWriter::getLocalVarNumber(const llvm::Value *v) {
  if (!localVars.count(v)) {
    localVars[v] = usedRegisters++;
    if (getBitWidth(v->getType()) == 64)
      usedRegisters++; // 64 bit types occupy 2 registers
  }
  return localVars[v];
}

/**
 * Print the block to catch Jump objects (thrown by longjmp).
 * 
 * @param numJumps  the number of setjmp calls made by the current function
 */
void JVMWriter::printCatchJump(unsigned int numJumps) {
  unsigned int jumpVarNum = usedRegisters++;
  printSimpleInstruction(".catch lljvm/runtime/Jump "
                           "from begin_method to catch_jump using catch_jump");
  printLabel("catch_jump");
  printSimpleInstruction("astore", llvm::utostr(jumpVarNum));
  printSimpleInstruction("aload", llvm::utostr(jumpVarNum));
  printSimpleInstruction("getfield", "lljvm/runtime/Jump/value I");
  for (unsigned int i = usedRegisters - 1 - numJumps,
         e = usedRegisters - 1; i < e; i++) {
    if (debug >= 2)
      printSimpleInstruction(".var " + llvm::utostr(i) + " is setjmp_id_"
                             + llvm::utostr(i) + " I from begin_method to end_method");
    printSimpleInstruction("aload", llvm::utostr(jumpVarNum));
    printSimpleInstruction("getfield", "lljvm/runtime/Jump/id I");
    printSimpleInstruction("iload", llvm::utostr(i));
    printSimpleInstruction("if_icmpeq", "setjmp$" + llvm::utostr(i));
  }
  printSimpleInstruction("pop");
  printSimpleInstruction("aload", llvm::utostr(jumpVarNum));
  printSimpleInstruction("athrow");
  if (debug >= 2)
    printSimpleInstruction(".var " + llvm::utostr(jumpVarNum) + " is jump "
      "Llljvm/runtime/Jump; from begin_method to end_method");
}

/**
 * Print the given function.
 * 
 * @param f  the function
 */
void JVMWriter::printFunction(const llvm::Function &f) {
  localVars.clear();
  usedRegisters = 0;

  out << '\n';
  out << ".method " << (f.hasLocalLinkage() ? "private " : "public ")
  << "static " << getValueName(&f) << '(';
  for (llvm::Function::const_arg_iterator i = f.arg_begin(), e = f.arg_end();
       i != e; i++)
    out << getTypeDescriptor(i->getType());
  if (f.isVarArg())
    out << "I";
  out << ')' << getTypeDescriptor(f.getReturnType()) << '\n';

  for (llvm::Function::const_arg_iterator i = f.arg_begin(), e = f.arg_end();
       i != e; i++) {
    // getLocalVarNumber must be called at least once in each iteration
    unsigned int varNum = getLocalVarNumber(i);
    if (debug >= 2)
      printSimpleInstruction(".var " + llvm::utostr(varNum) + " is "
                             + getValueName(i) + ' ' + getTypeDescriptor(i->getType())
                             + " from begin_method to end_method");
  }
  if (f.isVarArg()) {
    vaArgNum = usedRegisters++;
    if (debug >= 2)
      printSimpleInstruction(".var " + llvm::utostr(vaArgNum)
                             + " is varargptr I from begin_method to end_method");
  }

  // TODO: better stack depth analysis
  unsigned int stackDepth = 8;
  unsigned int numJumps = 0;
  for (llvm::const_inst_iterator i = llvm::inst_begin(&f), e = llvm::inst_end(&f);
       i != e; i++) {
    if (stackDepth < i->getNumOperands())
      stackDepth = i->getNumOperands();
    if (i->getType() != llvm::Type::getVoidTy(f.getContext()))
      printLocalVariable(f, &*i);
    if (const llvm::CallInst *inst = llvm::dyn_cast<llvm::CallInst>(&*i)) if (!llvm::isa<llvm::IntrinsicInst>(inst) &&
                                                                              getValueName(inst->getOperand(0)) ==
                                                                              "setjmp")
      numJumps++;
  }

  for (unsigned int i = 0; i < numJumps; i++) {
    // initialise jump IDs to prevent class verification errors
    printSimpleInstruction("iconst_0");
    printSimpleInstruction("istore", llvm::utostr(usedRegisters + i));
  }

  printLabel("begin_method");
  printSimpleInstruction("invokestatic",
                         "lljvm/runtime/Memory/createStackFrame()V");
  printFunctionBody(f);
  if (numJumps) printCatchJump(numJumps);
  printSimpleInstruction(".limit stack", llvm::utostr(stackDepth * 2));
  printSimpleInstruction(".limit locals", llvm::utostr(usedRegisters));
  printLabel("end_method");
  out << ".end method\n";
}
