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
 * Align the given offset.
 * 
 * @param offset  the offset
 * @param align   the required alignment
 */
static unsigned int alignOffset(unsigned int offset, unsigned int align) {
  return offset + ((align - (offset % align)) % align);
}

/**
 * Print an icmp/fcmp instruction.
 * 
 * @param predicate  the predicate for the instruction
 * @param left       the first operand of the instruction
 * @param right      the second operand of the instruction
 */
void JVMWriter::printCmpInstruction(unsigned int predicate,
                                    const llvm::Value *left,
                                    const llvm::Value *right) {
  std::string inst;
  switch (predicate) {
    case llvm::ICmpInst::ICMP_EQ:
      inst = "icmp_eq";
      break;
    case llvm::ICmpInst::ICMP_NE:
      inst = "icmp_ne";
      break;
    case llvm::ICmpInst::ICMP_ULE:
      inst = "icmp_ule";
      break;
    case llvm::ICmpInst::ICMP_SLE:
      inst = "icmp_sle";
      break;
    case llvm::ICmpInst::ICMP_UGE:
      inst = "icmp_uge";
      break;
    case llvm::ICmpInst::ICMP_SGE:
      inst = "icmp_sge";
      break;
    case llvm::ICmpInst::ICMP_ULT:
      inst = "icmp_ult";
      break;
    case llvm::ICmpInst::ICMP_SLT:
      inst = "icmp_slt";
      break;
    case llvm::ICmpInst::ICMP_UGT:
      inst = "icmp_ugt";
      break;
    case llvm::ICmpInst::ICMP_SGT:
      inst = "icmp_sgt";
      break;
    case llvm::FCmpInst::FCMP_UGT:
      inst = "fcmp_ugt";
      break;
    case llvm::FCmpInst::FCMP_OGT:
      inst = "fcmp_ogt";
      break;
    case llvm::FCmpInst::FCMP_UGE:
      inst = "fcmp_uge";
      break;
    case llvm::FCmpInst::FCMP_OGE:
      inst = "fcmp_oge";
      break;
    case llvm::FCmpInst::FCMP_ULT:
      inst = "fcmp_ult";
      break;
    case llvm::FCmpInst::FCMP_OLT:
      inst = "fcmp_olt";
      break;
    case llvm::FCmpInst::FCMP_ULE:
      inst = "fcmp_ule";
      break;
    case llvm::FCmpInst::FCMP_OLE:
      inst = "fcmp_ole";
      break;
    case llvm::FCmpInst::FCMP_UEQ:
      inst = "fcmp_ueq";
      break;
    case llvm::FCmpInst::FCMP_OEQ:
      inst = "fcmp_oeq";
      break;
    case llvm::FCmpInst::FCMP_UNE:
      inst = "fcmp_une";
      break;
    case llvm::FCmpInst::FCMP_ONE:
      inst = "fcmp_one";
      break;
    case llvm::FCmpInst::FCMP_ORD:
      inst = "fcmp_ord";
      break;
    case llvm::FCmpInst::FCMP_UNO:
      inst = "fcmp_uno";
      break;
    default:
      llvm::errs() << "Predicate = " << predicate << '\n';
      llvm_unreachable("Invalid cmp predicate");
  }
  printVirtualInstruction(
    inst + "("
    + getTypeDescriptor(left->getType(), true)
    + getTypeDescriptor(right->getType(), true)
    + ")Z", left, right);
}

/**
 * Print an arithmetic instruction.
 * 
 * @param op     the opcode for the instruction
 * @param left   the first operand of the instruction
 * @param right  the second operand of the instruction
 */
void JVMWriter::printArithmeticInstruction(unsigned int op,
                                           const llvm::Value *left,
                                           const llvm::Value *right) {
  printValueLoad(left);
  printValueLoad(right);
  std::string typePrefix = getTypePrefix(left->getType(), true);
  std::string typeDescriptor = getTypeDescriptor(left->getType());
  switch (op) {
    case llvm::Instruction::Add:
    case llvm::Instruction::FAdd:
      printSimpleInstruction(typePrefix + "add");
      break;
    case llvm::Instruction::Sub:
    case llvm::Instruction::FSub:
      printSimpleInstruction(typePrefix + "sub");
      break;
    case llvm::Instruction::Mul:
    case llvm::Instruction::FMul:
      printSimpleInstruction(typePrefix + "mul");
      break;
    case llvm::Instruction::SDiv:
    case llvm::Instruction::FDiv:
      printSimpleInstruction(typePrefix + "div");
      break;
    case llvm::Instruction::SRem:
    case llvm::Instruction::FRem:
      printSimpleInstruction(typePrefix + "rem");
      break;
    case llvm::Instruction::And:
      printSimpleInstruction(typePrefix + "and");
      break;
    case llvm::Instruction::Or:
      printSimpleInstruction(typePrefix + "or");
      break;
    case llvm::Instruction::Xor:
      printSimpleInstruction(typePrefix + "xor");
      break;
    case llvm::Instruction::Shl:
      if (getBitWidth(right->getType()) == 64) printSimpleInstruction("l2i");
      printSimpleInstruction(typePrefix + "shl");
      break;
    case llvm::Instruction::LShr:
      if (getBitWidth(right->getType()) == 64) printSimpleInstruction("l2i");
      printSimpleInstruction(typePrefix + "ushr");
      break;
    case llvm::Instruction::AShr:
      if (getBitWidth(right->getType()) == 64) printSimpleInstruction("l2i");
      printSimpleInstruction(typePrefix + "shr");
      break;
    case llvm::Instruction::UDiv:
      printVirtualInstruction(
        "udiv(" + typeDescriptor + typeDescriptor + ")" + typeDescriptor);
      break;
    case llvm::Instruction::URem:
      printVirtualInstruction(
        "urem(" + typeDescriptor + typeDescriptor + ")" + typeDescriptor);
      break;
  }
}

/**
 * Print a bitcast instruction.
 * 
 * @param ty     the destination type
 * @param srcTy  the source type
 */
void JVMWriter::printBitCastInstruction(const llvm::Type *ty, const llvm::Type *srcTy) {
  char typeID = getTypeID(ty);
  char srcTypeID = getTypeID(srcTy);
  if (srcTypeID == 'J' && typeID == 'D')
    printSimpleInstruction("invokestatic",
                           "java/lang/Double/longBitsToDouble(J)D");
  else if (srcTypeID == 'I' && typeID == 'F')
    printSimpleInstruction("invokestatic",
                           "java/lang/Float/intBitsToFloat(I)F");
  if (srcTypeID == 'D' && typeID == 'J')
    printSimpleInstruction("invokestatic",
                           "java/lang/Double/doubleToRawLongBits(D)J");
  else if (srcTypeID == 'F' && typeID == 'I')
    printSimpleInstruction("invokestatic",
                           "java/lang/Float/floatToRawIntBits(F)I");
}

/**
 * Print a cast instruction.
 * 
 * @param typePrefix     the type prefix of the destination type
 * @param srcTypePrefix  the type prefix of the source type
 */
void JVMWriter::printCastInstruction(const std::string &typePrefix,
                                     const std::string &srcTypePrefix) {
  if (srcTypePrefix != typePrefix)
    printSimpleInstruction(srcTypePrefix + "2" + typePrefix);
}

/**
 * Print a cast instruction.
 * 
 * @param op    the opcode for the instruction
 * @param v     the value to be casted
 * @param ty    the destination type
 * @param srcTy the source type
 */
void JVMWriter::printCastInstruction(unsigned int op, const llvm::Value *v,
                                     const llvm::Type *ty, const llvm::Type *srcTy) {
  printValueLoad(v);
  switch (op) {
    case llvm::Instruction::SIToFP:
    case llvm::Instruction::FPToSI:
    case llvm::Instruction::FPTrunc:
    case llvm::Instruction::FPExt:
    case llvm::Instruction::SExt:
      if (getBitWidth(srcTy) < 32)
        printCastInstruction(getTypePrefix(srcTy), "i");
      printCastInstruction(getTypePrefix(ty, true),
                           getTypePrefix(srcTy, true));
      break;
    case llvm::Instruction::Trunc:
      if (getBitWidth(srcTy) == 64 && getBitWidth(ty) < 32) {
        printSimpleInstruction("l2i");
        printCastInstruction(getTypePrefix(ty), "i");
      } else
        printCastInstruction(getTypePrefix(ty),
                             getTypePrefix(srcTy, true));
      break;
    case llvm::Instruction::IntToPtr:
      printCastInstruction("i", getTypePrefix(srcTy, true));
      break;
    case llvm::Instruction::PtrToInt:
      printCastInstruction(getTypePrefix(ty), "i");
      break;
    case llvm::Instruction::ZExt:
      printVirtualInstruction("zext_" + getTypePostfix(ty, true)
                              + "(" + getTypeDescriptor(srcTy) + ")"
                              + getTypeDescriptor(ty, true));
      break;
    case llvm::Instruction::UIToFP:
      printVirtualInstruction("uitofp_" + getTypePostfix(ty)
                              + "(" + getTypeDescriptor(srcTy) + ")" + getTypeDescriptor(ty));
      break;
    case llvm::Instruction::FPToUI:
      printVirtualInstruction("fptoui_" + getTypePostfix(ty)
                              + "(" + getTypeDescriptor(srcTy) + ")" + getTypeDescriptor(ty));
      break;
    case llvm::Instruction::BitCast:
      printBitCastInstruction(ty, srcTy);
      break;
    default:
      llvm::errs() << "Opcode = " << op << '\n';
      llvm_unreachable("Invalid cast instruction");
  }
}

/**
 * Print a getelementptr instruction.
 * 
 * @param v  the aggregate data structure to index
 * @param i  an iterator to the first type indexed by the instruction
 * @param e  an iterator specifying the upper bound on the types indexed by the
 *           instruction
 */
void JVMWriter::printGepInstruction(const llvm::Value *v,
                                    llvm::gep_type_iterator i,
                                    llvm::gep_type_iterator e) {
  // load address
  printCastInstruction(llvm::Instruction::IntToPtr, v, NULL, v->getType());

  // calculate offset
  for (; i != e; i++) {
    unsigned int size = 0;
    const llvm::Value *indexValue = i.getOperand();

    if (const llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(*i)) {
      for (unsigned int f = 0,
             fieldIndex = llvm::cast<llvm::ConstantInt>(indexValue)->getZExtValue();
           f < fieldIndex; f++)
        size = alignOffset(
          size + dataLayout->getTypeAllocSize(
            structTy->getContainedType(f)),
          dataLayout->getABITypeAlignment(
            structTy->getContainedType(f + 1)));
      printPtrLoad(size);
      printSimpleInstruction("iadd");
    } else {
      if (const llvm::SequentialType *seqTy = llvm::dyn_cast<llvm::SequentialType>(*i))
        size = dataLayout->getTypeAllocSize(seqTy->getElementType());
      else
        size = dataLayout->getTypeAllocSize(*i);

      if (const llvm::ConstantInt *c = llvm::dyn_cast<llvm::ConstantInt>(indexValue)) {
        // constant optimisation
        if (c->isNullValue()) {
          // do nothing
        } else if (c->getValue().isNegative()) {
          printPtrLoad(c->getValue().abs().getZExtValue() * size);
          printSimpleInstruction("isub");
        } else {
          printPtrLoad(c->getZExtValue() * size);
          printSimpleInstruction("iadd");
        }
      } else {
        printPtrLoad(size);
        printCastInstruction(llvm::Instruction::IntToPtr, indexValue,
                             NULL, indexValue->getType());
        printSimpleInstruction("imul");
        printSimpleInstruction("iadd");
      }
    }
  }
}

/**
 * Print an alloca instruction.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printAllocaInstruction(const llvm::AllocaInst *inst) {
  uint64_t size = dataLayout->getTypeAllocSize(inst->getAllocatedType());
  if (const llvm::ConstantInt *c = llvm::dyn_cast<llvm::ConstantInt>(inst->getOperand(0))) {
    // constant optimization
    printPtrLoad(c->getZExtValue() * size);
  } else {
    printPtrLoad(size);
    printValueLoad(inst->getOperand(0));
    printSimpleInstruction("imul");
  }
  printSimpleInstruction("invokestatic",
                         "lljvm/runtime/Memory/allocateStack(I)I");
}


/**
 * Print a va_arg instruction.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printVAArgInstruction(const llvm::VAArgInst *inst) {
  printIndirectLoad(inst->getOperand(0));
  printSimpleInstruction("dup");
  printConstLoad(
    llvm::APInt(32, dataLayout->getTypeAllocSize(inst->getType()), false));
  printSimpleInstruction("iadd");
  printValueLoad(inst->getOperand(0));
  printSimpleInstruction("swap");
  printIndirectStore(llvm::PointerType::getUnqual(
    llvm::IntegerType::get(inst->getContext(), 8)));
  printIndirectLoad(inst->getType());
}

/**
 * Print a vararg intrinsic function.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printVAIntrinsic(const llvm::IntrinsicInst *inst) {
  const llvm::Type *valistTy = llvm::PointerType::getUnqual(
    llvm::IntegerType::get(inst->getContext(), 8));
  switch (inst->getIntrinsicID()) {
    case llvm::Intrinsic::vastart:
      printValueLoad(inst->getOperand(1));
      printSimpleInstruction("iload", llvm::utostr(vaArgNum) + " ; varargptr");
      printIndirectStore(valistTy);
      break;
    case llvm::Intrinsic::vacopy:
      printValueLoad(inst->getOperand(1));
      printValueLoad(inst->getOperand(2));
      printIndirectLoad(valistTy);
      printIndirectStore(valistTy);
      break;
    case llvm::Intrinsic::vaend:
      break;
  }
}

/**
 * Print a memory intrinsic function.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printMemIntrinsic(const llvm::MemIntrinsic *inst) {
  printValueLoad(inst->getDest());
  if (const llvm::MemTransferInst *minst = llvm::dyn_cast<llvm::MemTransferInst>(inst))
    printValueLoad(minst->getSource());
  else if (const llvm::MemSetInst *minst = llvm::dyn_cast<llvm::MemSetInst>(inst))
    printValueLoad(minst->getValue());
  printValueLoad(inst->getLength());
  printConstLoad(inst->getAlignmentCst());

  std::string lenDescriptor = getTypeDescriptor(
    inst->getLength()->getType(), true);
  switch (inst->getIntrinsicID()) {
    case llvm::Intrinsic::memcpy:
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/memcpy(II" + lenDescriptor + "I)V");
      break;
    case llvm::Intrinsic::memmove:
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/memmove(II" + lenDescriptor + "I)V");
      break;
    case llvm::Intrinsic::memset:
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/memset(IB" + lenDescriptor + "I)V");
      break;
  }
}

/**
 * Print a mathematical intrinsic function.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printMathIntrinsic(const llvm::IntrinsicInst *inst) {
  bool f32 = (getBitWidth(inst->getOperand(1)->getType()) == 32);
  printValueLoad(inst->getOperand(1));
  if (f32) printSimpleInstruction("f2d");
  if (inst->getNumOperands() >= 3) {
    printValueLoad(inst->getOperand(2));
    if (f32) printSimpleInstruction("f2d");
  }
  switch (inst->getIntrinsicID()) {
    case llvm::Intrinsic::exp:
      printSimpleInstruction("invokestatic", "java/lang/Math/exp(D)D");
      break;
    case llvm::Intrinsic::log:
      printSimpleInstruction("invokestatic", "java/lang/Math/log(D)D");
      break;
    case llvm::Intrinsic::log10:
      printSimpleInstruction("invokestatic", "java/lang/Math/log10(D)D");
      break;
    case llvm::Intrinsic::sqrt:
      printSimpleInstruction("invokestatic", "java/lang/Math/sqrt(D)D");
      break;
    case llvm::Intrinsic::pow:
      printSimpleInstruction("invokestatic", "java/lang/Math/pow(DD)D");
      break;
  }
  if (f32) printSimpleInstruction("d2f");
}

/**
 * Print a bit manipulation intrinsic function.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printBitIntrinsic(const llvm::IntrinsicInst *inst) {
  // TODO: ctpop, ctlz, cttz
  const llvm::Value *value = inst->getOperand(1);
  const std::string typeDescriptor = getTypeDescriptor(value->getType());
  switch (inst->getIntrinsicID()) {
    case llvm::Intrinsic::bswap:
      printVirtualInstruction(
        "bswap(" + typeDescriptor + ")" + typeDescriptor, value);
      break;
  }
}
