/*
* Copyright (c) 2009 David Roberts <d@vidr.cc>
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
 * Load the given pointer.
 * 
 * @param n  the value of the pointer
 */
void JVMWriter::printPtrLoad(uint64_t n) {
  if (module->getDataLayout()->getPointerSize() != 32)
    llvm_unreachable("Only 32-bit pointers are allowed");
  printConstLoad(llvm::APInt(32, n, false));
}

/**
 * Load the given integer.
 * 
 * @param i  the integer
 */
void JVMWriter::printConstLoad(const llvm::APInt &i) {
  if (i.getBitWidth() <= 32) {
    int64_t value = i.getSExtValue();
    if (value == -1)
      printSimpleInstruction("iconst_m1");
    else if (0 <= value && value <= 5)
      printSimpleInstruction("iconst_" + i.toString(10, true));
    else if (-0x80 <= value && value <= 0x7f)
      printSimpleInstruction("bipush", i.toString(10, true));
    else if (-0x8000 <= value && value <= 0x7fff)
      printSimpleInstruction("sipush", i.toString(10, true));
    else
      printSimpleInstruction("ldc", i.toString(10, true));
  } else {
    if (i == 0)
      printSimpleInstruction("lconst_0");
    else if (i == 1)
      printSimpleInstruction("lconst_1");
    else
      printSimpleInstruction("ldc2_w", i.toString(10, true));
  }
}

/**
 * Load the given single-precision floating point value.
 * 
 * @param f  the value
 */
void JVMWriter::printConstLoad(float f) {
  if (f == 0.0)
    printSimpleInstruction("fconst_0");
  else if (f == 1.0)
    printSimpleInstruction("fconst_1");
  else if (f == 2.0)
    printSimpleInstruction("fconst_2");
  else if (llvm::IsNAN(f))
    printSimpleInstruction("getstatic", "java/lang/Float/NaN F");
  else if (llvm::IsInf(f) > 0)
    printSimpleInstruction("getstatic",
                           "java/lang/Float/POSITIVE_INFINITY F");
  else if (llvm::IsInf(f) < 0)
    printSimpleInstruction("getstatic",
                           "java/lang/Float/NEGATIVE_INFINITY F");
  else
    printSimpleInstruction("ldc", ftostr(f));
}

/**
 * Load the given double-precision floating point value.
 * 
 * @param d  the value
 */
void JVMWriter::printConstLoad(double d) {
  if (d == 0.0)
    printSimpleInstruction("dconst_0");
  else if (d == 1.0)
    printSimpleInstruction("dconst_1");
  else if (llvm::IsNAN(d))
    printSimpleInstruction("getstatic", "java/lang/Double/NaN D");
  else if (llvm::IsInf(d) > 0)
    printSimpleInstruction("getstatic",
                           "java/lang/Double/POSITIVE_INFINITY D");
  else if (llvm::IsInf(d) < 0)
    printSimpleInstruction("getstatic",
                           "java/lang/Double/NEGATIVE_INFINITY D");
  else
    printSimpleInstruction("ldc2_w", ftostr(d));
}

/**
 * Load the given constant.
 * 
 * @param c  the constant
 */
void JVMWriter::printConstLoad(const llvm::Constant *c) {
  if (const llvm::ConstantInt *i = llvm::dyn_cast<llvm::ConstantInt>(c)) {
    printConstLoad(i->getValue());
  } else if (const llvm::ConstantFP *fp = llvm::dyn_cast<llvm::ConstantFP>(c)) {
    if (fp->getType()->getTypeID() == llvm::Type::FloatTyID)
      printConstLoad(fp->getValueAPF().convertToFloat());
    else
      printConstLoad(fp->getValueAPF().convertToDouble());
  } else if (llvm::isa<llvm::UndefValue>(c)) {
    printPtrLoad(0);
  } else {
    llvm::errs() << "Constant = " << *c << '\n';
    llvm_unreachable("Invalid constant value");
  }
}

/**
 * Load the given string.
 * 
 * @param str      the string
 * @param cstring  true iff the string contains a single null character at the
 *                 end
 */
void JVMWriter::printConstLoad(const std::string &str, bool cstring) {
  out << "\tldc \"";
  if (cstring)
    for (std::string::const_iterator i = str.begin(),
           e = str.end() - 1; i != e; i++)
      switch (*i) {
        case '\\':
          out << "\\\\";
          break;
        case '\b':
          out << "\\b";
          break;
        case '\t':
          out << "\\t";
          break;
        case '\n':
          out << "\\n";
          break;
        case '\f':
          out << "\\f";
          break;
        case '\r':
          out << "\\r";
          break;
        case '\"':
          out << "\\\"";
          break;
        case '\'':
          out << "\\\'";
          break;
        default:
          out << *i;
          break;
      }
  else
    for (std::string::const_iterator i = str.begin(),
           e = str.end(); i != e; i++) {
      const char c = *i;
      out << "\\u00" << llvm::hexdigit((c >> 4) & 0xf) << llvm::hexdigit(c & 0xf);
    }
  out << "\"\n";
}

/**
 * Store the given static constant. The constant is stored to the address
 * currently on top of the stack, pushing the first address following the
 * constant onto the stack afterwards.
 * 
 * @param c  the constant
 */
void JVMWriter::printStaticConstant(const llvm::Constant *c) {
  if (llvm::isa<llvm::ConstantAggregateZero>(c) || c->isNullValue()) {
    // zero initialised constant
    printPtrLoad(dataLayout->getTypeAllocSize(c->getType()));
    printSimpleInstruction("invokestatic",
                           "lljvm/runtime/Memory/zero(II)I");
    return;
  }
  std::string typeDescriptor = getTypeDescriptor(c->getType());
  switch (c->getType()->getTypeID()) {
    case llvm::Type::IntegerTyID:
    case llvm::Type::FloatTyID:
    case llvm::Type::DoubleTyID:
      printConstLoad(c);
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/pack(I" + typeDescriptor + ")I");
      break;
    case llvm::Type::ArrayTyID:
      if (const llvm::ConstantDataSequential *ca = llvm::dyn_cast<llvm::ConstantDataSequential>(
        c)) if (ca->isString()) {
        bool cstring = ca->isCString();
        printConstLoad(ca->getAsString(), cstring);
        if (cstring)
          printSimpleInstruction("invokestatic",
                                 "lljvm/runtime/Memory/pack(ILjava/lang/String;)I");
        else {
          printSimpleInstruction("invokevirtual",
                                 "java/lang/String/toCharArray()[C");
          printSimpleInstruction("invokestatic",
                                 "lljvm/runtime/Memory/pack(I[C)I");
        }
        break;
      }
      // else fall through
    case llvm::Type::VectorTyID:
    case llvm::Type::StructTyID:
      for (unsigned int i = 0, e = c->getNumOperands(); i < e; i++)
        printStaticConstant(llvm::cast<llvm::Constant>(c->getOperand(i)));
      break;
    case llvm::Type::PointerTyID:
      if (const llvm::Function *f = llvm::dyn_cast<llvm::Function>(c))
        printValueLoad(f);
      else if (const llvm::GlobalVariable *g = llvm::dyn_cast<llvm::GlobalVariable>(c))
        // initialise with address of global variable
        printValueLoad(g);
      else if (llvm::isa<llvm::ConstantPointerNull>(c) || c->isNullValue())
        printSimpleInstruction("iconst_0");
      else if (const llvm::ConstantExpr *ce = llvm::dyn_cast<llvm::ConstantExpr>(c))
        printConstantExpr(ce);
      else {
        llvm::errs() << "Constant = " << *c << '\n';
        llvm_unreachable("Invalid static initializer");
      }
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/pack(I" + typeDescriptor + ")I");
      break;
    default:
      llvm::errs() << "TypeID = " << c->getType()->getTypeID() << '\n';
      llvm_unreachable("Invalid type in printStaticConstant()");
  }
}

/**
 * Print the given constant expression.
 * 
 * @param ce  the constant expression
 */
void JVMWriter::printConstantExpr(const llvm::ConstantExpr *ce) {
  const llvm::Value *left, *right;
  if (ce->getNumOperands() >= 1) left = ce->getOperand(0);
  if (ce->getNumOperands() >= 2) right = ce->getOperand(1);
  switch (ce->getOpcode()) {
    case llvm::Instruction::Trunc:
    case llvm::Instruction::ZExt:
    case llvm::Instruction::SExt:
    case llvm::Instruction::FPTrunc:
    case llvm::Instruction::FPExt:
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
    case llvm::Instruction::FPToUI:
    case llvm::Instruction::FPToSI:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::BitCast:
      printCastInstruction(ce->getOpcode(), left, ce->getType(), left->getType());
      break;
    case llvm::Instruction::Add:
    case llvm::Instruction::FAdd:
    case llvm::Instruction::Sub:
    case llvm::Instruction::FSub:
    case llvm::Instruction::Mul:
    case llvm::Instruction::FMul:
    case llvm::Instruction::UDiv:
    case llvm::Instruction::SDiv:
    case llvm::Instruction::FDiv:
    case llvm::Instruction::URem:
    case llvm::Instruction::SRem:
    case llvm::Instruction::FRem:
    case llvm::Instruction::And:
    case llvm::Instruction::Or:
    case llvm::Instruction::Xor:
    case llvm::Instruction::Shl:
    case llvm::Instruction::LShr:
    case llvm::Instruction::AShr:
      printArithmeticInstruction(ce->getOpcode(), left, right);
      break;
    case llvm::Instruction::ICmp:
    case llvm::Instruction::FCmp:
      printCmpInstruction(ce->getPredicate(), left, right);
      break;
    case llvm::Instruction::GetElementPtr:
      printGepInstruction(ce->getOperand(0), gep_type_begin(ce), gep_type_end(ce));
      break;
    case llvm::Instruction::Select:
      printSelectInstruction(ce->getOperand(0), ce->getOperand(1), ce->getOperand(2));
      break;
    default:
      llvm::errs() << "Expression = " << *ce << '\n';
      llvm_unreachable("Invalid constant expression");
  }
}
