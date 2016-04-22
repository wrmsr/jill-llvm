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
 * Print the given basic block.
 * 
 * @param block  the basic block
 */
void JVMWriter::printBasicBlock(const llvm::BasicBlock *block) {
  printLabel(getLabelName(block));
  for (llvm::BasicBlock::const_iterator i = block->begin(), e = block->end();
       i != e; i++) {
    instNum++;
    if (debug >= 3) {
      // print current instruction as comment
      // note that this block of code significantly increases
      // code generation time
      std::string str;
      llvm::raw_string_ostream ss(str);
      ss << *i;
      std::string::size_type pos = 0;
      while ((pos = str.find("\n", pos)) != std::string::npos)
        str.replace(pos++, 1, "\n;");
      out << ';' << str << '\n';
    }
    if (debug >= 1)
      printSimpleInstruction(".line", llvm::utostr(instNum));

    if (i->getOpcode() == llvm::Instruction::PHI)
      // don't handle phi instruction in current block
      continue;
    printInstruction(i);
    if (i->getType() != llvm::Type::getVoidTy(block->getContext())
        && i->getOpcode() != llvm::Instruction::Invoke)
      // instruction doesn't return anything, or is an invoke instruction
      // which handles storing the return value itself
      printValueStore(i);
  }
}

/**
 * Print the given instruction.
 * 
 * @param inst  the instruction
 */
void JVMWriter::printInstruction(const llvm::Instruction *inst) {
  const llvm::Value *left, *right;
  if (inst->getNumOperands() >= 1) left = inst->getOperand(0);
  if (inst->getNumOperands() >= 2) right = inst->getOperand(1);
  switch (inst->getOpcode()) {
    case llvm::Instruction::Ret:
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/destroyStackFrame()V");
      if (inst->getNumOperands() >= 1) {
        printValueLoad(left);
        printSimpleInstruction(
          getTypePrefix(left->getType(), true) + "return");
      } else {
        printSimpleInstruction("return");
      }
      break;
    // case llvm::Instruction::Unwind:
    //   printSimpleInstruction("getstatic",
    //                          "lljvm/runtime/Instruction$Unwind/instance "
    //                            "Llljvm/runtime/Instruction$Unwind;");
    //   printSimpleInstruction("athrow");
    //   // TODO: need to destroy stack frames
    //   break;
    case llvm::Instruction::Unreachable:
      printSimpleInstruction("getstatic",
                             "lljvm/runtime/Instruction$Unreachable/instance "
                               "Llljvm/runtime/Instruction$Unreachable;");
      printSimpleInstruction("athrow");
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
      printArithmeticInstruction(inst->getOpcode(), left, right);
      break;
    case llvm::Instruction::SExt:
    case llvm::Instruction::Trunc:
    case llvm::Instruction::ZExt:
    case llvm::Instruction::FPTrunc:
    case llvm::Instruction::FPExt:
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
    case llvm::Instruction::FPToUI:
    case llvm::Instruction::FPToSI:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::BitCast:
      printCastInstruction(inst->getOpcode(), left,
                           llvm::cast<llvm::CastInst>(inst)->getDestTy(),
                           llvm::cast<llvm::CastInst>(inst)->getSrcTy());
      break;
    case llvm::Instruction::ICmp:
    case llvm::Instruction::FCmp:
      printCmpInstruction(llvm::cast<llvm::CmpInst>(inst)->getPredicate(),
                          left, right);
      break;
    case llvm::Instruction::Br:
      printBranchInstruction(llvm::cast<llvm::BranchInst>(inst));
      break;
    case llvm::Instruction::Select:
      printSelectInstruction(inst->getOperand(0),
                             inst->getOperand(1),
                             inst->getOperand(2));
      break;
    case llvm::Instruction::Load:
      printIndirectLoad(inst->getOperand(0));
      break;
    case llvm::Instruction::Store:
      printIndirectStore(inst->getOperand(1), inst->getOperand(0));
      break;
    case llvm::Instruction::GetElementPtr:
      printGepInstruction(inst->getOperand(0),
                          gep_type_begin(inst),
                          gep_type_end(inst));
      break;
    case llvm::Instruction::Call:
      printCallInstruction(llvm::cast<llvm::CallInst>(inst));
      break;
    case llvm::Instruction::Invoke:
      printInvokeInstruction(llvm::cast<llvm::InvokeInst>(inst));
      break;
    case llvm::Instruction::Switch:
      printSwitchInstruction(llvm::cast<llvm::SwitchInst>(inst));
      break;
    case llvm::Instruction::Alloca:
      printAllocaInstruction(llvm::cast<llvm::AllocaInst>(inst));
      break;
    case llvm::Instruction::VAArg:
      printVAArgInstruction(llvm::cast<llvm::VAArgInst>(inst));
      break;
    default:
      llvm::errs() << "Instruction = " << *inst << '\n';
      llvm_unreachable("Unsupported instruction");
  }
}
