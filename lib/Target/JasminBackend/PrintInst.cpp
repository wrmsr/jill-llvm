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

#include "Backend.h"

using namespace llvm;

/**
 * Print the given binary instruction.
 * 
 * @param name   the name of the instruction
 * @param left   the first operand
 * @param right  the second operand
 */
void JasminWriter::printBinaryInstruction(const char *name,
                                       const Value *left,
                                       const Value *right) {
  printValueLoad(left);
  printValueLoad(right);
  Out << '\t' << name << '\n';
}

/**
 * Print the given binary instruction.
 * 
 * @param name   the name of the instruction
 * @param left   the first operand
 * @param right  the second operand
 */
void JasminWriter::printBinaryInstruction(const std::string &name,
                                       const Value *left,
                                       const Value *right) {
  printValueLoad(left);
  printValueLoad(right);
  Out << '\t' << name << '\n';
}

/**
 * Print the given instruction.
 * 
 * @param inst  the instruction
 */
void JasminWriter::printSimpleInstruction(const char *inst) {
  Out << '\t' << inst << '\n';
}

/**
 * Print the given instruction.
 * 
 * @param inst     the instruction
 * @param operand  the operand to the instruction
 */
void JasminWriter::printSimpleInstruction(const char *inst, const char *operand) {
  Out << '\t' << inst << ' ' << operand << '\n';
}

/**
 * Print the given instruction.
 * 
 * @param inst  the instruction
 */
void JasminWriter::printSimpleInstruction(const std::string &inst) {
  Out << '\t' << inst << '\n';
}

/**
 * Print the given instruction.
 * 
 * @param inst     the instruction
 * @param operand  the operand to the instruction
 */
void JasminWriter::printSimpleInstruction(const std::string &inst,
                                       const std::string &operand) {
  Out << '\t' << inst << ' ' << operand << '\n';
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig  the signature of the instruction
 */
void JasminWriter::printVirtualInstruction(const char *sig) {
  Out << '\t' << "invokestatic lljvm/runtime/Instruction/" << sig << '\n';
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig      the signature of the instruction
 * @param operand  the operand to the instruction
 */
void JasminWriter::printVirtualInstruction(const char *sig,
                                        const Value *operand) {
  printValueLoad(operand);
  printVirtualInstruction(sig);
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig    the signature of the instruction
 * @param left   the first operand
 * @param right  the second operand
 */
void JasminWriter::printVirtualInstruction(const char *sig,
                                        const Value *left,
                                        const Value *right) {
  printValueLoad(left);
  printValueLoad(right);
  printVirtualInstruction(sig);
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig  the signature of the instruction
 */
void JasminWriter::printVirtualInstruction(const std::string &sig) {
  printVirtualInstruction(sig.c_str());
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig      the signature of the instruction
 * @param operand  the operand to the instruction
 */
void JasminWriter::printVirtualInstruction(const std::string &sig,
                                        const Value *operand) {
  printValueLoad(operand);
  printVirtualInstruction(sig);
}

/**
 * Print the virtual instruction with the given signature.
 * 
 * @param sig    the signature of the instruction
 * @param left   the first operand
 * @param right  the second operand
 */
void JasminWriter::printVirtualInstruction(const std::string &sig,
                                        const Value *left,
                                        const Value *right) {
  printValueLoad(left);
  printValueLoad(right);
  printVirtualInstruction(sig);
}

/**
 * Print the given label.
 * 
 * @param label  the label
 */
void JasminWriter::printLabel(const char *label) {
  Out << label << ":\n";
}

/**
 * Print the given label.
 * 
 * @param label  the label
 */
void JasminWriter::printLabel(const std::string &label) {
  Out << label << ":\n";
}
