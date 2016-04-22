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
 * Load the given value.
 * 
 * @param v  the value to load
 */
void JVMWriter::printValueLoad(const llvm::Value *v) {
  if (const llvm::Function *f = llvm::dyn_cast<llvm::Function>(v)) {
    std::string sig = getValueName(f)
                      + getCallSignature(f->getFunctionType());
    if (externRefs.count(v))
      printSimpleInstruction("CLASSFORMETHOD", sig);
    else
      printSimpleInstruction("ldc", '"' + classname + '"');
    printSimpleInstruction("ldc", '"' + sig + '"');
    printSimpleInstruction("invokestatic",
                           "lljvm/runtime/Function/getFunctionPointer"
                             "(Ljava/lang/String;Ljava/lang/String;)I");
  } else if (llvm::isa<llvm::GlobalVariable>(v)) {
    const llvm::Type *ty = llvm::cast<llvm::PointerType>(v->getType())->getElementType();
    if (externRefs.count(v))
      printSimpleInstruction("getstatic", getValueName(v) + " I");
    else
      printSimpleInstruction("getstatic",
                             classname + "/" + getValueName(v) + " I");
  } else if (llvm::isa<llvm::ConstantPointerNull>(v)) {
    printPtrLoad(0);
  } else if (const llvm::ConstantExpr *ce = llvm::dyn_cast<llvm::ConstantExpr>(v)) {
    printConstantExpr(ce);
  } else if (const llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(v)) {
    printConstLoad(c);
  } else {
    if (getLocalVarNumber(v) <= 3)
      printSimpleInstruction(
        getTypePrefix(v->getType(), true) + "load_"
        + llvm::utostr(getLocalVarNumber(v))
        + " ; " + getValueName(v));
    else
      printSimpleInstruction(
        getTypePrefix(v->getType(), true) + "load",
        llvm::utostr(getLocalVarNumber(v))
        + " ; " + getValueName(v));
  }
}

/**
 * Store the value currently on top of the stack to the given local variable.
 * 
 * @param v  the Value representing the local variable
 */
void JVMWriter::printValueStore(const llvm::Value *v) {
  if (llvm::isa<llvm::Function>(v) || llvm::isa<llvm::GlobalVariable>(v) || llvm::isa<llvm::Constant>(v)) {
    llvm::errs() << "Value  = " << *v << '\n';
    llvm_unreachable("Invalid value");
  }
  unsigned int bitWidth = getBitWidth(v->getType());
  // truncate int
  if (bitWidth == 16)
    printSimpleInstruction("i2s");
  else if (bitWidth == 8)
    printSimpleInstruction("i2b");
  else if (bitWidth == 1) {
    printSimpleInstruction("iconst_1");
    printSimpleInstruction("iand");
  }
  if (getLocalVarNumber(v) <= 3)
    printSimpleInstruction(
      getTypePrefix(v->getType(), true) + "store_"
      + llvm::utostr(getLocalVarNumber(v))
      + " ; " + getValueName(v));
  else
    printSimpleInstruction(
      getTypePrefix(v->getType(), true) + "store",
      llvm::utostr(getLocalVarNumber(v))
      + " ; " + getValueName(v));
}

/**
 * Load a value from the given address.
 * 
 * @param v  the address
 */
void JVMWriter::printIndirectLoad(const llvm::Value *v) {
  printValueLoad(v);
  const llvm::Type *ty = v->getType();
  if (const llvm::PointerType *p = llvm::dyn_cast<llvm::PointerType>(ty))
    ty = p->getElementType();
  printIndirectLoad(ty);
}

/**
 * Load a value of the given type from the address curently on top of the
 * stack.
 * 
 * @param ty  the type of the value
 */
void JVMWriter::printIndirectLoad(const llvm::Type *ty) {
  printSimpleInstruction("invokestatic", "lljvm/runtime/Memory/load_"
                                         + getTypePostfix(ty) + "(I)" + getTypeDescriptor(ty));
}

/**
 * Store a value at the given address.
 * 
 * @param ptr  the address at which to store the value
 * @param val  the value to store
 */
void JVMWriter::printIndirectStore(const llvm::Value *ptr, const llvm::Value *val) {
  printValueLoad(ptr);
  printValueLoad(val);
  printIndirectStore(val->getType());
}

/**
 * Indirectly store a value of the given type.
 * 
 * @param ty  the type of the value
 */
void JVMWriter::printIndirectStore(const llvm::Type *ty) {
  printSimpleInstruction("invokestatic",
                         "lljvm/runtime/Memory/store(I" + getTypeDescriptor(ty) + ")V");
}
