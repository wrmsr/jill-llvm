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

#include "Backend.h"

using namespace llvm;

/**
 * Print the header.
 */
void JasminWriter::printHeader() {
  if (debug >= 1)
    Out << ".source " << sourceName << "\n";
  Out << ".class public final " << className << "\n"
    ".super java/lang/Object\n\n";
}

/**
 * Print the field declarations.
 */
void JasminWriter::printFields() {
  Out << "; Fields\n";
  for (Module::const_global_iterator i = TheModule->global_begin(), e = TheModule->global_end(); i != e; i++) {
    if (i->isDeclaration()) {
      Out << ".extern field ";
      externRefs.insert(&*i);
    } else
      Out << ".field " << (i->hasLocalLinkage() ? "private " : "public ") << "static final ";
    Out << getValueName(&*i) << ' ' << getTypeDescriptor(i->getType());
    if (debug >= 3)
      Out << " ; " << *i;
    else
      Out << '\n';
  }
  Out << '\n';
}

/**
 * Print the list of external methods.
 */
void JasminWriter::printExternalMethods() {
  Out << "; External methods\n";
  for (Module::const_iterator i = TheModule->begin(), e = TheModule->end(); i != e; i++) {
    if (i->isDeclaration() && !i->isIntrinsic()) {
      const Function *f = &*i;
      const FunctionType *ty = f->getFunctionType();
      Out << ".extern method " << getValueName(f) << getCallSignature(ty);
      // FIXME:
      // if(debug >= 3)
      // Out << " ; " << *ty;
      Out << '\n';
      externRefs.insert(f);
    }
  }
  Out << '\n';
}

/**
 * Print the class constructor.
 */
void JasminWriter::printConstructor() {
  Out << "; Constructor\n"
    ".method private <init>()V\n"
    "\taload_0\n"
    "\tinvokespecial java/lang/Object/<init>()V\n"
    "\treturn\n"
    ".end method\n\n";
}

/**
 * Print the static class initialization method.
 */
void JasminWriter::printClInit() {
  Out << ".method public <clinit>()V\n";
  printSimpleInstruction(".limit stack 4");

  Out << "\n\t; allocate global variables\n";
  for (Module::global_iterator i = TheModule->global_begin(), e = TheModule->global_end(); i != e; i++) {
    if (!i->isDeclaration()) {
      const GlobalVariable *g = &*i;
      const Constant *c = g->getInitializer();
      printConstLoad(
        APInt(32, dataLayout->getTypeAllocSize(c->getType()), false));
      printSimpleInstruction("invokestatic",
                             "lljvm/runtime/Memory/allocateData(I)I");
      printSimpleInstruction("putstatic",
                             className + "/" + getValueName(g) + " I");
    }
  }

  Out << "\n\t; initialise global variables\n";
  for (Module::global_iterator i = TheModule->global_begin(), e = TheModule->global_end(); i != e; i++) {
    if (!i->isDeclaration()) {
      const GlobalVariable *g = &*i;
      const Constant *c = g->getInitializer();
      printSimpleInstruction("getstatic",
                             className + "/" + getValueName(g) + " I");
      printStaticConstant(c);
      printSimpleInstruction("pop");
      Out << '\n';
    }
  }

  printSimpleInstruction("return");
  Out << ".end method\n\n";
}

/**
 * Print the main method.
 */
void JasminWriter::printMainMethod() {
  const Function *f = TheModule->getFunction("main");
  if (!f || f->isDeclaration())
    return;

  Out << ".method public static main([Ljava/lang/String;)V\n";
  printSimpleInstruction(".limit stack 4");

  if (f->arg_size() == 0) {
    printSimpleInstruction("invokestatic", className + "/main()I");
  } else if (f->arg_size() == 2) {
    Function::const_arg_iterator arg1, arg2;
    arg1 = arg2 = f->arg_begin();
    arg2++;
    if (!arg1->getType()->isIntegerTy()
        || arg2->getType()->getTypeID() != Type::PointerTyID)
      llvm_unreachable("main function has invalid type signature");
    printSimpleInstruction("aload_0");
    printSimpleInstruction("arraylength");
    printSimpleInstruction("aload_0");
    printSimpleInstruction("invokestatic",
                           "lljvm/runtime/Memory/storeStack([Ljava/lang/String;)I");
    printSimpleInstruction("invokestatic", className + "/main("
                                           + getTypeDescriptor(arg1->getType())
                                           + getTypeDescriptor(arg2->getType()) + ")I");
  } else {
    llvm_unreachable("main function has invalid number of arguments");
  }

  printSimpleInstruction("invokestatic", "lljvm/lib/c/exit(I)V");
  printSimpleInstruction("return");
  Out << ".end method\n";
}
