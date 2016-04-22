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

#include <iostream>

#include "backend.hpp"


static llvm::cl::opt<std::string> input(llvm::cl::Positional, llvm::cl::desc("<input bitcode>"), llvm::cl::init("-"));
static llvm::cl::opt<std::string> classname("classname", llvm::cl::desc("Binary name of the generated class"));

enum DebugLevel {
  g0 = 0,
  g1 = 1,
  g2 = 2,
  g3 = 3
};

llvm::cl::opt<DebugLevel> debugLevel(
  llvm::cl::desc("Debugging level:"),
  llvm::cl::init(g1),
  llvm::cl::values(
    clEnumValN(g2, "g", "Same as -g2"),
    clEnumVal(g0, "No debugging information"),
    clEnumVal(g1, "Source file and line number information (default)"),
    clEnumVal(g2, "-g1 + Local variable information"),
    clEnumVal(g3, "-g2 + Commented LLVM assembly"),
    clEnumValEnd));

int main(int argc, const char *const *argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "LLJVM Backend\n");

  auto bufOrErr = llvm::MemoryBuffer::getFileOrSTDIN(input);
  if (bufOrErr.getError()) {
    std::cerr << "Unable to open bitcode file: " << bufOrErr.getError().message() << std::endl;
    return 1;
  }
  auto buf = std::move(bufOrErr.get());

  auto modOrErr = llvm::parseBitcodeFile(buf->getMemBufferRef(), llvm::getGlobalContext());
  if (!modOrErr) {
    std::cerr << "Unable to parse bitcode file: " << modOrErr.getError().message() << std::endl;
    return 1;
  }
  auto *mod = std::move(modOrErr.get());

  llvm::DataLayout dl(
    "e-p:32:32:32"
      "-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64"
      "-f32:32:32-f64:64:64");
  mod->setDataLayout(&dl);

  llvm::PassManager pm;
  pm.add(new llvm::DataLayoutPass());
  pm.add(llvm::createVerifierPass());
  pm.add(llvm::createGCLoweringPass());
  // TODO: fix switch generation so the following pass is not needed
  pm.add(llvm::createLowerSwitchPass());
  pm.add(llvm::createCFGSimplificationPass());
  pm.add(new JVMWriter(&dl, llvm::fouts(), classname, debugLevel));
  // pm.add(llvm::createGCInfoDeleter());
  pm.run(*mod);

  return 0;
}
