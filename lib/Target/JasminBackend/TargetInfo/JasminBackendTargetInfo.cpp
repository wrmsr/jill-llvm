//===-- JasminBackendTargetInfo.Jasmin - JasminBackend Target Implementation -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "JasminTargetMachine.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target TheJasminBackendTarget;

static bool JasminBackend_TripleMatchQuality(Triple::ArchType Arch) {
  // This backend doesn't correspond to any architecture. It must be explicitly
  // selected with -march.
  return false;
}

extern "C" void LLVMInitializeJasminBackendTargetInfo() {
  TargetRegistry::RegisterTarget(llvm::TheJasminBackendTarget, "jasmin",
                                  "Jasmin backend",
                                  &JasminBackend_TripleMatchQuality);
}

extern "C" void LLVMInitializeJasminBackendTargetMC() {}
