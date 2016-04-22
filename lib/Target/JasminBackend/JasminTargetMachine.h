//===-- JasminTargetMachine.h - TargetMachine for the Jasmin backend --*- Jasmin -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the TargetMachine that is used by the Jasmin backend.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_JasminBACKEND_JasminTARGETMACHINE_H
#define LLVM_LIB_TARGET_JasminBACKEND_JasminTARGETMACHINE_H

#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

namespace llvm {

class formatted_raw_ostream;

struct JasminTargetMachine : public TargetMachine {
  JasminTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                   StringRef FS, const TargetOptions &Options, Reloc::Model RM,
                   CodeModel::Model CM, CodeGenOpt::Level OL)
      : TargetMachine(T, "", TT, CPU, FS, Options) {}

public:
  bool addPassesToEmitFile(PassManagerBase &PM, raw_pwrite_stream &Out,
                           CodeGenFileType FileType, bool DisableVerify,
                           AnalysisID StartBefore, AnalysisID StartAfter,
                           AnalysisID StopAfter,
                           MachineFunctionInitializer *MFInitializer) override;
};

extern Target TheJasminBackendTarget;

} // End llvm namespace


#endif
