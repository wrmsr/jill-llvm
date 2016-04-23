//===-- JasminBackend.Jasmin - Library for converting LLVM code to Jasmin code -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the writing of the LLVM IR as a set of Jasmin calls to the
// LLVM IR interface. The input module is assumed to be verified.
//
//===----------------------------------------------------------------------===//

#include "Backend.h"

using namespace llvm;

static cl::opt<std::string> input(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));
static cl::opt<std::string> className("className", cl::desc("Binary name of the generated class"));

enum DebugLevel {
  g0 = 0,
  g1 = 1,
  g2 = 2,
  g3 = 3
};

cl::opt<DebugLevel> debugLevel(
  cl::desc("Debugging level:"),
  cl::init(g1),
  cl::values(
    clEnumValN(g2, "g", "Same as -g2"),
    clEnumVal(g0, "No debugging information"),
    clEnumVal(g1, "Source file and line number information (default)"),
    clEnumVal(g2, "-g1 + Local variable information"),
    clEnumVal(g3, "-g2 + Commented LLVM assembly"),
    clEnumValEnd));

extern "C" void LLVMInitializeJasminBackendTarget() {
  // Register the target.
  RegisterTargetMachine<JasminTargetMachine> X(TheJasminBackendTarget);
}

namespace llvm {

void initializeJasminWriterPass(PassRegistry&);

}

INITIALIZE_PASS_BEGIN(JasminWriter, "jasmin-writer", "Jasmin Writer", false, false)
  INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(JasminWriter, "jasmin-writer", "Jasmin Writer", false, false)

JasminWriter::JasminWriter(std::unique_ptr<formatted_raw_ostream> o)
: ModulePass(ID), OutOwner(std::move(o)), Out(*OutOwner), uniqueNum(0),
  is_inline(false), indent_level(0) {
  initializeJasminWriterPass(*PassRegistry::getPassRegistry());
}

formatted_raw_ostream &JasminWriter::nl(formatted_raw_ostream &Out, int delta) {
  Out << '\n';
  if (delta >= 0 || indent_level >= unsigned(-delta))
    indent_level += delta;
  Out.indent(indent_level);
  return Out;
}

static inline void sanitize(std::string &str) {
  for (size_t i = 0; i < str.length(); ++i)
    if (!isalnum(str[i]) && str[i] != '_')
      str[i] = '_';
}

static std::string getTypePrefix(Type *Ty) {
  switch (Ty->getTypeID()) {
    case Type::VoidTyID:
      return "void_";
    case Type::IntegerTyID:
      return "int" + utostr(cast<IntegerType>(Ty)->getBitWidth()) + "_";
    case Type::FloatTyID:
      return "float_";
    case Type::DoubleTyID:
      return "double_";
    case Type::LabelTyID:
      return "label_";
    case Type::FunctionTyID:
      return "func_";
    case Type::StructTyID:
      return "struct_";
    case Type::ArrayTyID:
      return "array_";
    case Type::PointerTyID:
      return "ptr_";
    case Type::VectorTyID:
      return "packed_";
    default:
      return "other_";
  }
}

void JasminWriter::error(const std::string &msg) {
  report_fatal_error(msg);
}

// printCFP - Print a floating point constant .. very carefully :)
// This makes sure that conversion to/from floating yields the same binary
// result so that we don't lose precision.
void JasminWriter::printCFP(const ConstantFP *CFP) {
  bool ignored;
  APFloat APF = APFloat(CFP->getValueAPF());  // copy
  if (CFP->getType() == Type::getFloatTy(CFP->getContext()))
    APF.convert(APFloat::IEEEdouble, APFloat::rmNearestTiesToEven, &ignored);
  Out << "ConstantFP::get(mod->getContext(), ";
  Out << "APFloat(";
#if HAVE_PRINTF_A
  char Buffer[100];
  sprintf(Buffer, "%A", APF.convertToDouble());
  if ((!strncmp(Buffer, "0x", 2) ||
       !strncmp(Buffer, "-0x", 3) ||
       !strncmp(Buffer, "+0x", 3)) &&
      APF.bitwiseIsEqual(APFloat(atof(Buffer)))) {
    if (CFP->getType() == Type::getDoubleTy(CFP->getContext()))
      Out << "BitsToDouble(" << Buffer << ")";
    else
      Out << "BitsToFloat((float)" << Buffer << ")";
    Out << ")";
  } else {
#endif
  std::string StrVal = ftostr(CFP->getValueAPF());

  while (StrVal[0] == ' ')
    StrVal.erase(StrVal.begin());

  // Check to make sure that the stringized number is not some string like
  // "Inf" or NaN.  Check that the string matches the "[-+]?[0-9]" regex.
  if (((StrVal[0] >= '0' && StrVal[0] <= '9') ||
       ((StrVal[0] == '-' || StrVal[0] == '+') &&
        (StrVal[1] >= '0' && StrVal[1] <= '9'))) &&
      (CFP->isExactlyValue(atof(StrVal.c_str())))) {
    if (CFP->getType() == Type::getDoubleTy(CFP->getContext()))
      Out << StrVal;
    else
      Out << StrVal << "f";
  } else if (CFP->getType() == Type::getDoubleTy(CFP->getContext()))
    Out << "BitsToDouble(0x"
    << utohexstr(CFP->getValueAPF().bitcastToAPInt().getZExtValue())
    << "ULL) /* " << StrVal << " */";
  else
    Out << "BitsToFloat(0x"
    << utohexstr((uint32_t) CFP->getValueAPF().
      bitcastToAPInt().getZExtValue())
    << "U) /* " << StrVal << " */";
  Out << ")";
#if HAVE_PRINTF_A
  }
#endif
  Out << ")";
}

void JasminWriter::printCallingConv(CallingConv::ID cc) {
  // Print the calling convention.
  switch (cc) {
    case CallingConv::C:
      Out << "CallingConv::C";
      break;
    case CallingConv::Fast:
      Out << "CallingConv::Fast";
      break;
    case CallingConv::Cold:
      Out << "CallingConv::Cold";
      break;
    case CallingConv::FirstTargetCC:
      Out << "CallingConv::FirstTargetCC";
      break;
    default:
      Out << cc;
      break;
  }
}

// printEscapedString - Print each character of the specified string, escaping
// it if it is not printable or if it is an escape char.
void JasminWriter::printEscapedString(const std::string &Str) {
  for (unsigned i = 0, e = Str.size(); i != e; ++i) {
    unsigned char C = Str[i];
    if (isprint(C) && C != '"' && C != '\\') {
      Out << C;
    } else {
      Out << "\\x"
      << (char) ((C / 16 < 10) ? (C / 16 + '0') : (C / 16 - 10 + 'A'))
      << (char) (((C & 15) < 10) ? ((C & 15) + '0') : ((C & 15) - 10 + 'A'));
    }
  }
}

std::string JasminWriter::getJasminName(Type *Ty) {
  switch (Ty->getTypeID()) {
    default:
      break;
    case Type::VoidTyID:
      return "Type::getVoidTy(mod->getContext())";
    case Type::IntegerTyID: {
      unsigned BitWidth = cast<IntegerType>(Ty)->getBitWidth();
      return "IntegerType::get(mod->getContext(), " + utostr(BitWidth) + ")";
    }
    case Type::X86_FP80TyID:
      return "Type::getX86_FP80Ty(mod->getContext())";
    case Type::FloatTyID:
      return "Type::getFloatTy(mod->getContext())";
    case Type::DoubleTyID:
      return "Type::getDoubleTy(mod->getContext())";
    case Type::LabelTyID:
      return "Type::getLabelTy(mod->getContext())";
    case Type::X86_MMXTyID:
      return "Type::getX86_MMXTy(mod->getContext())";
  }

  // Now, see if we've seen the type before and return that
  TypeMap::iterator I = TypeNames.find(Ty);
  if (I != TypeNames.end())
    return I->second;

  // Okay, let's build a new name for this type. Start with a prefix
  const char *prefix = nullptr;
  switch (Ty->getTypeID()) {
    case Type::FunctionTyID:
      prefix = "FuncTy_";
      break;
    case Type::StructTyID:
      prefix = "StructTy_";
      break;
    case Type::ArrayTyID:
      prefix = "ArrayTy_";
      break;
    case Type::PointerTyID:
      prefix = "PointerTy_";
      break;
    case Type::VectorTyID:
      prefix = "VectorTy_";
      break;
    default:
      prefix = "OtherTy_";
      break; // prevent breakage
  }

  // See if the type has a name in the symboltable and build accordingly
  std::string name;
  if (StructType *STy = dyn_cast<StructType>(Ty)) if (STy->hasName())
    name = STy->getName();

  if (name.empty())
    name = utostr(uniqueNum++);

  name = std::string(prefix) + name;
  sanitize(name);

  // Save the name
  return TypeNames[Ty] = name;
}

void JasminWriter::printJasminName(Type *Ty) {
  printEscapedString(getJasminName(Ty));
}

std::string JasminWriter::getJasminName(const Value *val) {
  std::string name;
  ValueMap::iterator I = ValueNames.find(val);
  if (I != ValueNames.end() && I->first == val)
    return I->second;

  if (const GlobalVariable *GV = dyn_cast<GlobalVariable>(val)) {
    name = std::string("gvar_") + getTypePrefix(GV->getType()->getElementType());
  } else if (isa<Function>(val)) {
    name = std::string("func_");
  } else if (const Constant *C = dyn_cast<Constant>(val)) {
    name = std::string("const_") + getTypePrefix(C->getType());
  } else if (const Argument *Arg = dyn_cast<Argument>(val)) {
    if (is_inline) {
      unsigned argNum = std::distance(Arg->getParent()->arg_begin(),
                                      Function::const_arg_iterator(Arg)) + 1;
      name = std::string("arg_") + utostr(argNum);
      NameSet::iterator NI = UsedNames.find(name);
      if (NI != UsedNames.end())
        name += std::string("_") + utostr(uniqueNum++);
      UsedNames.insert(name);
      return ValueNames[val] = name;
    } else {
      name = getTypePrefix(val->getType());
    }
  } else {
    name = getTypePrefix(val->getType());
  }
  if (val->hasName())
    name += val->getName();
  else
    name += utostr(uniqueNum++);
  sanitize(name);
  NameSet::iterator NI = UsedNames.find(name);
  if (NI != UsedNames.end())
    name += std::string("_") + utostr(uniqueNum++);
  UsedNames.insert(name);
  return ValueNames[val] = name;
}

void JasminWriter::printJasminName(const Value *val) {
  printEscapedString(getJasminName(val));
}

void JasminWriter::printAttributes(const AttributeSet &PAL,
                                   const std::string &name) {
  Out << "AttributeSet " << name << "_PAL;";
  nl(Out);
  if (!PAL.isEmpty()) {
    Out << '{';
    in();
    nl(Out);
    Out << "SmallVector<AttributeSet, 4> Attrs;";
    nl(Out);
    Out << "AttributeSet PAS;";
    in();
    nl(Out);
    for (unsigned i = 0; i < PAL.getNumSlots(); ++i) {
      unsigned index = PAL.getSlotIndex(i);
      AttrBuilder attrs(PAL.getSlotAttributes(i), index);
      Out << "{";
      in();
      nl(Out);
      Out << "AttrBuilder B;";
      nl(Out);

#define HANDLE_ATTR(X)                                                  \
      if (attrs.contains(Attribute::X)) {                               \
        Out << "B.addAttribute(Attribute::" #X ");"; nl(Out);           \
        attrs.removeAttribute(Attribute::X);                            \
      }

      HANDLE_ATTR(SExt);
      HANDLE_ATTR(ZExt);
      HANDLE_ATTR(NoReturn);
      HANDLE_ATTR(InReg);
      HANDLE_ATTR(StructRet);
      HANDLE_ATTR(NoUnwind);
      HANDLE_ATTR(NoAlias);
      HANDLE_ATTR(ByVal);
      HANDLE_ATTR(InAlloca);
      HANDLE_ATTR(Nest);
      HANDLE_ATTR(ReadNone);
      HANDLE_ATTR(ReadOnly);
      HANDLE_ATTR(NoInline);
      HANDLE_ATTR(AlwaysInline);
      HANDLE_ATTR(OptimizeNone);
      HANDLE_ATTR(OptimizeForSize);
      HANDLE_ATTR(StackProtect);
      HANDLE_ATTR(StackProtectReq);
      HANDLE_ATTR(StackProtectStrong);
      HANDLE_ATTR(SafeStack);
      HANDLE_ATTR(NoCapture);
      HANDLE_ATTR(NoRedZone);
      HANDLE_ATTR(NoImplicitFloat);
      HANDLE_ATTR(Naked);
      HANDLE_ATTR(InlineHint);
      HANDLE_ATTR(ReturnsTwice);
      HANDLE_ATTR(UWTable);
      HANDLE_ATTR(NonLazyBind);
      HANDLE_ATTR(MinSize);
#undef HANDLE_ATTR

      if (attrs.contains(Attribute::StackAlignment)) {
        Out << "B.addStackAlignmentAttr(" << attrs.getStackAlignment() << ')';
        nl(Out);
        attrs.removeAttribute(Attribute::StackAlignment);
      }

      Out << "PAS = AttributeSet::get(mod->getContext(), ";
      if (index == ~0U)
        Out << "~0U,";
      else
        Out << index << "U,";
      Out << " B);";
      out();
      nl(Out);
      Out << "}";
      out();
      nl(Out);
      nl(Out);
      Out << "Attrs.push_back(PAS);";
      nl(Out);
    }
    Out << name << "_PAL = AttributeSet::get(mod->getContext(), Attrs);";
    nl(Out);
    out();
    nl(Out);
    Out << '}';
    nl(Out);
  }
}

void JasminWriter::printType(Type *Ty) {
  // We don't print definitions for primitive types
  if (Ty->isFloatingPointTy() || Ty->isX86_MMXTy() || Ty->isIntegerTy() ||
      Ty->isLabelTy() || Ty->isMetadataTy() || Ty->isVoidTy() ||
      Ty->isTokenTy())
    return;

  // If we already defined this type, we don't need to define it again.
  if (DefinedTypes.find(Ty) != DefinedTypes.end())
    return;

  // Everything below needs the name for the type so get it now.
  std::string typeName(getJasminName(Ty));

  // Print the type definition
  switch (Ty->getTypeID()) {
    case Type::FunctionTyID: {
      FunctionType *FT = cast<FunctionType>(Ty);
      Out << "std::vector<Type*>" << typeName << "_args;";
      nl(Out);
      FunctionType::param_iterator PI = FT->param_begin();
      FunctionType::param_iterator PE = FT->param_end();
      for (; PI != PE; ++PI) {
        Type *argTy = static_cast<Type *>(*PI);
        printType(argTy);
        std::string argName(getJasminName(argTy));
        Out << typeName << "_args.push_back(" << argName;
        Out << ");";
        nl(Out);
      }
      printType(FT->getReturnType());
      std::string retTypeName(getJasminName(FT->getReturnType()));
      Out << "FunctionType* " << typeName << " = FunctionType::get(";
      in();
      nl(Out) << "/*Result=*/" << retTypeName;
      Out << ",";
      nl(Out) << "/*Params=*/" << typeName << "_args,";
      nl(Out) << "/*isVarArg=*/" << (FT->isVarArg() ? "true" : "false") << ");";
      out();
      nl(Out);
      break;
    }
    case Type::StructTyID: {
      StructType *ST = cast<StructType>(Ty);
      if (!ST->isLiteral()) {
        Out << "StructType *" << typeName << " = mod->getTypeByName(\"";
        printEscapedString(ST->getName());
        Out << "\");";
        nl(Out);
        Out << "if (!" << typeName << ") {";
        nl(Out);
        Out << typeName << " = ";
        Out << "StructType::create(mod->getContext(), \"";
        printEscapedString(ST->getName());
        Out << "\");";
        nl(Out);
        Out << "}";
        nl(Out);
        // Indicate that this type is now defined.
        DefinedTypes.insert(Ty);
      }

      Out << "std::vector<Type*>" << typeName << "_fields;";
      nl(Out);
      StructType::element_iterator EI = ST->element_begin();
      StructType::element_iterator EE = ST->element_end();
      for (; EI != EE; ++EI) {
        Type *fieldTy = static_cast<Type *>(*EI);
        printType(fieldTy);
        std::string fieldName(getJasminName(fieldTy));
        Out << typeName << "_fields.push_back(" << fieldName;
        Out << ");";
        nl(Out);
      }

      if (ST->isLiteral()) {
        Out << "StructType *" << typeName << " = ";
        Out << "StructType::get(" << "mod->getContext(), ";
      } else {
        Out << "if (" << typeName << "->isOpaque()) {";
        nl(Out);
        Out << typeName << "->setBody(";
      }

      Out << typeName << "_fields, /*isPacked=*/"
      << (ST->isPacked() ? "true" : "false") << ");";
      nl(Out);
      if (!ST->isLiteral()) {
        Out << "}";
        nl(Out);
      }
      break;
    }
    case Type::ArrayTyID: {
      ArrayType *AT = cast<ArrayType>(Ty);
      Type *ET = AT->getElementType();
      printType(ET);
      if (DefinedTypes.find(Ty) == DefinedTypes.end()) {
        std::string elemName(getJasminName(ET));
        Out << "ArrayType* " << typeName << " = ArrayType::get("
        << elemName << ", " << AT->getNumElements() << ");";
        nl(Out);
      }
      break;
    }
    case Type::PointerTyID: {
      PointerType *PT = cast<PointerType>(Ty);
      Type *ET = PT->getElementType();
      printType(ET);
      if (DefinedTypes.find(Ty) == DefinedTypes.end()) {
        std::string elemName(getJasminName(ET));
        Out << "PointerType* " << typeName << " = PointerType::get("
        << elemName << ", " << PT->getAddressSpace() << ");";
        nl(Out);
      }
      break;
    }
    case Type::VectorTyID: {
      VectorType *PT = cast<VectorType>(Ty);
      Type *ET = PT->getElementType();
      printType(ET);
      if (DefinedTypes.find(Ty) == DefinedTypes.end()) {
        std::string elemName(getJasminName(ET));
        Out << "VectorType* " << typeName << " = VectorType::get("
        << elemName << ", " << PT->getNumElements() << ");";
        nl(Out);
      }
      break;
    }
    default:
      error("Invalid TypeID");
  }

  // Indicate that this type is now defined.
  DefinedTypes.insert(Ty);

  // Finally, separate the type definition from other with a newline.
  nl(Out);
}

void JasminWriter::printTypes(const Module *M) {
  // Add all of the global variables to the value table.
  for (Module::const_global_iterator I = TheModule->global_begin(),
         E = TheModule->global_end(); I != E; ++I) {
    if (I->hasInitializer())
      printType(I->getInitializer()->getType());
    printType(I->getType());
  }

  // Add all the functions to the table
  for (Module::const_iterator FI = TheModule->begin(), FE = TheModule->end();
       FI != FE; ++FI) {
    printType(FI->getReturnType());
    printType(FI->getFunctionType());
    // Add all the function arguments
    for (Function::const_arg_iterator AI = FI->arg_begin(),
           AE = FI->arg_end(); AI != AE; ++AI) {
      printType(AI->getType());
    }

    // Add all of the basic blocks and instructions
    for (Function::const_iterator BB = FI->begin(),
           E = FI->end(); BB != E; ++BB) {
      printType(BB->getType());
      for (BasicBlock::const_iterator I = BB->begin(), E = BB->end(); I != E;
           ++I) {
        printType(I->getType());
        for (unsigned i = 0; i < I->getNumOperands(); ++i)
          printType(I->getOperand(i)->getType());
      }
    }
  }
}


// printConstant - Print out a constant pool entry...
void JasminWriter::printConstant(const Constant *CV) {
  // First, if the constant is actually a GlobalValue (variable or function)
  // or its already in the constant list then we've printed it already and we
  // can just return.
  if (isa<GlobalValue>(CV) || ValueNames.find(CV) != ValueNames.end())
    return;

  std::string constName(getJasminName(CV));
  std::string typeName(getJasminName(CV->getType()));

  if (const ConstantInt *CI = dyn_cast<ConstantInt>(CV)) {
    std::string constValue = CI->getValue().toString(10, true);
    Out << "ConstantInt* " << constName
    << " = ConstantInt::get(mod->getContext(), APInt("
    << cast<IntegerType>(CI->getType())->getBitWidth()
    << ", StringRef(\"" << constValue << "\"), 10));";
  } else if (isa<ConstantAggregateZero>(CV)) {
    Out << "ConstantAggregateZero* " << constName
    << " = ConstantAggregateZero::get(" << typeName << ");";
  } else if (isa<ConstantPointerNull>(CV)) {
    Out << "ConstantPointerNull* " << constName
    << " = ConstantPointerNull::get(" << typeName << ");";
  } else if (const ConstantFP *CFP = dyn_cast<ConstantFP>(CV)) {
    Out << "ConstantFP* " << constName << " = ";
    printCFP(CFP);
    Out << ";";
  } else if (const ConstantArray *CA = dyn_cast<ConstantArray>(CV)) {
    Out << "std::vector<Constant*> " << constName << "_elems;";
    nl(Out);
    unsigned N = CA->getNumOperands();
    for (unsigned i = 0; i < N; ++i) {
      printConstant(CA->getOperand(i)); // recurse to print operands
      Out << constName << "_elems.push_back("
      << getJasminName(CA->getOperand(i)) << ");";
      nl(Out);
    }
    Out << "Constant* " << constName << " = ConstantArray::get("
    << typeName << ", " << constName << "_elems);";
  } else if (const ConstantStruct *CS = dyn_cast<ConstantStruct>(CV)) {
    Out << "std::vector<Constant*> " << constName << "_fields;";
    nl(Out);
    unsigned N = CS->getNumOperands();
    for (unsigned i = 0; i < N; i++) {
      printConstant(CS->getOperand(i));
      Out << constName << "_fields.push_back("
      << getJasminName(CS->getOperand(i)) << ");";
      nl(Out);
    }
    Out << "Constant* " << constName << " = ConstantStruct::get("
    << typeName << ", " << constName << "_fields);";
  } else if (const ConstantVector *CVec = dyn_cast<ConstantVector>(CV)) {
    Out << "std::vector<Constant*> " << constName << "_elems;";
    nl(Out);
    unsigned N = CVec->getNumOperands();
    for (unsigned i = 0; i < N; ++i) {
      printConstant(CVec->getOperand(i));
      Out << constName << "_elems.push_back("
      << getJasminName(CVec->getOperand(i)) << ");";
      nl(Out);
    }
    Out << "Constant* " << constName << " = ConstantVector::get("
    << typeName << ", " << constName << "_elems);";
  } else if (isa<UndefValue>(CV)) {
    Out << "UndefValue* " << constName << " = UndefValue::get("
    << typeName << ");";
  } else if (const ConstantDataSequential *CDS =
    dyn_cast<ConstantDataSequential>(CV)) {
    if (CDS->isString()) {
      Out << "Constant *" << constName <<
      " = ConstantDataArray::getString(mod->getContext(), \"";
      StringRef Str = CDS->getAsString();
      bool nullTerminate = false;
      if (Str.back() == 0) {
        Str = Str.drop_back();
        nullTerminate = true;
      }
      printEscapedString(Str);
      // Determine if we want null termination or not.
      if (nullTerminate)
        Out << "\", true);";
      else
        Out << "\", false);";// No null terminator
    } else {
      // TODO: Could generate more efficient code generating CDS calls instead.
      Out << "std::vector<Constant*> " << constName << "_elems;";
      nl(Out);
      for (unsigned i = 0; i != CDS->getNumElements(); ++i) {
        Constant *Elt = CDS->getElementAsConstant(i);
        printConstant(Elt);
        Out << constName << "_elems.push_back(" << getJasminName(Elt) << ");";
        nl(Out);
      }
      Out << "Constant* " << constName;

      if (isa<ArrayType>(CDS->getType()))
        Out << " = ConstantArray::get(";
      else
        Out << " = ConstantVector::get(";
      Out << typeName << ", " << constName << "_elems);";
    }
  } else if (const ConstantExpr *CE = dyn_cast<ConstantExpr>(CV)) {
    if (CE->getOpcode() == Instruction::GetElementPtr) {
      Out << "std::vector<Constant*> " << constName << "_indices;";
      nl(Out);
      printConstant(CE->getOperand(0));
      for (unsigned i = 1; i < CE->getNumOperands(); ++i) {
        printConstant(CE->getOperand(i));
        Out << constName << "_indices.push_back("
        << getJasminName(CE->getOperand(i)) << ");";
        nl(Out);
      }
      Out << "Constant* " << constName << " = ConstantExpr::getGetElementPtr(" << getJasminName(CE->getOperand(0)) <<
      ", " << constName << "_indices);";
    } else if (CE->isCast()) {
      printConstant(CE->getOperand(0));
      Out << "Constant* " << constName << " = ConstantExpr::getCast(";
      switch (CE->getOpcode()) {
        default:
          llvm_unreachable("Invalid cast opcode");
        case Instruction::Trunc:
          Out << "Instruction::Trunc";
          break;
        case Instruction::ZExt:
          Out << "Instruction::ZExt";
          break;
        case Instruction::SExt:
          Out << "Instruction::SExt";
          break;
        case Instruction::FPTrunc:
          Out << "Instruction::FPTrunc";
          break;
        case Instruction::FPExt:
          Out << "Instruction::FPExt";
          break;
        case Instruction::FPToUI:
          Out << "Instruction::FPToUI";
          break;
        case Instruction::FPToSI:
          Out << "Instruction::FPToSI";
          break;
        case Instruction::UIToFP:
          Out << "Instruction::UIToFP";
          break;
        case Instruction::SIToFP:
          Out << "Instruction::SIToFP";
          break;
        case Instruction::PtrToInt:
          Out << "Instruction::PtrToInt";
          break;
        case Instruction::IntToPtr:
          Out << "Instruction::IntToPtr";
          break;
        case Instruction::BitCast:
          Out << "Instruction::BitCast";
          break;
      }
      Out << ", " << getJasminName(CE->getOperand(0)) << ", "
      << getJasminName(CE->getType()) << ");";
    } else {
      unsigned N = CE->getNumOperands();
      for (unsigned i = 0; i < N; ++i) {
        printConstant(CE->getOperand(i));
      }
      Out << "Constant* " << constName << " = ConstantExpr::";
      switch (CE->getOpcode()) {
        case Instruction::Add:
          Out << "getAdd(";
          break;
        case Instruction::FAdd:
          Out << "getFAdd(";
          break;
        case Instruction::Sub:
          Out << "getSub(";
          break;
        case Instruction::FSub:
          Out << "getFSub(";
          break;
        case Instruction::Mul:
          Out << "getMul(";
          break;
        case Instruction::FMul:
          Out << "getFMul(";
          break;
        case Instruction::UDiv:
          Out << "getUDiv(";
          break;
        case Instruction::SDiv:
          Out << "getSDiv(";
          break;
        case Instruction::FDiv:
          Out << "getFDiv(";
          break;
        case Instruction::URem:
          Out << "getURem(";
          break;
        case Instruction::SRem:
          Out << "getSRem(";
          break;
        case Instruction::FRem:
          Out << "getFRem(";
          break;
        case Instruction::And:
          Out << "getAnd(";
          break;
        case Instruction::Or:
          Out << "getOr(";
          break;
        case Instruction::Xor:
          Out << "getXor(";
          break;
        case Instruction::ICmp:
          Out << "getICmp(ICmpInst::ICMP_";
          switch (CE->getPredicate()) {
            case ICmpInst::ICMP_EQ:
              Out << "EQ";
              break;
            case ICmpInst::ICMP_NE:
              Out << "NE";
              break;
            case ICmpInst::ICMP_SLT:
              Out << "SLT";
              break;
            case ICmpInst::ICMP_ULT:
              Out << "ULT";
              break;
            case ICmpInst::ICMP_SGT:
              Out << "SGT";
              break;
            case ICmpInst::ICMP_UGT:
              Out << "UGT";
              break;
            case ICmpInst::ICMP_SLE:
              Out << "SLE";
              break;
            case ICmpInst::ICMP_ULE:
              Out << "ULE";
              break;
            case ICmpInst::ICMP_SGE:
              Out << "SGE";
              break;
            case ICmpInst::ICMP_UGE:
              Out << "UGE";
              break;
            default:
              error("Invalid ICmp Predicate");
          }
          break;
        case Instruction::FCmp:
          Out << "getFCmp(FCmpInst::FCMP_";
          switch (CE->getPredicate()) {
            case FCmpInst::FCMP_FALSE:
              Out << "FALSE";
              break;
            case FCmpInst::FCMP_ORD:
              Out << "ORD";
              break;
            case FCmpInst::FCMP_UNO:
              Out << "UNO";
              break;
            case FCmpInst::FCMP_OEQ:
              Out << "OEQ";
              break;
            case FCmpInst::FCMP_UEQ:
              Out << "UEQ";
              break;
            case FCmpInst::FCMP_ONE:
              Out << "ONE";
              break;
            case FCmpInst::FCMP_UNE:
              Out << "UNE";
              break;
            case FCmpInst::FCMP_OLT:
              Out << "OLT";
              break;
            case FCmpInst::FCMP_ULT:
              Out << "ULT";
              break;
            case FCmpInst::FCMP_OGT:
              Out << "OGT";
              break;
            case FCmpInst::FCMP_UGT:
              Out << "UGT";
              break;
            case FCmpInst::FCMP_OLE:
              Out << "OLE";
              break;
            case FCmpInst::FCMP_ULE:
              Out << "ULE";
              break;
            case FCmpInst::FCMP_OGE:
              Out << "OGE";
              break;
            case FCmpInst::FCMP_UGE:
              Out << "UGE";
              break;
            case FCmpInst::FCMP_TRUE:
              Out << "TRUE";
              break;
            default:
              error("Invalid FCmp Predicate");
          }
          break;
        case Instruction::Shl:
          Out << "getShl(";
          break;
        case Instruction::LShr:
          Out << "getLShr(";
          break;
        case Instruction::AShr:
          Out << "getAShr(";
          break;
        case Instruction::Select:
          Out << "getSelect(";
          break;
        case Instruction::ExtractElement:
          Out << "getExtractElement(";
          break;
        case Instruction::InsertElement:
          Out << "getInsertElement(";
          break;
        case Instruction::ShuffleVector:
          Out << "getShuffleVector(";
          break;
        default:
          error("Invalid constant expression");
          break;
      }
      Out << getJasminName(CE->getOperand(0));
      for (unsigned i = 1; i < CE->getNumOperands(); ++i)
        Out << ", " << getJasminName(CE->getOperand(i));
      Out << ");";
    }
  } else if (const BlockAddress *BA = dyn_cast<BlockAddress>(CV)) {
    Out << "Constant* " << constName << " = ";
    Out << "BlockAddress::get(" << getOpName(BA->getBasicBlock()) << ");";
  } else {
    error("Bad Constant");
    Out << "Constant* " << constName << " = 0; ";
  }
  nl(Out);
}

void JasminWriter::printConstants(const Module *M) {
  // Traverse all the global variables looking for constant initializers
  for (Module::const_global_iterator I = TheModule->global_begin(),
         E = TheModule->global_end(); I != E; ++I)
    if (I->hasInitializer())
      printConstant(I->getInitializer());

  // Traverse the LLVM functions looking for constants
  for (Module::const_iterator FI = TheModule->begin(), FE = TheModule->end();
       FI != FE; ++FI) {
    // Add all of the basic blocks and instructions
    for (Function::const_iterator BB = FI->begin(),
           E = FI->end(); BB != E; ++BB) {
      for (BasicBlock::const_iterator I = BB->begin(), E = BB->end(); I != E;
           ++I) {
        for (unsigned i = 0; i < I->getNumOperands(); ++i) {
          if (Constant *C = dyn_cast<Constant>(I->getOperand(i))) {
            printConstant(C);
          }
        }
      }
    }
  }
}

void JasminWriter::printVariableBody(const GlobalVariable *GV) {
  if (GV->hasInitializer()) {
    printJasminName(GV);
    Out << "->setInitializer(";
    Out << getJasminName(GV->getInitializer()) << ");";
    nl(Out);
  }
}

std::string JasminWriter::getOpName(const Value *V) {
  if (!isa<Instruction>(V) || DefinedValues.find(V) != DefinedValues.end())
    return getJasminName(V);

  // See if its alread in the map of forward references, if so just return the
  // name we already set up for it
  ForwardRefMap::const_iterator I = ForwardRefs.find(V);
  if (I != ForwardRefs.end())
    return I->second;

  // This is a new forward reference. Generate a unique name for it
  std::string result(std::string("fwdref_") + utostr(uniqueNum++));

  // Yes, this is a hack. An Argument is the smallest instantiable value that
  // we can make as a placeholder for the real value. We'll replace these
  // Argument instances later.
  Out << "Argument* " << result << " = new Argument("
  << getJasminName(V->getType()) << ");";
  nl(Out);
  ForwardRefs[V] = result;
  return result;
}

static StringRef ConvertAtomicOrdering(AtomicOrdering Ordering) {
  switch (Ordering) {
    case NotAtomic:
      return "NotAtomic";
    case Unordered:
      return "Unordered";
    case Monotonic:
      return "Monotonic";
    case Acquire:
      return "Acquire";
    case Release:
      return "Release";
    case AcquireRelease:
      return "AcquireRelease";
    case SequentiallyConsistent:
      return "SequentiallyConsistent";
  }
  llvm_unreachable("Unknown ordering");
}

static StringRef ConvertAtomicSynchScope(SynchronizationScope SynchScope) {
  switch (SynchScope) {
    case SingleThread:
      return "SingleThread";
    case CrossThread:
      return "CrossThread";
  }
  llvm_unreachable("Unknown synch scope");
}

/**
 * Register required analysis information.
 *
 * @param au  AnalysisUsage object representing the analysis usage information
 *            of this pass.
 */
void JasminWriter::getAnalysisUsage(AnalysisUsage &au) const {
//  au.addRequired<LoopInfoWrapperPass>();
//  au.setPreservesAll();

  au.addRequired<LoopInfoWrapperPass>();
  au.addPreserved<LoopInfoWrapperPass>();
}

/**
 * Process the given function.
 *
 * @param f  the function to process
 * @return   whether the function was modified (always false)
 */
bool JasminWriter::runOnFunction(Function &f) {
  if (!f.isDeclaration() && !f.hasAvailableExternallyLinkage())
    printFunction(f);
  return false;
}

//int main(int argc, const char *const *argv) {
//  cl::ParseCommandLineOptions(argc, argv, "LLJVM Backend\n");
//
//  auto bufOrErr = MemoryBuffer::getFileOrSTDIN(input);
//  if (bufOrErr.getError()) {
//    std::cerr << "Unable to open bitcode file: " << bufOrErr.getError().message() << std::endl;
//    return 1;
//  }
//  auto buf = std::move(bufOrErr.get());
//
//  auto modOrErr = parseBitcodeFile(buf->getMemBufferRef(), getGlobalContext());
//  if (!modOrErr) {
//    std::cerr << "Unable to parse bitcode file: " << modOrErr.getError().message() << std::endl;
//    return 1;
//  }
//  auto *mod = std::move(modOrErr.get());
//
//  DataLayout dl(
//    "e-p:32:32:32"
//      "-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64"
//      "-f32:32:32-f64:64:64");
//  mod->setDataLayout(&dl);
//
//  PassManager pm;
//  pm.add(new DataLayout());
//  pm.add(createVerifierPass());
//  pm.add(createGCLoweringPass());
//  // TODO: fix switch generation so the following pass is not needed
//  pm.add(createLowerSwitchPass());
//  pm.add(createCFGSimplificationPass());
//  pm.add(new JasminWriter(&dl, fouts(), className, debugLevel));
//  // pm.add(createGCInfoDeleter());
//  pm.run(*mod);
//
//  return 0;
//}

//  : className(cls), debug(dbg), dataLayout(dl) {

const DataLayout dataLayout(
  "e-p:32:32:32"
    "-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64"
    "-f32:32:32-f64:64:64");

bool JasminWriter::runOnModule(Module &M) {
  TheModule = &M;

  className = "wtf";
  dataLayout = &::dataLayout;
  TheModule->setDataLayout(*dataLayout);
  debug = debugLevel.getValue();

  instNum = 0;

  std::string modID = TheModule->getModuleIdentifier();
  size_t slashPos = modID.rfind('/');
  if (slashPos == std::string::npos)
    sourceName = modID;
  else
    sourceName = modID.substr(slashPos + 1);

  if (!className.empty()) {
    for (std::string::iterator i = className.begin(),
           e = className.end(); i != e; i++)
      if (*i == '.') *i = '/';
  } else {
    className = sourceName.substr(0, sourceName.rfind('.'));
    for (std::string::iterator i = className.begin(),
           e = className.end(); i != e; i++)
      if (*i == '.') *i = '_';
  }

  printHeader();
  printFields();
  printExternalMethods();
  printConstructor();
  printClInit();
  printMainMethod();

  for (Function &I : *TheModule) {
    runOnFunction(I);
  }
  nl(Out);

  return false;
}

char JasminWriter::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool JasminTargetMachine::addPassesToEmitFile(
  PassManagerBase &PM, raw_pwrite_stream &o, CodeGenFileType FileType,
  bool DisableVerify, AnalysisID StartBefore, AnalysisID StartAfter,
  AnalysisID StopAfter, MachineFunctionInitializer *MFInitializer) {
  if (FileType != TargetMachine::CGFT_AssemblyFile)
    return true;
  auto FOut = llvm::make_unique<formatted_raw_ostream>(o);
  PM.add(new JasminWriter(std::move(FOut)));
  return false;
}
