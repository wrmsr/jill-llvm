#include "Backend.h"

using namespace llvm;

std::string ftostr(const APFloat &V) {
  std::string Buf;
  if (&V.getSemantics() == &APFloat::IEEEdouble) {
    raw_string_ostream(Buf) << V.convertToDouble();
    return Buf;
  } else if (&V.getSemantics() == &APFloat::IEEEsingle) {
    raw_string_ostream(Buf) << (double) V.convertToFloat();
    return Buf;
  }
  return "<unknown format in ftostr>"; // error
}

std::string ftostr(double V) {
  char Buffer[200];
  sprintf(Buffer, "%20.6e", V);
  char *B = Buffer;
  while (*B == ' ') ++B;
  return B;
}
