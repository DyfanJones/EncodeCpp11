// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// encoding.cpp
cpp11::strings url_encoder(cpp11::strings urls, cpp11::strings safe);
extern "C" SEXP _EncodeCpp11_url_encoder(SEXP urls, SEXP safe) {
  BEGIN_CPP11
    return cpp11::as_sexp(url_encoder(cpp11::as_cpp<cpp11::decay_t<cpp11::strings>>(urls), cpp11::as_cpp<cpp11::decay_t<cpp11::strings>>(safe)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_EncodeCpp11_url_encoder", (DL_FUNC) &_EncodeCpp11_url_encoder, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_EncodeCpp11(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}