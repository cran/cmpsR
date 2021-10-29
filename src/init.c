#include "cmpsR.h"
#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
        {"compute_cross_corr_c",  (DL_FUNC) &COMPUTE_CROSS_CORR_,  3}, 
        {"na_trim_c",             (DL_FUNC) &NA_TRIM_,             1},
        {"local_max_c",           (DL_FUNC) &LOCAL_MAX_,           2},
        {NULL,                    NULL,                            0}
};

void R_init_cmpsR(DllInfo *info)
{
//   SymbolShortcuts();
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, TRUE);

  /* used by external packages linking to internal xts code from C */
  R_RegisterCCallable("cmpsR","compute_cross_corr",      (DL_FUNC) &COMPUTE_CROSS_CORR_);
  R_RegisterCCallable("cmpsR","na_trim_cmps",            (DL_FUNC) &NA_TRIM_);
  R_RegisterCCallable("cmpsR","local_max_cmps",          (DL_FUNC) &LOCAL_MAX_);
  
}
