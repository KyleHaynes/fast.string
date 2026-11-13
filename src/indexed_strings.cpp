// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <R_ext/Altrep.h>

#include "indexed_strings.h"

// The ALTREP class behind indexed_strings.h.
//
// data1 is list(source, index, nthreads). data2 is NULL until something asks
// for the data pointer or writes an element, and from then on is the
// expanded vector, which is what every method reads: the expanded copy can
// be written through its pointer, so the index no longer describes it.
//
// There is no Serialized_state method, so R saves one of these as the
// ordinary character vector it stands for, and a saved object never depends
// on this package being there to read it back. None is ever returned to the
// caller in any case: rLink() hands them to comparators and drops them.

namespace {

R_altrep_class_t indexed_class;
bool indexed_class_ready = false;

inline SEXP ix_source(SEXP x) { return VECTOR_ELT(R_altrep_data1(x), 0); }
inline SEXP ix_index(SEXP x) { return VECTOR_ELT(R_altrep_data1(x), 1); }

inline SEXP ix_read(SEXP source, R_xlen_t n_source, const int* index,
                    R_xlen_t i) {
    const int k = index[i];
    if (k == NA_INTEGER || k < 1 || k > n_source) return NA_STRING;
    return STRING_ELT(source, static_cast<R_xlen_t>(k) - 1);
}

SEXP ix_expand(SEXP x) {
    SEXP expanded = R_altrep_data2(x);
    if (expanded != R_NilValue) return expanded;
    SEXP source = ix_source(x);
    SEXP index = ix_index(x);
    const R_xlen_t n = XLENGTH(index);
    const R_xlen_t n_source = XLENGTH(source);
    const int* at = INTEGER_RO(index);
    expanded = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i)
        SET_STRING_ELT(expanded, i, ix_read(source, n_source, at, i));
    R_set_altrep_data2(x, expanded);
    UNPROTECT(1);
    return expanded;
}

R_xlen_t ix_length(SEXP x) {
    SEXP expanded = R_altrep_data2(x);
    return XLENGTH(expanded != R_NilValue ? expanded : ix_index(x));
}

SEXP ix_elt(SEXP x, R_xlen_t i) {
    SEXP expanded = R_altrep_data2(x);
    if (expanded != R_NilValue) return STRING_ELT(expanded, i);
    SEXP source = ix_source(x);
    return ix_read(source, XLENGTH(source), INTEGER_RO(ix_index(x)), i);
}

void ix_set_elt(SEXP x, R_xlen_t i, SEXP value) {
    SET_STRING_ELT(ix_expand(x), i, value);
}

void* ix_dataptr(SEXP x, Rboolean) {
    return const_cast<void*>(DATAPTR_RO(ix_expand(x)));
}

const void* ix_dataptr_or_null(SEXP x) {
    SEXP expanded = R_altrep_data2(x);
    return expanded != R_NilValue ? DATAPTR_RO(expanded) : nullptr;
}

Rboolean ix_inspect(SEXP x, int, int, int,
                    void (*)(SEXP, int, int, int)) {
    Rprintf("fast.string indexed strings (len=%.0f, %s)\n",
            static_cast<double>(ix_length(x)),
            R_altrep_data2(x) != R_NilValue ? "expanded" : "compact");
    return TRUE;
}

}  // namespace

// [[Rcpp::init]]
void fast_string_init_indexed_strings(DllInfo* dll) {
    indexed_class =
        R_make_altstring_class("fs_indexed_strings", "fast.string", dll);
    R_set_altrep_Length_method(indexed_class, ix_length);
    R_set_altrep_Inspect_method(indexed_class, ix_inspect);
    R_set_altvec_Dataptr_method(indexed_class, ix_dataptr);
    R_set_altvec_Dataptr_or_null_method(indexed_class, ix_dataptr_or_null);
    R_set_altstring_Elt_method(indexed_class, ix_elt);
    R_set_altstring_Set_elt_method(indexed_class, ix_set_elt);
    indexed_class_ready = true;
}

bool indexed_strings(SEXP x, IndexedStrings& out) {
    if (!indexed_class_ready || !ALTREP(x) ||
        !R_altrep_inherits(x, indexed_class))
        return false;
    if (R_altrep_data2(x) != R_NilValue) return false;
    SEXP index = ix_index(x);
    out.source = ix_source(x);
    out.index = INTEGER_RO(index);
    out.size = static_cast<std::size_t>(XLENGTH(index));
    out.nthreads = INTEGER_RO(VECTOR_ELT(R_altrep_data1(x), 2))[0];
    return true;
}

SEXP expanded_indexed_strings(SEXP x) {
    if (!indexed_class_ready || !ALTREP(x) ||
        !R_altrep_inherits(x, indexed_class))
        return R_NilValue;
    return ix_expand(x);
}

// [[Rcpp::export]]
SEXP fast_indexed_strings_impl(SEXP source, SEXP index, int nthreads) {
    if (TYPEOF(source) != STRSXP)
        Rcpp::stop("`source` must be a character vector.");
    if (TYPEOF(index) != INTSXP)
        Rcpp::stop("`index` must be an integer vector.");
    Rcpp::List state = Rcpp::List::create(
        source, index, Rcpp::IntegerVector::create(nthreads));
    return R_new_altrep(indexed_class, state, R_NilValue);
}
