/* decode B encoding into R objects */

/* since R doesn't allow NULs in strings, any string that
   contains values below TAB will be encoded as a
   raw vector.

   Dictionaries and lists are converted into pairlists.
 */

#include <stdlib.h>
#include <string.h>
#define USE_RINTERNALS 1
#include <Rinternals.h>

#define SET_CDR SETCDR

static SEXP readValue(char **b, char *end) {
    SEXP res = R_NilValue, tail;
    char *c = *b;
    switch(*c) {
    case 'd':
	c++;
	do {
	    *b = c;
	    SEXP key = PROTECT(readValue(b, end));
	    if (TYPEOF(key) != STRSXP || LENGTH(key) != 1)
		Rf_error("invalid key value");
	    SEXP val = PROTECT(readValue(b, end));
	    if (res == R_NilValue) {
		res = PROTECT(CONS(val, R_NilValue));
		SET_TAG(res, Rf_install(CHAR(STRING_ELT(key, 0))));
		UNPROTECT(3);
		PROTECT(res);
		tail = res;
	    } else {
		SET_CDR(tail, CONS(val, R_NilValue));
		tail = CDR(tail);
		SET_TAG(tail, Rf_install(CHAR(STRING_ELT(key, 0))));
		UNPROTECT(2);
	    }
	    c = *b;
	    if (c >= end) Rf_error("Unterminated dictionary");
	} while (*c != 'e');
	c++;
	*b = c;
	if (res != R_NilValue) UNPROTECT(1);
	break;
    case 'i':
	c++;
	/* use double for storage, becasue that's the largest R supports */
	double val = 0.0, sign = 1.0;
	if (*c == '-') {
	    sign = -1.0;
	    c++;
	}
	while (*c >= '0' && *c <= '9') {
	    double v = (double) (*c - '0');
	    val = val * 10.0 + v;
	    c++;
	}
	val *= sign;
	if (*c != 'e')
	    Rf_error("Unterminated integer");
	res = (val <= (double) NA_INTEGER || val > (double) INT_MAX) ? ScalarReal(val) : ScalarInteger(val);
	c++;
	*b = c;
	break;
    case 'l':
	c++;
	do {
	    *b = c;
	    SEXP val = readValue(b, end);
	    if (res == R_NilValue)
		tail = res = PROTECT(CONS(val, R_NilValue));
	    else {
		SET_CDR(tail, CONS(val, R_NilValue));
		tail = CDR(tail);
	    }
	    c = *b;
	    if (c >= end) Rf_error("Unterminated list");
	} while (*c != 'e');
	c++;
	*b = c;
	if (res != R_NilValue) UNPROTECT(1);
	break;
    default:
	if (*c < '0' || *c > '9') Rf_error("Invalid element (leading character is '%c')", *c);
	int len = atoi(c);
	while (*c >= '0' && *c <= '9') c++;
	if (*c != ':')
	    Rf_error("Invalid element (expected ':', found '%c')", *c);
	c++;

	int j = 0;
	int ok = 1;
	while (j < len)
	    if (c[j++] < '\t') { ok = 0; break; }
	if (ok)
	    res = ScalarString(mkCharLen(c, len));
	else {
	    res = allocVector(RAWSXP, len);
	    memcpy(RAW(res), c, len);
	}
	c += len;
	*b = c;
	break;
    }
    return res;
}

SEXP benc_decode(SEXP sWhat) {
    if (TYPEOF(sWhat) != RAWSXP) Rf_error("input must be a raw vector");
    char *b = (char*) RAW(sWhat);
    return readValue(&b, b + LENGTH(sWhat));
}
