# nonfinite
Support of infinity and nan values in a standard, implementation independent way in common lisp.

Basically this defines constants for +/- infinity and nan as keywords and some helper functions.

I chose not to use IEEE special values on implementations that support it because it was inconsistent.

This is mostly useful for dealing with NaN or infinite values received or sent over an
external protocol.

## Constants

### +NAN+
Not a number. Defined as :NAN. Not there there is no difference between signalling and
non signalling NaN.

### +INFINITY+
Positive Infinity. defined as :INFINITY.

### +NEGATIVE-INFINITY+
Negative Infinity. Defineas as :NEGATIVE-INFINITY.

### +SPECIAL-FLOATS+
A list of the special float values. Includes +NAN+, +INFINITY+, and +NEGATIVE-INFINITY+.

## Types

### EXPANDED-FLOAT
A type containing all floats and the special values stored in +SPECIAL-FLOATS+.

### EXPANDED-REAL
A type containing all reals and the special values stored in +SPECIAL-FLOATS+.

## Functions

### NAN-P number
A function that tests if a number is EQL to +NAN+.

### INFINITY-P number
Test if a number is positive or negative infinity.

### NORMAL-NUMBER-P number
Test if a number isn't NaN or an infinity.
