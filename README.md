# JaiLang
Work in progress JAI compiler

Slowly building a LLVM frontend for a dialect of Jon Blow's JAI language.

It currently can compile and statically link very simple programs. It doesn't do runtime code execution yet.

Currently supports:
  * Pointers
  * String literals
  * Variadic functions (*cough* printf)
  * Switched to the C interface for LLVM.
  * Pre/post increment/decrement
  * While loops
  * Arrays
    - fixed size arrays (with count/data members)
    - base array references (fixed/dynamic agnostic)
  * Structures
  * Constants
  * Type aliasing (typedefs)
  * Enums (still need value/names arrays and typedefs)

Coming soon:
  * Explicit casting
  * Advanced array support
    - array literals
    - dynamic arrays
  * Method overloading
  * Method pointers
  * Improved error handling and unit testing of error cases.
  * Default initialization of global variables
  * For Loops.

Further out:
  * Short-circuiting logical operators
  * Function overloading
  * UTF8 identifier support

Known Issues:
  * Void pointers are not supported (llvm wants to codegen them as *u8)
  * static linking of C runtime into libraries won't be happy with two libraries.

Deviations from current JAI syntax:
  * pointers use & for pointer type specification/referencing and @ for a dereference operator, rather than <<
    - I would use & and *, but the asterisk is ambiguous because conditional statements aren't forced into parens. (ie. "if pN *pN=2;"" is parsed as if (pN * pN)=2; )
  * still support single quote character literals. (might #char once the single char is used elsewhere)
  * float types are: float, double, f32 and f64. because... why wouldn't they be?
  * type aliasing uses the typedef keyword to disambiguate constant values from type aliases. (ie 'IntAlias :: typedef s32;')
  * Implicit enum constants are nil(-1), min, last and max rather than lowest_value and highest_value.

Q. Why are you deviating from the language at all? Aren't these all small changes that just amout to bikeshedding?

A. Yes and no, I've gone back and forth on this a bit and feel comfortable deviating from Jon's design whenever I'm convinced that it's an improvement. It seems likely that the best shot for a new language comes from several different takes on the same ideas, than one monolithic language dictated from a single source.