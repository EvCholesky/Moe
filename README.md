# JaiLang
Work in progress JAI compiler

Slowly building a LLVM frontend for a dialect of Jon Blow's JAI language.

It currently can compile and statically link very simple programs. It doesn't do runtime code execution yet.

New:
  * Codegen for simple pointers
  * String literals
  * Variadic functions (*cough* printf)
  * Switched to the C interface for LLVM.
  * Pre/post increment/decrement
  * While loops
  * Arrays
  * Pointer arithmetic.
  * Structures
  * Constants
  * Type aliasing (typedefs)
  * Enums (still need implicit members)

Coming soon:
  * Optionals (ala swift)
  * Advanced array support
    - generic array references
    - array literals
    - dynamic arrays
  * Explicit casting
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
  * currently using @ for a dereference operator, rather than <<
  * still support single quote character literals. (might #char once the single char is used elsewhere)
  * float types are: float, double, f32 and f64. because... why wouldn't they be?
  * type aliasing uses the typedef keyword to disambiguate constant values from type aliases. (ie 'IntAlias :: typedef s32;')

Q. Why are you deviating from the language at all? Aren't these all small changes that just amout to bikeshedding?

A. Yes and no, I've gone back and forth on this a bit and feel comfortable deviating from Jon's design whenever I'm convinced that it's an improvement. It seems likely that the best shot for a new language comes from several different takes on the same ideas, than one monolithic language dictated from a single source.