# JaiLang
Work in progress JAI compiler

Slowly building a LLVM frontend for a dialect of Jon Blow's JAI language.

It currently can compile and statically link very simple programs. It doesn't do runtime code execution yet.

Currently supports:
  * Pointers
  * String literals
  * Variadic functions
  * Pre/post increment/decrement
  * While loops
  * Arrays
    - fixed size arrays (with count/data members)
    - base array references (fixed/dynamic agnostic)
    - array literals
  * Structures
  * Constants
  * Type aliasing (typedefs)
  * Enums
  * Short-circuiting logical operators
  * Explicit casts
  * Debug Info - minimal first pass

Coming soon:
  * Debug info
    - enums
    - typedefs
    - loops
    - handle nested scopes
    - verify initializer functions 
  * Method overloading
  * Method pointers
  * break/continue to labels;
  * Dynamic arrays
  * Improved error handling and unit testing of error cases.
  * Default initialization of global variables
  * For Loops.

Further out:
  * UTF8 identifier support

Deviations from current JAI syntax:
  * pointers use & for pointer type specification/referencing and @ for a dereference operator, rather than <<
  * still support single quote character literals. (might #char once the single char is used elsewhere)
  * float types are: float, double, f32 and f64. because... why wouldn't they be?
  * type aliasing uses the typedef keyword to disambiguate constant values from type aliases. (ie 'IntAlias :: typedef s32;')
  * Implicit enum constants are nil(-1), min, last and max rather than lowest_value and highest_value.

Q. Why are you deviating from the language at all? Aren't these all small changes that just amout to bikeshedding?

A. Yes and no, I've gone back and forth on this a bit and feel comfortable deviating from Jon's design whenever I'm convinced that it's an improvement. It seems likely that the best shot for a new language comes from several different takes on the same ideas, than one monolithic language dictated from a single source.

Known ambiguities:
   * Contants with parens in the literal "APlusB :: (A + B)" parses as a method declaration.
   * Boolean expressions are not wrapped in parens so "if pN *pN = 2" could be a multiply or a dereference.