# Jade
Work in progress compiler inspired by JAI

Slowly building a LLVM frontend for a language inspired by Jon Blow's JAI videos.

The name is likely going to change and this document (which has become a dumping ground for task bullet points) 
needs to be rewritten.

It currently can compile and link simple programs. It doesn't do runtime code execution yet.

Currently supports:
  * Pointers
  * String literals
  * Variadic functions
  * While loops
  * For Loops.
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
  * Debug Info - first pass
  * Proper handling of global variables (including default initialization)
  * Method overloading
  * Method pointers
  * UTF8 identifiers

Coming soon:
  * Operator overloading
  * Debug info
    - handle nested scopes
  * break/continue to labels;
  * Dynamic arrays
  * Improved error handling and unit testing of error cases.

Clean up
  * consolidate PARK_ReferenceDecl, PARK_ArrayDecl, ParkMethodReferenceDecl into PARK_Decl
  * clean up CAlloc - It's currently overflowing into system memory, which is ok, but needs to track cbAllocated, not cBFree.

Deviations from current JAI syntax:
  * pointers use & for pointer type specification/referencing and @ for a dereference operator, rather than <<
  * still support single quote character literals. (might #char once the single char is used elsewhere)
  * float types are: float, double, f32 and f64. because... why wouldn't they be?
  * type aliasing uses the typedef keyword to disambiguate constant values from type aliases. (ie 'IntAlias :: typedef s32;')
  * Implicit enum constants are nil(-1), min, last and max rather than lowest_value and highest_value.
  * Different for loop syntax

Known ambiguities:
   * Contants with parens in the literal "APlusB :: (A + B)" parses as a method declaration.
   * Boolean expressions are not wrapped in parens so "if pN *pN = 2" could be a multiply or a dereference.
   * while fSomeBool ++pV; doesn't know if the increment is for the condition or the loop body.

Open Asthetic Questions:
   * Ditch semicolons except for multiple statements on one line, ala Swift?
   * Force all flow control structure bodies to use brackets (even one line)