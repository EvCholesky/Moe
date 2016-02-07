# JaiLang
Work in progress JAI compiler

Slowly building a LLVM frontend for Jon Blow's JAI language.

It currently can compile and statically link very simple programs. It doesn't do runtime code execution yet: the plan is to use LLVM's jit compiler.

New:
  * Codegen for simple pointers
  * string literals
  * variadic functions (*cough* printf)

Coming soon:
  * switching to the C interface for LLVM.
  * loops.
 
Deviations from current JAI syntax:
  * currently using @ for a dereference operator, rather than <<
  * still support single quote character literals. (will add #char once the single char is used elsewhere)
