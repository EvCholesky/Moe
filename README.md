# Moe

Moe is a custom imperative procedural programming language with static type checking. It is intended to be used for high performance game code. It does not support garbage collection or exceptions. This language and compiler is *very* much still a work in progress and is not really ready to be used by anyone else.

It's current feature set is roughly comprable to C: enumerations, structures, pointers, procedures, conditions and loops are all there, but there are a few changes to make it nicer to use:
  - Top level symbol lookup is order independent 
  - Type inference is supported.
  - Procedure overloading is supported.
  - Instances are initialized unless explicitly marked as uninitialized.
  - Break and continue support breaking to a labeled outer loop

There are quite a few upcoming features that are missing from C
  - Run time type reflection
  - Operator overloading (with in/out reference arguments)
  - Optional array bounds checking
  - Generics (templates)
  - Compile time code execution

Dependencies:
  - LLVM 3.9

# Syntax:
* this section describes the intended syntax once the current set of changes is complete *

## Types And Declarations:

Moe supports the standard set of built in basic types.

Machine word sized types
  - `int`, `float`, `bool`
  - pointer sized values: `sSize`, `uSize`

Explicitly sized types
  - signed integers: `s8`, `s16`, `s32`, `s64`
  - unsigned integers: `u8`, `u16`, `u32`, `u64`
  - floating point numbers: `f32`, `f64`

## Simple declarations

```
num: s8       // signed 8 bit
num: u8       // unsigned eight bit

num: int = 2  // also allows explicit init.

// Type inferred declaration

n := 22       // literals promote to default type 

x, y, z: f32  // compound declaration

n : s32
```

Declarations are zero initialized unless explicitly marked as uninitialized
```
nUninit : s32 = ---
```


Constants are defined with a name followed by the `const` keyword and the constant value.
```
kPi const = 3.14159 
```

Literals and constants are unsized until assigned to an instance value. Variables that are type inferred from an untyped value will use the fitting default type (ie `n := 2` is an `int`).



## Arrays

```
aN : [2] int;   // fixed size array
aN : [] int;    // array reference (pointer + size)
aN : [..] int;  // dynamic array (not yet implemented)

aN := {:s16: 2, 3, 4}
```



## Pointers

You can find the address of an instance types with `&` and dereference a pointer with `@`
```
pN = &n
n = @pN
```

Pointers are declared with the 'address-of' operator
```
pN = & int
```

Pointers also support the array operator and pointer arithmetic;
```
n3 = pN[3]    // NOTE: no bounds checking here!
pN3 = pN + 3  // offsets by 3 * the size of the type being pointed to, ala C.
```



## Structures

```
SFoo struct
{
  m_n: int
  m_g: float

  m_n := 22
  kVal const = 33   // member constants *should* work
}
```



## Procedures

```
DoThing proc (a: int, b: int) -> int
{
  return a + b
}
```

Procedure qualifiers
  - `inline`, `noinline`: Forces the function to inline (or not). Not treated as a suggestion as in C.
  - `#foreign`: Used to declare a foreign function that should be found by the linker. (Will prevent name mangling so no overloading)
  - `#stdcall`: Used to specify the stdcall calling convention.

Procedures will support default parameters and named arguments:

```
DoThing proc (nFoo: int = 10, nBar: int = 11) -> int
{ }

DoThing(.nFoo=20)
```

Procedure references are declared with the same syntax as declaring them.
```
pFn: (pChz: & u8)->void
pFn(2) // calling by reference
```

Support for variadic arguments is currently limited to foreign functions (*Cough* `printf`) but it is specified with the `..` operator.
```
printf (pChzFormat: & u8, ..) #foreign -> s32
```

// TODO: structure literal syntax is just like a procedure call - with default values as specified in the definition
```
foo := SFoo(.m_n = 45)
```


## Syntax Miscellanea
  - semicolons are optional (only needed to separate multiple statements on one line.
  - conditional predicates are not enclosed in parenthesis, but all child blocks must be enclosed in curly braces (even single lines)



## TODO

- [ ] Remove semicolon requirement
- [ ] Add required brackets for single line conditionals
- [ ] Add c-style for loop
- [ ] Switch :: definitions for more descriptive keywords struct, enum, proc & const
- [ ] Move inline to after the parameters like the other procedure qualifiers
- [ ] Error if return value is ignored 

longer term tasks
- [ ] run time type info
- [ ] 'Any' type-boxing
- [ ] array bounds checking
- [ ] named default arguments
- [ ] struct literal support
- [ ] support for inheritance, protocols
- [ ] first class bitfield type