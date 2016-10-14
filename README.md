# Moe

Moe is a custom imperative procedural programming language with static type checking. It is intended to be used for high performance game code. It does not support garbage collection or exceptions. This language and compiler is *very* much still a work in progress and is not ready to be used by anyone else.

It's current feature set is roughly comprable to C: enumerations, structures, pointers, procedures, conditions and loops are all there, but there are a few changes to make it nicer to use:
  - top level symbol lookup is order independent.
  - variable declarations support type inference.
  - procedure overloading is supported.
  - instances are initialized unless explicitly marked as uninitialized.
  - break and continue support breaking to a labeled outer loop.

There are quite a few upcoming features that are missing from C:
  - run time type reflection
  - operator overloading (with in/out reference arguments)
  - optional array bounds checking
  - generics (templates)
  - compile time code execution

To build the compiler and debug compiled source you will need to get the following dependencies:
  - The base compiler requires LLVM 3.9
  - Debugging with lldb requires Python 3.5, gnuWin32, swig and ninja 
  - Debugging with Visual Studio has been tested with MSVC 2016 
  - puck.moe has a dependency on GLFW 3.1.2

# Syntax
#### this section describes the intended syntax once the current set of changes is complete

## Types And Declarations:

Moe supports the standard set of built in basic types.

Machine word sized types:
  - basic types: `int`, `float`, `bool`
  - pointer sized values: `sSize`, `uSize`

Explicitly sized types:
  - signed integers: `s8`, `s16`, `s32`, `s64`
  - unsigned integers: `u8`, `u16`, `u32`, `u64`
  - floating point numbers: `f32`, `f64`

## Simple Declarations

```
num: s8       // signed 8 bit
num: u8       // unsigned eight bit

num: int = 2  // also allows explicit init.

// Type inferred declaration

n := 22       // literals promote to default type 

x, y, z: f32  // compound declaration

n : s32
```

Declarations are zero initialized unless explicitly marked as uninitialized.
```
nUninitialized : s32 = ---
```


Constants are defined with a name, followed by the `::` operator and the constant value.
```
kPi :: 3.14159         
kAnswer :: s32 = 42   // constants can be explicitly typed
```

Literals and most constants are unsized until assigned to an instance value. Variables that are type inferred from an untyped value will use the fitting default type (ie `n := 2` is an `int`).




## Arrays

```
aN : [2] int;   // fixed size array
aN : [] int;    // array reference (pointer + size)
aN : [..] int;  // dynamic array (not yet implemented)

aN := {:s16: 2, 3, 4}
```

Each array type also has a member `count` to query the number of elements it contains.
```
numElements := aN.count
```



## Pointers

You can find the address of a variable with `&`, and dereference a pointer with `@`.
```
pN = &n
n = @pN
```

Pointers are declared with the 'address-of' operator:
```
pN = & int
```

Pointers also support the array operator and pointer arithmetic:
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



## Typedef
Types can be aliased with the typedef keyword:
```
NewName typedef s32
IntPointer typedef & s32;
```



## Procedures

```
DoThing proc (a: int, b: int) -> int
{
  return a + b
}
```

Procedure Qualifiers
  - `inline`, `noinline`: forces the function to be inlined (or not). Not treated as a suggestion as in C.
  - `#foreign`: Used to declare a foreign function that should be found by the linker. (This will prevent name mangling so it won't work with overloaded procedures)
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

Coming soon: Structure literals with syntax like a procedure call. (including support for default values.)
To Do: structure literal syntax is just like a procedure call - with default values as specified in the definition
```
foo := SFoo(.m_n = 45)
```



## Enumerations
```
SOMEENUM enum
{
    Ack : 0,
    Bah : Ack+1,
    Ugh,  
}
```

Enumeration values are referred to with the enum name as a namespace:
``` 
s := SOMEENUM.Bah
```

Enumerations also define several implicit constants:
  * `min` the value of the lowest defined constant.
  * `last` the value of the highest defined constant.
  * `max` one past the last value.
  * `nil` a sentinel to signal an invalid value.

An enumeration's loose type is the type used to store it in memory. The loose type can be used directly in a declaration and the loose type can be specified explicitly in the enum definition:
```
SOMEENUM enum s16
{
}
n: SOMEENUM.loose   // this is an s16
```

Each enum type also defines static arrays containing the names and values of each enum constant.
```
for i:=0; i<SOMEENUM.names.count; ++i
{
  print("%s == %d", SOMEENUM.names[i], SOMEENUM.values[i]) 
}
```



## Syntax Miscellanea
  - semicolons are optional (needed only to separate multiple statements on one line.
  - conditional predicates are not enclosed in parentheses, but all child blocks must be enclosed in curly braces, even single lines.



## To Do

short term tasks:
- [x] remove semicolon requirement
- [x] add required brackets for single line conditionals
- [x] switch :: definitions for more descriptive keywords struct, enum, proc & const
- [x] add c-style for loop
- [ ] move inline to after the parameters like the other procedure qualifiers
- [ ] error if return value is ignored 

longer term tasks:
- [ ] run time type info
- [ ] 'Any' type-boxing
- [ ] array bounds checking
- [ ] named default arguments
- [ ] struct literal support
- [ ] support for inheritance, protocols
- [ ] first class bitfield type