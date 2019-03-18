# Moe

Moe is a custom procedural programming language with static type checking. It is intended to be used for high performance game code. It does not support garbage collection or exceptions. This language and compiler is still a work in progress - new users are welcome, but it might still be a bumpy ride. 

It's current feature set is roughly comprable to C: enumerations, structures, pointers, procedures, conditions and loops are all there, but there are a few changes to make it nicer to use:
  - top level symbol lookup is order independent.
  - variable declarations support type inference.
  - procedure/operator overloading is supported.
  - instances are initialized unless explicitly marked as uninitialized.
  - break and continue support breaking to a labeled outer loop.
  - run time type reflection
  - generic procedures and structures

There are a few upcoming features that are C lacks:
  - optional array bounds checking
  - compile time code execution

To build the compiler and debug compiled source you will need to get the following dependencies:
  - The base compiler requires LLVM 5.0
  - dyncall 1.0 is used for calling foreign functions from bytecode
  - Debugging with lldb requires Python 3.6, gnuWin32, swig and ninja 
  - Debugging with Visual Studio has been tested with MSVC 2015 
  - puck.moe has a dependency on GLFW 3.2.1

# Syntax

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
> In Moe a single colon is used to mark the begining of a type specification. For example passing the type bool to a generic procedure would be written `genproc(:int)` where ommiting the colon would make the compiler look for a value type named `bool` and cause a compilation error.

All declarations in Moe are zero initialized unless explicitly marked as uninitialized.
```
nInitialized : s32
nUninitialized : s32 = ---
```


Constants are defined with a declaration that has the keyword `immutable` before the type specification or initial value.
```
kPi immutable := 3.14159         
kAnswer immutable : s32 = 42   // constants can be explicitly typed
```

Literals and most constants are untyped and unsized until assigned to an instance value. Variables that are type inferred from an untyped value will use the fitting default type (ie `n := 2` is an `int`).




## Arrays

```
aN : [2] int;   // fixed size array
aN : [] int;    // array reference (pointer + size)
aN : [..] int;  // dynamic array (not yet implemented)
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




## Typedef
You can create a type alias and refer to a type with a different name using the typedef keyword:
```
NewName typedef s32
IntPointer typedef & s32;
```



## Enumerations
```
SOMEENUM enum
{
    Ack := 0,
    Bah := Ack+1,
    Ugh,  
}
```

Enumeration values are referred to with the enum name as a namespace:
``` 
s := SOMEENUM.Bah
```

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
  printf("%s == %d", SOMEENUM.names[i], SOMEENUM.values[i]) 
}
```

### Basic Enumerations
Basic enum constants that don't explicitly specify a value will start at zero and increment by one per constant.

Basic enumerations also define several implicit constants:
  * `min` the value of the lowest defined constant.
  * `last` the value of the highest defined constant.
  * `max` one past the last value.
  * `nil` a sentinel to signal an invalid value.

### Flag Enumerations
Flag enums are used to manage a group of flags in a bitmask. Constants that don't explicitly specify a value will start at one and increase rounding up to the next power of two.
```
FLAGS flag_enum
{
    Ack,  // 0x1
    Bah,  // 0x2
    Ugh,  // 0x4
}
```

Flag enumerations define the following implicit constants:
  * `none` no flags defined. Always zero.
  * `all` the value of all of the defined flag constants.

Individual flags can be read or written on an instance by name:

```
flags: FLAGS

flags.Ack = true    // the same as flags |= FLAGS.Ack
flags.Ugh = false   // equivalent to flags &= FLAGS.all ^ FLAGS.Ugh

pFlags := &flags
pFlags.Bah = true   // pointer to flag enum is dereferenced the same as structs

f: bool = flags.Bah
PrintBool(flags.Ugh)
```

### Enum TODOs:
- [ ] supply a syntax for opting out of implicit constants
- [ ] syntax for specifying an enum's initial value
- [ ] generate errors when implicit constants don't fit in a type or overlap an explicit constant



# Control Flow

## If/else

If statements check a condition and execute a block of code if it evaluates to true. You can chain `else if` and `else` statements to handle multiple cases in one block.

```
if n == 3
{
  DoSomething()
}
else if n < 0
{
  SomethingElse()
}
else
{
  return false
}
```
> conditional predicates are not enclosed in parentheses, but all child blocks must be enclosed in curly braces, even single lines.

## Switch Statements

Switch statements allow you to compare an integer value against multiple cases. 

```
switch m_foo
{
  case FOO.First: 
    printf("first")

  case FOO.Next,
  case FOO.Other: 
    printf("next")

  else:
    printf("default")
}
```

  - Break statements are not required at the end of each case as in C, but can be used to exit the case early.
  - The `fallthrough` keyword allows code execution to pass through to the following case.
  - Switch statements can be labeled for breaking out of a loop nested inside a switch case.

## While Loop
  
The simplest loop is the `while` loop that continues to loop as long as its condition evaluates to `true`

```
while f == true
{
  printf("woo.")
}
```

## For Loop

Moe currently supports a C-Style for loop, but I'm experimenting with various attempts at a range-based for_each loop. For loops specify three expressions separated by a semicolon:
  - an initialization statement
  - condition tested each iteration
  - an increment statement run at the end of each iteration

```
for i:=0; i<15; ++i
{
  printf("whoop.")
}
```

## Break, Continue statements

The `break` keyword allows execution to exit the innermost loop or switch body.

The `continue` keyword will jump to the end of the current loop body and execute the loop increment statement (if it exists) and test the loop predicate for the next iteration.

Both the `break` and `continue` keywords support supplying a label for a named loop rather than exiting the innermost one.

```
#label Outer
for y:=0; y<10; ++y
{
  for x:=0; x<10; ++x
  {
    if x == 4
      { continue Outer }
    
    if y == 4
      { break Outer }
  }
}
```

# Procedures

```
DoThing proc (a: int, b: int) -> int
{
  return a + b
}
```

Procedure Qualifiers are placed after the return type, but before the procedure body.
  - `inline`, `noinline`: forces the function to be inlined (or not). Not treated as a suggestion as in C.
  - `#foreign`: Used to declare a foreign function that should be found by the linker. (This will prevent name mangling so it won't work with overloaded procedures)
  - `#stdcall`: Used to specify the stdcall calling convention.


Procedures support default argument values using the same initialization and inference syntax as variable declarations.
```
DoThing proc (nFoo: int = 10, nBar := 11)
{ }
```

Parameters can be specified by name at the procedure call.
```
foo := SFoo(`nBar 45, `nFoo 22)
```

Procedure references are declared with the same syntax as declaring them.
```
pFn: (pChz: & u8) -> void
pFn(2) // calling by reference
```

Support for variadic arguments is currently limited to foreign functions (*Cough* `printf`) but it is specified with the `..` operator.
```
printf (pChzFormat: & u8, ..) #foreign -> s32
```


## Generic Procedures

If you need to write a procedure that can be used on a variety of types you can mark one or more of the parameter types as generic using the `$` token before a typename in a type specification.
```
AddValues proc (lhs : $T, rhs: T) -> T
{
    return lhs + rhs
}
```
The definition of the parameter lhs is marked as the anchor for the generic type `T`. When the procedure is called it will infer the type `T` from the value passed to the anchored arguemnt and bake out a procedure for that type. Notice that only one use of the type `T` has the `$` token so the inference is not ambiguous.
```
n := AddValues(1, 2)      // n is an int
g := AddValues(1.0, 2.2)  // g is a float
e := AddValues(1, 2.2)    // error: cannot convert 2.2 to an int to pass to AddValues
```

Generic type anchors can also be embedded inside a more complex type specification.

```
GetElement proc (pN :[] & $T, i: int)->T
{
    return @pN[i]
}

apN: [10] & int
n := GetElement(apN, 2)  // GetElement instantiates with $T matched to an int
```

A generic procedure argument that is only used to infer a type can omit the argument name and won't require a value to be passed in when the procedure is invoked. these 'Type Arguments' can also supply a default type if one isn't explicitly given when the procedure is called.

Type anchors can also be specified by name during a procedure call using the named argument syntax.
```
ReturnZero proc ( :$T = :u8) -> T
{
    return 0
}

n1 := ReturnZero(:s16)    // n1 is a s16
g1 := ReturnZero(:f32)    // g1 is a f32
n2 := ReturnZero()        // n2 is a u8
n3 := ReturnZero(`T u64)  // n3 is a u64
```

generic procedures can also be baked out with a constant value argument. Baked generic values are denoted by putting the `$` operator by the argument name (rather than inside the type specification)
```
AddNum proc ($VALUE : int, n : int) -> int
{
  return n + VALUE
}

AddNumm(123, x) // Generates an AddNum procedure that always adds 123
```

# Structures

Structures are Moe's main user defined data type. A struct is a packed collection of member variables. Moe does not support member functions, there are no classes - just structures and functions.

Structure members are zero initialized by default, but you can specify a default value inline within the structure definition. You can also infer the type of a member variable from a default value just like a standalone variable or parameter definition.

```
SFoo struct
{
  m_n: int
  m_g: float

  m_n := 22
  kVal const = 33   // member constants are supported
}
```



## Generic Structures

Structures can be parameterized with a generic type or value anchor. Like generic procededure a structure defines its generic arguments with the '$' operator. Unlike procedures structure argument values must be a compile-time constant.

In the following example SFoo defines a generic struct that takes the size of an internal fixed size array as an argument.

```
struct SFoo ( $COUNT: int )
{
    m_aN : [COUNT] int
}
```
These structures are then instantiated using an argument list in the type specification:
```
// create a 'foo' structure with an array of 12 integers
foo :SFoo(12)
```
Structs can also have parameterized types:
```
struct SGen ( :$T)
{
    m_x :T
}

gen : SGen(:s16)    //  parameterized with m_x as a 16-bit integer
``` 
Generic structure parameters support both default values and supplying arguments by name.
```
struct SMulti ( :$K, $V = :bool)
{
    ...
}

// instantiate struct where K is s32 and V is a bool

multi :SMulti(`K :s32) 
```



## Compound literals
Compound literals are expressed by an optional type specification followed by a comma separated list of values inside curly braces. Compound literals that don't specify a type are inferred to be fixed sized array. The type of each element is inferred from the first value listed.
```
aN1 := :[3] s16 {1, 2, 3}    // typeof(aN1) is [3] s16
aN2 := {1, 2, 3, 4}          // typeof(aN2) is [4] int
```

Structure literals can also be expressed with this syntax.
```
SPoint struct
{
  m_x, m_y, m_z: int
}

Pos1 := :SPoint {1, 2, 3}    // m_x = 1, m_y = 2, m_z = 3
Pos1 := :SPoint {`m_z 3 }    // m_x = 0, m_y = 0, m_z = 3
```




## Interfaces


## 'using' statements

Often times it is a desirable shorthand to access members of a local namespace without specifying the instance itself. This can be done with the 'using' statement. for example:
```
SBase struct
{
    m_x: int
    m_y: int
}

SFoo struct
{
    using m_base: SBase
}
```
In this example m_base is an instance of SBase contained within SFoo, but the using statement allows you to access members of SBase as if it they were direct members of SFoo.
```
foo: SFoo
foo.m_x = 5         // this is the same as foo.m_base.m_x
```
Using statements can also be added to a pointer to provide an alias to data stored in a different location

```
SFoo struct
{
    using m_pBase: SBase
}

foo.m_x = 5       // is the same as foo.m_pBase.m_x
```
A using statement can also omit the variable name and use the following form:
```
SFoo struct
{
    using SBase;
}
```
> Note: this does not currently support aliasing the static members of a type into a given namespace (as this would be ambiguous with anonymous using statements)

Using statments can also be used in local methods or method arguments to emulate member methods from other languages 

```
DoThing Proc (using pFoo: & SFoo)
{
    m_x = 5   // is a shortcut for pFoo.m_x
}

OtherThing proc ()
{
   using foo: SFoo
   m_x = 5;   // is equivalent to foo.m_sx
}
```



## Operator Overloading

Operator overload procedures are defined with the operator keyword followed by the operator itself.

```
operator + (fooLhs: SFoo, fooRhs: SFoo) -> SFoo
{
  ...
}
```

The function signature expected for each operator is as follows:

| Type | Operands | Signature |
|------|----------|-----------|
| Binary Operators | `+,-,*,/,%,|,&,>>,<<` | `operator(Lhs:LType, Rhs:RType)->RetType` |
| Comparison Operators | `<,>,<=,>=,==,!=`|`operator(Lhs:LType, Rhs:RType)->bool` |
| Assignment Operators | `=,:=,+=,-=,*=,/=`|`operator(pLhs:&LType, Rhs:RType)` |
| Unary Operators | `+,-,&,@,~,!`|`operator(pLhs:LType)->RetType` |
| Postfix Unary Operators | `++,--`|`operator(pLhs:&LType)->RetType` |

- there is currently no way to differentiate preincrement/predecrement and postincrement/postdecrement when overloading.

The `#commutative` keyword can be added to binary operators that have different argument types to allow the arguments to be passed in either order.

```
operator * (fooLhs: SFoo, r: float) -> SFoo #commutative
{
  ...
}

foo = foo * 2.2
foo = 2.2 * foo // still ok!
```


## Syntax Miscellanea
  - semicolons are optional (needed only to separate multiple statements on one line.


## To Do

smallish tasks:
- [x] fix typeinfo operator
- [x] FTypesAreEquivalent function

longer term tasks:
- [ ] Interfaces
- [ ] 'Boxed' type (empty interface)
- [x] Generic structs
- [ ] non-foreign variadic args
- [x] using struct members
- [x] run time type info
- [ ] first class string type
- [ ] array bounds checking
- [x] struct literal support

tasks that might happen:
- [ ] #mustuse return qualifier that generates an error if ignored