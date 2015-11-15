# The Batbridge ISA specification

The Batbridge machine standard was designed to be a simple 32 bit, word
addressed RISC-like instruction set used in the first semester of UT Austin's CS
Computer Architecture course by
[Ahmed Gheith's CS375H](http://www.cs.utexas.edu/~gheith/).

## BB32v0

This is the original, unextended Batbridge ISA as I wrote it up and as Gheith
assigned it to us to implement.

The machine features 32 registers, numbered `[0 ... 31]`, of which the following
are special.

The PC, register 31 (0b11111)
  - Reading it produces the PC of the next instruction
  - Writing to the PC causes a branch, along with any other required
    system wide side effects for proper branching.

The zero register, register #30 (0b11110)
  - Always reads as having the value 0
  - Writing to the zero register does not change its value
  - Writing to the zero register on simulators writes the written
    value as a character to standard output as a debugging device.

The immediate value, register #29 (0b1101)
  - When read, yields the 11 bit immediate field in the current instruction
  - Writing the immediate does not change the immediate or any other system state.
  - On simulators, writing to the immediate value prints the written
    value on the console as a hex string.

Most instructions are formatted as follows, although the exact interpretation of
these bit slices depends on the particular instruction.

```c
struct opcode {
  uint32_t opcode:6;
  uint32_t d:5;
  uint32_t a:5;
  uint32_t b:5;
  int      i:11;
}
```

The above format is interpreted for each instruction given the following shorthand

```
 t : Target register
 s : Source register
 a : Register representing left operand
 b : Register representing right operand
 x : Register representing index (multiplied by operand size)
 i : Immediate signed quantity
 _ : Value is not read. Set to 0 when assembled by convention.
```

So the sequence "ttttt" means the bit ID of the target register, and the
corresponding metavariable t in the opcode description is taken to mean the
value of the named register. If the named register is `29` as above, then the
value of the immediate field _in that instruction_ is used.

The top bit of an instruction is set IFF the instruction does not go to
memory. The second highest bit indicates that instruction is not a conditional
control instruction. The remaining 4 bits have no particular meaning and for the
most part were sequentially assigned.

Note that, by having register 31 as its destination, any opcode which produces a
result may cause a branch and so the criteria of checking the top bit is not
sufficient.

### HLT
Opcode 0x00

```
000000 _____ _____ _____ ___________
```

Halts the machine immediately.

### LD
Opcode 0x10

```
010000 ttttt aaaaa xxxxx iiiiiiiiiii
```

Loads the word `(+ a (* 4 x))` to `t`. Load addresses are rounded down
to the nearest multiple of 4.

### ST Opcode 0x11

```
010001 sssss aaaaa xxxxx iiiiiiiiiii
```

Stores the word in register `s` to the address `(+ a (* 4 x))`. Store
addresses are rounded down to the nearest multiple of 4.

### IFLT
Opcode 0x20
```
100000 _____ aaaaa bbbbb iiiiiiiiiii
```

Executes the next instruction IFF `(< a b)`.
 
### IFLE
Opcode 0x21

```
100001 _____ aaaaa bbbbb iiiiiiiiiii
```
Execute the next instruction IFF `(<= a  b)`.

## IFEQ
Opcode 0x22

```
100010 _____ aaaaa bbbbb iiiiiiiiiii
```

Execute the next instruction IFF `(== a b)`.
 
### IFNE
Opcode 0x23

```
100011 _____ aaaaa bbbbb iiiiiiiiiii
```

Execute the next instruction IFF `(!= a b)`
 
### ADD
Opcode 0x30

```
110000 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores `(+ a b)` to the targeted register.
 
### SUB
Opcode 0x31

```
110001 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores `(- a b)` to the targeted register.
 
### DIV
Opcode 0x32

```
110010 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the integer division of `(/ a b)` to the targeted register.
Does not store the remainder.
 
### MOD
Opcode 0x33

```
110011 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the integer remainder `(mod a b)` to the targeted register.
 
### MUL
Opcode 0x34

```
110100 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the product `(* a b)` to the targeted register.
 
### AND
Opcode 0x35

```
110101 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the bitwise and of `a` and `b` to the targeted register.
 
### OR
Opcode 0x36

```
110110 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the bitwise or of `a` and `b` to the targeted register.

### NAND
Opcode 0x37

```
110111 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the bitwise nand of `a` and `b` to the targeted register.
 
### XOR
Opcode 0x38

```
111000 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the xor of `a` and `b` to the targeted register.
 
### SL
Opcode 0x3a

```
111010 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the left shift of `a` by `b` bits to the target register. 0-extends the
result, does not rotate.

### SR
Opcode 0x3b

```
111011 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the right shift of `a` by `b` bits to the targeted register.
 
### SAL
Opcode 0x3c

```
111100 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the arithmatic shift left of `a` by `b` bits to the targeted register.

### SAR
Opcode 0x3d

```
111101 ttttt aaaaa bbbbb iiiiiiiiiii
```

Stores the arithmatic right shift of `a` by `b` bits to the targeted register.
