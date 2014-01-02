# The Batbridge ISA specification

The Batbridge machine standard was designed to be a simple RISC-like
instruction set used in
[Ahmed Gheith's CS375H](http://www.cs.utexas.edu/~gheith/). The
Batbridge architecture is a 32 bit instruction encoding, the first six
bits of which represent an opcode, the next three five bit hunks are
opcode register parameters and the last eleven bits of which is an
inline literal. The Batbridge machine features 32 registers, numbered
[0,31], of which the following are special.

The PC, register #31 (0b11111)
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

## Encoding specification notes
```
 t : Target register. The sequence "ttttt" means the bit ID of the
     target register, the metavariable t in the opcode description is
     taken to mean the target register itself
 s : Source register
 a : Register representing left operand
 b : Register representing right operand
 x : Register representing index (multiplied by operand size)
 i : Immediate signed quantity
 _ : Value is not read. Set to 0 when assembled by convention.

Opcodes are encoded as length 32 bit vectors. By convention the
rightmost character in the bit field specification is the 1st bit, and
the leftmost character is the 32nd bit. Note that this makes the
literal field the lowest order parameter and the opcode the highest
order parameter.

So for the HLT instruction, having the opcode 0x0 as defined below the
literal 0b00000000000000000000000000000000 would be a correct encoding
of the halt instruction. For add, being opcode 0x30 again as defined
below the encoding 0b11000000000000000000000000000000 would correctly
place the opcode and zero all the other fields.
```

## Instruction set
```
HLT  0x00 000000 _____ _____ _____ ___________
     halts the machine immediately

LD   0x10 010000 ttttt aaaaa xxxxx iiiiiiiiiii
     loads the word a + 4 * x to register dst

ST   0x11 010001 sssss aaaaa xxxxx iiiiiiiiiii
     stores the word in register src to the address a + 4 * x

IFLT 0x20 100000 _____ aaaaa bbbbb iiiiiiiiiii
     execute the next instruction IFF a < b
 
IFLE 0x21 100001 _____ aaaaa bbbbb iiiiiiiiiii
     execute the next instruction IFF a <= b
 
IFEQ 0x22 100010 _____ aaaaa bbbbb iiiiiiiiiii
     execute the next instruction IFF a == b
 
IFNE 0x23 100013 _____ aaaaa bbbbb iiiiiiiiiii
     execute the next instruction IFF a != b
 
ADD  0x30 110000 ttttt aaaaa bbbbb iiiiiiiiiii
     stores a + b to t
 
SUB  0x31 110001 ttttt aaaaa bbbbb iiiiiiiiiii
     stores a - b to t
 
DIV  0x32 110010 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the integer division of a / b to t
 
MOD  0x33 110011 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the integer remainder a / b to t
 
MUL  0x34 110100 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the product a * b to t
 
AND  0x35 110101 ttttt aaaaa bbbbb iiiiiiiiiii
     stores a & b (bitwise and) to t
 
OR   0x36 110110 ttttt aaaaa bbbbb iiiiiiiiiii
     stores a | b (bitwise or) to t

NAND 0x37 110111 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the bitwise nand of a and b to t
 
XOR  0x38 111000 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the xor of a and b to t
 
SL   0x3a 111010 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the left shift of a by b bits to t

SR   0x3b 111011 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the right shift of a by b bits to t
 
SAL  0x3c 111100 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the arithmatic shift left of a b bits to t

SAR  0x3d 111101 ttttt aaaaa bbbbb iiiiiiiiiii
     stores the arithmatic right shift of a by b bits to t
```
