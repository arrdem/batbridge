# The Batbridge ISA specification

The PC 31 (0b11111)
  - reading it produces the PC of the next instruction
  - having it as a target causes a branch

The Zero register 30 (0b11110)
  - always read as 0
  - writing to it prints its value on the console (ASCII)

The Immediate value 29 (0b1101)
  - when read produces the 11 bit immediate field in the instruction
  - writing to it prints its value on the console (HEX)

## Encoding specification

character : value type
 t : target register
 s : source register
 a : register representing left operand
 b : register representing right operand
 x : register representing index (multiplied by operand size)
 i : immediate signed quantity
 _ : value is not read. Set to 0 when assembled.

HLT 0x00 `000000 _____ _____ _____ ___________`
halts the machine immediately

LD  0x10 `010000 ttttt aaaaa xxxxx iiiiiiiiiii`
loads the word (+ a (* 4 x)) to register dst

ST  0x11 `010001 sssss aaaaa xxxxx iiiiiiiiiii`
stores the word in register src to (+ a (* 4 x))

IFLT 0x20 `100000 _____ aaaaa bbbbb iiiiiiiiiii`
execute the next instruction IFF (< a b)
 
IFLE 0x21 `100001 _____ aaaaa bbbbb iiiiiiiiiii`
execute the next instruction IFF (<= a b)
 
IFEQ 0x22 `100010 _____ aaaaa bbbbb iiiiiiiiiii`
execute the next instruction IFF (= a b)
 
IFNE 0x23 `100013 _____ aaaaa bbbbb iiiiiiiiiii`
execute the next instruction IFF (!= a b)
 
ADD  0x30 `110000 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (+ a b) to t
 
SUB  0x31 `110001 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (- a b) to t
 
DIV  0x32 `110010 ttttt aaaaa bbbbb iiiiiiiiiii`
stores the integer division of (/ a b) to t
 
MOD  0x33 `110011 ttttt aaaaa bbbbb iiiiiiiiiii`
stores the integer remainder (mod a b) to t
 
MUL  0x34 `110100 ttttt aaaaa bbbbb iiiiiiiiiii`
stores the product (* a b) to t
 
AND  0x35 `110101 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (bit-and a b) to t
 
OR   0x36 `110110 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (bit-or a b) to t

NAND 0x37 `110111 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (bit-not (bit-and a b)) to t
 
XOR  0x38 `111000 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (bit-xor a b) to t
 
SL   0x3a `111010 ttttt aaaaa bbbbb iiiiiiiiiii`
stores (bit-shift-left a b) to t
 
SAR  0x3b `111011 ttttt aaaaa bbbbb iiiiiiiiiii`
stores the arithmatic right shift of a, b bits to t
 
SLR  0x3c `111100 ttttt aaaaa bbbbb iiiiiiiiiii`
stores the shift of a b bits to t
