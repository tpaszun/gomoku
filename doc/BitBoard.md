# Board as Vector of Word64

```
  Store Board as a U.Vector Word64, where
    each line is Word64
    each field is 2 bits of Word64, where first field in list is least significant 2 bits
    and last field in list most significant 2 bits
    ie. X|O| | |X|O|O|X -> 01 10 10 01 00 00 10 01 -> 0110100100001001 -> 26889 (decimal) -> 0x6909
        |           | |    |  |                 |
        |           | `----'  |                 |
        |           `---------'                 |
        `---------------------------------------'

    X - 01
    O - 10
    _ - 00

    Word32 - 32bit => max 16 fields
    Word64 - 64bit => max 32 fields
    For boards 16x16 < x <= 32x32 (19x19 particularly) Word64 should be used
```

## Empty board (of size 8x8)

```
[
    -- horizontal lines
index  line                         line length
 0:     00 00 00 00 00 00 00 00     8
 1:     00 00 00 00 00 00 00 00     8
 2:     00 00 00 00 00 00 00 00     8
 3:     00 00 00 00 00 00 00 00     8
 4:     00 00 00 00 00 00 00 00     8
 5:     00 00 00 00 00 00 00 00     8
 6:     00 00 00 00 00 00 00 00     8
 7:     00 00 00 00 00 00 00 00     8

    -- vertical lines
 8:     00 00 00 00 00 00 00 00     8
 9:     00 00 00 00 00 00 00 00     8
10:     00 00 00 00 00 00 00 00     8
11:     00 00 00 00 00 00 00 00     8
12:     00 00 00 00 00 00 00 00     8
13:     00 00 00 00 00 00 00 00     8
14:     00 00 00 00 00 00 00 00     8
15:     00 00 00 00 00 00 00 00     8

    -- left diagonal lines
16:    (00 00 00 00 00 00 00)00     0
17:    (00 00 00 00 00 00)00 00     1
18:    (00 00 00 00 00)00 00 00     2
19:    (00 00 00 00)00 00 00 00     3
20:    (00 00 00)00 00 00 00 00     4
21:    (00 00)00 00 00 00 00 00     5
22:    (00)00 00 00 00 00 00 00     6
23:     00 00 00 00 00 00 00 00     7
24:    (00)00 00 00 00 00 00 00     8
25:    (00 00)00 00 00 00 00 00     9
26:    (00 00 00)00 00 00 00 00    10
27:    (00 00 00 00)00 00 00 00    11
28:    (00 00 00 00 00)00 00 00    12
29:    (00 00 00 00 00 00)00 00    13
30:    (00 00 00 00 00 00 00)00    14

    -- right diagonal lines
31:    (00 00 00 00 00 00 00)00     0
32:    (00 00 00 00 00 00)00 00     1
33:    (00 00 00 00 00)00 00 00     2
34:    (00 00 00 00)00 00 00 00     3
35:    (00 00 00)00 00 00 00 00     4
36:    (00 00)00 00 00 00 00 00     5
37:    (00)00 00 00 00 00 00 00     6
38:     00 00 00 00 00 00 00 00     7
39:    (00)00 00 00 00 00 00 00     8
40:    (00 00)00 00 00 00 00 00     9
41:    (00 00 00)00 00 00 00 00    10
42:    (00 00 00 00)00 00 00 00    11
43:    (00 00 00 00 00)00 00 00    12
44:    (00 00 00 00 00 00)00 00    13
45:    (00 00 00 00 00 00 00)00    14
]
```

# Update board with move x, y

- update horizontal line
- update vertical line
- update left diagonal line
- update right diagonal line


# Evaluate player

- apply patterns matching on each line
  - prefixes
  - suffixes
  - infixes
    - on each subsegment
  - exact


## Patterns (fives)

```
[
    -- prefix
    00 10 10 10 10 10
    01 10 10 10 10 10
    -- suffix
    10 10 10 10 10 00
    10 10 10 10 10 01
    -- infix
    00 10 10 10 10 10 00
    00 10 10 10 10 10 01
    01 10 10 10 10 10 00
    01 10 10 10 10 10 01
    -- exact
    10 10 10 10 10
    10 10 10 10 10
]
```
