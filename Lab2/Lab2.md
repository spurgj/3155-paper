Justin Spurgeon
spurgeoj
Lab 2 Written Answers
=====================

###Problem 2

#2a

V in VObjects
-------------
a in VObjects
b in VObjects

A in AObjects
-------------
V in AObjects
A in AObjects
B in AObjects

#2b

See image "trees1.jpg". The grammar can be made into two distinct parse trees depending on which A is evaluated first, meaning it is ambiguous.

#2c

It can generate a string of a's, a string of b's, a string of c's, each of arbitrary length, or an empty string. It is right associative. 

#2d

1. S => AaBb
	 => baBb
	 => baab

In language.

2. Not in language.
3. Not in language.

4. S => AaBb
	 => AbaBb
	 => bbaBb
	 => bbaab

In language.

#2e

1. See image "trees2.jpg". In language.
2. Not in language.
3. Not in language.
4. Not in language.
5. See image "trees3.jpg". In language.

###Problem 3

#3a

i. The first grammar generates expressions consisting of either a single operand, or a string of operands (O) and operators (X) in the form OXOXOX... It is left-associative.
The second grammar generates expressions consisting of either a single operand, or a string of operands (O) and operators(X) in the form OXOXOX... It is right-associative.

ii. They are not the same because they have different associativities.

#3b

i. The first grammar can generate a number, Id, or string of numbers and Id's followed by an equal number of [expressions]. It is left associative.
The second grammar can generate a number, Id, or string of numbers and Id's followed by an equal number of [expressions]. It is right associative.

ii. They can generate the same output, but have different associativities, so they are not the same.

#3c

'+' is left associative because Expr always branches to the left, so the operators will be evaluated from left to right. It has the same precedence as '>' because they are interchangeable
and neither occurs in a place where the other cannot occur.

#3d

- has higher precedence than <<. 1000 - 0100 << 1 evaluates to 0100, rather than 0000. All the numbers mentioned were binary.

#3e

S ::= I N . M X
I ::= - | epsilon
N ::= NM | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
M ::= 0M | 1M | 2M | 3M | 4M | 5M | 6M | 7M | 8M | 9M | epsilon
X ::= E I N

Epsilon is the empty character, E is the exponent sign.

