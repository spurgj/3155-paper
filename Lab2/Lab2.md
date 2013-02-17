# Lab 2 #
## Kevin Barry - kbarry ##

## 2. ##
> a. Axiom: a|b ε VObjects </p>

		x ε VObjects
		x ε AObjects

	(Imagine a line under x ε VObjects)

> b. There are 2 possible parse trees (See attached)

> c. This language will produce sentences of at least one a, sentences of zero or more b's and sentences of at least one c.

> d. 1 & 4 are the only possible sentences described by the grammar (See attached)

>e. 1 & 5 are the only possible sentences (See attached)

## 3 ##
> a. i. The expressions contain one or more operands. If more than one operand is there, it is separated by an operator.

>a. ii.Yes, they generate the same expression. The only difference is that one is left associative, and the other right associative.

>b. i. Operand can be a number, Id or an operand with an expression in brackets.

>b. ii. They will generate the same expression. Like the previous problem, the first is left associative, and the other right associative.

>c. The + operator is left associative because the non-terminal Operator appears in one rule which can only expand to the left. It also has the same precedence as > because there is nothing that forces precedence between the two operators.

>d. First, I tested the expression 10 << 7-4 << 0 and 10 << (7-4) << 0, which both gave the same answer. Next, I tried (10 << 7) - (4 << 0), which gave a much larger answer. Ergo, - has a higher precedence.

>e. </p>
**Float ::= Int, Nat Exponent</p>
Int ::= 0 | NotZero</p>
Nat ::= 0 | Pos</p>
Exponent ::= ε | NotZero</p>
NotZero ::= Sign Pos</p>
Nums ::= 1|2|3|4|5|6|7|8|9</p>
Pos ::= Nums Seq</p>
Seq ::= ε | 0 Seq | Nums Seq</p>
Sign ::= ε | -</p>**