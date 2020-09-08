open CS17SetupRackette;
open Read;
open Read.Reader;
open Types;

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

plus: list(value) => value
Input: a list of values, listVa
Output: a VNum result of the sum of 
the two elements in listVa, a failwith otherwise
*/
let plus: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VNum(hd + tl)
| []
| [_] 
| [_, ..._]=> failwith("plus operates on a list of two number values")
};

/* Test cases for plus */
checkExpect(plus([VNum(1), VNum(2)]), VNum(3), "positive hd, positive tl");

checkExpect(plus([VNum(1), VNum(0)]), VNum(1), "positive hd, zero tl");

checkExpect(plus([VNum(1), VNum(-2)]), VNum(-1), "positive hd, negative tl");

checkExpect(plus([VNum(0), VNum(2)]), VNum(2), "zero hd, positive tl");

checkExpect(plus([VNum(0), VNum(0)]), VNum(0), "zero hd, zero tl");

checkExpect(plus([VNum(0), VNum(-2)]), VNum(-2), "zero hd, negative tl");

checkExpect(plus([VNum(-1), VNum(2)]), VNum(1), "negative hd, positive tl");

checkExpect(plus([VNum(-1), VNum(0)]), VNum(-1), "negative hd, zero tl");

checkExpect(plus([VNum(-1), VNum(-2)]), VNum(-3), "negative hd, negative tl");

checkError(() => plus([]), "plus operates on a list of two number values");

checkError(() => plus([VNum(1)]), 
"plus operates on a list of two number values");

checkError(() => plus([VBool(true), VBool(false)]), 
"plus operates on a list of two number values");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

sub: list(value) => value
Input: a list of values, listVa
Output: the VNum result of the two elements in listVa 
subtracted from one another, a failwith otherwise
*/
let sub: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VNum(hd - tl)
| []
| [_] 
| [_, ..._]=> failwith("sub operates on a list of two number values")
};

/* Test cases for sub */
checkExpect(sub([VNum(1), VNum(2)]), VNum(-1), "positive hd, positive tl");

checkExpect(sub([VNum(1), VNum(0)]), VNum(1), "positive hd, zero tl");

checkExpect(sub([VNum(1), VNum(-2)]), VNum(3), "positive hd, negative tl");

checkExpect(sub([VNum(0), VNum(2)]), VNum(-2), "zero hd, positive tl");

checkExpect(sub([VNum(0), VNum(0)]), VNum(0), "zero hd, zero tl");

checkExpect(sub([VNum(0), VNum(-2)]), VNum(2), "zero hd, negative tl");

checkExpect(sub([VNum(-1), VNum(2)]), VNum(-3), "negative hd, positive tl");

checkExpect(sub([VNum(-1), VNum(0)]), VNum(-1), "negative hd, zero tl");

checkExpect(sub([VNum(-1), VNum(-2)]), VNum(1), "negative hd, negative tl");

checkError(() => sub([]), "sub operates on a list of two number values");

checkError(() => sub([VNum(1)]), "sub operates on a list of two number values");

checkError(() => sub([VBool(true), VBool(false)]), 
"sub operates on a list of two number values");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

mult: list(value) => value
Input: a list of values, listVa
Output: the VNum
*/
let mult: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VNum(hd * tl)
| [] 
| [_]
| [_, ..._] => failwith("mult operates on a list of two number values")
};

/* Test cases for mult */
checkExpect(mult([VNum(1), VNum(2)]), VNum(2), "positive hd, positive tl");

checkExpect(mult([VNum(1), VNum(0)]), VNum(0), "positive hd, zero tl");

checkExpect(mult([VNum(1), VNum(-2)]), VNum(-2), "positive hd, negative tl");

checkExpect(mult([VNum(0), VNum(2)]), VNum(0), "zero hd, positive tl");

checkExpect(mult([VNum(0), VNum(0)]), VNum(0), "zero hd, zero tl");

checkExpect(mult([VNum(0), VNum(-2)]), VNum(0), "zero hd, negative tl");

checkExpect(mult([VNum(-1), VNum(2)]), VNum(-2), "negative hd, positive tl");

checkExpect(mult([VNum(-1), VNum(0)]), VNum(0), "negative hd, zero tl");

checkExpect(mult([VNum(-1), VNum(-2)]), VNum(2), "negative hd, negative tl");

checkError(() => mult([]), "mult operates on a list of two number values");

checkError(() => mult([VNum(1)]), "mult operates on a list of two number values");

checkError(() => mult([VBool(true), VBool(false)]), 
"mult operates on a list of two number values");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

div: list(value) => value
Input: a list of values, listVa
Output: the VNum result of dividing the two elements in 
listVa; a failwith is given if conditions aren't met
*/
let div: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VNum(hd / tl)
| []
| [_] 
| [_, ..._]=> failwith("div operates on a list of two number values")
};

/* Test cases for div */
checkExpect(div([VNum(1), VNum(2)]), VNum(0), "positive hd, positive tl");

checkExpect(div([VNum(1), VNum(-2)]), VNum(0), "positive hd, negative tl");

checkExpect(div([VNum(0), VNum(2)]), VNum(0), "zero hd, positive tl");

checkExpect(div([VNum(0), VNum(-2)]), VNum(0), "zero hd, negative tl");

checkExpect(div([VNum(-1), VNum(2)]), VNum(0), "negative hd, positive tl");

checkExpect(div([VNum(-1), VNum(-2)]), VNum(0), "negative hd, negative tl");

checkError(() => div([]), "div operates on a list of two number values");

checkError(() => div([VNum(1)]), "div operates on a list of two number values");

checkError(() => div([VBool(true), VBool(false)]), 
"div operates on a list of two number values");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

remainder: list(value) => value
Input: a list of values, listVa
Output: the VNum result of taking the remainder of the 
first element in listVa divided by the second; a failwith
is given if condtions aren't met.
*/
let remainder: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VNum(hd - (tl * (hd / tl)))
| []
| [_] 
| [_, ..._]=> failwith("remainder operates on a list of two number values")
};

/* Test cases for remainder */
checkExpect(remainder([VNum(1), VNum(2)]), VNum(1), "positive hd, positive tl");

checkExpect(remainder([VNum(1), VNum(-2)]), VNum(1),
 "positive hd, negative tl");

checkExpect(remainder([VNum(0), VNum(2)]), VNum(0), "zero hd, positive tl");

checkExpect(remainder([VNum(0), VNum(-2)]), VNum(0), "zero hd, negative tl");

checkExpect(remainder([VNum(-1), VNum(2)]), VNum(-1), 
"negative hd, positive tl");

checkExpect(remainder([VNum(-1), VNum(-2)]), VNum(-1),
 "negative hd, negative tl");

checkError(() => remainder([]), 
"remainder operates on a list of two number values");

checkError(() => remainder([VNum(1)]), 
"remainder operates on a list of two number values");

checkError(() => remainder([VBool(true), VBool(false)]), 
"remainder operates on a list of two number values");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

equalP: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether the two elements
in listVa are equivalent to each other; a failwith is given
if conditions aren't met.
*/
let equalP: list(value) => value = listVa =>
switch(listVa) {
| [VNum(x), VNum(y)] => VBool(x == y)
| [VBool(x), VBool(y)] => VBool(x == y)
| [VList(x), VList(y)] => VBool(x == y)
| [VNum(_), _]
| [VBool(_), _]
| [VList(_), _] => VBool(false)
| []
| [_] 
| [_, ..._]=> failwith("equal? takes two arguments")
};

/* Test cases for equalP */
checkExpect(equalP([VNum(1), VNum(1)]), VBool(true), 
"positive hd, positive tl");

checkExpect(equalP([VNum(1), VNum(0)]), VBool(false), "positive hd, zero tl");

checkExpect(equalP([VNum(1), VNum(-2)]), VBool(false), 
"positive hd, negative tl");

checkExpect(equalP([VNum(0), VNum(2)]), VBool(false), 
"zero hd, positive tl");

checkExpect(equalP([VNum(0), VNum(0)]), VBool(true), 
"zero hd, zero tl");

checkExpect(equalP([VNum(0), VNum(-2)]), VBool(false), 
"zero hd, negative tl");

checkExpect(equalP([VNum(-1), VNum(2)]), VBool(false), 
"negative hd, positive tl");

checkExpect(equalP([VNum(-1), VNum(0)]), VBool(false), 
"negative hd, zero tl");

checkExpect(equalP([VNum(-1), VNum(-1)]), VBool(true), 
"negative hd, negative tl");

checkExpect(equalP([VBool(true), VBool(true)]), VBool(true),
 "true hd, true tl");

checkExpect(equalP([VBool(true), VBool(false)]), VBool(false), 
"true hd, false tl");

checkExpect(equalP([VBool(false), VBool(true)]), VBool(false), 
"false hd, true tl");

checkExpect(equalP([VBool(false), VBool(false)]), VBool(true), 
"false hd, false tl");

checkExpect(equalP([VList([VNum(1), VNum(2)]), 
VList([VNum(1), VNum(2)])]), VBool(true), "cons lists");

checkExpect(equalP([VList([]), VList([])]), VBool(true), "empty lists");

checkExpect(equalP([VNum(1), VList([VNum(1), VNum(2)])]), VBool(false),
 "unmatched num");

checkExpect(equalP([VBool(true), VList([VNum(1), VNum(2)])]), VBool(false), 
"unmatched bool");

checkExpect(equalP([VList([VNum(1), VNum(2)]), VNum(1)]), VBool(false), 
"unmatched list");

checkError(() => equalP([]), "equal? takes two arguments");

checkError(() => equalP([VNum(1)]), "equal? takes two arguments");

checkError(() => equalP([VBool(true), VBool(false), VBool(true)]), 
"equal? takes two arguments");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

lessThan: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether
the first element in listVa is less than the second
element; failwiths are given if conditions aren't met.
*/
let lessThan: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VBool(hd < tl)
| [_, _] => failwith("< operates on a list of two integers")
| []
| [_] 
| [_, ..._]=> failwith("< takes two arguments")
};

/* Test cases for lessThan */
checkExpect(lessThan([VNum(1), VNum(2)]), VBool(true), 
"positive hd, positive tl");

checkExpect(lessThan([VNum(1), VNum(0)]), VBool(false), 
"positive hd, zero tl");

checkExpect(lessThan([VNum(1), VNum(-2)]), VBool(false), 
"positive hd, negative tl");

checkExpect(lessThan([VNum(0), VNum(2)]), VBool(true), 
"zero hd, positive tl");

checkExpect(lessThan([VNum(0), VNum(0)]), VBool(false), 
"zero hd, zero tl");

checkExpect(lessThan([VNum(0), VNum(-2)]), VBool(false), 
"zero hd, negative tl");

checkExpect(lessThan([VNum(-1), VNum(2)]), VBool(true), 
"negative hd, positive tl");

checkExpect(lessThan([VNum(-1), VNum(0)]), VBool(true), 
"negative hd, zero tl");

checkExpect(lessThan([VNum(-1), VNum(-2)]), VBool(false), 
"negative hd, negative tl");

checkError(() => lessThan([VBool(true), VBool(false)]), 
"< operates on a list of two integers");

checkError(() => lessThan([]), "< takes two arguments");

checkError(() => lessThan([VNum(1)]), "< takes two arguments");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

greaterThan: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether
the first element in listVa is greater than the second
element; failwiths are given if conditions aren't met.
*/
let greaterThan: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VBool(hd > tl)
| [_, _] => failwith("> operates on a list of two integers")
| []
| [_] 
| [_, ..._] => failwith("> takes two arguments")
};

/* Test cases for greaterThan */
checkExpect(greaterThan([VNum(1), VNum(2)]), VBool(false), 
"positive hd, positive tl");

checkExpect(greaterThan([VNum(1), VNum(0)]), VBool(true), 
"positive hd, zero tl");

checkExpect(greaterThan([VNum(1), VNum(-2)]), VBool(true), 
"positive hd, negative tl");

checkExpect(greaterThan([VNum(0), VNum(2)]), VBool(false),
 "zero hd, positive tl");

checkExpect(greaterThan([VNum(0), VNum(0)]), VBool(false), 
"zero hd, zero tl");

checkExpect(greaterThan([VNum(0), VNum(-2)]), VBool(true),
 "zero hd, negative tl");

checkExpect(greaterThan([VNum(-1), VNum(2)]), VBool(false), 
"negative hd, positive tl");

checkExpect(greaterThan([VNum(-1), VNum(0)]), VBool(false), 
"negative hd, zero tl");

checkExpect(greaterThan([VNum(-1), VNum(-2)]), VBool(true), 
"negative hd, negative tl");

checkError(() => greaterThan([VBool(true), VBool(false)]), 
"> operates on a list of two integers");

checkError(() => greaterThan([]), "> takes two arguments");

checkError(() => greaterThan([VNum(1)]), "> takes two arguments");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

lessThanEq: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether
the first element in listVa is less than or equal to the second
element; failwiths are given if conditions aren't met.
*/
let lessThanEq: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VBool(hd <= tl)
| [_, _] => failwith("<= operates on a list of two integers")
| []
| [_] 
| [_, ..._]=> failwith("<= takes two arguments")
};

/* Test cases for lessThanEq */
checkExpect(lessThanEq([VNum(1), VNum(2)]), VBool(true), 
"positive hd, positive tl");

checkExpect(lessThanEq([VNum(1), VNum(0)]), VBool(false),
 "positive hd, zero tl");

checkExpect(lessThanEq([VNum(1), VNum(-2)]), VBool(false), 
"positive hd, negative tl");

checkExpect(lessThanEq([VNum(0), VNum(2)]), VBool(true), 
"zero hd, positive tl");

checkExpect(lessThanEq([VNum(0), VNum(0)]), VBool(true), "zero hd, zero tl");

checkExpect(lessThanEq([VNum(0), VNum(-2)]), VBool(false), 
"zero hd, negative tl");

checkExpect(lessThanEq([VNum(-1), VNum(2)]), VBool(true), 
"negative hd, positive tl");

checkExpect(lessThanEq([VNum(-1), VNum(0)]), VBool(true), 
"negative hd, zero tl");

checkExpect(lessThanEq([VNum(-1), VNum(-2)]), VBool(false), 
"negative hd, negative tl");

checkError(() => lessThanEq([VBool(true), VBool(false)]), 
"<= operates on a list of two integers");

checkError(() => lessThanEq([]), "<= takes two arguments");

checkError(() => lessThanEq([VNum(1)]), "<= takes two arguments");


/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

greaterThanEq: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether
the first element in listVa is greater than or equal to the second
element; failwiths are given if conditions aren't met.
*/
let greaterThanEq: list(value) => value = listVa =>
switch(listVa) {
| [VNum(hd), VNum(tl)] => VBool(hd >= tl)
| [_, _] => failwith(">= operates on a list of two integers")
| []
| [_]
| [_, ..._] => failwith(">= takes two arguments")
};

/* Test cases for greaterThanEq */
checkExpect(greaterThanEq([VNum(1), VNum(2)]), VBool(false), 
"positive hd, positive tl");

checkExpect(greaterThanEq([VNum(1), VNum(0)]), VBool(true), 
"positive hd, zero tl");

checkExpect(greaterThanEq([VNum(1), VNum(-2)]), VBool(true), 
"positive hd, negative tl");

checkExpect(greaterThanEq([VNum(0), VNum(2)]), VBool(false), 
"zero hd, positive tl");

checkExpect(greaterThanEq([VNum(0), VNum(0)]), VBool(true), "zero hd, zero tl");

checkExpect(greaterThanEq([VNum(0), VNum(-2)]), VBool(true), 
"zero hd, negative tl");

checkExpect(greaterThanEq([VNum(-1), VNum(2)]), VBool(false), 
"negative hd, positive tl");

checkExpect(greaterThanEq([VNum(-1), VNum(0)]), VBool(false),
 "negative hd, zero tl");

checkExpect(greaterThanEq([VNum(-1), VNum(-2)]), VBool(true), 
"negative hd, negative tl");

checkError(() => greaterThanEq([VBool(true), VBool(false)]), 
">= operates on a list of two integers");

checkError(() => greaterThanEq([]), ">= takes two arguments");

checkError(() => greaterThanEq([VNum(1)]), ">= takes two arguments");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

equal: list(value) => value 
Input: a list of values, listVa
Output: A VBool that reflects whether or not
the first element in listVa is equal to the second.
The condition is that they both have to be VNums for
equal not to give a failwith.
*/
let equal: list(value) => value = listVa =>
switch(listVa) {
| [VNum(x), VNum(y)] => VBool(x == y)
| [_, _] => failwith("must compare two numbers")
| []
| [_] 
| [_, ..._]=> failwith("= operates on a list of two numbers")
};

/* Test cases for equal */
checkExpect(equal([VNum(1), VNum(1)]), VBool(true), "positive hd, positive tl");

checkExpect(equal([VNum(1), VNum(0)]), VBool(false), "positive hd, zero tl");

checkExpect(equal([VNum(1), VNum(-2)]), VBool(false), 
"positive hd, negative tl");

checkExpect(equal([VNum(0), VNum(2)]), VBool(false), "zero hd, positive tl");

checkExpect(equal([VNum(0), VNum(0)]), VBool(true), "zero hd, zero tl");

checkExpect(equal([VNum(0), VNum(-2)]), VBool(false), "zero hd, negative tl");

checkExpect(equal([VNum(-1), VNum(2)]), VBool(false), 
"negative hd, positive tl");

checkExpect(equal([VNum(-1), VNum(0)]), VBool(false), 
"negative hd, zero tl");

checkExpect(equal([VNum(-1), VNum(-1)]), VBool(true), 
"negative hd, negative tl");

checkError(() => equal([VBool(true), VBool(false)]), 
"must compare two numbers");

checkError(() => equal([]), "= operates on a list of two numbers");

checkError(() => equal([VNum(1)]), "= operates on a list of two numbers");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

numberP: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether the
input given is a number
*/
let numberP: list(value) => value = listVa =>
switch(listVa) {
| [VNum(_)] => VBool(true)
| []
| [_] => VBool(false)
| [_, ..._] => failwith("number? can only act on one element")
};

/* Test cases for numberP */
checkExpect(numberP([VNum(1)]), VBool(true), "positive num");

checkExpect(numberP([VNum(0)]), VBool(true), "zero num");

checkExpect(numberP([VNum(-1)]), VBool(true), "negative num");

checkExpect(numberP([]), VBool(false), "empty list");

checkExpect(numberP([VBool(true)]), VBool(false), "non-number list");

checkError(() => numberP([VBool(true), VBool(false)]), 
"number? can only act on one element");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

zeroP: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether or not 
the input is zero.
*/
let zeroP: list(value) => value = listVa =>
switch(listVa) {
| [VNum(0)] => VBool(true)
| [VNum(_)] => VBool(false)
| []
| [VNum(_), ..._] => failwith("zero? works on one element only")
| [_] 
| [_, ..._]=> failwith("zero? is a test that only works on integers")
};

/* Test cases for zeroP */
checkExpect(zeroP([VNum(1)]), VBool(false), "positive num");

checkExpect(zeroP([VNum(0)]), VBool(true), "zero num");

checkExpect(zeroP([VNum(-1)]), VBool(false), "negative num");

checkError(() => zeroP([]), "zero? works on one element only");

checkError(() => zeroP([VNum(1), VBool(true)]), 
"zero? works on one element only");

checkError(() => zeroP([VBool(true)]), 
"zero? is a test that only works on integers");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

emptyP: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether or not 
the input is an empty list.
*/
let emptyP: list(value) => value = listVa =>
switch(listVa) {
| [VList([])] => VBool(true)
| [_] => VBool(false)
| [] 
| [_, ..._] => failwith("empty? expects one argument only")
};

/* Test cases for emptyP */
checkExpect(emptyP([VList([])]), VBool(true), "empty list");

checkExpect(emptyP([VList([VNum(1)])]), VBool(false), "one element list");

checkExpect(emptyP([VList([VNum(1), VNum(2)])]), VBool(false), 
"multi element list");

checkExpect(emptyP([VNum(2)]), VBool(false), "positive number input");

checkExpect(emptyP([VNum(0)]), VBool(false), "zero number input");

checkExpect(emptyP([VNum(-2)]), VBool(false), "negative number input");

checkExpect(emptyP([VBool(true)]), VBool(false), "bool input");

checkExpect(emptyP([VBool(false)]), VBool(false), "bool input");

checkError(() => emptyP([]), "empty? expects one argument only");

checkError(() => emptyP([VNum(1), VBool(true)]), 
"empty? expects one argument only");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

consP: list(value) => value
Input: a list of values, listVa
Output: a VBool that reflects whether or not 
the input is a cons list.
*/
let consP: list(value) => value = listVa =>
switch(listVa) {
| [VList([_, ..._])] => VBool(true)
| [_] => VBool(false)
| []
| [_, ..._] => failwith ("cons? works on one argument only")
};

/* Test cases for consP */
checkExpect(consP([VList([])]), VBool(false), "empty list");

checkExpect(consP([VList([VNum(1)])]), VBool(true), "one element list");

checkExpect(consP([VList([VNum(1), VNum(2)])]), VBool(true), 
"multi element list");

checkExpect(consP([VNum(2)]), VBool(false), "positive number input");

checkExpect(consP([VNum(0)]), VBool(false), "zero number input");

checkExpect(consP([VNum(-2)]), VBool(false), "negative number input");

checkExpect(consP([VBool(true)]), VBool(false), "bool input");

checkExpect(consP([VBool(false)]), VBool(false), "bool input");

checkError(() => consP([]), "cons? works on one argument only");

checkError(() => consP([VNum(1), VBool(true)]), 
"cons? works on one argument only");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

cons: list(value) => value
Input: a list of values, listVa
Output: a VList that is the result of joining appending
the first element with the second: the second must be a 
VList, failwiths are written if condtiions arent met.
*/
let cons: list(value) => value = listVa =>
switch(listVa) {
| [x, VList(y)] => VList([x, ...y])
| [_, _] => failwith("cons takes in an item and a list")
| []
| [_] 
| [_, ..._]=> failwith("cons has two inputs, an item and a list")
};

/* Test cases for cons */
checkExpect(cons([VNum(1), VList([])]), VList([VNum(1)]), "empty list");

checkExpect(cons([VNum(1), VList([VNum(2)])]), VList([VNum(1), VNum(2)]), 
"one element list");

checkExpect(cons([VNum(1), VList([VNum(2), VNum(3)])]), 
VList([VNum(1), VNum(2), VNum(3)]), "multi element list");

checkExpect(cons([VList([VNum(1)]), VList([])]), VList([VList([VNum(1)])]), 
"empty list");

checkExpect(cons([VList([VNum(1)]), 
VList([VNum(2)])]), VList([VList([VNum(1)]), VNum(2)]), "one element list");

checkExpect(cons([VList([VNum(1)]),
 VList([VNum(2), VNum(3)])]), VList([VList([VNum(1)]), VNum(2), VNum(3)]), 
 "multi element list");
 
checkExpect(cons([VBool(true), VList([])]), VList([VBool(true)]), "empty list");

checkExpect(cons([VBool(true), VList([VBool(false)])]),
 VList([VBool(true), VBool(false)]), "one element list");

checkExpect(cons([VBool(true), VList([VBool(false), VBool(false)])]), 
VList([VBool(true), VBool(false), VBool(false)]), "multi element list");

checkError(() => cons([VNum(1), VNum(2)]), "cons takes in an item and a list");

checkError(() => cons([]), "cons has two inputs, an item and a list");

checkError(() => cons([VNum(1)]), "cons has two inputs, an item and a list");

checkError(() => cons([VNum(1), VNum(2), VNum(3)]), 
"cons has two inputs, an item and a list");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

first: list(value) => value
Input: a list of values, listVa
Output: the first element of the VList
that should be the only item in listVa
*/
let first: list(value) => value = listVa =>
switch(listVa) {
| [VList([hd, ..._])] => hd
| [VList([])] => failwith("cannot take the first of the empty list")
| [] => failwith("first needs an argument to act upon")
| [_] => failwith("first operates on a list")
| [_, ..._] => failwith("first operates on one argument only")
};

/* Test cases for first */
checkExpect(first([VList([VNum(2)])]), VNum(2), "one element list");

checkExpect(first([VList([VNum(2), VNum(3)])]), VNum(2), "multi element list");

checkError(() => first([VList([])]), "cannot take the first of the empty list");

checkError(() => first([]), "first needs an argument to act upon");

checkError(() => first([VNum(1)]), "first operates on a list");

checkError(() => first([VNum(1), VNum(2), VNum(3)]), 
"first operates on one argument only");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

rest: list(value) => value
Input: a list of values, listVa
Output: a VList that is the rest of the 
VList that should be the only element in listVa
*/
let rest: list(value) => value = listVa =>
switch(listVa) {
| [VList([hd])] => hd /* can this be removed? */
| [VList([_, ...tl])] => VList(tl)
| [VList([])] => failwith("cannot take rest of the empty list")
| [] => failwith("rest needs an argument to act upon")
| [_] => failwith("rest operates on a list")
| [_, ..._] => failwith("rest operates on one argument only")
};

/* Test cases for rest */
checkExpect(rest([VList([VNum(2)])]), VNum(2), "one element list");

checkExpect(rest([VList([VNum(2), VNum(3)])]), VList([VNum(3)]), 
"multi element list");

checkError(() => rest([VList([])]), "cannot take rest of the empty list");

checkError(() => rest([]), "rest needs an argument to act upon");

checkError(() => rest([VNum(1)]), "rest operates on a list");

checkError(() => rest([VNum(1), VNum(2), VNum(3)]), 
"rest operates on one argument only");

/*
Data Definitions:
a list of values can either be 
empty or
(cons a b) where a is a value and b is a list of values
nothing else qualifies as a list of values

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure,
and represent different types of outputs of our code.

Example Data:
list(value):
[]
[VNum(3)]
[VBool(true), VNum(5)]

value:
VNum(2)
VBool(true)
VList([VNum(1)])

not: list(value) => value
Input: a list of values, listVa
Output: a VBool that is the opposite
of the VBool that should be the only element
in listVa; failwiths are written if conditions aren't met.
*/
let not: list(value) => value = listVa =>
switch(listVa) {
| [VBool(true)] => VBool(false)
| [VBool(false)] => VBool(true)
| [] => failwith("not expects one argument")
| [_] => failwith ("not takes in a bool input")
| [_, ..._] => failwith("not accepts one argument only")

};

/* Test cases for not */
checkExpect(not([VBool(true)]), VBool(false), "not of true");

checkExpect(not([VBool(false)]), VBool(true), "not of false");

checkError(() => not([]), "not expects one argument");

checkError(() => not([VNum(1)]), "not takes in a bool input");

checkError(() => not([VNum(1), VNum(2), VNum(3)]), 
"not accepts one argument only");

/*
Data definitions: 
an environment is a list of bindings which are (name, value) tuples.

Example Data: 
[(ID("+"), VBuiltin("builtin proc: +", plus))]
[(ID("x"), VNum(5)), (ID("y"), VNum(2))]

initialTLE ontains all of our builtin expressions that we were required 
to have from the project instructions and should be initialized each time 
Rackette is run. The builtins all use functions that are detailed above.
*/
let initialTLE: environment = [
  (ID("+"), VBuiltin("builtin proc: +", plus)),
  (ID("-"), VBuiltin("builtin proc: -", sub)),
  (ID("*"), VBuiltin("builtin proc: *", mult)),
  (ID("/"), VBuiltin("builtin proc: /", div)),
  (ID("remainder"), VBuiltin("builtin proc: remainder", remainder)),
  (ID("="), VBuiltin("builtin proc: =", equal)),
  (ID("<"), VBuiltin("builtin proc: <", lessThan)),
  (ID(">"), VBuiltin("builtin proc: >", greaterThan)),
  (ID("<="), VBuiltin("builtin proc: <=", lessThanEq)),
  (ID(">="), VBuiltin("builtin proc: >=", greaterThanEq)),
  (ID("equal?"), VBuiltin("builtin proc: equal?", equalP)),
  (ID("number?"), VBuiltin("builtin proc: number?", numberP)),
  (ID("zero?"), VBuiltin("builtin proc: zero?", zeroP)),
  (ID("empty?"), VBuiltin("builtin proc: empty?", emptyP)),
  (ID("cons?"), VBuiltin("builtin proc: cons?", consP)),
  (ID("cons"), VBuiltin("builtin proc: cons", cons)),
  (ID("first"), VBuiltin("builtin proc: first", first)),
  (ID("rest"), VBuiltin("builtin proc: rest", rest)),
  (ID("not"), VBuiltin("builtin proc: not", not))
];

/* 
Data Definitions: 
an environment is a list of bindings, which are defined
as (name, value) tuples. 

Example Data:
[]
[(ID("num1"), VNum(1))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2)), (ID("num3"), vNum(3))]

I/O spec for create:
Input: N/A
Output: [], an empty environment 
*/

let create: environment = [];

/* 
test-cases for create 
*/
checkExpect(create, [], "an empty environment");

/*
Data Definitions:

an environment is defined as a list of bindings, 
which are (name, value) tuples

a name is defined in Types.re as ID(string) and can 
be tied to a value in an environment.

a value is another type defined in Types.re; it may be a VNum,
VBool, VList, VBuiltin, or VClosure.

Example Data:
environment:
[]
[(ID("num1"), VNum(1))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2)), (ID("num3"), vNum(3))]

name:
ID("x")
ID("y")
ID("num1")

value:
VNum(1)
VBool(true)
VBool(false)
VList([])
VList([VNum(5), VBool(true)])
VBuiltin("builtin proc: number?", numberP)

Recursion Diagram #1 for lookup:

Original Input: ID("x"), [], []
Recursive Input: N/A
Recursive Output: N/A

Ideation Space (how to get from recursive output to original output): 
This is one of our bases cases, if the local and top-level environment are empty
then that means name is not there, so we return a failwith that says 
variable not defined

Original Output: Error - "variable not defined "

Recursion Diagram #2 for lookup:

Original Input: ID("x"), [(ID("w"), VNum(1)), 
(ID("x"), VNum(2)) (ID("y"), VNum(3)) (ID("z"), VNum(4))], []
Recursive Input: ID("x"), [(ID("x"), VNum(2)), 
(ID("y"), VNum(3)), (ID("z"), VNum(4))]
Recursive Output: true

Ideation Space (how to get from recursive output to original output):
Recursively call lookup on the recursive output, which is removing the first 
element from our merged environmentst if the names don't match.

Original Output: VNum(2)
*/
let rec lookup: (name, environment, environment) => 
value = (target, tle, env) =>
switch(List.append(env, tle)) {
    | [] => failwith("variable not defined")
    | [(namex, va), ...tl] => if (namex == target) {va} 
    else {lookup(target, tl, [])} 
};

/* Test cases for lookup */
checkExpect(lookup(ID("c"), [(ID("a"), VNum(1)), (ID("b"), VNum(2)), 
    (ID("c"), VNum(6))], []), VNum(6), "successful tle, empty env");

checkExpect(lookup(ID("c"), [], [(ID("a"), VNum(1)), (ID("b"), VNum(2)), 
    (ID("c"),VNum(6))]), VNum(6), "empty tle, succesful env");

checkExpect(lookup(ID("f"), [(ID("f"), VNum(1))], 
    [(ID("f"), VNum(2))]), VNum(2), "local and top level definition");

checkError(() => lookup(ID("a"), [], []), "variable not defined");

checkError(() => lookup(ID("a"), [(ID("b"), VNum(2))], [(ID("c"), VNum(2))]), 
    "variable not defined");

/* 
Data Definitions:

a concreteProgram Piece is a type defined in Types.re and is a 
portion of the product of the read procedure
given to us already. It may be a Number(int), Symbol(string), or 
List(list(concreteProgramPiece)). We will be trying to turn a 
concreteProgramPiece into an expression with this procedure.

an expression is also a type defined in Types.re
and is the output for our program. It can be a variety
of different things, including Num(int), Bool(bool), Empty, etc.

Example Data:

concreteProgramPiece:
Number(0)
Number(5)
Symbol("+")
Symbol("x")
List([Symbol("+"), Number(0), Number(5)])
List([Symbol("define"), Symbol("x"), Number(7)])

expression:
Num(5)
Bool(true)
Bool(false)
Empty
Name(ID("x"))
Name(ID("y"))
And(Bool(true), Bool(false))
etc. 

parseExpression: concreteProgramPiece => expression
Input: a concreteProgramPiece, input
Output: input converted into an expression

Recursion Diagram #1 for parseExpression:

Original Input: Number(5)
Recursive Input: N/A
Recursive Output: N/A

Ideation Space (how to get from recursive output to original output):
This is one of our base cases, if there's a number, bool, or empty as our 
input (the latter two in symbol form) we don't perform recursion.

Original Output: Num(5)

Recursion Diagram #2 for parseExpression:

Original Input: List([Symbol("and"), Symbol("true"), Symbol("false")])
Recursive Input: Symbol("true"), Symbol("false")
Recursive Output: VBool(true), VBool(false)

Ideation Space(how to get from recursive output to original output):
if the first element of the list is Symbol("and"), then we place 
the result of parseExpression of the recursive input into And().

Original Output: And(VBool(true), VBool(false))
*/

let rec parseExpression = (input: concreteProgramPiece): expression =>
    switch(input) {
    | Number(x) => Num(x)
    | Symbol("true") => Bool(true)
    | Symbol("false") => Bool(false)
    | Symbol("empty") => Empty
    | Symbol(str) => Name(ID(str))
    | List([Symbol("and"), (expr1: concreteProgramPiece), 
                           (expr2: concreteProgramPiece)]) => 
        And(parseExpression(expr1), parseExpression(expr2))
    | List([Symbol("or"), (expr1: concreteProgramPiece), 
                          (expr2: concreteProgramPiece)]) =>
        Or(parseExpression(expr1), parseExpression(expr2))
    | List([Symbol("if"), (expr1: concreteProgramPiece), 
                          (expr2: concreteProgramPiece), 
        (expr3: concreteProgramPiece)]) => If(parseExpression(expr1), 
        parseExpression(expr2), parseExpression(expr3))
    | List([Symbol("cond"), ...exprList]) => 
      switch(exprList) {
      | []
      | [List([])] => failwith ("cond needs to be followed with expressions")
      | [List([_, _]), ..._] => 
        {
          let rec condHelper: list(concreteProgramPiece) => 
        list((expression, expression)) = pairs => switch(pairs){
        | [] => []
        | [List([Number(_), _])] => failwith("your predicate can't be a number")
        | [List([hd, tl]), ...rest] => 
        [(parseExpression(hd), parseExpression(tl)), ...condHelper(rest)]
        }
        Cond(condHelper(exprList))
      }
    }
    | List([Symbol("lambda"), List(names), List(expressionBody)]) => 
    switch(names){
    | [] => failwith("a lambda expression needs to have one or more names")
    | [_, ..._] =>
    { let pullName: concreteProgramPiece => name =  
      fun
      | Symbol(str) => ID(str)
      | _ => failwith("a lambda expression needs valid names")
    Lambda(List.map(pullName, names), parseExpression(List(expressionBody))) 
    }
  }
    | List([Symbol("let"), List(nameExpressionPairs), List(expressionBody)]) => 
    { let pullNameExpPairs: concreteProgramPiece => (name, expression) =
      fun
      | List([Symbol(str), expr]) => (ID(str), parseExpression(expr))
      | _ => failwith("a let expression needs valid (name, expression) pairs")
    Let(List.map(pullNameExpPairs, nameExpressionPairs), 
    parseExpression(List(expressionBody)))
    }
    | List([]) => failwith("() is not valid Rackette code")
    | List([hd, ...tl]) => Application(List.map(parseExpression, [hd, ...tl]))
  };

/* test-cases for parseExpression */

checkExpectExpression(parseExpression(read("3")), Num(3), "number-parse");

checkExpectExpression(parseExpression(read("true")), 
Bool(true), "bool-parse true");

checkExpectExpression(parseExpression(read("false")), 
Bool(false), "bool-parse false");

checkExpectExpression(parseExpression(read("empty")), Empty, "empty-parse");

checkError(() => parseExpression(read("()")), "() is not valid Rackette code");

checkExpectExpression(parseExpression(read("(and true false)")), 
And(Bool(true), Bool(false)), "And-parse basic");

checkExpectExpression(parseExpression(read("(or true false)")), 
Or(Bool(true), Bool(false)), "Or-parse basic");

checkExpectExpression(parseExpression(read("(if true 1 2)")), 
If(Bool(true), Num(1), Num(2)), "If-parse basic");

checkExpectExpression(parseExpression(read("(if true true false)")), 
If(Bool(true), Bool(true), Bool(false)), "If-parse basic 2.0");

checkExpectExpression(parseExpression(read("(if true a b)")), 
If(Bool(true), Name(ID("a")), Name(ID("b"))), "If-parse basic 2.0");

checkExpectExpression(parseExpression(read("(cond ((empty? x) 0))")), 
Cond([(Application([Name(ID("empty?")), Name(ID("x"))]), Num(0))]) ,
 "One-case cond");

checkExpectExpression(parseExpression(
  read("(cond ((empty? x) 0) ((cons? x) 1))")), 
  Cond([(Application([Name(ID("empty?")), Name(ID("x"))]), Num(0)), 
  (Application([Name(ID("cons?")), Name(ID("x"))]), Num(1))]) , 
  "Two-case cond");

checkError(() => parseExpression(read("(cond)")), 
"cond needs to be followed with expressions");

checkError(() => parseExpression(read("(cond ())")), 
"cond needs to be followed with expressions");

checkError(() => parseExpression(read("(cond (5 4))")),
 "your predicate can't be a number");

checkExpectExpression(parseExpression(read("(+ 3 5)")), 
Application([Name(ID("+")), Num(3), Num(5)]), "Application test #1");

checkExpectExpression(parseExpression(read("(f 1 2 3 4)")), 
Application([Name(ID("f")), Num(1), Num(2), Num(3), Num(4)]),
 "Application test #2");

checkExpectExpression(parseExpression(read("(lambda (x) (+ x 1))")),
 Lambda([ID("x")], Application([Name(ID("+")), Name(ID("x")), Num(1)])), 
 "Lambda test #1");

checkExpectExpression(parseExpression(read("(lambda (x y) (+ x y))")), 
Lambda([ID("x"), ID("y")], Application([Name(ID("+")), Name(ID("x")),
 Name(ID("y"))])), "Lambda test #2");

checkError(() => parseExpression(read("(lambda () (+ x 5))")), 
"a lambda expression needs to have one or more names");

checkError(() => parseExpression(read("(lambda (x 5) (+ x 5))")), 
"a lambda expression needs valid names");

checkExpectExpression(parseExpression(read("(let  ((x 7)) (+ x 1))")), 
Let([(ID("x"), Num(7))],  Application([Name(ID("+")), Name(ID("x")), Num(1)])), 
"one-variable let expression");

checkError(() => parseExpression(read("(let ((x)) (+ x 1))")), 
"a let expression needs valid (name, expression) pairs");

checkExpectExpression(parseExpression(read("(let  ((x 7) (y 5)) (+ x y))")),
 Let([(ID("x"), Num(7)), (ID("y"), Num(5))], Application([Name(ID("+")),
  Name(ID("x")), Name(ID("y"))])), "two-variable let expression");

/* 
Data Definitions: 

a concreteProgram Piece is a portion of the product of the read procedure
already given to us and is defined in Types.re. It may be a Number(int), 
Symbol(string), or List(list(concreteProgramPiece)). We will be trying to turn a 
concreteProgramPiece that is a List(list(concreteProgramPiece)) 
starting with Symbol("define") into a definition. 

a definition is defined in Types.re and is a (name, expression) tuple 

Example Data:

concreteProgramPiece:
Number(0)
Number(5)
Symbol("+")
Symbol("x")
List([Symbol("+"), Number(0), Number(5)])
List([Symbol("define"), Symbol("x"), Number(7)])

definition:
(ID("x"), Num(5))
(ID("y"), Num(17))
(ID("bool1"), Bool(true))
(ID("bool2"), Bool(false))
(ID("empty"), Empty)

parseDefinition: concreteProgramPiece => definition
Input: a concreteProgramPiece, input
Output: the definition version of input.
*/

let parseDefinition: concreteProgramPiece => definition = input =>
switch(input) {
| List([Symbol("define"), Symbol(x), Number(y)]) => (ID(x), Num(y))
| List([Symbol("define"), Symbol(x), Symbol(y)]) => 
    (ID(x), parseExpression(Symbol(y)))
| List([Symbol("define"), Symbol(x), y]) => 
    (ID(x), parseExpression(y))
| List([Symbol("define"), Number(_), _]) => 
    failwith("You cannot define a number")
| List([Symbol("define"), _]) => failwith("define takes two arguments")
| List([Symbol("define"), _, _, ..._]) => failwith("define takes two arguments")
};

/* Test cases for parseDefinition */
checkExpectDefinition(parseDefinition(List([Symbol("define"), 
Symbol("one"), Number(1)])), (ID("one"), Num(1)), "number definition");

checkExpectDefinition(parseDefinition(List([Symbol("define"), Symbol("one"), 
    Symbol("two")])), (ID("one"), Name(ID("two"))), "name definition");

checkExpectDefinition(parseDefinition(List([Symbol("define"), Symbol("one"), 
    List([Symbol("cond"), List([Symbol("false"), Number(0)]), 
    List([Symbol("true"), Number(1)])])])), 
    (ID("one"), Cond([((Bool(false), Num(0))), ((Bool(true), Num(1)))])), 
    "cond definition");

checkExpectDefinition(parseDefinition(List([Symbol("define"), Symbol("one"), 
    List([Symbol("if"), Symbol("false"), Number(0), Number(1)])])),
    (ID("one"), If(Bool(false), Num(0), Num(1))), "if definition");

checkExpectDefinition(parseDefinition
(Reader.read("(define one (if false 0 1))")),
(ID("one"), If(Bool(false), Num(0), Num(1))), "if definition"); 

checkError(() => parseDefinition(List([Symbol("define"), Number(1), 
    Number(2)])), "You cannot define a number");

checkError(() => parseDefinition(List([Symbol("define"), Symbol("one")])), 
    "define takes two arguments");

checkError(() => parseDefinition(List([Symbol("define"), Symbol("one"), 
    Symbol("two"), Symbol("three")])), "define takes two arguments");

/*
Data Definitions:

a concreteProgram Piece is a portion of the product of the read procedure
already given to us and is defined in Types.re. It may be a Number(int), 
Symbol(string), or List(list(concreteProgramPiece)).

an abstractProgramPiece is also defined in Types.re and is either
Definition(definition) or Expression(expression). 

Example Data:

concreteProgramPiece:
Number(0)
Number(5)
Symbol("+")
Symbol("x")
List([Symbol("+"), Number(0), Number(5)])
List([Symbol("define"), Symbol("x"), Number(7)])

abstractProgramPiece:

Definition((ID("x"), Num(5)))
Definition((ID("y"), Bool(true)))
Expression(Num(17))
Expression(Bool(false))
Expression(Empty)

parsePiece: concreteProgramPiece => abstractProgramPiece
Input: a concreteProgramPiece, input
Output: input converted into an abstractProgramPiece
*/
let parsePiece: concreteProgramPiece => abstractProgramPiece = input =>
  switch(input) {
  | List([Symbol("define"), ..._]) => Definition(parseDefinition(input))
  | _ => Expression(parseExpression(input))
  };

/* Test cases for parsePiece */
checkExpect(parsePiece(Number(1)), Expression(Num(1)), "parsePiece expression");

checkExpect(parsePiece(Symbol("howdy")), Expression(Name(ID("howdy"))), 
    "parsePiece expression");

checkExpect(parsePiece(List([Symbol("define"), Symbol("one"), Number(1)])), 
    Definition((ID("one"), Num(1))), "parsePiece definition");

/* 

Data Definitions: 
a concreteProgram is defined in Types.re and is 
a list of concreteProgramPieces, which can hold value
Number(int), Symbol(string), or List(list(concreteProgramPiece)). We will be 
seeking to parse all elements of our concreteProgram, which are all individual
concreteProgram Pieces 

an abstractProgramPiece is also defined in Types.re and is either
Definition(definition) or Expression(expression).  

Example Data: 

concreteProgram:
[Symbol("CS"), Number(17)]
[Symbol("Hello"), Symbol("World")]

abstractProgram:
[Expression(Num(5))]
[Expression(Bool(true)), Definition(ID("x"), Bool(false))]

parse: concreteProgram => abstractProgram
Input: a concreteProgram, input
Output: input converted into an abstractProgram
We'll just map our parsePiece function across input
*/

let parse: concreteProgram => abstractProgram = input =>
/* this will parse all of the pieces of this program,
* giving us a list of pieces, our abstract syntax */
  List.map(parsePiece, input);

/* Test cases for parse */
checkExpect(parse([Number(1), Number(2), Number(3)]), [Expression(Num(1)), 
    Expression(Num(2)), Expression(Num(3))], "parse of numbers");

checkExpect(parse([Symbol("howdy"), Symbol("there"), Symbol("partner")]), 
    [Expression(Name(ID("howdy"))), Expression(Name(ID("there"))), 
    Expression(Name(ID("partner")))], "parse of symbols");

checkExpect(parse([List([Symbol("define"), Symbol("one"), Number(1)]), 
    List([Symbol("zero?"), Symbol("one")])]), 
    [Definition((ID("one"), Num(1))), Expression(Application([Name(ID("zero?")), 
    Name(ID("one"))]))], "parse of lists");

/* 
Data Definitions:

an environment is defined in Types.re as a list of bindings, 
which are (name, value) tuples

an expression is defined in Types.re and can be a variety of different things
including a Num(int), Bool(bool), Empty, or Name(name), etc.

A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure.

Example Data:

environment: 
[]
[(ID("num1"), VNum(1))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2)), (ID("num3"), vNum(3))]\

value: 
VNum(2)
VBool(true)
VList([VNum(1)])

expression:
Num(5)
Bool(true)
Bool(false)
Empty
Name(ID("x"))
Name(ID("y"))
And(Bool(true), Bool(false))
etc. 

eval: ((environment, environment, expression)) => value
Input: 
an environment, initialTLE
an environment, env
an expression, expr

Recursion Diagram #1 for eval:

Original Input: initialTLE, [], Num(5)
Recursive Input: N/A
Recursive Output: N/A

Ideation Space(how to get from recursive output to original output):
In the base cases where our input is a Num, Bool, Empty, or Lambda, there is
no recursion involved. Here, we just turn a Num into a VNum

Original Output: VNum(5)

Recursion Diagram #2 for eval:

Original Input: initialTLE, [], Application(Name(ID("+")), Num(3), Num(5)])
Recursive Input: VBuiltin("builtin proc: +", plus), VNum(3), VNum(5)
Recursive Output: VBuiltin("builtin proc: +", plus), [VNum(3), VNum(5)]

Ideation Space(how to get from recursive output to original output):
We apply the proc in VBuiltin to the list of values that now follows it to get
from the recursive output to the original output. 

Original Output: VNum(8)
*/
let rec eval: ((environment, environment, expression)) => value = 
((initialTLE, env, expr)) 
    => switch(expr) {
    | Num(x) => VNum(x)
    | Bool(boolValue) => VBool(boolValue)
    | Empty => VList([])
    | Name(ID(x)) => lookup(ID(x), initialTLE, env)
    | And(pred1, pred2) => 
      switch(eval((initialTLE, env, pred1))){
      | VBool(true) => 
        switch(eval((initialTLE, env, pred2))){
        | VBool(true) => VBool(true)
        | VBool(false) => VBool(false)
        | _ => failwith("And must have two arguments that evaluate to bools")
        }
      | VBool(false) => VBool(false)
      | _ => failwith("And must have two arguments that evaluate to bools")
      }
    | Or(pred1, pred2) =>
      switch(eval((initialTLE, env, pred1))){
      | VBool(false) => 
        switch(eval((initialTLE, env, pred2))){
        | VBool(true) => VBool(true)
        | VBool(false) => VBool(false)
        | _ => failwith("Or must have two arguments that evaluate to bools")
        }
      | VBool(true) => VBool(true)
      | _ => failwith("Or must have two arguments that evaluate to bools")
      }
    | If(expr1, expr2, expr3) =>
      switch(eval((initialTLE, env, expr1))){
      | VBool(true) => eval((initialTLE, env, expr2))
      | VBool(false) => eval((initialTLE, env, expr3))
      | _ => failwith(
        "The first argument of an if statement must be a predicate")
      }
    | Cond(expressionPairs) => 
        {
          let rec evalCondHelper: list((expression,expression)) => value = 
          pairs =>
          switch(pairs){
          | [] => failwith("all conditional statement were false")
          | [(expr1, expr2), ...tl] => 
            switch(eval((initialTLE, env, expr1))){
            | VBool(true) => eval((initialTLE, env, expr2))
            | VBool(false)=> evalCondHelper(tl)
            | _ => failwith("a predicate was expected but not given")
            }
          }
          evalCondHelper(expressionPairs)
        }
    | Lambda(names, expr1) => 
      VClosure(names, expr1, env)
    | Let([(names, expressions), ...rest], body) =>
    {
      let enviMaker: ((_, expression)) => ((_, value)) = fun
      | ((hd, tl)) => ((hd, eval((initialTLE, env, tl))))
      eval((initialTLE, List.append(List.map(enviMaker, 
      [(names, expressions), ...rest]), env), body))
    }
    
    | Application(appBody) => 
      {
        let evalMapper: expression => value = expr1 => 
        eval((initialTLE, env, expr1))
        switch(List.map(evalMapper, appBody)){
        | [VBuiltin(_, proc), ...tl] => proc(tl)
        | [VClosure(names, expr1, birthEnv), ...tl] => 
          if(List.length(names) == List.length(tl)){
            {
              let namestlBinder: (name, value) => (name, value) = (n, v) =>
              (n, v)
              eval((initialTLE, 
              List.append(List.append(List.map2(namestlBinder, names, tl), 
              birthEnv), env), expr1))
            }
          }
          else{
            failwith("improper number of arguments assigned for user-defined
            proc")
          }
        | [_, ..._] => failwith("did not receive valid Rackette code")
        }
      } 
    }
    
  /* NOTE: initialTLE is the top level environment and 
  env is the local environment, we might have to make a tle variable
  to replace initialTLE but I'm not sure */ 
  
/* Test cases for eval: */
checkExpect(eval((initialTLE, [], Num(5))), VNum(5), "eval of a number");

checkExpect(eval((initialTLE, [], Bool(true))), VBool(true), 
"eval of a boolean");

checkExpect(eval((initialTLE, [], Empty)), VList([]), "eval of Empty");

checkExpect(eval((initialTLE, [(ID("x"), VNum(5))], Name(ID("x")))), VNum(5), 
"eval of Name, present");

checkError(() => eval((initialTLE, [], Name(ID("x")))), "variable not defined");



/* 
Data definitions:
an environment is defined in Types.re and is a list of bindings, which are
(name, value) tuples.

a name is also defined in Types.re and is ID(string)

an expression is defined in Types.re and can be a variety of different things
including a Num(int), Bool(bool), Empty, or Name(name), etc.

Example Data:
environment: 
[]
[(ID("num1"), VNum(1))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2))]
[(ID("num1")), VNum(1)), (ID("num2"), VNum(2)), (ID("num3"), vNum(3))]\

value: 
VNum(2)
VBool(true)
VList([VNum(1)])

expression:
Num(5)
Bool(true)
Bool(false)
Empty
Name(ID("x"))
Name(ID("y"))
And(Bool(true), Bool(false))
etc. 

name:
ID("x")
ID("y")
ID("z")
ID("num1")
ID("bool1")

addDefinition: (environment, (name,expression)) => environment
Input: 
an environment, env: 
a (name, expression) tuple, (id, expr)

Output: an environment with
(id, eval(expr)) added to env if a name with id wasn't already defined
and a failwith statement informing the user the name has already been defined
if it has.
*/

let rec addDefinition: (environment, (name, expression)) => 
environment = (env, (id, expr)) => 
    switch(env){
    | [] => [(id, eval((env, [], expr)))]
    | ([(hd, tl), ...rest]) => 
    if (id == hd){
      failwith("this name is already defined")
    }
    else{
      [(hd, tl), ...addDefinition(rest, (id, expr))]
    }
    };

/* Test cases for addDefinition */

checkExpect(addDefinition([], (ID("x"), Num(5))), [(ID("x"), VNum(5))], 
  "addDef empty");

checkExpect(addDefinition([(ID("y"), VNum(3))], (ID("x"), Num(5))), 
  [(ID("y"), VNum(3)), (ID("x"), VNum(5))], "addDef non-empty");

checkError(() => addDefinition([(ID("x"), VNum(3))], (ID("x"), Num(5))), 
  "this name is already defined");

/* 
Data Definitions:
A value is a type defined in the Types module. 
It may be a VNum, VBool, VList, VBuiltin, or VClosure.
A string is an atomic data type. 

Example Data: 
value: 
VNum(2)
VBool(true)
VList([VNum(1)])

stringOfValue: value => string
input: a value, a_value
output: a string representation of a_value 

Recursion Diagram #1 for stringOfValue:

Original Input: VNum(17)
Recursive Input: N/A
Recursive Output: N/A

Ideation Space (how to get from recursive output to original output):
This is one of our base cases where we don't need to use recursion.
We just directly print out whatever's in VNum()

Original Output: "17"

Recursion Diagram #2 for stringOfValue:

Original Input: VList([VNum(1), VNum(2)])
Recursive Input: VNum(1), VNum(2)
Recursive Output: "1", "2"

Ideation Space(how to get from recursive output to original output):
We print out "(list " ++ recursive output +")"

Original Output: (list 1 2)
*/

let rec stringOfValue: value => string = a_value => switch(a_value) {
  | VNum(int) => string_of_int(int)
  | VBool(bool) => string_of_bool(bool)
  | VBuiltin(builtinName, _) => builtinName
  | VList(values)  => "(list " ++
  { 
  let rec vListHelper: list(value) => string = fun
    | [] => "())"
    | [hd] =>stringOfValue(hd) ++ ")"
    | [hd, ...tl] => stringOfValue(hd) ++ " " ++ vListHelper(tl)
     vListHelper(values)
  } 
  | VClosure(_, _, _) => "<user-proc>"
};


/*
test-cases for stringOfValue
*/

checkExpect(stringOfValue(VNum(5)), "5", "int case #1");

checkExpect(stringOfValue(VNum(0)), "0", "int case #2");

checkExpect(stringOfValue(VNum(-2)), "-2", "int case #2");

checkExpect(stringOfValue(VBool(true)), "true", "bool case true");

checkExpect(stringOfValue(VBool(false)), "false", "bool case false");

checkExpect(stringOfValue(VBuiltin("builtin proc: +", plus)), 
"builtin proc: +", "builtin proc case #1");

checkExpect(stringOfValue(VBuiltin("builtin proc: -", plus)), 
"builtin proc: -", "builtin proc case #2");

checkExpect(stringOfValue(VClosure([ID("x"), ID("y")], Name(ID("x")), 
initialTLE)), "<user-proc>", "printed Closure");

checkExpect(stringOfValue(VList([])), "(list ())", "printing empty list");

checkExpect(stringOfValue(VList([VNum(1)])), "(list 1)",
 "printing one-element list");

checkExpect(stringOfValue(VList([VBool(true), VBool(false)])),
 "(list true false)", "multi-element list #1");

checkExpect(stringOfValue(VList([VNum(1), VNum(2), VNum(3)])),
 "(list 1 2 3)", "multi-element list #2");

/* process: this procedure processes the abstract program
   representation of a Rackette program following the
   Rackette rules of processing

   Data Definitions:
   an abstractProgramPiece is also defined in Types.re and is either
   Definition(definition) or Expression(expression). 

   a list of values is either 
   empty or 
   (cons a b) where a is a value and b is a list of values

   Example Data:
   abstractProgram:
   [Expression(Num(5))]
   [Expression(Bool(true)), Definition(ID("x"), Bool(false))]

   list of values:
   []
   [VNum(1)]
   [VBool(true), VBool(false)]

   process: abstractProgram => list(value)
   Input: pieces, an abstract program representation of a Rackette program
   Output: the list of values corresponding to
   the evaluation of any expressions present in pieces */

let process: abstractProgram => list(value) = pieces => {
  let rec processHelper:
  (environment, abstractProgram) => list(value) = (initialTLE, pieces) =>
    switch (pieces) {
    | [] => []
    | [Definition(d), ...tl] => processHelper(addDefinition(initialTLE, d), tl) 
    /* Removed double parentheses */
    | [Expression(e), ...tl] => [
        eval((initialTLE, [], e)),
        ...processHelper(initialTLE, tl),
      ]
    };
  processHelper(initialTLE, pieces);
};
/*
test-cases for process:
*/

checkExpect(process([]), [], "empty abstract program");

checkExpect(process([Expression(Num(1)), Expression(Num(2)), 
  Expression(Num(3))]), [VNum(1), VNum(2), VNum(3)], "process numbers");

checkExpect(process([Definition((ID("one"), Num(1)))]), [], 
  "process definition");

checkExpect(process([Definition((ID("one"), Num(1))), 
  Expression(Application([Name(ID("zero?")), 
  Name(ID("one"))]))]), [VBool(false)], "definition and expression");

checkError(() => process([Expression(Name(ID("howdy"))), 
  Expression(Name(ID("there"))), Expression(Name(ID("partner")))]), 
  "variable not defined");

/* rackette: this procedure will interpret a Rackette program
   and return its value as a string, if it has one
 
   Data Definitions:

   a rawProgram is defined in Types.re and is a string

   Example Data:
   "(define x 5)"
   "(+ 3 5)"
   "(- 5 3)"

   rackette: rawProgram => list(string)
   Input: a Rackette program represented as a raw program, program
   Output: a list of the string representations of
        the evaluated Rackette expressions in programs */

let rackette: rawProgram => list(string) = program =>
  List.map(stringOfValue, process(parse(readAll(program))));

/*
test-cases for rackette
*/

checkExpect(rackette("((lambda (x y)((lambda (y)(+ x y))x))17 18)"), ["34"], 
"rackette handout test-case #1");

checkExpect(rackette("((lambda (x y)((lambda (x)(+ x y))x))17 18)"), ["35"], 
"rackette handout test-case #2");

checkExpect(rackette("((lambda (x y)((lambda (x)(+ x y))y))17 18)"), ["36"],
"rackette handout test-case #3");

checkExpect(rackette("(let ((x 0))(let 
(( f (lambda (a)(* x a ))))(let ((x 1 ))(f 5))))
"), ["0"], "rackette handout test-case #4");

checkExpect(rackette("(let ((x 0)(y 18))(let 
((f (lambda (a b)(+ x b )))(x 17))(f y x)))"), ["17"], 
"rackette handout test-case #5");

checkExpect(rackette("(+ 3 5)"), ["8"], "rackette handout test-case #6");

checkExpect(rackette("(define fact (lambda (x)(if (zero? x) 1 
(* x (fact ( - x 1))))))(fact 3)"), ["6"], 
"rackette handout test-case #7");

checkExpect(rackette("(define y 17)(let ((y 3))(+ y 7))"), ["10"],
 "rackette handout test-case #8");

checkExpect(rackette("(define x 1) (+ x 1)"), ["2"],
 "basic definitions and operation");

checkExpect(rackette("3 5"), ["3", "5"], "basic numbers");

checkExpect(rackette("(if (= 0 1) 2 3)"), ["3"], "basic if statement");


/* 
We moved some of our check-expects for eval down to the bottom
since they were vanishing when they were in a large block together.
*/
checkExpect(eval((initialTLE, [], And(Bool(true), Bool(true)))), VBool(true),
 "eval of And: true, true"); 

checkExpect(eval((initialTLE, [], And(Bool(true), Bool(false)))), VBool(false),
 "eval of And: true, false");

checkExpect(eval((initialTLE, [], And(Bool(false), Bool(true)))), VBool(false), 
"eval of And: false, true");

checkExpect(eval((initialTLE, [], And(Bool(false), Bool(false)))), VBool(false),
 "eval of And: false, false");

checkError(() => eval((initialTLE, [], And(Num(5), Bool(false)))), 
"And must have two arguments that evaluate to bools");

checkError(() => eval((initialTLE, [], And(Bool(true), Num(5)))), 
"And must have two arguments that evaluate to bools");

checkExpect(eval((initialTLE, [], Or(Bool(true), Bool(true)))), VBool(true), 
"eval of Or: true, true");

checkExpect(eval((initialTLE, [], Or(Bool(true), Bool(false)))), VBool(true), 
"eval of Or: true, false");

checkExpect(eval((initialTLE, [], Or(Bool(false), Bool(true)))), VBool(true),
 "eval of Or: false, true");

checkExpect(eval((initialTLE, [], Or(Bool(false), Bool(false)))), VBool(false), 
"eval of Or: false, false");

checkError(() => eval((initialTLE, [], Or(Num(5), Bool(false)))), 
"Or must have two arguments that evaluate to bools");

checkError(() => eval((initialTLE, [], Or(Bool(false), Num(5)))), 
"Or must have two arguments that evaluate to bools"); 

checkExpect(eval((initialTLE, [], If(Bool(true), Num(5), Num(6)))), VNum(5), 
"eval of If: true predicate");

checkExpect(eval((initialTLE, [], If(Bool(false), Num(5), Num(6)))), VNum(6), 
"eval of If: false predicate");

checkError(() => eval((initialTLE, [], If(Num(4), Num(5), Num(6)))), 
"The first argument of an if statement must be a predicate");

checkExpect(eval((initialTLE, [(ID("x"), VNum(5))],
 Cond([((Bool(false), Num(0))), ((Bool(true), Num(1)))]))), VNum(1), 
 "two case cond expression");

checkExpect(eval((initialTLE, [(ID("x"), VNum(5))], 
Cond([((Bool(true), Num(1)))]))), VNum(1), 
"one case cond expression");

checkError(() => eval((initialTLE, [(ID("x"), VNum(5))], 
Cond([((Bool(false), Num(1)))]))), "all conditional statement were false");

checkError(() => eval((initialTLE, [(ID("x"), VNum(5))], 
Cond([((Num(1), Num(1)))]))), "a predicate was expected but not given");

checkExpect(eval((initialTLE, [], Lambda([ID("x")], 
Application([Name(ID("+")), Name(ID("x")), Num(5)])))), VClosure([ID("x")], 
Application([Name(ID("+")), Name(ID("x")), Num(5)]), []), 
"lambda with one name");

checkExpect(eval((initialTLE, [], Lambda([ID("x"), ID("y")], 
Application([Name(ID("+")), Name(ID("x")), Name(ID("y"))])))), 
VClosure([ID("x"), ID("y")], Application([Name(ID("+")), Name(ID("x")), 
Name(ID("y"))]), []), "lambda with two names");

checkExpect(eval((initialTLE, [], Let([(ID("x"), Num(7))], 
Name(ID("x"))))), VNum(7), "let case #1");

checkExpect(eval((initialTLE, [], Let([(ID("x"), Num(7)), (ID("y"), Num(5))], 
Name(ID("x"))))), VNum(7), "let case #2");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("+")), Num(3), Num(5)]))), VNum(8),
 "Application VBuiltin plus");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("-")), Num(4), Num(6)]))), VNum(-2),
 "Application VBuiltin sub");

 checkExpect(eval((initialTLE, [], 
 Application([Name(ID("*")), Num(4), Num(6)]))), VNum(24),
"Application VBuiltin mult");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("/")), Num(36), Num(5)]))), VNum(7),
 "Application VBuiltin div");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("remainder")), Num(17), Num(3)]))), VNum(2),
"Application VBuiltin remainder");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("=")), Num(7), Num(7)]))), VBool(true),
"Application VBuiltin = true");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("=")), Num(6), Num(7)]))), VBool(false),
"Application VBuiltin = false");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("<")), Num(6), Num(7)]))), VBool(true),
"Application VBuiltin <");

checkExpect(eval((initialTLE, [], 
Application([Name(ID(">")), Num(6), Num(7)]))), VBool(false),
"Application VBuiltin >");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("<=")), Num(6), Num(7)]))), VBool(true),
"Application VBuiltin <=");

checkExpect(eval((initialTLE, [], 
Application([Name(ID(">=")), Num(6), Num(7)]))), VBool(false),
"Application VBuiltin >=");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("equal?")), Bool(true), Bool(true)]))), VBool(true),
"Application VBuiltin equal?");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("number?")), Num(6)]))), VBool(true),
"Application VBuiltin number?");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("zero?")), Num(6)]))), VBool(false),
"Application VBuiltin zero?");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("empty?")), Empty]))), VBool(true),
"Application VBuiltin empty?");

checkExpect(eval((initialTLE, [], 
Application([Name(ID("cons?")), Empty]))), VBool(false),
"Application VBuiltin cons?"); 
/* eval of Application matched with VBuiltin is clearly effective. */

checkExpect(eval((initialTLE, [], Application([Lambda([ID("x")], 
Application([Name(ID("+")), Name(ID("x")), Num(1)])), Num(1)]))), VNum(2), 
"lambda test case 1");

checkExpect(eval((initialTLE, [], Application([Lambda([ID("x"), ID("y")], 
Application([Name(ID("+")), Name(ID("x")), Name(ID("y"))])), 
Num(1), Num(2)]))), VNum(3), "lambda test case 2");






