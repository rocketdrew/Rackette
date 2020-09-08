exception Overflow;

open Types;

let (+) = (a: int, b: int): int => {
  let c = a + b;
  if (a lxor b lor (a lxor lnot(c)) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let (-) = (a: int, b: int): int => {
  let c = a - b;
  if (a lxor lnot(b) lor (b lxor c) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let ( * ) = (a: int, b: int): int => {
  let c = a * b;
  if (Int64.of_int(c) == Int64.mul(Int64.of_int(a), Int64.of_int(b))) {
    c;
  } else {
    raise(Overflow);
  };
};

let (/) = (a: int, b: int): int =>
  if (a == min_int && b == (-1)) {
    raise(Overflow);
  } else {
    a / b;
  };

let (~-) = (x: int): int =>
  if (x != min_int) {
    - x;
  } else {
    raise(Overflow);
  };

/* take in a string, s, and print it with green color */
let printGreen = (s: string): unit =>
  print_string("\027[32m" ++ s ++ "\027[0m\n");

/* take in a string, s, and print it with red color */
let printRed = (s: string): unit =>
  print_string("\027[31m" ++ s ++ "\027[0m\n");

type result('a) =
  | Actual_Result('a)
  | Expected_Result('a)
  | Actual_Error(string)
  | Expected_Error(string);

type check_result('a) =
  | Test_Passed
  | Test_Failed(result('a), result('a));

/* checkExpect
  Inputs: actual and expected, two 'a and message, a string
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
  let checkExpect = (actual: 'a, expected: 'a, message: string): check_result('a) =>
  if (actual == expected) {
    printGreen("ce_Success: " ++ message);
    Test_Passed;
  } else {
    printRed("ce_Fail: " ++ message);
    printRed("expected output: ");
    Js.log(expected);
    printRed("actual output: ");
    Js.log(actual)
    Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };




/* takes in a name, namex, and returns its string representation */
let stringOfName = (namex : name) : string => 
    switch (namex) { 
    | ID(stringx) => "ID(\"" ++ stringx ++ "\")"
    }

/* takes in a list of names, nameList, and returns its string representation */
let rec stringOfNameList = (namelist : list(name)) : string => 
    switch (namelist) {
    | [] => ""
    | [namex] => stringOfName(namex)
    | [namex, ... tl] => stringOfName(namex) ++ ", " ++ stringOfNameList(tl)
    }

/* takes in an Expression, expr, and returns its string representation */
let rec stringOfExpression = (expr : expression) : string => 
    switch (expr) {
    | Num(intx) => "Num(" ++ string_of_int(intx) ++ ")"
    | Bool(boolx) => "Bool(" ++ string_of_bool(boolx) ++ ")"
    | Empty => "Empty"
    | Name(namex) => "Name(" ++ stringOfName(namex) ++ ")"
    | And(expression1, expression2) => "And(" ++ stringOfExpression(expression1) ++ ", " ++ stringOfExpression(expression2) ++ ")"
    | Or(expression1, expression2) => "Or(" ++ stringOfExpression(expression1) ++ ", " ++ stringOfExpression(expression2) ++ ")"
    | If(expression1, expression2, expression3) => "If(" ++ stringOfExpression(expression1) ++ stringOfExpression(expression2) ++ stringOfExpression(expression3) ++ ")"
    | Cond(exprpairlist : list((expression, expression))) => 
        let rec stringOfCondExpressionPairList = (exprpairlist : list((expression, expression))) : string =>
            switch (exprpairlist) {
                | [] => ""
                | [(expression1, expression2)] => "(" ++ stringOfExpression(expression1) ++ ", " ++ stringOfExpression(expression2) ++ ")" 
                | [(expression1, expression2), ...tl] => "(" ++ stringOfExpression(expression1) ++ ", " ++ stringOfExpression(expression2) ++ "), " ++ stringOfCondExpressionPairList(tl)
            }; 
        "Cond([" ++ stringOfCondExpressionPairList(exprpairlist) ++ "])"
    
    | Lambda(namelist, expression1) => "Lambda(" ++ "[" ++ stringOfNameList(namelist) ++ "], " ++ stringOfExpression(expression1) ++ ")" /* todo */
    | Let(letexprpairlist, expression1) => 
        let rec stringOfLetExpressionPairList = (letexprpairlist : list((name, expression))) : string =>
        switch (letexprpairlist) {
            | [] => ""
            | [(name1, expression2)] => "(" ++ stringOfName(name1) ++ ", " ++ stringOfExpression(expression2) ++ ")" 
            | [(name1, expression2), ...tl] => "(" ++ stringOfName(name1) ++ ", " ++ stringOfExpression(expression2) ++ "), " ++ stringOfLetExpressionPairList(tl)
        }; 
        "Let([" ++ stringOfLetExpressionPairList(letexprpairlist) ++ "], " ++ stringOfExpression(expression1) ++ ")"
    | Application(expressionlist) => 
        let rec stringOfApplication = (exprlist : list(expression)) : string => 
        switch (exprlist) {
            | [] => ""
            | [expr] => stringOfExpression(expr)
            | [expr, ...tl] => stringOfExpression(expr) ++ ", " ++  stringOfApplication(tl)
        };
        "Application([" ++ stringOfApplication(expressionlist) ++ "])"  
    ; 
    }

/* takes in a Definition, def, and returns its string representation */
let stringOfDefinition = (def : definition) : string => "(" ++ 
    switch (def) {
    | (name1, expression1) => stringOfName(name1) ++ ", " ++ stringOfExpression(expression1) ++ ")"
    }

/* takes in an abstractProgramPiece, piece, and returns its string representation */
let stringOfAbstractProgramPiece = (piece : abstractProgramPiece) : string => 
    switch (piece) {
    | Expression(expr) => "Expression(" ++ stringOfExpression(expr) ++ ")"
    | Definition(def) => "Definition(" ++ stringOfDefinition(def) ++ ")"
  }

/* takes in an abstractProgram, abstr, and returns its string representation */
let rec stringOfAbstractProgram = (abstr : abstractProgram) : string => 
  {
    let rec abstrProgHelper = (abstr : abstractProgram) : string => 
        switch (abstr) {
        | [] => ""
        | [abstrProgPiece] => stringOfAbstractProgramPiece(abstrProgPiece) 
        | [abstrProgPiece, ... tl] => stringOfAbstractProgramPiece(abstrProgPiece) ++ ", " ++ abstrProgHelper(tl)
        }; 
    "[" ++ abstrProgHelper(abstr) ++ "]"
  }
    
/* takes in a concreteProgram, concrprog, and returns its string representation */
let rec stringOfConcreteProgram = (concrprog : concreteProgram) : string => 
  {
    let rec concrProgHelper = (concrprog2 : concreteProgram) : string =>
    {
        let stringOfConcreteProgramPiece2 = (concrpiece : concreteProgramPiece) : string => 
        {
            switch (concrpiece) {
            | Number(intx) => "Number(" ++ string_of_int(intx) ++ ")"
            | Symbol(stringx) => "Symbol(\"" ++ stringx ++ "\")"
            | List(concreteProgramPieceList) => "List([" ++ concrProgHelper(concreteProgramPieceList : concreteProgram) ++ "])"
            }
        };
        switch (concrprog2) {
        | [] => ""
        | [concrpiece] => stringOfConcreteProgramPiece2(concrpiece)
        | [concrpiece, ... tl] => stringOfConcreteProgramPiece2(concrpiece) ++ ", " ++ concrProgHelper(tl)
        };
    };
        "[" ++ concrProgHelper(concrprog) ++ "]" 
    }

/* takes in a concreteProgramPiece, concrpiece, and returns its string representation */
let stringOfConcreteProgramPiece = (concrpiece : concreteProgramPiece) : string => 
    switch (concrpiece) {
    | Number(intx) => "Number(" ++ string_of_int(intx) ++ ")"
    | Symbol(stringx) => "Symbol(" ++ stringx ++ ")"
    | List(concreteProgramPieceList) => "List(" ++ stringOfConcreteProgram(concreteProgramPieceList : concreteProgram) ++ ")"
  };

/* checkExpectExpression
  Inputs: two expressions, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectExpression = (actual: expression, expected: expression, message: string): check_result('a) =>
    if (actual == expected) {
    printGreen("ce_Success: " ++ message);
    Test_Passed;
    } else {
        printRed("ce_Fail: " ++ message);
        printRed("expected output: ");
        printRed(stringOfExpression(expected));
        printRed("actual output: ");
        printRed(stringOfExpression(actual));
        Test_Failed(Actual_Result(actual), Expected_Result(expected));
    };

/* checkExpectConcreteProgram
  Inputs: two concretePrograms, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectConcreteProgram = (actual: concreteProgram, expected: concreteProgram, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfConcreteProgram(expected));
      printRed("actual output: ");
      printRed(stringOfConcreteProgram(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* checkExpectConcreteProgramPiece
  Inputs: two concreteProgramPieces, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectConcreteProgramPiece = (actual: concreteProgramPiece, expected: concreteProgramPiece, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfConcreteProgramPiece(expected));
      printRed("actual output: ");
      printRed(stringOfConcreteProgramPiece(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* checkExpectAbstractProgram
  Inputs: two abstractPrograms, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectAbstractProgram = (actual: abstractProgram, expected: abstractProgram, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfAbstractProgram(expected));
      printRed("actual output: ");
      printRed(stringOfAbstractProgram(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* checkExpectAbstractProgramPiece
  Inputs: two abstractProgramPieces, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectAbstractProgramPiece = (actual: abstractProgramPiece, expected: abstractProgramPiece, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfAbstractProgramPiece(expected));
      printRed("actual output: ");
      printRed(stringOfAbstractProgramPiece(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* checkExpectDefinition
  Inputs: two definitions, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectDefinition = (actual: definition, expected: definition, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfDefinition(expected));
      printRed("actual output: ");
      printRed(stringOfDefinition(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* checkExpectName
  Inputs: two names, actual and expected, and a string, message
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let checkExpectName = (actual: name, expected: name, message: string): check_result('a) =>
  if (actual == expected) {
  printGreen("ce_Success: " ++ message);
  Test_Passed;
  } else {
      printRed("ce_Fail: " ++ message);
      printRed("expected output: ");
      printRed(stringOfName(expected));
      printRed("actual output: ");
      printRed(stringOfName(actual));
      Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };



/* checkWithin
  Input: Three floats, input, expected, and within. Input is a given value to be checked.
  Output: The boolean "true" if input lies within the range (expected - within) and (expected + within),
   		and "false" otherwise. */
let checkWithin =
    (input: float, expected: float, within: float): check_result(float) =>
  if (abs_float(input -. expected) <= abs_float(within)) {
    printGreen("cw_Success ");
    Test_Passed;
  } else {
    printRed("cw_Fail ");
    Test_Failed(Actual_Result(input), Expected_Result(expected));
  };


/* checkError
  Input: a one-argument procedure 'thunk' that returns the thing you want to test when it's applied to an int
          and a string of the error message of the 'failwith' clause in the procedure
   Output: a Test_Passed or Test_Failed */
let checkError = (input: unit => 'a, expect: string): check_result('a) =>
  try (
    {
      Test_Failed(Actual_Result(input()), Expected_Error(expect));
      failwith("Error did not occur");
    }
  ) {
  | Failure(err) when err == expect =>
    printGreen("checkErrorSuccess ");
    Test_Passed;
  | Failure(err) when err == "Error did not occur" =>
    printRed("Error did not occur");
    Test_Failed(Actual_Error(err), Expected_Error(expect));
  | Failure(err) =>
    printRed("checkErrorFail. Expected error: " ++ expect ++ "; Actual error: " ++ err);
    Test_Failed(Actual_Error(err), Expected_Error(expect));
  };

