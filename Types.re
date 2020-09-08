type rawProgram = string;

type concreteProgramPiece =
  | Number(int)
  | Symbol(string)
  | List(list(concreteProgramPiece));

type concreteProgram = list(concreteProgramPiece);

/* a Rackette name */
type name =
  | ID(string);

/* a Rackette expression */
type expression =
  | Num(int)
  | Bool(bool)
  | Empty
  | Name(name)
  | And(expression, expression)
  | Or(expression, expression)
  | If(expression, expression, expression)
  | Cond(list((expression, expression)))
  | Lambda(list(name), expression)
  | Let(list((name, expression)), expression)
  | Application(list(expression));

/* a Rackette definition */
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 * either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program -
 * any number of pieces */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression */
type value =
  | VNum(int)
  | VBool(bool)
  | VList(list(value))
  | VBuiltin(string, list(value) => value)
  | VClosure(list(name), expression, environment)
  and environment = (list(binding))
  and binding = (name, value);