open System

type Expression = 
    | Const of float
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Pow of Expression * Expression
    | Sqrt of Expression
    | Mod of Expression * Expression
    | UserInput of string
    | Var of string
    | Call of string * List<Expression>    

type Condition = 
    | Equals of Expression * Expression
    | And of Condition * Condition
    | Or of Condition * Condition
    | Not of Condition

type Statement = 
    | Noop
    | PrintStr of string      
    | PrintExp of Expression 
    | Branch of Condition * List<Statement> * List<Statement>
    | Repeat of int * List<Statement>
    | Set of string * Expression
    | While of Condition * List<Statement>
    | Function of string * List<string> * List<Statement>
    | Return of Expression


(*
type NamedEntity =
    | Variable of float
    | Function of List<string> * List<Statement>
*)

(*~~ The function declarations are a jumble of parentheses and angle brackets, so let me explain them here:
~~~~ 
~~~~ evaluate(), interpret(), and interpretProgram() all return a tuple with the Variables State (varState) and the Functions State (funcState).
~~~~ testCondition() returns a single boolean.
~~~~ The states are each maps, varState being <string, float>, and funcState being <string,(List<string> * List<Statement>)>.  We can pass around the two states as a single tuple for ease. 
~~~~ evaluate() expects to be passed a single Expression and a tuple of the two states.  
~~~~ interpret() expects to be passed a single statement and the tuple of the two states.
~~~~ Following this pattern, interpretProgram() expects a statement list and the tuple of the states
~~~~
~~~~ In the end, the final state should be a tuple of the Variables State and the Functions State in the form of two maps. 
~~*)
   
// evaluate converts an Expression object into the floating-point number it represents.
// Takes an expression and the tuple of our program states.
// Returns a float
let rec evaluate expr ((varState: Map<string, float>), (funcState: Map<string, (List<string> * List<Statement>)>)) =
    match expr with
    // I feel like the math expressions are pretty self explanatory.  They do math things.
    | Const c -> c
    | Neg expr -> -1.0 * (evaluate expr (varState, funcState))
    | Add (expr1, expr2) -> (evaluate expr1 (varState, funcState)) + (evaluate expr2 (varState, funcState))
    | Sub (expr1, expr2) -> (evaluate expr1 (varState, funcState)) - (evaluate expr2 (varState, funcState))
    | Mul (expr1, expr2) -> (evaluate expr1 (varState, funcState)) * (evaluate expr2 (varState, funcState))
    | Div (expr1, expr2) -> (evaluate expr1 (varState, funcState)) / (evaluate expr2 (varState, funcState))
    | Pow (expr1, expr2) -> (evaluate expr1 (varState, funcState)) ** (evaluate expr2 (varState, funcState))
    | Sqrt expr -> sqrt (evaluate expr (varState, funcState))
    | Mod (expr1, expr2) -> (evaluate expr1 (varState, funcState)) % (evaluate expr2 (varState, funcState))
    // The console asks the user for a float.  "request" represents the text the console will ask of the user.
    | UserInput request -> 
        printf "%s" request
        let mutable UserInput = Console.ReadLine()|> float
        UserInput
    // Gets the value of a variable in varState.  Map.find throws an exception if the variable does not exist.  
    | Var variable -> Map.find variable varState
    // Calls a function.  First creates a list of the floats that result from evaluating the arguments (an expression list).
    // Next, uses List.zip to smash together the results list with the names of the arguments as defined in the function definition.
    // Then, that list gets converted into a map, that represents the local initial state for the function.
    // Finally, we use interpretProgram to execute the body of the function as defined, resulting in a new state local to the function.
    // We return the variable labeled "return" in this state, representing the final desired value to come out of the function.
    | Call (funcName, arguments) ->
        let argResults = List.map (fun x -> evaluate x (varState, funcState)) arguments 
        let argNames = fst (Map.find funcName funcState)
        let argValues = List.zip argNames argResults
        let initialFunctionState = Map.ofList argValues

        let finalFunctionState = fst(interpretProgram (snd(Map.find funcName funcState)) (initialFunctionState, funcState))
        Map.find "return" finalFunctionState

    // This isn't a requirement (as written at least) for the assignment, but it might be nice to get this to work with our new expressions.
    // Essentially, this is meant to make a print statement pretty for each given expression.  
and formatExpression expr =
    match expr with
    | Const c -> sprintf "%f" c
    | Neg expr -> sprintf "-(%s)" (formatExpression expr)
    | Add (expr1, expr2) -> sprintf "(%s + %s)" (formatExpression expr1) (formatExpression expr2)
    | Sub (expr1, expr2) -> sprintf "(%s - %s)" (formatExpression expr1) (formatExpression expr2)
    | Mul (expr1, expr2) -> sprintf "(%s * %s)" (formatExpression expr1) (formatExpression expr2)
    | Div (expr1, expr2) -> sprintf "(%s / %s)" (formatExpression expr1) (formatExpression expr2)
    | Pow (expr1, expr2) -> sprintf "(%s ^ %s)" (formatExpression expr1) (formatExpression expr2)
    | Sqrt expr -> sprintf "SQRT(%s)" (formatExpression expr)
    | Mod (expr1, expr2) -> sprintf "(%s mod %s)" (formatExpression expr1) (formatExpression expr2)
    | UserInput (request) -> sprintf "%s\n" request
    | Var (variableName) -> sprintf "Variable %s" variableName
    | Call (funcName, statements) -> sprintf "Function: %s\nBody: %A" funcName statements

    // Tests two expressions with a conditional to return a Boolean.  Again, these are pretty self explanatory.
    // Simply evaluates the expressions and applies the conditional to get either a true or false value.
    // Takes one condition and a tuple with our two states.
    // Returns a boolean
and testCondition condition ((varState: Map<string, float>), (funcState: Map<string, (List<string> * List<Statement>)>)) =
    match condition with
    | Equals (expr1, expr2) -> 
        if(evaluate expr1 (varState, funcState) = evaluate expr2 (varState, funcState)) then
            true
        else
            false
    | And (cond1, cond2) ->
        if(testCondition cond1 (varState, funcState) && testCondition cond2 (varState, funcState)) then
            true
        else  
            false
    | Or (cond1, cond2) ->
        if(testCondition cond1 (varState, funcState) || testCondition cond2 (varState, funcState)) then
            true
        else  
            false
    | Not (cond1) ->
        not (testCondition cond1 (varState, funcState))

    // Interprets a statement, changing our program state accordingly.  
    // Takes one statement and a tuple with our two states.
    // Returns a tuple representing the updated states 
and interpret statement ((varState: Map<string, float>), (funcState: Map<string, (List<string> * List<Statement>)>)) = 
    match statement with
    // Does nothing.
    | Noop -> (varState, funcState)
    // Prints a string to the console output.
    | PrintStr string -> 
        printf "%s" string
        (varState, funcState)
    // Evaluates an expression then prints the resulting float to the console.
    | PrintExp expression -> 
        evaluate expression (varState, funcState) |> printf "%f"
        (varState, funcState)
    // Tests a condition.  Then, if true, interprets the first list of statements.  If false, interprets the second list of statements.  
    | Branch (cond, statements1, statements2) ->
        if testCondition cond (varState, funcState) then
            interpretProgram statements1 (varState, funcState)
        else   
            interpretProgram statements2 (varState, funcState)
    // Repeats a list of statements "iterations" times.
    // If iterations != 0, will interpret the list of statements, then recursively call Repeat() again with "iterations" - 1 passed into the iterations parameter.
    // If iterations = 0, returns the tuple with our two states.  
    | Repeat (iterations, statements) -> 
        if (iterations = 0) then
            (varState, funcState)
        else 
            interpretProgram statements (varState, funcState)
            interpret (Repeat (iterations - 1, statements)) (varState, funcState)
    // Evaluates an expression, then sets the variable specified by variable_name with that value.  If that variable does not exist, it gets added to the state.  
    | Set (variable_name, expression) ->
        (varState |> Map.add variable_name (evaluate expression (varState, funcState)), funcState) 
    // Interprets a list of statements while a conditional is true.  
    // If true, interprets the list of statements, and passes the resulting state into a recursive call of While().
    // If false, finally returns the tuple with our two states.  
    | While (cond, statements) -> 
        if(testCondition cond (varState, funcState)) then
            interpretProgram statements (varState, funcState) |> interpret (While(cond, statements))
        else
            (varState, funcState)
    
    // Defines a function to be added into our program state (specifically into funcState)
    | Function (name, arguments, body) ->
        (varState, (funcState |> Map.add name (arguments, body)))
    // Specifies something as a return value.  The variable "return" will be hardcoded to be the expected variable.
    // Evaluated an expression then sets the variable "return" to that float value.
    | Return (expr) ->
        (varState |> Map.add "return" (evaluate expr (varState, funcState)), funcState)

    // Interprets a list of statements, then returns a tuple with our two states.
    // If the list is empty, returns the tuple with our two states.
    // Otherwise, interpret the statement at Head, and pass that state tuple into a recursive interpretProgram() call using the tail of the statement list.
and interpretProgram statements ((varState: Map<string, float>), (funcState: Map<string, (List<string> * List<Statement>)>)) = 
    match statements with
    | [] -> (varState, funcState)
    | _ ->  interpret statements.Head (varState, funcState) |> interpretProgram statements.Tail
           

//  TEST CASES BELOW //

(*
//let state = Map<string, int>
//Testing Evaluate Expressions
let state = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let expr = Add (Const 1.0, Var "x")
evaluate expr state |> printfn "%f"

//Statements
let initialState1 = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let statement1 = PrintExp (Add (Const 1.0, Var "x"))
interpret statement1 initialState1 |> printfn "%O"

// Defining Variables 
let initialState2 = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let statement2 = Set ("x", Add (Const 1.0, Var "x"))
interpret statement2 initialState2 |> printfn "%O"

//Running program
let initialState3 = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
let programResult =
  initialState3
  |> interpret (Set ("x", Add (Const 1.0, Var "x")))
  |> interpret (PrintExp (Var "x"))
  |> interpret (Set ("x", Const 0))
  |> interpret (PrintExp (Var "x"))
  |> printfn "%O"
  *)

(*
let myProgram = [
    Set ("i", Const 0);
    While (Not (Equals (Const 10, Var "i")), [
      PrintExp (Var "i");
      Set ("i", (Add (Var "i", Const 1)))
    ];)
    PrintExp (Mul (Const 2, Var "i"))
]*)

(*
let myProgram = [
  Function ("squared", ["a"], [
    Return (Mul (Var "a", Var "a"))
  ]);
  Set ("x", UserInput ("Please enter a float:"));
  Set ("result", Call ("squared", [Var "x"]));
  PrintExp (Var "x");
  PrintStr "squared is equal to ";
  PrintExp (Var "result")
]

//Begin a program with two empty states; one each for variables and functions respectively
let testVarState = Map.ofList[]
let testFuncState = Map.ofList[]
interpretProgram myProgram (testVarState, testFuncState) |> printfn "%O"
*)

//FINAL CHALLENGE//
// This is the program we were instructed to write in our new language.//
let finalChallenge = [
    
    //GCD
    Function ("gcd", ["a"; "b"], [
      While (Not(Equals(Var "b", Const 0)), [
          Set("t", Var "b");
          Set ("b", Mod(Var "a", Var "b"));
          Set("a", Var "t");
          ]);
      Return(Var "a") //Outside the while loop
    ]);

    //GCDREC
    Function ("gcdRec", ["i"; "j"],[
       Branch(Equals(Var "j", Const 0.0), [
           Return (Var "i")
           ], [
           Return(Call ("gcdRec",[Var "j"; Mod(Var"i", Var"j")]))
           ])     
       ]);

    Set("x", UserInput "Enter first number for GCD test: ")
    Set("y", UserInput "Enter second number for GCD test: ")
    Set("result", Call("gcd",[Var "x"; Var "y"]))
    PrintExp(Var "x");
    PrintStr " and ";
    PrintExp(Var "y");
    PrintStr " is ";
    PrintExp(Var "result");
    PrintStr("\n")

    Set("c", UserInput "Enter first number for recursive GCD test: ")
    Set("d", UserInput "Enter second number for recursive GCD test: ")
    Set("result2", Call("gcdRec", [Var "c"; Var "d"]))
    PrintExp(Var "c");
    PrintStr " and ";
    PrintExp(Var "d");
    PrintStr " is ";
    PrintExp(Var "result2");
    PrintStr("\n");
]

let testVarState = Map.ofList[]
let testFuncState = Map.ofList[]
interpretProgram finalChallenge (testVarState, testFuncState)