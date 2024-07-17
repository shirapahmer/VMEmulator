module Tokenizer
open System
open System.IO
open System.Text.RegularExpressions

(*Below is a program that represents the Syntax Analyzer part of our VM Emulator.
The program reads a path and tokenizes the input of the .jack files (into XML token code).
The parser then creates the appropriate output based on the tokens into an XML file.
We then build the Code Generator, which turns the program into a full-scale Jack compiler
that generates VM code.
*)

//reads in a path from the user and creates a .asm file with the same name as the directory
printfn "Enter path of Jack directory: "
let path = Console.ReadLine()  
let split_path = path.Split('\\') //split path based on /
let file_name = Seq.last split_path //get the name of the file


//looks in the directory for all .vm files
let directory =DirectoryInfo(path) //get directory
let files = directory.GetFiles()  //get all file paths of all files in the directory
let sub_files = files |> Array.map (fun fileInfo -> fileInfo.Name.Split("\\")) //isolates the file names of each file from their paths
let sub_file_names = sub_files |> Array.map (fun x -> Seq.last x ) //puts all the file names into one array
let find_substring (str1:string) = if str1.Contains(".jack") then str1 else null //takes only the .jack files

let vm_files = sub_file_names |> Array.map (fun name -> find_substring name)
let filtered = vm_files |> Array.filter(String.IsNullOrEmpty >> not) //.jack files stored in filtered
   
//reference lists to be used in functions to check if
//we're up to char, num, or symbol
let chars = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
let nums = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
let symbols = ['{';'}';'(';')';'~';'[';']';'+';'-';'*';',';';';'/';'<';'>';'=';'&';'|';'.']
let term = ["+"; "-"; "*"; "/"; "&amp;"; "|"; "&lt;";"&gt;";"="]
let mutable index = 0 
let tab = "  "
let mutable label_counter = 0

//The Jack compiler represents all the program’s vars as entries in two symbol tables
let classSymbolTable = ResizeArray<string>() //array of strings to use as class symbol table
let methodSymbolTable = ResizeArray<string>() //array of strings to use as method symbol table


//method to handle instance where "class" is read in file
//will be first handle- every jack file is in 1 class
let handleClass(line:string, streamWriter:StreamWriter)=
    let words = line.Split(" ")
    streamWriter.WriteLine("<class>")
    streamWriter.WriteLine("<keyword> class </keyword>")
    streamWriter.WriteLine("<identifier> " + words[1] + " </identifier>")
    streamWriter.WriteLine("<symbol> { </symbol>")
    streamWriter.WriteLine("<symbol> } </symbol>")
    streamWriter.WriteLine("</class>")

//method to handle when keyword is read in the file
let handleKeyword(token:string, streamWriter:StreamWriter) =
    printfn "in keyword"
    streamWriter.WriteLine("<keyword> "+token+" </keyword>")

//method to handle when sybol is read in file
let handleSymbol(token:string, streamWriter:StreamWriter) =
    printfn "in symbol"
    match token with
    | "<" -> streamWriter.WriteLine("<symbol> &lt; </symbol>")
    | ">" -> streamWriter.WriteLine("<symbol> &gt; </symbol>")
    | "&" -> streamWriter.WriteLine("<symbol> &amp; </symbol>")
    | "" -> streamWriter.WriteLine("<symbol> &quot; </symbol>")
    | _ -> streamWriter.WriteLine("<symbol> "+token+" </symbol>")
   
//method to handle when integer is read
let handleIntegerConst(token:string, streamWriter:StreamWriter) =
    printfn "in integer const"
    streamWriter.WriteLine("<integerConstant> "+token+" </integerConstant>")

//method to handle when string is read
let handleStringConst(token:string, streamWriter:StreamWriter) =
    printfn "in string const"
    streamWriter.WriteLine("<stringConstant> "+token+" </stringConstant>")

//method to handle when indentifier is read
let handleIdentifier(token:string, streamWriter:StreamWriter) =
    printfn "in identifier"
    streamWriter.WriteLine("<identifier> "+token+" </identifier>")

(* method to carry out the tokenizing. Reads the file char by char
and builds a token. Then uses the above helper functions to
print the token to the output file*)
let tokenize (line:string, streamWriter:StreamWriter) =
        printfn "%A" line
        printfn "%b" (line.Equals(""))
        printfn "%b" (not (line.Equals("")))
        if (not (line.Equals(""))) then //if the line is not blank:
            printfn "current line= %A" line
            let mutable token = "" //token is empty to start
            let mutable counter = 0 //counter to index current char
            let mutable integer_const = "" //spaceholder for when number is read
            let mutable string_const = "" //for when string is read
            printfn "in while loop: counter is %i, and length is %i" counter line.Length

            while(counter <line.Length) do //iterate through each character in the line
                printfn "TOKEN IN WHILE %A" token
                while line[counter].Equals(Convert.ToChar(32)) do //skip blank space in beg of line
                    counter <- counter+1

                if List.exists (fun elem -> elem = line[counter]) chars then  //if the first char is a letter then its a keyword or identifier and get the whole word
                    let mutable first = not (line[counter].Equals("")) //if the cur char is not a space " "
                    let mutable second = not (List.exists(fun x -> x = line[counter]) symbols) //if the cur char is not a symbol
                    while( first && second) do    //keep adding to the token if the cur_char is a letter or number or _
                        token <- token + Convert.ToString(line[counter])
                        printfn "%A" token
                        counter <- counter+1
                        let c = line[counter]
                        first <-not (line[counter].Equals(Convert.ToChar(32))) //if the cur char is not a space " "
                        second <- not (List.exists(fun x -> x = line[counter]) symbols) //if the cur char is not a symbol
                else if List.exists (fun elem -> elem = line[counter]) nums then //if the first char is a number
                    while(not (line[counter].Equals(" ")) && (not (List.exists(fun x -> x = line[counter]) symbols))) do //if cur_char is not a space " ", keep adding to the token until the cur_char is not a number
                        integer_const <- integer_const + Convert.ToString(line[counter])
                        token <- "integerConst" //flag to show we're in integerConst
                        counter <- counter+1
                else if (List.exists(fun x -> x = line[counter]) symbols) then //if first char read is a symbol
                    token <- Convert.ToString(line[counter])
                    counter <- counter+1
                else if line[counter].Equals(Convert.ToChar(34)) then //if the first char is a quote then collect the whole string in the quote
                    counter <- counter + 1
                    while(not (line[counter].Equals(Convert.ToChar(34)))) do //keep going until end of quote
                        string_const <- string_const + Convert.ToString(line[counter]) //this holds actual string
                        token <- "stringConst"
                        counter <- counter+1
                    counter <- counter+1
                printfn "in while loop: counter is %d, and length is %d" counter line.Length
                printfn "FINISHED TOKEN %A" token

                   
                //once token is complete, match it and call appropriate handle function
                match token with
                | "class" | "constructor" | "function" | "method" | "field" | "static" | "var" | "int"
                | "char" | "boolean" | "void" | "true" | "false" | "null" | "this"
                | "let" | "do" | "if" | "else" | "while" | "return" -> handleKeyword(token, streamWriter)
                | "{" | "}" | "(" | ")"| "[" | "]" | "." | "," | ";" | "+" | "-" | "*" | "/"
                | "&" | "|" | "<" | ">" | "=" | "~" -> handleSymbol(token, streamWriter)
                | "integerConst" -> handleIntegerConst(integer_const,streamWriter)
                | "stringConst" -> handleStringConst(string_const,streamWriter)
                | _ -> handleIdentifier(token,streamWriter)

                token <- ""
                string_const <- ""
                integer_const <- ""

//method to calculate how many elements of the given type are already in the methodSymbolTable
let checkMethodForType(type_of:string)=
    if methodSymbolTable.Exists( fun x -> x.Split(",")[2] = type_of) then
            let amount = methodSymbolTable |> Seq.filter (fun x -> x.Split(",")[2] = type_of) |> Seq.length
            amount
    else 
        0

//method to calculate how many elements of the given type are already in the classSymbolTable
let checkClassForType(type_of:string)=
    if classSymbolTable.Exists( fun x -> x.Split(",")[2] = type_of) then
        let amount = classSymbolTable |> Seq.filter (fun x -> x.Split(",")[2] = type_of) |> Seq.length
        amount
    else
        0

//method to add an entry into the symbol table   
let addToSymbolTable(tokenized_file:Array, className:string) = 
    let mutable symbol_string = "" //will store entry
    let mutable numOfVars = 0 //to track number of vars of certain type
    let line= tokenized_file.GetValue(index).ToString().Split(" ")
    if(line[1].Equals("method")) then //add this parameter of method 
        symbol_string <- "this,"+className+",argument,0"
        methodSymbolTable.Add(symbol_string)

    else if(line[1].Equals("static")) then //if there are any static variables 
        index <- index+1
        let typeOf = tokenized_file.GetValue(index).ToString().Split(" ")[1]  //get type of the variable
        numOfVars <- numOfVars + checkClassForType("static")
        index <- index+1
        let mutable name = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get  name of the variable
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",static,"+Convert.ToString(numOfVars) //create the string entry for the variable
        classSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment index counter for vars of type static
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then //check if there are any more variables being declared now
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString().Split(" ")[1]  //get name of next variable
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",static,"+Convert.ToString(numOfVars) //create a new entry for that variable
                classSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1  //increment num of variables 
            else i <- false

    else if(line[1].Equals("field")) then //if there are any field variables
        index <- index+1
        let typeOf = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get type of variable
        numOfVars <- numOfVars + checkClassForType("this")
        index <- index+1 
        let mutable name = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get variable name
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",this,"+Convert.ToString(numOfVars) //create new entry string for this variable
        classSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment the number of variables 
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then // check if there are any more variable declared
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString().Split(" ")[1] //get their names
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",this,"+Convert.ToString(numOfVars) //create a new entry string for them
                classSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1 //increment num of field variables
            else i <- false

    else if(line[1].Equals("var")) then 
             index <- index+1 //move past var
             let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
             let typeOf =next_line[1] //get local variable type 
             numOfVars <- numOfVars + checkMethodForType("local")
             index <- index+1 
             let mutable name = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get local name
             index <- index+1
             symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",local,"+Convert.ToString(numOfVars) //create new entry string for this variable
             methodSymbolTable.Add(symbol_string) //add the new entry to the list 
             numOfVars <- numOfVars+1 //increment the number of local variables 
             let mutable i = true
             while i do
                let next_line1 = tokenized_file.GetValue(index).ToString().Split(" ")
                if next_line1[1].Equals(",") then // check if there are any more variable declared
                    index <- index+1
                    name <- tokenized_file.GetValue(index).ToString().Split(" ")[1] //get their names
                    index <- index+1
                    symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",local,"+Convert.ToString(numOfVars) //create a new entry string for them
                    methodSymbolTable.Add(symbol_string) //add the new entry to the list 
                    numOfVars <- numOfVars+1 //increment num of field variables
                else
                    i <- false

    else if(line[0].Equals("<keyword>") || line[0].Equals("<identifier>")) then //if there are any arguments
        let typeOf =line[1] //get argument type 
        numOfVars <- numOfVars + checkMethodForType("argument")
        index <- index+1 
        let mutable name = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get argument name
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",argument,"+Convert.ToString(numOfVars) //create new entry string for this variable
        methodSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment the number of variables 
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then // check if there are any more variable declared
                index <- index+2
                name <- tokenized_file.GetValue(index).ToString().Split(" ")[1] //get their names
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",argument,"+Convert.ToString(numOfVars) //create a new entry string for them
                methodSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1 //increment num of field variables
            else i <- false
    else //default case
        symbol_string <- "this,"+className+",argument,0"
        methodSymbolTable.Add(symbol_string) //add the new entry to the list 
      
//method to handle class var declaration from tokenized file
//and add appropriate entry to symbol table
let handleClassVarDec(tokenized_file:Array, streamWriter: StreamWriter) =
    addToSymbolTable(tokenized_file, "")
    index <- index+1 //move past semi colon

//method to handle parameter list and add appropriate entry to symbol table
let handleParamList(tokenized_file:Array)=
    printfn "made it to ParamList"
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[0].Equals("<keyword>") || line[0].Equals("<identifier>") then
        addToSymbolTable(tokenized_file, "")
//method that handles a variable declaration from tok file and adds 
//appropriate entry to symbol table
let handleVarDec(tokenized_file:Array) =
    printfn "made it to handleVarDec"
    addToSymbolTable(tokenized_file, "")

//method that reads in a symbol and prints out corresponding operation   
let printOp(line:string array, streamWriter:StreamWriter)=
    
    match line[1] with
    | "+" -> streamWriter.WriteLine("add")
    | "*" -> streamWriter.WriteLine("call Math.multiply 2")
    | "-" -> streamWriter.WriteLine("sub")
    | "/" -> streamWriter.WriteLine("call Math.divide 2")
    | "=" -> streamWriter.WriteLine("eq")
    | "&gt;" -> streamWriter.WriteLine("gt")
    | "&lt;" -> streamWriter.WriteLine("lt")
    | "|" -> streamWriter.WriteLine("or")
    | "&amp;" -> streamWriter.WriteLine("and")

//method that reads in symbol and prints mathching unary operation
let printUnaryOp(line: string array, streamWriter: StreamWriter) = 
    match line[1] with
    | "-" -> streamWriter.WriteLine("neg")
    | "~" -> streamWriter.WriteLine("not")

//method to print entry from method level symbol table
let printMethodEntry(entry:string, streamWriter:StreamWriter) =
    let kind = entry.Split(",")[2]
    let index = entry.Split(",")[3]
    streamWriter.WriteLine("push "+kind+" "+index)

//find and return entry from symbol table
let findEntry(line:string) =
    let mutable entry = ""
    //checks if there is entry whose varName matches line
    if methodSymbolTable.Exists( fun x -> x.Split(",")[0] = line) then
        //if so, assign it to entry
        entry <- methodSymbolTable.Find( fun x -> x.Split(",")[0] = line)
    //if not found in method, check in class symbol table
    else if classSymbolTable.Exists( fun x -> x.Split(",")[0] = line) then
        entry <- classSymbolTable.Find( fun x -> x.Split(",")[0] = line)
    //return entry
    entry

//concat elements from string array into single string
let putStringTogether(line:string array) =
    let mutable i = 2 //index for iterating through array
    let mutable full_string = line[1]
    //while not up to space and first char of line[i] is <:
    while not (line[i].Equals(Convert.ToChar(32)) && (line[i][0]).Equals("<")) && (i < Array.length(line)-1) do
        //add line[i] to full_string with space
        full_string <- full_string+" "+line[i]
        //increment index
        i<- i+1
    //return full string
    full_string

//convert char to byte representation, print approp lines
let convertAndPrintString (c: char) (streamWriter:StreamWriter)=
    streamWriter.WriteLine("push constant "+Convert.ToString(Convert.ToByte(c)))
    streamWriter.WriteLine("call String.appendChar 2")
    
//method to handle term from tokenized file
let rec handleTerm(tokenized_file:Array, streamWriter:StreamWriter, className:string)=
    printfn "made it to hadleTerm"
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if starts with (, then term = (expression)
    if line[1].Equals("(") then 
        index <- index+1 //move past (
        handleExpression(tokenized_file, streamWriter, className)
        index <- index+1 //move past )
    //term = - or ~. Will be followed by term
    else if line[1].Equals("~") || line[1].Equals("-") then
        index <- index+1 //move past - or ~
        handleTerm(tokenized_file, streamWriter, className)
        printUnaryOp(line, streamWriter)
    else
        //term = subroutineCall
        let next_line = tokenized_file.GetValue(index+1).ToString().Split(" ")
        if next_line[1].Equals(".") || next_line[1].Equals("(") then
            //will be followed by subroutineName 
            handleSubroutineCall(tokenized_file, streamWriter,className)
            //term = constant or variable var       
        else 
            //term = integerConst, stringConst, or keyWordConst
            if line[0].Equals("<integerConstant>") then
                streamWriter.WriteLine("push constant "+ line[1])
                index <- index+1 //move past const
            else if  line[0].Equals("<stringConstant>") then
                let full_string = putStringTogether(line)
                streamWriter.WriteLine("push constant " + Convert.ToString(String.length(full_string)))
                streamWriter.WriteLine("call String.new 1")
                full_string |> Seq.iter(fun char -> convertAndPrintString char streamWriter) 
                index <- index+1 //move past const

            else if line[0].Equals("<keyword>") then
                if line[1].Equals("true") then
                    streamWriter.WriteLine("push constant 0")
                    streamWriter.WriteLine("not")
                else if line[1].Equals("false") || line[1].Equals("null") then
                    streamWriter.WriteLine("push constant 0")
                else 
                    streamWriter.WriteLine("push pointer 0")
                index <- index+1 //move past const
            //term = varName
            else if line[0].Equals("<identifier>") then
                let entry = findEntry(line[1])
                printMethodEntry(entry, streamWriter)
                index <- index+1 //move past varName
                //term = varName[expression]
                let line1 = tokenized_file.GetValue(index).ToString().Split(" ")
                if line1[1].Equals("[") then
                    index <- index+1 //move past [
                    handleExpression(tokenized_file, streamWriter, className)
                    streamWriter.WriteLine("add") //add the expression value index to base index of varName array
                    streamWriter.WriteLine("pop pointer 1") //put memory location into THAT 
                    streamWriter.WriteLine("push that 0") //add THAT memory to the stack
                    index <- index+1 //move past ]

//handles expression from tokenized file       
and handleExpression(tokenized_file:Array, streamWriter:StreamWriter, className:string)=
    printfn "made it to handleExpression"
    //expression is term and then 0+ operations then term
    handleTerm(tokenized_file, streamWriter, className)
    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    let mutable i = true
    while i do //while there are more operations
        if List.exists(fun x -> x = line[1]) term then
            index <- index+1 //move past op
            handleTerm(tokenized_file, streamWriter, className)  
            printOp(line, streamWriter) //print the op after both left and right expressions
            //print op
            line <- tokenized_file.GetValue(index).ToString().Split(" ")
        else
            i <- false

//handles ExpressionList that may or may not exist when a function is called.  
//subroutineName is either the function name or the class/var name
//class name is the name of the current class
//flag = "regular" if this function was called from subroutineName(expressionList) and "dot" if from class/VarName.subroutineName(expressionList)
and handleExpressionList(tokenized_file:Array, streamWriter:StreamWriter, subroutineName: string,className:string, flag:string)=
    printfn "made it to handleExpressionList"

    let mutable name = subroutineName
    let mutable numExprs = 0 //number of expressions we have seen so far in the expressionList

    //if we got here from class/varName.subroutineName(expressionList)
    if flag.Equals("dot") then
        let funcName = tokenized_file.GetValue(index).ToString().Split(" ") //get functionName
        let entry = findEntry(subroutineName).Split(",") //check if the subroutineName is in the symbolTable
        if not (entry[0].Equals("")) then  //check if the callee is an object
            streamWriter.WriteLine("push "+entry[2]+" "+ entry[3]) //pushing the calling object to stack
            name <- entry[1]+"."+funcName[1] 
            numExprs <- numExprs+1
        else   //else the callee is a class name
            name <- subroutineName+"."+funcName[1] 
        index <- index+2 //move past subroutineName and ( 

    //else if we got here from subroutineName(expressionList)
    else if flag.Equals("regular") then
        streamWriter.WriteLine("push pointer 0")
        numExprs <- numExprs+1
        name <- className+"."+subroutineName

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if expressionList is not empty:
    if not (line[1].Equals(")")) then
        handleExpression(tokenized_file, streamWriter, className)
        numExprs <- numExprs+1
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then
                index <- index+1
                handleExpression(tokenized_file, streamWriter, className)
                numExprs <- numExprs+1
            else i <- false
        
    streamWriter.WriteLine("call "+name+" "+Convert.ToString(numExprs)) //print function call to file

//function that handles calling a subroutine
//className is the name of the current class
and handleSubroutineCall(tokenized_file:Array, streamWriter:StreamWriter, className:string)=
    printfn "made it to subroutineCall"

    let mutable subroutineName = tokenized_file.GetValue(index).ToString().Split(" ")[1]
    index <- index+1 //move to ( or .
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if subroutineCall = subroutineName(expressionList)
    if line[1].Equals("(") then
        index <- index+1 //move past (
        handleExpressionList(tokenized_file, streamWriter, subroutineName,className, "regular")
        index <- index+1     //move past )
    
    //else if subroutineCall = class/varName.subroutineName(expressionList)
    else if line[1].Equals(".") then    
        index <- index+1 //move past .
        handleExpressionList(tokenized_file, streamWriter, subroutineName,className, "dot")
        index <- index+1 //move past )

//function that matches the line to the correct statement and calls the corresponding function
let rec handleStatements(tokenized_file:Array, streamWriter:StreamWriter, className:string) =
    printfn "made it to handleStatements"

    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    while line[1].Equals("let") || line[1].Equals("if") || line[1].Equals("while") || line[1].Equals("do") || line[1].Equals("return") do
        match line[1] with
        | "let" -> handleLet(tokenized_file, streamWriter, className)
        | "if" -> handleIf(tokenized_file, streamWriter, className)
        | "while" -> handleWhile(tokenized_file, streamWriter, className)
        | "do" -> handleDo(tokenized_file, streamWriter, className)
        | "return" -> handleReturn(tokenized_file, streamWriter, className)
        line <- tokenized_file.GetValue(index).ToString().Split(" ")
   
//function that hanles Let statements
and handleLet(tokenized_file:Array, streamWriter:StreamWriter, className:string) =
    printfn "made it to handleLet"

    index <- index+1 //move past "Let"
    let varName = tokenized_file.GetValue(index).ToString().Split(" ") //get the varName
    index <- index+1 //move past varName

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if we have let varName[expression] =
    if line[1].Equals("[") then
        index <- index+1 //move past [
        let entry = findEntry(varName[1])
        printMethodEntry(entry, streamWriter) //push variable name to file
        handleExpression(tokenized_file, streamWriter, className) 
        index <- index+2 //move past ] and = to next expression
        streamWriter.WriteLine("add")  //add the variable and index to get correct memory location
        handleExpression(tokenized_file, streamWriter, className) //calculate value of right side of =

        streamWriter.WriteLine("pop temp 0") //store result of expression in temp 0
        streamWriter.WriteLine("pop pointer 1") //save address of left side of expression in THAT
        streamWriter.WriteLine("push temp 0") //put result of expression on top of the stack
        streamWriter.WriteLine("pop that 0") //pop result of expression from top of stack to THAT
        index <- index+1 //move past ; 

    //if there is no expression on the left side of the equal sign
    else 
        index <- index+1 //move past =
        let line = tokenized_file.GetValue(index+1).ToString().Split(" ") //check if expression is b[i] 
        handleExpression(tokenized_file, streamWriter, className) //calculate value of right side of =
        if line[1].Equals("[") then //if the expression was a list index then push its value to the stack 
            streamWriter.WriteLine("push that 0")

        let entry = findEntry(varName[1]).Split(",")
        streamWriter.WriteLine("pop "+entry[2]+" "+entry[3]) ///pop result into variable 
        index <- index+1 //move past ;

//function to handle If statements 
and handleIf(tokenized_file:Array, streamWriter:StreamWriter, className:string) =
    printfn "made it to handleIF"

    index <- index+2 //move past if (

    handleExpression(tokenized_file, streamWriter, className)
    let L1 = Convert.ToString(label_counter) //make label for if
    label_counter <- label_counter+1
    let L2 = Convert.ToString(label_counter) //make label for else
    label_counter <- label_counter+1
    streamWriter.WriteLine("not") 
    streamWriter.WriteLine("if-goto L"+L1)
    
    index <- index+2 //move past ) {

    handleStatements(tokenized_file, streamWriter, className)
    streamWriter.WriteLine("goto L"+L2)
    streamWriter.WriteLine("label L"+L1)
    index <- index+1
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if there is an "else" block
    if line[1].Equals("else") then
        index <- index+2
        handleStatements(tokenized_file, streamWriter, className)
        index <- index+1 //move past }
    streamWriter.WriteLine("label L"+L2)

//function to handle While statements
and handleWhile(tokenized_file:Array, streamWriter:StreamWriter, className:string) =
    printfn "made it to while"
    let L1 = Convert.ToString(label_counter) //make label for while condition
    label_counter <- label_counter+1
    let L2 = Convert.ToString(label_counter) //make label for after while statement
    label_counter <- label_counter+1

    streamWriter.WriteLine("label L"+L1)
    index <- index+2
    handleExpression(tokenized_file, streamWriter, className)
    streamWriter.WriteLine("not")
    streamWriter.WriteLine("if-goto L"+L2)
    index <- index+2 
    handleStatements(tokenized_file, streamWriter, className)
    index <- index+1
    streamWriter.WriteLine("goto L"+L1)
    streamWriter.WriteLine("label L"+L2)

//function to handle Do statements
and handleDo(tokenized_file:Array, streamWriter:StreamWriter, className) =
    printfn "made it to do"
    
    index <- index+1 //move past "do"
    handleExpression(tokenized_file, streamWriter, className)
    index<- index+1 //move past ;
    streamWriter.WriteLine("pop temp 0")

//function to handle Return statements 
and handleReturn(tokenized_file:Array, streamWriter:StreamWriter, className)=
    printfn "made it to return"

    index <- index+1 //move past "return"
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if there is a return value
    if not (line[1].Equals(";")) then
        //if returning from a constructor
        if(line[1].Equals("this")) then
            streamWriter.WriteLine("push pointer 0")
            index <- index+1 //move past "this"
        else
            handleExpression(tokenized_file, streamWriter, className)
    //if there is no return value
    else 
        streamWriter.WriteLine("push constant 0") //write this when return is void
    
    streamWriter.WriteLine("return")
    index <- index+1

//function to handle subroutineBody
let handleSubroutineBody(tokenized_file:Array, streamWriter:StreamWriter, className:string) =
    printfn "made it to subroutineBody token"

    index <- index+1
    //check if any variables are being declared
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("var") then
            handleVarDec(tokenized_file)
            index <- index+1
        else i <- false

    handleStatements(tokenized_file, streamWriter, className)
    index <- index+1

//function to count how many local variables are declared in a function to know how to declare it
let getNumLocals(tokenized_file:Array)=
        let mutable start_index = index 
        let mutable check_line = tokenized_file.GetValue(start_index).ToString().Split(" ")
        //iterate through the token file until reach { that indicates start of function
        while not (check_line[1].Equals("{")) do
            start_index <- start_index+1 
            check_line <- tokenized_file.GetValue(start_index).ToString().Split(" ")
        start_index <- start_index+1
        check_line <- tokenized_file.GetValue(start_index).ToString().Split(" ")
        let mutable local_counter = 0
        //for each var that is declared, increment the counter
        while check_line[1].Equals("var") do
            local_counter <- local_counter+1 //increment local variable counter
            start_index <- start_index+3
            check_line <- tokenized_file.GetValue(start_index).ToString().Split(" ")
            //if more than one variable is being declared in one line
            while(check_line[1].Equals(",")) do
                local_counter <- local_counter+1 
                start_index <- start_index+2
                check_line <- tokenized_file.GetValue(start_index).ToString().Split(" ")
            start_index <- start_index+1 //move past semi colon
            check_line <- tokenized_file.GetValue(start_index).ToString().Split(" ")

        local_counter //return local_counter 

//FUNCTION TO FIND OUT HOW MUCH SPACE TO ALLOCATE FOR THE OBJECT
let getNumSize(tokenized_file:Array)=
    //count how many non static fields are declared in the classSymbolTable
    let amount = classSymbolTable |> Seq.filter (fun x -> x.Split(",")[2] = "this") |> Seq.length
    amount

//function to handle subroutineDeclaration
let handleSubroutineDec(tokenized_file:Array,streamWriter:StreamWriter, className: string)=
    printfn "made it to subroutineDec token"

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //if its a method declaration, add "this" entry to methodSymbolTable
    if line[1].Equals("method") then
        addToSymbolTable(tokenized_file, className) //only do if it's a method not function or constructor
        index <- index+2 //move to subroutine name
        let line = tokenized_file.GetValue(index).ToString().Split(" ")
        let local_counter = getNumLocals(tokenized_file) 
        
        streamWriter.WriteLine("function "+ className + "." + line[1] + " "+Convert.ToString(local_counter))
        streamWriter.WriteLine("push argument 0")
        streamWriter.WriteLine("pop pointer 0")
    //if its a constructor declaration, allocate memory space for the object we want to create
    if line[1].Equals("constructor") then
        index <- index+2 //move to subroutine Name
        let local_counter = getNumLocals(tokenized_file) //number of variables declared in the function
        let size_counter = getNumSize(tokenized_file) //size of the object we need to allocate space for
        let line = tokenized_file.GetValue(index).ToString().Split(" ")

        streamWriter.WriteLine("function " + className + "." + line[1] + " "+Convert.ToString(local_counter))
        streamWriter.WriteLine("push constant "+Convert.ToString(size_counter)) 
        streamWriter.WriteLine("call Memory.alloc 1") //allocate memory for the object
        streamWriter.WriteLine("pop pointer 0")

    //if its a function declaration
    else if line[1].Equals("function") then
        index <- index+2 //move to subroutine name
        let local_counter = getNumLocals(tokenized_file)
        let line = tokenized_file.GetValue(index).ToString().Split(" ")
        streamWriter.WriteLine("function "+ className + "." + line[1]+ " "+Convert.ToString(local_counter))

    index <- index+2 //move to beginning of param list
    handleParamList(tokenized_file) //deal with the subroutine's parameters 
    index <- index+1
    handleSubroutineBody(tokenized_file, streamWriter, className) //deal with the body of the subroutine
    //when we reach this line we are done declaring the subroutine so clear the methodSymbolTable
    methodSymbolTable.Clear() 

//function that handles class
let handleClassToken(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to class token"
 
    index <- index+1
    let className = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get the class name to use for potential future methodSymbolTables 
    index <- index+2

    //check if any static or field variables are declared
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("static") || next_line[1].Equals("field") then
            
            handleClassVarDec(tokenized_file, streamWriter)
       
        else i <- false

    //check if any subroutines are declared
    i <- true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("constructor") || next_line[1].Equals("method") || next_line[1].Equals("function") then
            handleSubroutineDec(tokenized_file, streamWriter, className)
        else i <- false
    //when we get to here we are done declaring the class and can clear the classSymbolTable
    classSymbolTable.Clear()

//makes sure the first token is a class declaration and calls corresponding function
let handleKeywordToken(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to keyword token"
    let token = tokenized_file.GetValue(index).ToString().Split(" ")
    match token[1] with
    | "class" -> handleClassToken(tokenized_file, streamWriter)

////
//let parseTokens(tokenized_file:Array, streamWriter1:StreamWriter) =  
//    let words = tokenized_file.GetValue(index).ToString().Split(" ")

//    match words[0] with
//    | "<keyword>" -> handleKeywordToken(tokenized_file, streamWriter1)
//    | _ -> ()


let removeLeadingWhitespace input: string =
    // Regex to match leading whitespace
    let regex = Regex(@"^\s+", RegexOptions.Compiled)
    // Replace leading whitespace with an empty string
    regex.Replace(input, "")
   
//reads in a jack file, cleans it from comments and emtpy lines 
let read_jack_file(file_name:string) =
    let just_name = file_name.Split(".")[0]
    //creates new directory to store the created files in
    Directory.CreateDirectory(path+"\\Test") 
    let full_path = path + "\\Test\\" + just_name + "T.xml"
    let file = File.Create(full_path) //create the new .xml file
    file.Close()
    use streamWriter = new StreamWriter(full_path)

    let file_path = path + "\\" + file_name
    let words = File.ReadAllLines(file_path) //reads in all the words of the jack file
   
   //cleans the file
    let regComment = Regex(@"^\s*(//|/\*|\*|\*/).*$|(/\*.*\*/)|(/\*.*)|(^.*\*/)", RegexOptions.Compiled)
    let inlineCommentPattern = Regex(@"(.*?)\s*//.*$", RegexOptions.Compiled)
    let clean_midline_comments line =
        let matches = inlineCommentPattern.Match(line)
        if matches.Success then
            Some(matches.Groups.[1].Value.Trim())
        else
            Some(line.Trim())

    let cleaned_files = words|> Seq.filter (regComment.IsMatch >> not) |> Seq.choose clean_midline_comments 

    let no_whiteSpace = cleaned_files|> Seq.map(fun clean -> Parser.removeLeadingWhitespace clean)
    
    //tokenizes the whole jack file into a new file
    no_whiteSpace |> Seq.iter (fun item -> tokenize (item, streamWriter))
    streamWriter.Close()

    //create a .vm file corresponding to the jack file 
    let file_path2 = path + "\\Test\\"+just_name+".vm"
    use streamWriter1 = new StreamWriter(file_path2)
    let tokenized_file = File.ReadAllLines(full_path)
    //translate the tokens in vm commands
    handleKeywordToken(tokenized_file, streamWriter1) 
    streamWriter1.Close()
    index <- 0

//tokenize, and translate to vm commands, every jack file from the directory
filtered |> Array.map(fun name -> read_jack_file name)