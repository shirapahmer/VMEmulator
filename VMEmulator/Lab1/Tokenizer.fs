module Tokenizer
open System
open System.IO
open System.Text.RegularExpressions

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
   
let chars = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
let nums = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
let symbols = ['{';'}';'(';')';'~';'[';']';'+';'-';'*';',';';';'/';'<';'>';'=';'&';'|';'.']
let term = ["+"; "-"; "*"; "/"; "&amp;"; "|"; "&lt;";"&gt;";"="]
let mutable index = 0
let tab = "  "

let classSymbolTable = ResizeArray<string>()
let methodSymbolTable = ResizeArray<string>()



let handleClass(line:string, streamWriter:StreamWriter)=
    let words = line.Split(" ")
    streamWriter.WriteLine("<class>")
    streamWriter.WriteLine("<keyword> class </keyword>")
    streamWriter.WriteLine("<identifier> " + words[1] + " </identifier>")
    streamWriter.WriteLine("<symbol> { </symbol>")
    streamWriter.WriteLine("<symbol> } </symbol>")
    streamWriter.WriteLine("</class>")

let handleKeyword(token:string, streamWriter:StreamWriter) =
    printfn "in keyword"
    streamWriter.WriteLine("<keyword> "+token+" </keyword>")


let handleSymbol(token:string, streamWriter:StreamWriter) =
    printfn "in symbol"
    match token with
    | "<" -> streamWriter.WriteLine("<symbol> &lt; </symbol>")
    | ">" -> streamWriter.WriteLine("<symbol> &gt; </symbol>")
    | "&" -> streamWriter.WriteLine("<symbol> &amp; </symbol>")
    | "" -> streamWriter.WriteLine("<symbol> &quot; </symbol>")
    | _ -> streamWriter.WriteLine("<symbol> "+token+" </symbol>")
   

let handleIntegerConst(token:string, streamWriter:StreamWriter) =
    printfn "in integer const"
    streamWriter.WriteLine("<integerConstant> "+token+" </integerConstant>")


let handleStringConst(token:string, streamWriter:StreamWriter) =
    printfn "in string const"
    streamWriter.WriteLine("<stringConstant> "+token+" </stringConstant>")


let handleIdentifier(token:string, streamWriter:StreamWriter) =
    printfn "in identifier"
    streamWriter.WriteLine("<identifier> "+token+" </identifier>")

let tokenize (line:string, streamWriter:StreamWriter) =
        printfn "%A" line
        printfn "%b" (line.Equals(""))
        printfn "%b" (not (line.Equals("")))
        if (not (line.Equals(""))) then
            printfn "current line= %A" line
            let mutable token = ""
            let mutable counter = 0
            let mutable integer_const = ""
            let mutable string_const = ""
            printfn "in while loop: counter is %i, and length is %i" counter line.Length

            while(counter <line.Length) do //iterate through each character in the line
                printfn "TOKEN IN WHILE %A" token
                while line[counter].Equals(Convert.ToChar(32)) do
                    counter <- counter+1

                if List.exists (fun elem -> elem = line[counter]) chars then  //if the first char is a letter then its a keyword or identifier and get the whole word
                    //counter <- counter+1 THINK WE NEED TO TAKE THIS OUT
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
                        token <- "integerConst"
                        counter <- counter+1
               
                else if (List.exists(fun x -> x = line[counter]) symbols) then
                    token <- Convert.ToString(line[counter])
                    counter <- counter+1

                else if line[counter].Equals(Convert.ToChar(34)) then //if the first char is a quote then collect the whole string in the quote
                    counter <- counter + 1
                    while(not (line[counter].Equals(Convert.ToChar(34)))) do
                        string_const <- string_const + Convert.ToString(line[counter])
                        token <- "stringConst"
                        counter <- counter+1
                    counter <- counter+1
                printfn "in while loop: counter is %d, and length is %d" counter line.Length
                printfn "FINISHED TOKEN %A" token

                   

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

let addToSymbolTable(tokenized_file:Array, className:string) = 
    let mutable symbol_string = "" 
    let mutable numOfVars = 0
    let line= tokenized_file.GetValue(index).ToString()
    if(line[1].Equals("static")) then //if there are any static variables 
        index <- index+1
        let typeOf = tokenized_file.GetValue(index).ToString()[1]  //get type of the variable
        index <- index+1
        let mutable name = tokenized_file.GetValue(index).ToString()[1] //get  name of the variable
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",static,"+Convert.ToString(numOfVars) //create the string entry for the variable
        classSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment index counter for vars of type static
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then //check if there are any more variables being declared now
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString()[1]  //get name of next variable
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",static,"+Convert.ToString(numOfVars) //create a new entry for that variable
                classSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1  //increment num of variables 
            else i <- false

    else if(line[1].Equals("field")) then //if there are any field variables
        index <- index+1
        let typeOf = tokenized_file.GetValue(index).ToString()[1] //get type of variable
        index <- index+1 
        let mutable name = tokenized_file.GetValue(index).ToString()[1] //get variable name
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",this,"+Convert.ToString(numOfVars) //create new entry string for this variable
        classSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment the number of variables 
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then // check if there are any more variable declared
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString()[1] //get their names
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",this"+Convert.ToString(numOfVars) //create a new entry string for them
                classSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1 //increment num of field variables
            else i <- false

    else if(line[0].Equals("<keyword>") || line[0].Equals("<identifier>")) then //if there are any arguments
        let typeOf =line[1] //get argument type 
        index <- index+1 
        let mutable name = tokenized_file.GetValue(index).ToString()[1] //get argument name
        index <- index+1
        symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",argument,"+Convert.ToString(numOfVars) //create new entry string for this variable
        classSymbolTable.Add(symbol_string) //add the new entry to the list 
        numOfVars <- numOfVars+1 //increment the number of variables 
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then // check if there are any more variable declared
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString()[1] //get their names
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",argument,"+Convert.ToString(numOfVars) //create a new entry string for them
                methodSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1 //increment num of field variables
            else i <- false

    else if(line[1].Equals("var")) then 
         index <- index+1
         let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
         let typeOf =line[1] //get local variable type 
         index <- index+1 
         let mutable name = tokenized_file.GetValue(index).ToString()[1] //get local name
         index <- index+1
         symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",local,"+Convert.ToString(numOfVars) //create new entry string for this variable
         classSymbolTable.Add(symbol_string) //add the new entry to the list 
         numOfVars <- numOfVars+1 //increment the number of local variables 
         let mutable i = true
         while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then // check if there are any more variable declared
                index <- index+1
                name <- tokenized_file.GetValue(index).ToString()[1] //get their names
                index <- index+1
                symbol_string <- Convert.ToString(name)+","+Convert.ToString(typeOf)+",local,"+Convert.ToString(numOfVars) //create a new entry string for them
                methodSymbolTable.Add(symbol_string) //add the new entry to the list 
                numOfVars <- numOfVars+1 //increment num of field variables
    else 
        symbol_string <- "this,"+className+",argument,0"
        methodSymbolTable.Add(symbol_string) //add the new entry to the list 

    //NEED TO ADD else if equals argument and else if equals local
      

let handleClassVarDec(tokenized_file:Array, streamWriter: StreamWriter) =
    addToSymbolTable(tokenized_file, "")
    
let handleParamList(tokenized_file:Array)=
    printfn "made it to ParamList"

    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<parameterList>")
   // addToSymbolTable(tokenized_file) THINK THIS IS A MISTAKE DON'T NEED THIS LINE
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[0].Equals("<keyword>") || line[0].Equals("<identifier>") then
        //let start_index = index
        //while index < start_index+2 do
            //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
          //  index <- index+1
        addToSymbolTable(tokenized_file, "")
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then
                addToSymbolTable(tokenized_file, "")
                //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                //index <- index+1
                //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                //index <- index+1
                //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                //index <- index+1

            else i <- false

    //streamWriter.WriteLine(String.replicate (indents-1) tab + "</parameterList>")

let handleVarDec(tokenized_file:Array) =
    printfn "made it to handleVarDec"

    //let spaces = String.replicate indents tab
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<varDec>")
    //let start_index = index
    //while index < start_index+3 do
        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    addToSymbolTable(tokenized_file, "")
    //let mutable i = true
    //while i do
    //    let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
    //    if next_line[1].Equals(",") then
    //        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    //        index <- index+1
    //        addToSymbolTable(tokenized_file, "")
    //        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    //        //index <- index+1 //NOT SURE IF WE NEED THIS

    //    else i <- false

    //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    //index <- index+1
    //streamWriter.WriteLine((String.replicate (indents-1) tab) + "</varDec>")
   
let printOp(line:string array, streamWriter:StreamWriter)=
    
    match line[1] with
    | "+" -> streamWriter.WriteLine("add")
    | "-" -> streamWriter.WriteLine("sub")
    | "=" -> streamWriter.WriteLine("eq")
    | "&gt" -> streamWriter.WriteLine("gt")
    | "&lt" -> streamWriter.WriteLine("lt")
    | "|" -> streamWriter.WriteLine("or")
    | "&amp" -> streamWriter.WriteLine("and")

let printUnaryOp(tokenized_file: Array, streamWriter: StreamWriter) = 
    let line = tokenized_file.GetValue(index).ToString().Split(" ")  //ERROR HERE

    match line[1] with
    | "-" -> streamWriter.WriteLine("neg")
    | "~" -> streamWriter.WriteLine("not")

let printMethodEntry(entry:string, streamWriter:StreamWriter) =
    let kind = entry.Split(",")[2]
    let index = entry.Split(",")[3]
    streamWriter.WriteLine("push "+kind+" "+index)

let findEntry(line:string array) =
    let mutable entry = ""
    if methodSymbolTable.Exists( fun x -> x.Split(",")[0] = line[1]) then
        entry <- methodSymbolTable.Find( fun x -> x.Split(",")[0] = line[1])
    else if classSymbolTable.Exists( fun x -> x.Split(",")[0] = line[1]) then
        entry <- classSymbolTable.Find( fun x -> x.Split(",")[0] = line[1])

    entry

let rec handleTerm(tokenized_file:Array, streamWriter:StreamWriter)=
    printfn "made it to hadleTerm"

    //let spaces = String.replicate indents tab
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<term>")

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    //Term = (expression)
    if line[1].Equals("(") then 
        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
        handleExpression(tokenized_file, streamWriter)
        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    //term = - or ~ 
    else if line[1].Equals("~") || line[1].Equals("-") then
        printUnaryOp(tokenized_file, streamWriter)
        handleTerm(tokenized_file, streamWriter)
    else
        //term = subroutineCall
        let next_line = tokenized_file.GetValue(index+1).ToString().Split(" ")
        if next_line[1].Equals(".") || line[1].Equals("(") then 
            handleSubroutineCall(tokenized_file, streamWriter)
            //term = constant or variable var            
          
        else 
            //term = integerConst, stringConst, or keyWordConst
            if line[0].Equals("<integerConstant>") || line[0].Equals("<stringConstant>") || line[0].Equals("<keyWordConstant>") then
                streamWriter.WriteLine("push constant "+ line[1])
                index <- index+1
            //term = varName
            else if line[0].Equals("<identifier>") then
                let entry = findEntry(line)
                printMethodEntry(entry, streamWriter)

                //term = varName[expression]
                let line1 = tokenized_file.GetValue(index).ToString().Split(" ")
                if line1[1].Equals("[") then
                    index <- index+1
                    handleExpression(tokenized_file, streamWriter)
                    streamWriter.WriteLine("add") //add the expression value index to base index of varName array
                    streamWriter.WriteLine("pop pointer 1") //put memory location into THAT 
                    index <- index+1

       
and handleExpression(tokenized_file:Array, streamWriter:StreamWriter)=
    printfn "made it to handleExpression"
    //let spaces = String.replicate indents tab
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<expression>")

    handleTerm(tokenized_file, streamWriter)
    index <- index+1
    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    let mutable i = true
    while i do
        if List.exists(fun x -> x = line[1]) term then
        
            //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
            handleTerm(tokenized_file, streamWriter)  
            index <- index+1
            printOp(line, streamWriter) //print the op after both left and right expressions
            //print op
            line <- tokenized_file.GetValue(index).ToString().Split(" ")
        else
            i <- false
   
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "</expression>")
    
and handleExpressionList(tokenized_file:Array, streamWriter:StreamWriter, subroutineName: string)=
    printfn "made it to handleExpressionList"

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    let mutable numExprs = 0
    if not (line[1].Equals(")")) then
        handleExpression(tokenized_file, streamWriter)
        numExprs <- numExprs+1
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then
                index <- index+1
                handleExpression(tokenized_file, streamWriter)
                numExprs <- numExprs+1
            else i <- false
    streamWriter.WriteLine("call "+subroutineName+" "+Convert.ToString(numExprs)) //print function call to file

and handleSubroutineCall(tokenized_file:Array, streamWriter:StreamWriter)=
    printfn "made it to subroutineCall"

    let mutable subroutineName = tokenized_file.GetValue(index).ToString().Split(" ")[1]
    index <- index+1
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("(") then
        index <- index+1
        handleExpressionList(tokenized_file, streamWriter, subroutineName)
        index <- index+1    

    else if line[1].Equals(".") then    
        index <- index+1
        let line = tokenized_file.GetValue(index).ToString().Split(" ")[1]
        subroutineName <- subroutineName+"."+line
        index <- index+1
        handleExpressionList(tokenized_file, streamWriter, subroutineName)
        index <- index+1

let rec handleStatements(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to handleStatements"

    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    while line[1].Equals("let") || line[1].Equals("if") || line[1].Equals("while") || line[1].Equals("do") || line[1].Equals("return") do
        match line[1] with
        | "let" -> handleLet(tokenized_file, streamWriter)
        | "if" -> handleIf(tokenized_file, streamWriter)
        | "while" -> handleWhile(tokenized_file, streamWriter)
        | "do" -> handleDo(tokenized_file, streamWriter)
        | "return" -> handleReturn(tokenized_file, streamWriter)
        line <- tokenized_file.GetValue(index).ToString().Split(" ")

   
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</statements>")

   
and handleLet(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to handleLet"

    index <- index+1 //move past "Let"
    let varName = tokenized_file.GetValue(index).ToString().Split(" ") //get the varName
    index <- index+1 //move past varName

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("[") then
        index <- index+1 //move past [
        let entry = findEntry(varName)
        printMethodEntry(entry, streamWriter)
        handleExpression(tokenized_file, streamWriter)
        index <- index+3
        streamWriter.WriteLine("add")  //add the variable and index to get correct mem location
        streamWriter.WriteLine("pop pointer 1") //save that address in THAT

        handleExpression(tokenized_file, streamWriter) //calculate value of right side of =
        streamWriter.WriteLine("pop that 0") //put that into correct array location
        index <- index+1 //move past ;

    else 
        index <- index+2 //move past =
        let line = tokenized_file.GetValue(index+1).ToString().Split(" ")
        handleExpression(tokenized_file, streamWriter) //calculate value of right side of =
        if line[1].Equals("[") then //if the expression was a list index then push its value to the stack 
            streamWriter.WriteLine("push that 0")

        let entry = findEntry(varName).Split(",")
        streamWriter.WriteLine("pop "+entry[2]+" "+entry[3]) ///pop result into variable 
        index <- index+1 //move past ;

and handleIf(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to handleIF"

    index <- index+2 //move past if (

    handleExpression(tokenized_file, streamWriter)
    streamWriter.WriteLine("not")
    streamWriter.WriteLine("if-goto L1")
    
    index <- index+2 //move past ) {

    handleStatements(tokenized_file, streamWriter)
    streamWriter.WriteLine("goto L2")
    streamWriter.WriteLine("label L1")
    index <- index+1
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("else") then
        index <- index+2
        handleStatements(tokenized_file, streamWriter)
        streamWriter.WriteLine("label L2")
    
    index <- index+1 //move past }


and handleWhile(tokenized_file:Array, streamWriter:StreamWriter) =
    printfn "made it to while"
    streamWriter.WriteLine("label L1")
    index <- index+2
    handleExpression(tokenized_file, streamWriter)
    streamWriter.WriteLine("not")
    streamWriter.WriteLine("if-goto L2")
    index <- index+2 
    handleStatements(tokenized_file, streamWriter)
    streamWriter.WriteLine("goto L1")
    streamWriter.WriteLine("label L2")

and handleDo(tokenized_file:Array, streamWriter:StreamWriter, indents:int) =
    printfn "made it to do"
    
    index <- index+1 //move past "do"
    handleExpression(tokenized_file, streamWriter)
    index<- index+1 //move past ;
    streamWriter.WriteLine("pop temp 0")

and handleReturn(tokenized_file:Array, streamWriter:StreamWriter)=
    printfn "made it to return"

    index <- index+1 //move past "return"
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if not (line[1].Equals(";")) then
        handleExpression(tokenized_file, streamWriter)
    else 
        streamWriter.WriteLine("push constant 0") //write this when return is void
    
    streamWriter.WriteLine("return")
    index <- index+1

let handleSubroutineBody(tokenized_file:Array) =
    printfn "made it to subroutineBody token"

    //let spaces = String.replicate indents tab
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<subroutineBody>")
    //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("var") then
            handleVarDec(tokenized_file)
           // index <- index+1
        else i <- false

    handleStatements(tokenized_file:Array)
    //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "</subroutineBody>")


let handleSubroutineDec(tokenized_file:Array, className: string)=
    printfn "made it to subroutineDec token"
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("method") then   
        addToSymbolTable(tokenized_file, className) //only do if it's a method not function or constructor
    if line[1].Equals("constructor") then
        index <- index+2
        let line = tokenized_file.GetValue(index).ToString().Split(" ")
        streamWriter.WriteLine("function " + className + "." + line[1]) //how do we know what number
        streamWriter.WriteLine("push constant ") //WHAT SHOULD NUMBER BE?
        streamWriter.WriteLine("call Memory.alloc ") //ALSO HERE?
        streamWriter.WriteLine("pop pointer 0")
    //let spaces = String.replicate indents tab
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "<subroutineDec>")
    let start_index = index
    while index < start_index+4 do
        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    handleParamList(tokenized_file)
    //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    handleSubroutineBody(tokenized_file)
    //streamWriter.WriteLine(String.replicate (indents-1) tab + "</subroutineDec>")


let handleClassToken(tokenized_file:Array, streamWriter:StreamWriter, indents:int) =
    printfn "made it to class token"
    let spaces = String.replicate indents tab
   // streamWriter.WriteLine(String.replicate (indents-1) tab + "<class>")
    index <- index+1
    let className = tokenized_file.GetValue(index).ToString().Split(" ")[1] //get the class name to use for potential future methodSymbolTables 
    let start_index = index
    while index < start_index+2 do
        //streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("static") || next_line[1].Equals("field") then
            
            handleClassVarDec(tokenized_file,streamWriter)
       
        else i <- false
    i <- true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("constructor") || next_line[1].Equals("method") || next_line[1].Equals("function") then
            handleSubroutineDec(tokenized_file,className)
        else i <- false

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    streamWriter.WriteLine((String.replicate (indents-1) tab) + "</class>")


   
let handleKeywordToken(tokenized_file:Array, streamWriter:StreamWriter, indents:int) =
    printfn "made it to keyword token"
    let token = tokenized_file.GetValue(index).ToString().Split(" ")
    printfn "token %A" token
    match token[1] with
    | "class" -> handleClassToken(tokenized_file, streamWriter, indents)
    | "let" -> handleLet(tokenized_file, streamWriter, indents)
    | "if" -> handleIf(tokenized_file, streamWriter, indents)
    //| "else" -> handleElse(tokenized_file, streamWriter, indents)
    | "while" -> handleWhile(tokenized_file, streamWriter, indents)
    | "do" -> handleDo(tokenized_file, streamWriter, indents)
    | "return" -> handleReturn(tokenized_file, streamWriter, indents)
          
let parseTokens(tokenized_file:Array, streamWriter1:StreamWriter) =  
    let words = tokenized_file.GetValue(index).ToString().Split(" ")

    match words[0] with
    | "<keyword>" -> handleKeywordToken(tokenized_file, streamWriter1, 1)
    | _ -> ()


let removeLeadingWhitespace input: string =
    // Regex to match leading whitespace
    let regex = Regex(@"^\s+", RegexOptions.Compiled)
    // Replace leading whitespace with an empty string
    regex.Replace(input, "")
   

let read_jack_file(file_name:string) =
    let just_name = file_name.Split(".")[0]
    Directory.CreateDirectory(path+"\\Test")
    let full_path = path + "\\Test\\" + just_name + "T.xml"
    let file = File.Create(full_path) //create the new .xml file
    file.Close()
    use streamWriter = new StreamWriter(full_path)

    let file_path = path + "\\" + file_name
    let words = File.ReadAllLines(file_path)
   
    let regComment = Regex(@"^\s*(//|/\*|\*|\*/).*$", RegexOptions.Compiled)
    let inlineCommentPattern = Regex(@"(.*?)\s*//.*$", RegexOptions.Compiled)
    let clean_midline_comments line =
        let matches = inlineCommentPattern.Match(line)
        if matches.Success then
            Some(matches.Groups.[1].Value.Trim())
        else
            Some(line.Trim())

    let cleaned_files = words|> Seq.filter (regComment.IsMatch >> not) |> Seq.choose clean_midline_comments

    let no_whiteSpace = cleaned_files|> Seq.map(fun clean -> Parser.removeLeadingWhitespace clean)
    printfn "The jack file is"
    printfn "%A" no_whiteSpace

    no_whiteSpace |> Seq.iter (fun item -> tokenize (item, streamWriter)) //read all jack files and translate into token file
    streamWriter.Close()

    let file_path2 = path + "\\Test\\"+just_name+".xml"
    use streamWriter1 = new StreamWriter(file_path2)
    let tokenized_file = File.ReadAllLines(full_path)
    parseTokens(tokenized_file, streamWriter1)
    streamWriter1.Close()
    index <- 0

filtered |> Array.map(fun name -> read_jack_file name)