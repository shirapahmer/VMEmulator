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


let handleClassVarDec(tokenized_file:Array, streamWriter: StreamWriter, indents: int) = 
    printfn "made it to classVarDec"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<classVarDec>")
    let start_index = index
    while index < start_index+3 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals(",") then 
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1

        else i <- false

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    streamWriter.WriteLine((String.replicate (indents-1) tab) + "</classVarDec>")

let handleParamList(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to PAramList"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<parameterList>")
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[0].Equals("<keyword>") || line[0].Equals("<identifier>") then 
        let start_index = index
        while index < start_index+2 do
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then 
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1

            else i <- false

    streamWriter.WriteLine(String.replicate (indents-1) tab + "</parameterList>")

let handleVarDec(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to handleVarDec"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<varDec>")
    let start_index = index
    while index < start_index+3 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals(",") then 
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1

        else i <- false

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    streamWriter.WriteLine((String.replicate (indents-1) tab) + "</varDec>")
   
let rec handleTerm(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to hadleTerm"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<term>")

    let line = tokenized_file.GetValue(index).ToString().Split(" ")  //ERROR HERE
    if line[1].Equals("(") then
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
        handleExpression(tokenized_file, streamWriter, indents+1)
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    else if line[1].Equals("~") || line[1].Equals("-") then
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
        handleTerm(tokenized_file, streamWriter, indents+1)
    else 
        let next_line = tokenized_file.GetValue(index+1).ToString().Split(" ")
        if next_line[1].Equals(".") then
            handleSubroutineCall(tokenized_file, streamWriter,indents)
        else 
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
            let line = tokenized_file.GetValue(index).ToString().Split(" ")
            if line[1].Equals("[") then
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1
                handleExpression(tokenized_file, streamWriter, indents+1)
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1

        
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</term>")

and handleExpression(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to handleExpression"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<expression>")

    handleTerm(tokenized_file, streamWriter, indents+1)

    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    let mutable i = true
    while i do
        if List.exists(fun x -> x = line[1]) term then
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
            handleTerm(tokenized_file, streamWriter, indents+1)
            line <- tokenized_file.GetValue(index).ToString().Split(" ")
        else
            i <- false
    
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</expression>")

and handleExpressionList(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to handleExpressionList"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<expressionList>")
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    
    if not (line[1].Equals(")")) then 
        handleExpression(tokenized_file, streamWriter, indents+1)
        let mutable i = true
        while i do
            let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
            if next_line[1].Equals(",") then 
                streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
                index <- index+1
                handleExpression(tokenized_file, streamWriter, indents+1)

            else i <- false

    streamWriter.WriteLine((String.replicate (indents-1) tab) + "</expressionList>")

and handleSubroutineCall(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to subroutineCall"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("(") then 
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
        handleExpressionList(tokenized_file, streamWriter, indents+1)
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    else if line[1].Equals(".") then
        let start_index = index
        while index < start_index+3 do
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
        handleExpressionList(tokenized_file, streamWriter, indents+1)
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

let rec handleStatements(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to handleStatements"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<statements>")
    let mutable line = tokenized_file.GetValue(index).ToString().Split(" ")
    while line[1].Equals("let") || line[1].Equals("if") || line[1].Equals("while") || line[1].Equals("do") || line[1].Equals("return") do
        match line[1] with 
        | "let" -> handleLet(tokenized_file, streamWriter, indents+1)
        | "if" -> handleIf(tokenized_file, streamWriter, indents+1)
        | "while" -> handleWhile(tokenized_file, streamWriter, indents+1)
        | "do" -> handleDo(tokenized_file, streamWriter, indents+1)
        | "return" -> handleReturn(tokenized_file, streamWriter, indents+1)
        line <- tokenized_file.GetValue(index).ToString().Split(" ")

    
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</statements>")

    
and handleLet(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to handleLet"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<letStatement>")
    let start_index = index
    while index < start_index+2 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("[") then
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
        handleExpression(tokenized_file, streamWriter, indents+1)
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString()) //print the = sign
    index <- index+1
    handleExpression(tokenized_file, streamWriter, indents+1)
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString()) //print the ; sign
    index <- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</letStatement>")

and handleIf(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to handleIF"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<ifStatement>")
    let start_index = index
    while index < start_index+2 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    handleExpression(tokenized_file, streamWriter, indents+1)
    let start_index = index
    while index < start_index+2 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    handleStatements(tokenized_file, streamWriter, indents+1)
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if line[1].Equals("else") then 
        let start_index = index
        while index < start_index+2 do
            streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
            index <- index+1
        handleStatements(tokenized_file, streamWriter, indents+1)
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</ifStatement>")

and handleWhile(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to while"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<whileStatement>")
    let start_index = index
    while index < start_index+2 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    handleExpression(tokenized_file, streamWriter, indents+1)
    let start_index = index

    while index < start_index+2 do
       streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
       index <- index+1
 
    handleStatements(tokenized_file, streamWriter, indents+1)
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</whileStatement>")

and handleDo(tokenized_file:Array, streamWriter:StreamWriter, indents:int) = 
    printfn "made it to do"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<doStatement>")
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1

    handleSubroutineCall(tokenized_file, streamWriter, indents+1)

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index<- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</doStatement>")

and handleReturn(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to return"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<returnStatement>")
    
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1

    let line = tokenized_file.GetValue(index).ToString().Split(" ")
    if not (line[1].Equals(";")) then
        handleExpression(tokenized_file, streamWriter, indents+1)

    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</returnStatement>")

let handleSubroutineBody(tokenized_file:Array, streamWriter:StreamWriter,indents:int) = 
    printfn "made it to subroutineBody token"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<subroutineBody>")
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString()) 
    index <- index+1
    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("var") then
            handleVarDec(tokenized_file,streamWriter,indents+1)
           // index <- index+1
        else i <- false

    handleStatements(tokenized_file, streamWriter, indents+1)
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString()) 
    index <- index+1
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</subroutineBody>")


let handleSubroutineDec(tokenized_file:Array, streamWriter:StreamWriter, indents:int)=
    printfn "made it to subroutineDec token"

    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<subroutineDec>")
    let start_index = index
    while index < start_index+4 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1
    handleParamList(tokenized_file, streamWriter,indents+1)
    streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
    index <- index+1
    handleSubroutineBody(tokenized_file, streamWriter, indents+1)
    streamWriter.WriteLine(String.replicate (indents-1) tab + "</subroutineDec>")


let handleClassToken(tokenized_file:Array, streamWriter:StreamWriter, indents:int) =
    printfn "made it to class token"
    let spaces = String.replicate indents tab
    streamWriter.WriteLine(String.replicate (indents-1) tab + "<class>")
    let start_index = index
    while index < start_index+3 do
        streamWriter.WriteLine(spaces + tokenized_file.GetValue(index).ToString())
        index <- index+1

    let mutable i = true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("static") || next_line[1].Equals("field") then
            handleClassVarDec(tokenized_file,streamWriter,indents+1)
        
        else i <- false
    i <- true
    while i do
        let next_line = tokenized_file.GetValue(index).ToString().Split(" ")
        if next_line[1].Equals("constructor") || next_line[1].Equals("method") || next_line[1].Equals("function") then
            handleSubroutineDec(tokenized_file,streamWriter,indents+1)
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