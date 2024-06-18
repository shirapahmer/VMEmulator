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
let symbols = ['{';'}';'(';')';'~';'[';']';'+';'-';'*';',';';';'/';'<';'>';'=';'&';'|']

let handleClass(line:string, streamWriter:StreamWriter)=
    let words = line.Split(" ")
    streamWriter.WriteLine("<class>")
    streamWriter.WriteLine("<keyword> class </keyword>")
    streamWriter.WriteLine("<identifier> " + words[1] + " </identifier>")
    streamWriter.WriteLine("<symbol> { </symbol>")
    //handleVarDeclare line
    //handleSubroutineDec line
    streamWriter.WriteLine("<symbol> } </symbol>")
    streamWriter.WriteLine("</class>")

//let handleConstructor(line:string, streamWriter:StreamWriter)=
let mutable index = 0     
let get_token(file:string, index:int) =
    let mutable token = ""
    use streamReader = new StreamReader(file) 
    token <- token + Convert.ToString(streamReader.Read())
    //maybe use stream reader which reads one char at a time but will that pointer be saved each time? probably not.
    //maybe use stream reader to read the whole file into one big string and then iterate through that using the index idea
    //??????????????????????????????????????

    //keep track of where we are in the file iteration through index variable 

let handleKeyword(token:string, streamWriter:StreamWriter) =
    printfn "in keyword"
    streamWriter.WriteLine("<keyword> "+token+" </keyword>")


let handleSymbol(token:string, streamWriter:StreamWriter) =
    printfn "in symbol"
    streamWriter.WriteLine("<symbol> "+token+" </symbol>")

    
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
                if List.exists (fun elem -> elem = line[counter]) chars then  //if the first char is a letter then its a keyword or identifier and get the whole word 
                    //counter <- counter+1 THINK WE NEED TO TAKE THIS OUT
                    let mutable first = not (line[counter].Equals(" ")) //if the cur char is not a space " "
                    let mutable second = not (List.exists(fun x -> x = line[counter]) symbols) //if the cur char is not a symbol
                    while( first && second) do    //keep adding to the token if the cur_char is a letter or number or _
                        token <- token + Convert.ToString(line[counter])
                        printfn "%A" token
                        counter <- counter+1
                        first <-not (line[counter].Equals(" ")) //if the cur char is not a space " "
                        second <- not (List.exists(fun x -> x = line[counter]) symbols) //if the cur char is not a symbol

                else if List.exists (fun elem -> elem = line[counter]) nums then //if the first char is a number
                    //let mutable first = true
                    while(not (line[counter].Equals(" "))) do //if cur_char is not a space " ", keep adding to the token until the cur_char is not a number
                        integer_const <- integer_const + Convert.ToString(line[counter])
                        token <- "integerConst"
                        counter <- counter+1 
                        //first <- not (line[counter].Equals(" ")) //if cur_char is not a space " "
                
                else if (List.exists(fun x -> x = line[counter]) symbols) then
                    token <- Convert.ToString(line[counter])
                    counter <- counter+1

                //GETS INTO INFINITE LOOP HERE BECAUSE COUNTER ISN'T INCREMENTED FOR SOME REASON???????
                else if line[counter].Equals("\"") then //if the first char is a quote then collect the whole string in the quote
                    counter <- counter + 1
                    while(not (line[counter].Equals("\""))) do
                        string_const <- string_const + Convert.ToString(line[counter])
                        token <- "stringConst"
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
        
        
        
        
        (*
        match cur_token with
            | "class" -> handleClass (line, streamWriter)
            | "constructor" -> handleConstructor (line, streamWriter)
            | "function" -> handleFunction line
            | "method" -> handleMethod line
            | "field" -> handleField line
            | "static" -> handleMethod line
            | "var" -> handleVar line
            | "int" -> handleInt line
            | "char" -> handleChar line
            | "boolean" -> handleBoolean line
            | "void" -> handleVoid line
            | "true" -> handleTrue line
            | "false" -> handleFalse line
            | "null" -> handleNull line
            | "this" -> handleThis line
            | "let" -> handleLet line
            | "do" -> handleDo line
            | "if" -> handleIf line
            | "else" -> handleElse line
            | "while" -> handleWhile line
            | "return" -> handleReturn line
            | "{" -> handleLeftBracket line
            | "}" -> handleRightBracket line
            | "(" -> handleLeftParen line
            | ")" -> handleRightParen line
            | "[" -> handleLeftSq line
            | "]" -> handleRightSq line
            | "." -> handleDot line
            | "," -> handleComma line
            | ";" -> handleSemiColon line
            | "+" -> handlePlus line
            | "-" -> handleMinus line
            | "*" -> handleStar line
            | "/" -> handleSlash line
            | "&" -> handleAnd line
            | "|" -> handleOr line
            | "<" -> handleLT line
            | ">" -> handleGT line
            | "=" -> handleEQ line
            | "~" ->handleCurl line
            *)


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
    //let new_file = File.WriteAllLines((path + "\\Test\\new"+just_name+".txt"), no_whiteSpace)
    printfn "The jack file is" 
    streamWriter.WriteLine("<tokens>")
    printfn "%A" no_whiteSpace
    no_whiteSpace |> Seq.iter (fun item -> tokenize (item, streamWriter))
    streamWriter.WriteLine("</tokens>")


filtered |> Array.map(fun name -> read_jack_file name)



