﻿module Parser
open System
open System.IO
open System.Text.RegularExpressions

//Shira Pahmer 575285308
//Jessica Zemble 599453058

(*
Program that takes in a path to a directory containing any number of .vm files, reads in the files, translates
the vm commands into hack and prints them to a .asm file with the same name as the .vm file
Implements bootstrapping on files that don't have their own
*)


//reads in a path from the user and creates a .asm file with the same name as the directory
printfn "Enter path of Test directory: "
let path = Console.ReadLine()  
let split_path = path.Split('\\') //split path based on / 
let file_name = Seq.last split_path //get the name of the file
let full_path = path + "\\" + file_name + ".asm"  //create the path of the new .asm file 
let file = File.Create(full_path) //create the new .asm file
file.Close()
//opens streamwriter to write to the new .asm file
use streamWriter = new StreamWriter(full_path)


//looks in the directory for all .vm files
let directory =DirectoryInfo(path) //get directory
let files = directory.GetFiles()  //get all file paths of all files in the directory 
let sub_files = files |> Array.map (fun fileInfo -> fileInfo.Name.Split("\\")) //isolates the file names of each file from their paths
let sub_file_names = sub_files |> Array.map (fun x -> Seq.last x ) //puts all the file names into one array
let find_substring (str1:string) = if str1.Contains(".vm") then str1 else null //takes only the .vm files

let vm_files = sub_file_names |> Array.map (fun name -> find_substring name)
let filtered = vm_files |> Array.filter(String.IsNullOrEmpty >> not) //.vm files stored in filtered

let convertToInt(str:string)=
    let mutable result = 0
    Int32.TryParse(str, &result)
    result
    
let mutable counter = 0  //global counter tracking number of logical commands
let mutable callCounter = 0
let mutable functionCounter = 0

//converts "add" command to hack
let handleAdd() = 
    printfn "in add"
    streamWriter.WriteLine("//ADD\n@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M+D\nM=D\nD=A+1\n@SP\nM=D")

//converts "sub" command to hack
let handleSub() = 
    printfn "in sub"
    streamWriter.WriteLine("//SUB\n@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M-D\nM=D\nD=A+1\n@SP\nM=D")

//converts "neg" command to hack
let handleNeg() = 
    streamWriter.WriteLine("@SP\nA=M-1\nD=M\nM=-D")

//converts "not" command to hack
let handleNot() = 
    streamWriter.WriteLine("//NOT\n@SP\nA=M-1\nD=M\nM=!D")

//converts "and" command to hack
let handleAnd() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M&D\nM=D\nD=A+1\n@SP\nM=D")

//converts "or" command to hack
let handleOr() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M|D\nM=D\nD=A+1\n@SP\nM=D")

//converts "eq" command to hack
let handleEq() = 
    counter <- counter+1 //ensures uniqueness of each label 
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("//equal\n@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=D-M\n" + "@EQL" + stringCounter
    + "\n" + "D;JEQ\n" + "@NOTEQUAL" + stringCounter + "\n" + "0;JMP" + "\n" + "(EQL" + stringCounter + ")\n" + "@LOADDEQ" + stringCounter + "\n" + "D=-1\n" + "0;JMP\n" + "(NOTEQUAL" + stringCounter + ")\n" +
    "@LOADDEQ" + stringCounter + "\n" + "D=0\n" + "0;JMP\n" + "(LOADDEQ" + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")

//converts "gt" command to hack
let handleGt() =
    counter <- counter+1 //ensures uniqueness of each label 
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("//greatherthan\n@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=M-D\n" + "@GREATERTHAN_" +  stringCounter + "\n" +
    "D;JGT\n" + "@LESSTHAN_"  + stringCounter + "\n" + "0;JMP\n" + "(GREATERTHAN_" + stringCounter + ")\n" + "@LOADDGT"  + stringCounter + "\n" 
    + "D=-1\n" + "0;JMP\n" + "(LESSTHAN_" + stringCounter + ")\n" + "@LOADDGT"  + stringCounter + "\n" + "D=0\n" + "0;JMP\n" + "(LOADDGT" 
    + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")

//converts "lt" command to hack
let handleLt() = 
    counter <- counter+1 //ensures uniqueness of each label 
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("//lessthan\n@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=D-M\n" + "@GREATERTHAN"  + stringCounter + "\n" + 
    "D;JGT\n" + "@LESSTHAN"  + stringCounter + "\n" + "0;JMP\n" + "(GREATERTHAN"  + stringCounter + ")\n"+ "@LOADDLT"  + stringCounter +
    "\n" + "D=-1\n" + "0;JMP\n" + "(LESSTHAN"  + stringCounter + ")\n" +
    "@LOADDLT"  + stringCounter + "\n"+"D=0\n" + "0;JMP\n" + "(LOADDLT" + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")

//converts "push segment index" command to hack
let handlePushSegment (segment,index:string) = 
    //printfn "in push segment %s" segment
    let mutable new_seg = segment
    match segment with
    | "argument" -> new_seg <- "ARG"
    | "local" -> new_seg <- "LCL"
    | "this" -> new_seg <- "THIS"
    | "that" -> new_seg  <- "THAT"
    streamWriter.WriteLine("//PUSHSEG\n@"+new_seg+"\nD=M\n@"+index+"\nD=D+A\n@"+new_seg+"\nM=D\n@"+new_seg+"\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@"+new_seg+"\nD=M\n@"+index+"\nD=D-A\n@"+new_seg+"\nM=D")

//converts "pop segment index" command to hack
let handlePopSegment (segment, index) = 
    //printfn "in pop segment %s" segment
    let mutable new_seg = segment
    match segment with
    | "argument" -> new_seg <- "ARG"
    | "local" -> new_seg <- "LCL"
    | "this" -> new_seg <- "THIS"
    | "that" -> new_seg  <- "THAT"
    streamWriter.WriteLine("//POPSEG\n@"+new_seg+"\nD=M\n@"+index+"\nD=D+A\n@"+new_seg+"\nM=D\n@SP\nM=M-1\n@SP\nA=M\nD=M\n@"+new_seg+"\nA=M\nM=D\n@"+new_seg+"\nD=M\n@"+index+"\nD=D-A\n@"+new_seg+"\nM=D")

//converts "push constant" command to hack
let handlePushConstant index= 
    //printfn "in push constant"
    streamWriter.WriteLine("//pushconst\n@"+index+"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1")

//converts "push static index" command to hack
let handlePushStatic index fileName =
    //printfn "in push static"
    let temp_index = Convert.ToString(convertToInt(index)+16) //calculate static index in ram
    let fileNameI = fileName + "." + temp_index
    streamWriter.WriteLine("//pushstatic\n@"+fileNameI+"\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1")

//converts "pop static" command to hack
let handlePopStatic index fileName =
    //printfn "in pop static"
    let temp_index = Convert.ToString(convertToInt(index)+16) //calculate static index in ram
    let fileNameI = fileName + "." + temp_index
    streamWriter.WriteLine("//popstatic\n@SP\n"+"AM=M-1\n"+"D=M\n"+"@"+fileNameI+"\nM=D")

//converts "push temp" command to hack
let handlePushTemp segment index =
    //printfn "in push temp"
    let temp_index = Convert.ToString(convertToInt(index)+5) //calculate temp index in ram
    streamWriter.WriteLine("//pushtemp\n@"+temp_index+"\nD=M\n@SP"+"\n"+"A=M\n"+"M=D\n@SP\nM=M+1")

//converts "pop temp" command to hack
let handlePopTemp segment index =
    //printfn "in pop temp"
    let temp_index = Convert.ToString(convertToInt(index)+5) //calculate temp index in ram
    streamWriter.WriteLine("//poptemp\n@SP\nM=M-1\nA=M\nD=M\n@"+temp_index+"\nM=D")

//calculate "push pointer index" command to hack
let handlePushPointer index =
    //printfn "in push pointer"
    if index = "1" then streamWriter.WriteLine("@THAT\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1") 
    else if index = "0" then streamWriter.WriteLine("@THIS\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1")

//calculate "pop pointer index" command to hack
let handlePopPointer index =  
    //printfn "in pop pointer"
    if index = "1" then streamWriter.WriteLine("@SP\n" + "AM=M-1\n" +  "D=M\n" + "@THAT\n" + "M=D")
    else if index = "0" then streamWriter.WriteLine("@SP\n" + "AM=M-1\n" +  "D=M\n" + "@THIS\n" + "M=D")

//takes in a push command and determines which specific push function should handle it
let handlePush(segment:string, index:string, fileName:string) = 
    match segment with
    |"local" | "argument" | "this" | "that" -> handlePushSegment(segment, index)
    | "constant" ->handlePushConstant index
    | "static" -> handlePushStatic index fileName
    | "temp" -> handlePushTemp  segment index
    | "pointer" -> handlePushPointer index

//takes in a pop command and determines which specific push function should handle it
let handlePop(segment:string, index:string, fileName:string) = 
    match segment with
    |"local" | "argument" | "this" | "that" -> handlePopSegment(segment, index)
    | "static" -> handlePopStatic index fileName
    | "temp" -> handlePopTemp segment index
    | "pointer" -> handlePopPointer index
    
//writes a label command in asm
let handleLabel (label:string) = 
    //printfn "in label"
    streamWriter.WriteLine("//LABEL\n("+label+")")

    //handles an if-goto command and prints in asm
let handleIfGoTo (label:string) =
    //printfn "in ifgoto"
    streamWriter.WriteLine("//ifgoto\n@SP\nM=M-1\nA=M\nD=M\n@R13\nM=D\n@R13\nD=M\n@"+label+"\nD;JNE")

    //handles and prints regular (unconditional) goto
let handleGoTo (label:string) = 
    //printfn "in goto"
    streamWriter.WriteLine("//goto\n@"+label+"\n0;JMP")

//takes in file name, func name, nArgs and writes the call command in asm
let handleCall(fileName:string,funcName:string, nArgs:string) =
    //printfn "in call"
    let retName = fileName + "." + funcName + "$ret" + Convert.ToString(callCounter)
    callCounter <- callCounter+1
    streamWriter.WriteLine("//CALL\n@"+retName+"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nD=M\n@5\nD=D-A\n@"+nArgs+"\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D")
    let funcNameI = funcName + Convert.ToString(functionCounter)
    handleGoTo funcNameI
    streamWriter.WriteLine("("+retName+")")

//writes the bootstrap 
let callBootstrap() =
    let retName = "bootstrap" + "." + "Sys.init" + "$ret" + Convert.ToString(callCounter)
    streamWriter.WriteLine("@256\nD=A\n@SP\nM=D")  //setting stack pointer
    handleCall("bootstrap", "Sys.init", "0")
  
//this writes the function in asm
let handleFunction(funcName:string, nVars:string) =
    //printfn "in function"
    let funcNameI = funcName + Convert.ToString(functionCounter)
    streamWriter.WriteLine("//FUNCTION\n("+funcNameI+")\n@LCL\nA=M\nD=A")
    for i = 0 to convertToInt(nVars) do
        streamWriter.WriteLine("@"+ Convert.ToString(i)+"\nA=D+A\nM=0")
    streamWriter.WriteLine("@SP\nD=M\n@"+nVars+"\nD=D+A\n@SP\nM=D")

//writes return in asm
let handleReturn() =
    printfn "in return"
    streamWriter.WriteLine("//RETURN\n@LCL\nA=M\nD=A\n@R14\nM=D\n@5\nA=D-A\nD=M\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n@ARG\nD=M\n@SP\nM=D+1\n@R14\nD=M\n@1\nD=D-A\nA=D\nD=M\n@THAT\nM=D\n@R14\nD=M\n@2\nD=D-A\nA=D\nD=M\n@THIS\nM=D\n@R14\nD=M\n@3\nD=D-A\nA=D\nD=M\n@ARG\nM=D\n@R14\nD=M\n@4\nD=D-A\nA=D\nD=M\n@LCL\nM=D\n@R13\nA=M\n0;JMP")
    

//takes in the command and calls correct corresponding function
let check_command(fileName:string, command:string) =
    let split = command.Split(" ")
    let file_split = fileName.Split(".") //get file name without .vm attached
    let mutable counter = 0 //reset logical counter for each new file
    let mutable callCounter = 0

    match split[0] with
    | "add" -> handleAdd()
    | "sub" -> handleSub()
    | "neg" -> handleNeg()
    | "eq" -> handleEq()
    | "gt" -> handleGt()
    | "lt" -> handleLt()
    | "or" -> handleOr()
    | "and" -> handleAnd()
    | "not" -> handleNot()
    | "push" -> handlePush(split[1], split[2], file_split[0])
    | "pop" -> handlePop(split[1], split[2], file_split[0])
    | "label" -> handleLabel(split[1])
    | "if-goto" -> handleIfGoTo(split[1])
    | "goto" -> handleGoTo(split[1])
    | "function" -> handleFunction(split[1], split[2])
    | "call" -> handleCall(file_split[0],split[1], split[2])
    | "return" -> handleReturn()


let removeLeadingWhitespace input: string =
    // Regex to match leading whitespace
    let regex = Regex(@"^\s+", RegexOptions.Compiled)
    // Replace leading whitespace with an empty string
    regex.Replace(input, "")

//read in each command from the .vm file ignoring comments and blank lines and send to check_command()
let read_vm_file (file_name:string) =
    printfn "file name: %s" file_name
    let file_path = path + "\\" + file_name
    let words = File.ReadAllLines(file_path)
    // clean file of comments and empty lines
    let regComment = Regex(@"^\s*(//.*)?$", RegexOptions.Compiled)
    let cleaned_files = words|> Seq.filter (regComment.IsMatch >> not)
    let no_whiteSpace = cleaned_files|> Seq.map(fun clean -> removeLeadingWhitespace clean)
    printfn "cleanedfiles is %A" no_whiteSpace
    no_whiteSpace |> Seq.iter (fun item -> check_command (file_name,item))
    counter <- 0
    callCounter <- 0
    functionCounter <- 0
    streamWriter.Flush()
    printfn "End of input file: %s" file_name

    //tests if file is Sys.vm and if so calls our bootstrap
let testForBootstrap(file_name:string) = 
    if file_name.Equals("Sys.vm") then callBootstrap()

//for each file in the filtered list, check if it is Sys.vm and if we need to do our own bootstrap
filtered |> Array.map(fun name -> testForBootstrap name)
//for each file in the filtered list, send to read_vm_file()
filtered |> Array.map(fun name -> read_vm_file name)
