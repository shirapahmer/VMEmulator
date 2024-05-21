open System
open System.IO
open System.Text.RegularExpressions

//read in file name or path from user
//open that file for reading
//for each line of the file, we want to extract what type of command it is
//based on the type of command, we need to write to the file a different output
//if it is an arithmetic command, do what that specific arithmetic command does- it does not have any arguments
//if it is a push or a pop, they need a push and a pop
//for each of these commands, write the correct output to the asm (figure out what those are)


//reads in a path from the user and creates an .asm file with the same name as the directory
printfn "Enter path: "
let path = Console.ReadLine()
let split_path = path.Split('\\')
let file_name = Seq.last split_path
let full_path = path + "\\" + file_name + ".asm"
let file = File.Create(full_path)
file.Close()
//opens streamwriter to write to the new .asm file
use streamWriter = new StreamWriter(full_path)

//checks int the directory for all .vm files
let directory =DirectoryInfo(path)
let files = directory.GetFiles()
let sub_files = files |> Array.map (fun fileInfo -> fileInfo.Name.Split("\\"))
let sub_file_names = sub_files |> Array.map (fun x -> Seq.last x )

let find_substring (str1:string) = if str1.Contains(".vm") then str1 else null

let vm_files = sub_file_names |> Array.map (fun name -> find_substring name)
let filtered = vm_files |> Array.filter(String.IsNullOrEmpty >> not) //.vm files stored in filtered

let convertToInt(str:string)=
    let mutable result = 0
    Int32.TryParse(str, &result)
    result
    
let mutable counter = 0

let handleAdd() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M+D\nM=D\nD=A+1\n@SP\nM=D")

let handleSub() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M-D\nM=D\nD=A+1\n@SP\nM=D")

let handleNeg() = 
    streamWriter.WriteLine("@SP\nA=M-1\nD=M\nM=-D")

let handleNot() = 
    streamWriter.WriteLine("@SP\nA=M-1\nD=M\nM=!D")

let handleAnd() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M&D\nM=D\nD=A+1\n@SP\nM=D")

let handleOr() = 
    streamWriter.WriteLine("@SP\nD=M-1\nA=D\nD=M\nA=A-1\nD=M|D\nM=D\nD=A+1\n@SP\nM=D")

let handleEq() = 
    counter <- counter+1
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=D-M\n" + "@EQL" + stringCounter
    + "\n" + "D;JEQ\n" + "@NOTEQUAL" + stringCounter + "\n" + "0;JMP" + "\n" + "(EQL" + stringCounter + ")\n" + "@LOADDEQ" + stringCounter + "\n" + "D=-1\n" + "0;JMP\n" + "(NOTEQUAL" + stringCounter + ")\n" +
    "@LOADDEQ" + stringCounter + "\n" + "D=0\n" + "0;JMP\n" + "(LOADDEQ" + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")


let handleGt() =
    counter <- counter+1
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=M-D\n" + "@GREATERTHAN_" +  stringCounter + "\n" +
    "D;JGT\n" + "@LESSTHAN_"  + stringCounter + "\n" + "0;JMP\n" + "(GREATERTHAN_" + stringCounter + ")\n" + "@LOADDGT"  + stringCounter + "\n" 
    + "D=-1\n" + "0;JMP\n" + "(LESSTHAN_" + stringCounter + ")\n" + "@LOADDGT"  + stringCounter + "\n" + "D=0\n" + "0;JMP\n" + "(LOADDGT" 
    + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")

let handleLt() = 
    counter <- counter+1
    let stringCounter = Convert.ToString(counter)
    streamWriter.WriteLine("@SP\n" + "AM=M-1\n" + "D=M\n" + "@SP\n" + "AM=M-1\n" + "D=D-M\n" + "@GREATERTHAN"  + stringCounter + "\n" + 
    "D;JGT\n" + "@LESSTHAN"  + stringCounter + "\n" + "0;JMP\n" + "(GREATERTHAN"  + stringCounter + ")\n"+ "@LOADDLT"  + stringCounter +
    "\n" + "D=-1\n" + "0;JMP\n" + "(LESSTHAN"  + stringCounter + ")\n" +
    "@LOADDLT"  + stringCounter + "\n"+"D=0\n" + "0;JMP\n" + "(LOADDLT" + stringCounter + ")\n" + "@SP\n" + "A=M\n"+"M=D\n"+"@SP\n" + "M=M+1")

let handlePushSegment (segment,index:string) = 
    let mutable new_seg = segment
    match segment with
    | "argument" -> new_seg <- "ARG"
    | "local" -> new_seg <- "LCL"
    | "this" -> new_seg <- "THIS"
    | "that" -> new_seg  <- "THAT"
    streamWriter.WriteLine("@"+new_seg+"\nD=M\n@"+index+"\nD=D+A\n@"+new_seg+"\nM=D\n@"+new_seg+"\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1")

let handlePopSegment (segment, index) = 
    let mutable new_seg = segment
    match segment with
    | "argument" -> new_seg <- "ARG"
    | "local" -> new_seg <- "LCL"
    | "this" -> new_seg <- "THIS"
    | "that" -> new_seg  <- "THAT"
    streamWriter.WriteLine("@"+new_seg+"\nD=M\n@"+index+"\nD=D+A\n@"+new_seg+"\nM=D\n@SP\nM=M-1\n@SP\nA=M\nD=M\n@"+new_seg+"\nA=M\nM=D\n@"+new_seg+"\nD=M\n@"+index+"\nD=D-A\n@"+new_seg+"\nM=D")

let handlePushConstant index= 
    streamWriter.WriteLine("@"+index+"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1")

let handlePushStatic index fileName =
    let fileNameI = fileName + "." + index
    streamWriter.WriteLine("@"+fileNameI+"\nD=M\n@"+index+"\nD=D+A\n@"+fileNameI+"\nM=D\n@"+fileNameI+"\nA=M\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1")

let handlePopStatic index fileName =
    let fileNameI = fileName + "." + index
    streamWriter.WriteLine("@"+fileNameI+"\nD=M\n@"+index+"\nD=D+A\n@"+fileNameI+"\nM=D\n@SP\nM=M-1\n@SP\nA=M\nD=M\n@"+fileNameI+"\nA=M\nM=D\n@"+fileNameI+"\nD=M\n@"+index+"\nD=D-A\n@"+fileNameI+"\nM=D")

let handlePushTemp segment index =
    let temp_index = Convert.ToString(convertToInt(index)+5)
    streamWriter.WriteLine("@"+temp_index+"\nD=M\n@SP"+"\nA=M\n"+"A=M\n"+"M=D\n@SP\nM=M+1")

let handlePopTemp segment index =
    let temp_index = Convert.ToString(convertToInt(index)+5)
    streamWriter.WriteLine("@SP\nAM=M-1\nD=M\n@"+temp_index+"\nM=D")

let handlePushPointer index = 
    if index = "1" then streamWriter.WriteLine("@THAT\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1") 
    else if index = "0" then streamWriter.WriteLine("@THIS\n" + "D=M\n" + "@SP\n" + "A=M\n" + "M=D\n" + "@SP\n" + "M=M+1")

let handlePopPointer index =  
    if index = "1" then streamWriter.WriteLine("@SP\n" + "AM=M-1\n" +  "D=M\n" + "@THAT\n" + "M=D")
    else if index = "0" then streamWriter.WriteLine("@SP\n" + "AM=M-1\n" +  "D=M\n" + "@THIS\n" + "M=D")


let handlePush(segment:string, index:string, fileName:string) = 
    match segment with
    |"local" | "argument" | "this" | "that" -> handlePushSegment(segment, index)
    | "constant" ->handlePushConstant index
    | "static" -> handlePushStatic index fileName
    | "temp" -> handlePushTemp  segment index
    | "pointer" -> handlePushPointer index

let handlePop(segment:string, index:string, fileName:string) = 
    match segment with
    |"local" | "argument" | "this" | "that" -> handlePopSegment(segment, index)
    | "static" -> handlePopStatic index fileName
    | "temp" -> handlePopTemp segment index
    | "pointer" -> handlePopPointer index
    

let check_command(fileName:string, command:string) =
    let split = command.Split(" ")
    let mutable counter = 0
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
    | "push" -> handlePush(split[1], split[2], fileName)
    | "pop" -> handlePop(split[1], split[2], fileName)
   


let read_vm_file (file_name:string) =
    let file_path = path + "\\" + file_name
    let words = File.ReadAllLines(file_path)
    // regex: beginning of line, followed by optional whitespace, 
    // followed by comment chars.
    let reComment = Regex(@"^\s*(//.*)?$", RegexOptions.Compiled)
    let cleaned_files = words|> Seq.filter (reComment.IsMatch >> not)
    printfn "%A" cleaned_files
    cleaned_files |> Seq.iter (fun item -> check_command (file_name,item))
    counter <- 0
    streamWriter.Flush()
    printfn "End of input file: %s" file_name

filtered |> Array.map(fun name -> read_vm_file name)
