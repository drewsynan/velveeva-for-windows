let PROGNAME = "VELVEEVA"
let VELVEEVA_CLI_VERSION = "1.1.1w"
let DOCKER_IMAGE_NAME = "drewsynan/velveeva"
let DOCKER_WORK_DIR = "/home/project"

type Flag = ON | OFF

type TTYComp<'a> = 
    | TTYSuccess 
    | TTYFailure of 'a
    with
        static member Zero () = TTYSuccess
        static member Combine(a,b) =
            match a with
            | TTYSuccess -> b
            | TTYFailure _ -> a
        static member (<+>)(x:TTYComp<'a>,y:TTYComp<'a>) =
            TTYComp<'a>.Combine(x,y)
        member m.Bind f =
            match m with
            | TTYFailure(a) -> f a
            | TTYSuccess -> TTYSuccess
        static member Return(x) = TTYSuccess
        member m.Apply f =
            match m with
            | TTYFailure(a) -> TTYFailure (f a)
            | TTYSuccess -> TTYSuccess

type Either<'a,'b> =
    | Left of 'a
    | Right of 'b
    with
    static member _pure (x:'c) =
        Either<'a,'c>.Right x

let inline (<*>) (f_wrap:Either<'a,('T1 -> 'T2)>) (x_wrap:Either<'a,'T1>) =
        match f_wrap with
        | Right f -> match x_wrap with
                     | Right x -> Right (f x)
                     | Left e -> Left e
        | Left e -> Left e

let inline (<!>) f (v:Either<'a,'b>) =
    Either<'a,'b>._pure f <*> v

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<DllImport("Kernel32")>]
extern void* GetStdHandle(int nStdHandle)

[<DllImport("Kernel32")>]
extern bool GetConsoleMode(void* hConsoleHandle, int* lpMode)

[<DllImport("Kernel32")>]
extern bool SetConsoleMode(void* hConsoleHandle, int lpMode)

#nowarn "9" // pointers are evil
let setConsoleMode handleConst modeConst flag =
    let op = match flag with
             | ON -> (fun (x:int) (y:int) -> x ||| y)
             | OFF -> (fun (x:int) (y:int) -> x ^^^ y)
    
    let getHandle const =
        match GetStdHandle(const) with
        | (nativeint -1) -> Left "Could not get console handle"
        | handle -> Right handle

    let getMode handle =
        let ptr = NativePtr.stackalloc<int> 1

        match GetConsoleMode(handle, ptr) with
        | true -> Right (NativePtr.read ptr)
        | false -> Left "Could not get console mode"

    let setMode handle mode =
        match SetConsoleMode(handle, mode) with
        | true -> Right ()
        | false -> Left "Could not set console mode" 

    let handle = getHandle handleConst
    let newMode = (op modeConst) <!> (getMode <!> handle)
    let success = setMode <!> handle <*> newMode

    match success with
    | Right _ -> TTYSuccess
    | Left e -> TTYFailure e

let setVTMode flag =
    // Console Handles
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms683231(v=vs.85).aspx

    // Console Modes
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms686033(v=vs.85).aspx

    let STD_OUTPUT_HANDLE = -11
    let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004

    let STD_INPUT_HANDLE = -10
    let ENABLE_VIRTUAL_TERMINAL_INPUT = 0x0200
    
    let outputEnabled = setConsoleMode STD_OUTPUT_HANDLE ENABLE_VIRTUAL_TERMINAL_PROCESSING flag
    let inputEnabled = setConsoleMode STD_INPUT_HANDLE ENABLE_VIRTUAL_TERMINAL_INPUT flag
    outputEnabled <+> inputEnabled

let enableVTMode () = setVTMode ON
let disableVTMode () = setVTMode OFF

let multibyte (stream:System.IO.Stream) =
    let mutable buff = Array.create<byte> 4 0uy

    // utf-8 multibyte sequence lengths, from first byte
    // from https://en.wikipedia.org/wiki/UTF-8#Description
    // this will correctly parse the utf-8 characters and marshall into .NET's
    // utf-16 char represenataion. Windows console doesn't play nice with utf at all though
    // so maybe some day all the emoji will come through, just not today.

    let ONE_BYTE     = byte 0b00000000
    let TWO_BYTES    = byte 0b11000000
    let THREE_BYTES  = byte 0b11100000
    let FOUR_BYTES   = byte 0b11110000
    let MAX          = byte 0b11110111

    async {
        let! _ = stream.ReadAsync(buff, 0, 1) |> Async.AwaitTask
        let firstByte = Array.get buff 0
        
        let getAdditional first =
            if first > MAX then 0 // invalid utf-8 byte
            elif first >= FOUR_BYTES then 3
            elif first >= THREE_BYTES then 2
            elif first >= TWO_BYTES then 1
            else 0
        
        let additionalBytes = getAdditional firstByte
        let! rest = stream.ReadAsync(buff, 0, additionalBytes) |> Async.AwaitTask
        let utfChar = Array.concat [ [| firstByte |]; [| for i in 0..(additionalBytes - 1) do yield byte (Array.get buff i) |] ]

        //if Array.length utfChar > 1 then printfn "%A" utfChar

        return System.Text.Encoding.UTF8.GetString(utfChar)
    }

let execString s =
    let psi = new System.Diagnostics.ProcessStartInfo("CMD.exe", "/C " + s)
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.RedirectStandardInput <- true
    psi.CreateNoWindow <- true

    let proc = System.Diagnostics.Process.Start(psi)
 

    let rec printStream (stream:System.IO.StreamReader) =
        async {
            
            (*
            let mutable c = Array.create 4 (char "\n")
            let! result = stream.ReadAsync(c, 0, 1) |> Async.AwaitTask
            System.Console.Write(Array.get c 0)
            if not stream.EndOfStream then return! printStream 
            *)
            
            
            let! c = multibyte stream.BaseStream
            System.Console.Write(c)
            if stream.BaseStream.CanRead then return! printStream stream
        }
    
    let rec putChars (stream:System.IO.StreamWriter) =
        async {
            let currentChar = char (System.Console.Read ())
            stream.Write(currentChar)
            if not (stream.BaseStream = null) then return! putChars stream
        }
    
    
    do printStream proc.StandardOutput |> Async.StartAsTask |> ignore
    do putChars proc.StandardInput  |> Async.StartAsTask |> ignore

    proc.WaitForExit()
    let stdErr = proc.StandardError.ReadToEnd()
    System.Console.Error.WriteLine(stdErr)
    let code = proc.ExitCode
    proc.Close()

    match code with
    | 0 -> TTYSuccess
    | _ -> TTYFailure stdErr

type FakeTTYBuilder() =
    member this.Bind(m:string, f:(TTYComp<string> -> TTYComp<string>)) = // let! step = "cmd string"       
        let result = execString m
        match result with
        | TTYSuccess -> f TTYSuccess
        | TTYFailure x -> TTYFailure x
    member this.Bind(m:TTYComp<string>, f:(TTYComp<string> -> TTYComp<string>)) = // let! step = shell { ... }
        match m with
        | TTYSuccess -> f TTYSuccess
        | TTYFailure x -> TTYFailure x
    member this.Bind(m:string, f:(unit -> TTYComp<'a>)) = // do! "cmd string"
        execString m |> ignore
        f()
    member this.Zero() = TTYComp<string>.Zero
    member this.Return(a) = 
        TTYComp<string>.Return a
    member this.ReturnFrom(m:TTYComp<'a>) = m
    member this.Delay(f) = f
    member this.Run(f) =
        match enableVTMode () with
        | TTYSuccess -> 
            System.Console.OutputEncoding <- System.Text.Encoding.Unicode
            let result = f ()
            
            let disabled = disableVTMode () // needs to execute regardless of f() succeeding
            System.Console.OutputEncoding <- System.Text.Encoding.Default

            result <+> disabled
        | TTYFailure x -> TTYFailure x

let shell = new FakeTTYBuilder()

let execDockerCommand (dockercmd: string list) =
    let cmd = dockercmd.Head
    let args = String.concat " " dockercmd.Tail
    let docker_call = "docker run -e \"PYTHONIOENCODING=UTF-8\" -e \"LC_ALL=en_US.UTF-8\" -e \"LC_CTYPE=en_US.UTF-8\" --privileged --interactive --rm --memory=4096M --memory-swap=-1 --volume \"%cd%\":{0} --workdir {0} {1} {2} {3}" // 0 = workdir, 1 = imgname, 2 = cmd, 3 = args
    let syscall = System.String.Format (docker_call, DOCKER_WORK_DIR, DOCKER_IMAGE_NAME, cmd, args)
    
    shell {
        let! status = syscall
        return! status
    }

let updateDockerImage () =
    shell {
        let! status = "docker pull " + DOCKER_IMAGE_NAME
        return! status
    }

let execVeevaCommand cmdlist =
    execDockerCommand (List.append ["/VELVEEVA/cli.py"] cmdlist)

[<EntryPoint>]
let main argv =
    let argsList = List.ofSeq argv
    let result = match argsList with
        | [] ->
            execVeevaCommand ["help"]
        | "version"::xs ->
            printfn "velveeva-cli version:"
            printfn "%s" VELVEEVA_CLI_VERSION
            printfn "Utility image SHA:"
            shell {
                do! "docker inspect --format=\"{{.RepoDigests}}\" " + DOCKER_IMAGE_NAME
            }
        | "update"::xs ->
            printfn "Update utility image"
            updateDockerImage ()
        | "bash"::xs ->
            execDockerCommand ["bash"]
        | "quote"::xs ->
            execDockerCommand xs
        | _ ->
            execVeevaCommand argsList

    match result with
    | TTYSuccess -> 0
    | TTYFailure f -> -1
