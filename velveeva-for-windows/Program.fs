// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

let INVALID_HANDLE_VALUE = nativeint -1
let STD_OUTPUT_HANDLE = -11
let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004

[<DllImport("Kernel32")>]
extern void* GetStdHandle(int nStdHandle)

[<DllImport("Kernel32")>]
extern bool GetConsoleMode(void* hConsoleHandle, int* lpMode)

[<DllImport("Kernel32")>]
extern bool SetConsoleMode(void* hConsoleHandle, int lpMode)

#nowarn "9"
let enableVTMode() =
    let handle = GetStdHandle(STD_OUTPUT_HANDLE)
    if handle <> INVALID_HANDLE_VALUE then
        let mode = NativePtr.stackalloc<int> 1
        if GetConsoleMode(handle, mode) then
            let value = NativePtr.read mode
            let value = value ||| ENABLE_VIRTUAL_TERMINAL_PROCESSING
            SetConsoleMode(handle, value)
        else 
            printfn "no get"
            false
     else
        printfn "no handle"
        false


let PROGNAME = "VELVEEVA"
let VELVEEVA_CLI_VERSION = "1.1.1w"

type ProcessResult = {exitCode : int; stdout: string; stderr: string}   

let execDockerCommand (dockercmd: string list) =
    enableVTMode() |> ignore

    let DOCKER_IMAGE_NAME = "drewsynan/velveeva"
    let DOCKER_WORK_DIR = "/home/project"
    let cmd = dockercmd.Head
    let args = String.concat " " dockercmd.Tail
    let docker_call = "docker run -e \"PYTHONIOENCODING=UTF-8\" -e \"LC_ALL=en_US.UTF-8\" -e \"LC_CTYPE=en_US.UTF-8\" --privileged --interactive --tty --rm --memory=4096M --memory-swap=-1 --volume \"%cd%\":{0} --workdir {0} {1} {2} {3}" // 0 = workdir, 1 = imgname, 2 = cmd, 3 = args
    let syscall = System.String.Format (docker_call, DOCKER_WORK_DIR, DOCKER_IMAGE_NAME, cmd, args)

    let psi = new System.Diagnostics.ProcessStartInfo("CMD.exe", "/C " + syscall)
    
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true

    let p = System.Diagnostics.Process.Start(psi)
    let output = new System.Text.StringBuilder()
    let error = new System.Text.StringBuilder()

    p.OutputDataReceived.Add(fun args -> System.Console.WriteLine args.Data |> ignore)
    p.ErrorDataReceived.Add(fun args -> System.Console.WriteLine args.Data |> ignore)

    p.BeginErrorReadLine()
    p.BeginOutputReadLine()
    p.WaitForExit()

    p.ExitCode // return an integer exit code


[<EntryPoint>]
let main argv =
    let argsList = List.ofSeq argv
    match argsList with
        | [] ->
            printfn "Usage"
            0
        | "version"::xs ->
            printfn "%s" VELVEEVA_CLI_VERSION
            0
        | "update"::xs ->
            printfn "Update utility image"
            0
        | "quote"::xs ->
            execDockerCommand xs
        | _ ->
            execDockerCommand (List.append ["/VELVEEVA/cli.py"] argsList)
