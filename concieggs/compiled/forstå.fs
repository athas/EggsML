open System.Net
open System
open System.IO

type clCall = {command:string; argument:string}

let HiddenExec {command=c; argument=arg} =
    let startInfo = new System.Diagnostics.ProcessStartInfo(c)
    startInfo.Arguments <- arg
    startInfo.UseShellExecute <- false

    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardOutput <- true

    use proc = System.Diagnostics.Process.Start(startInfo)
    proc.BeginErrorReadLine()
    let stdout = proc.StandardOutput.ReadToEnd();
    proc.WaitForExit()
    (proc.ExitCode,stdout,stdout) // TODO: 3. værdi burde være stderr

let fetchUrl url =
    let (ec, resp, err) = HiddenExec {command="curl"; argument=url}
    resp      // return all the html

[<EntryPoint>]
let main (argv : string[]) =
    let baseUrl = "https://ordnet.dk/ddo/ordbog?query="
    let query : string = argv.[0]

    let url = baseUrl + query
    let doc = fetchUrl url

    let startTag = """<span class="definition">"""
    let endTag = """<"""
    
    let startIndex = (doc.IndexOf(startTag)) + startTag.Length
    let lastIndex = doc.Length-1
    let cutDoc = doc.[startIndex..lastIndex]
    let endIndex = startIndex + cutDoc.IndexOf(endTag)

    printfn "%s" (query + " betyder: " + doc.[startIndex..endIndex-1] + ".")
    0
