type clCall = {command:string; argument:string}

let HiddenExec {command=c; argument=arg} =
    let startInfo = new System.Diagnostics.ProcessStartInfo(c)
    startInfo.Arguments <- arg
    startInfo.UseShellExecute <- false

    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardOutput <- true

    use proc = System.Diagnostics.Process.Start(startInfo)
    proc.WaitForExit()
    (proc.ExitCode,proc.StandardOutput.ReadToEnd(),proc.StandardError.ReadToEnd())
    
let (ec, resp, err) = HiddenExec {command="curl"; argument="icanhazip.com"}
printf "Min IPv4-adresse er: %s" resp
