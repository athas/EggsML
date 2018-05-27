open System.Net
open System
open System.IO
[<EntryPoint>]
let main argv = 
    
    //tyvstjålet fra fsharpforfunandprofit
    let fetchUrl callback url =
        let request = WebRequest.Create(Uri(url)) 
        use resp = request.GetResponse() 
        use stream = resp.GetResponseStream() 
        use reader = new IO.StreamReader(stream)
        callback reader url
    let myCallback (reader:IO.StreamReader) url = 
        let html = reader.ReadToEnd()
        html      // return all the html
    //resten er OC, jeg lover det
        
        
    let baseUrl = "https://ordnet.dk/ddo/ordbog?query="
    let query = argv.[0]
    
    let url = baseUrl + query
    let doc = fetchUrl myCallback url


    let startTag = """<span class="definition">"""
    let endTag = """</span>"""    
    
    let startIndex = (doc.IndexOf(startTag)) + startTag.Length
    let lastIndex = doc.Length-1
    let cutDoc = doc.[startIndex..lastIndex]
    let endIndex = startIndex + cutDoc.IndexOf(endTag)

    printfn "%s" (query + " betyder: " + doc.[startIndex..endIndex-1] + ".")
    0
    
    
    
    
    