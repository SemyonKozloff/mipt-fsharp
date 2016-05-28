module lab3

#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"

open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

let email = "semkozloff@gmail.com"

type Language = RU | EN

let detLang (pageAdress:string) = 
    let bases = HtmlDocument.Load(pageAdress)
    
    let explode (s:string) =
        [for c in s -> c]
    let pageText = bases.Descendants["html"] 
                |> Seq.map (fun (x:HtmlNode) -> x.InnerText ()) 
                |> String.concat "" 
                |> explode

    let ruCounter = List.map (fun x -> if (x >= 'а' && x <= 'я') then 1 else 0) pageText |> List.sum 
    let engCounter = List.map (fun x -> if (x >= 'a' && x <= 'z') then 1 else 0) pageText |> List.sum

    if ruCounter > engCounter then RU else EN


let lab3 (mainPageAdress:string) =
  let bases = HtmlDocument.Load(mainPageAdress)
  let ruCounter, engCounter = bases.Descendants["div"]
                            |> Seq.filter (fun (x:HtmlNode) -> x.HasClass("mw-body-content"))
                            |> Seq.collect (fun (x:HtmlNode) -> x.Descendants["a"])
                            |> Seq.choose (fun x -> x.TryGetAttribute("href") |> Option.map (fun a -> a.Value()))
                            |> Seq.filter (fun (x:String) -> (x.[0] <> '/' && x.[0] <> '#')) // отфильтровываем относительные ссылки
                            |> Seq.map (fun x -> detLang x)
                            |> Seq.fold (fun (ruAcc, engAcc) lang -> if lang = RU then (ruAcc + 1, engAcc) else (ruAcc, engAcc + 1)) (0, 0)
  ruCounter, engCounter 

let LISP_ADRESS = "https://ru.wikipedia.org/wiki/%D0%9B%D0%B8%D1%81%D0%BF"
lab3 LISP_ADRESS

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", (lab3 LISP_ADRESS).ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()