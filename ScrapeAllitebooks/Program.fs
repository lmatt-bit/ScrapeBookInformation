// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSharp.Data
open System
open System.Threading

type Book = { 
    Title: string; 
    mutable Author: string; 
    mutable Isbn: string; 
    mutable Year: string; 
    mutable Pages: string; 
    mutable Language: string; 
    mutable FSize: string; 
    mutable FileFormat: string; 
    mutable Category: string; 
    Link: string;}

let parseBook (article:HtmlNode) = 
    let title = article.Descendants["h1"] |> Seq.head |> fun x -> x.InnerText()
    let link = article.Descendants["footer"] |> Seq.head |> fun x -> x.Descendants["a"] |> Seq.head |> fun x -> x.Attribute("href").Value()
    let book = {
            Title = title;
            Author = null;
            Isbn = null;
            Year = null;
            Pages = null;
            Language = null;
            FSize = null;
            FileFormat = null;
            Category = null;
            Link = link;
        }
    article.Descendants["dl"] 
            |> Seq.head 
            |> fun x -> (Seq.zip (x.Descendants["dt"]) (x.Descendants["dd"]))
            |> Seq.map (fun kv -> (kv |> fst |> fun x -> x.InnerText().ToLower(), kv |> snd |> fun x -> x.InnerText().Trim()))
            |> Seq.iter (fun kv -> 
                                match kv with
                                | (x, y) when x.Contains("author") -> book.Author <- y 
                                | (x, y) when x.Contains("isbn") -> book.Isbn <- y
                                | (x, y) when x.Contains("year") -> book.Year <- y
                                | (x, y) when x.Contains("pages") -> book.Pages <- y
                                | (x, y) when x.Contains("language") -> book.Language <- y
                                | (x, y) when x.Contains("size") -> book.FSize <- y
                                | (x, y) when x.Contains("format") -> book.FileFormat <- y
                                | (x, y) when x.Contains("category") -> book.Category <- y
                                | (x, y) -> eprintfn "unknow %s:%s" x y
                                )
    book


let ParsePage n = 
    let page = n 
               |> sprintf "http://www.allitebooks.com/page/%d/"
               |> HtmlDocument.Load
    page.Descendants["article"]
    |> Seq.map (fun x -> (x.Descendants["h2"] |> Seq.head))
    |> Seq.map (fun x -> (x.InnerText(), x.Descendants["a"] |> Seq.head |> (fun y -> y.Attribute("href").Value())))
    //|> Seq.iter (fun kv -> Console.WriteLine("({0},{1})", kv |> fst, kv |> snd))

let ExtractBookInfo (kv:string * string) = 
    Thread.Sleep 100
    let bookPage = kv 
                   |> snd 
                   |> HtmlDocument.Load
    bookPage.Descendants["article"]
    |> Seq.head
    |> parseBook
    |> printfn "%A"

[<EntryPoint>]
let main argv = 
    for i in 1..560 do
        let books = ParsePage i
        books |> Seq.iter ExtractBookInfo
        Thread.Sleep 500
    0 // return an integer exit code

