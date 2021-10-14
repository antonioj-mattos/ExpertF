namespace Chapter03


module Recursion =

    let rec factorial n =
        if n <= 1 then
            1
        else
            n * factorial (n - 1)

    let http url : string = ""

    let rec repeatFetch url n =
        if n > 0 then
            let html = http url
            printfn "fetched <<< %s >>> on iteration %d" html n
            repeatFetch url (n - 1)

module PatternMatching =

    let isLikelySecretAgent url agent =
        match url, agent with
        | "http://www.control.org", 99 -> true
        | "http://www.control.org", 86 -> true
        | "http://www.kaos.org", _ -> true
        | _ -> false

    let printFirst xs =
        match xs with
        | h :: _t -> printfn "The first item in the list is %A" h
        | [] -> printfn "No items in the list"

    let showParents (name, parents) =
        match parents with
        | Some (dad, mum) -> printfn "%s has father %s and mother %s" name dad mum
        | None -> printfn "%s has no parents!" name

module FunctionValues =

    let http url : string = url

    let delimiters = [| ' '; '\n'; '\t'; '<'; '>'; '=' |]

    let getWords (s: string) = s.Split delimiters

    let getStats site =
        let url = "https://" + site
        let html = http url
        let hwords = html |> getWords

        let hrefs =
            hwords |> Array.filter (fun s -> s = "href")

        (site, html.Length, hwords.Length, hrefs.Length)

module AbstractingControl =

    open System

    let http url : string = url

    let time f =
        let start = DateTime.Now
        let result = f ()
        let finish = DateTime.Now
        (result, finish - start)

    time (fun () -> http "http://www.newscientist.com")
    |> ignore

module ObjectMethods =

    open System.IO

    let extensions =
        [ "file1.txt"; "file2.txt"; "file3.sh" ]
        |> List.map Path.GetExtension
