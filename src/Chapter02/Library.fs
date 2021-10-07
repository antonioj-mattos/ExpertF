namespace Chapter02

module Words =

    /// Split a string into words at spaces.
    let splitAtSpaces (text: string) = text.Split ' ' |> Array.toList

    /// Analyze a string for duplicate words.
    let wordCount text =
        let words = splitAtSpaces text
        let numWords = words.Length
        let distinctWords = List.distinct words
        let numDups = numWords - distinctWords.Length
        numWords, numDups

    /// Analyze a string for duplicate words and display the results.
    let showWordCount text =
        let numWords, numDups = wordCount text
        printfn "--> %d words in the text" numWords
        printfn "--> %d duplicate words" numDups


module DoingWeb =

    #r "nuget: FSharp.Data, 4.2.3"
    #r "nuget: Suave, 2.6.1"

    open FSharp.Data
    open Suave


    type Species = HtmlProvider<"https://en.wikipedia.org/wiki/The_world's_100_most_threatened_species">

    let species =
        [ for x in
              Species
                  .GetSample()
                  .Tables
                  .``Species listEdit``
                  .Rows -> x.Type, x.``Common name`` ]

    let speciesSorted =
        species
        |> List.countBy fst
        |> List.sortByDescending snd

    // let html =
    //     [ yield "<html><body><ul>"
    //       for (category, count) in speciesSorted do
    //           yield sprintf "<li>Category <b>%s</b>: <b>%d</b></li>" category count
    //       yield "</ul></body></html>" ]
    //     |> String.concat "\n"

    // startWebServer defaultConfig (Successful.OK html)

    let angularHeader =
        """<head>
           <link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css">
           <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.2.26/angular.min.js"></script>
           </head>
        """

    let fancyText =
        [ yield """<html>"""
          yield angularHeader
          yield """<body>"""
          yield """<table class="table table-striped">"""
          yield """<thead><tr><th>Category</th><th>Count</th></tr></thead>"""
          yield """<tbody>"""
          for (category, count) in speciesSorted do
              yield sprintf "<tr><td>%s</td><td>%d</td></tr>" category count
          yield """</tbody>"""
          yield """</table>"""
          yield """</body>"""
          yield """</html>""" ]
        |> String.concat "\n"

    startWebServer defaultConfig (Successful.OK fancyText)
