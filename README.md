# analyze-ors-amendment-haskell

A command line app, `analyze` which extracts info from an Oregon Amendment (in HTML), and outputs JSON:

```sh
$ pdf2html 2016orLaw0001.pdf | analyze

{
    "summary": "Relating to speed limits on highways..."
    "citations": [
        "801.462",
        "810.180",
        "810.243",
        "811.109",
        "811.111",
        "811.124"
    ]
}
```

This decouples the data import process: instead of writing more Ruby code for my Rails app, this JSON is a go-between format. This way I can experiment with other languages.


See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs).

## pdf2html

This is an alias which runs [Apache Tika](https://tika.apache.org/):

```bash
alias pdf2html="java -jar ~/lib/tika-app.jar --html"
```
