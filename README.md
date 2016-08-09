# analyze-ors-amendment-haskell

A command line app for transforming an Oregon Amendment in HTML form into JSON structured data:

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

See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs).

## pdf2html

This is an alias which runs Apache Tika:

```bash
alias pdf2html="java -jar ~/lib/tika-app.jar --html"
```
