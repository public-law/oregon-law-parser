# analyze-ors-amendment-haskell

A command line app for transforming an Oregon Amendment in HTML form into JSON structured data:

```sh
$ pdf2html 2016orLaw0001.pdf | analyze

{
    "summary": "Relating to speed limits on highways..."
}
```

See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs) for the actual Haskell code.
