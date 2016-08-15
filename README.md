# analyze-ors-amendment-haskell

A command line app, `analyze`, which extracts this information:


```sh
$ analyze 2016orLaw0001.pdf

{
    "summary": "Relating to speed limits on highways that traverse state lines; creating new provisions; amending ORS 811.111; and declaring an emergency."
    "bill": {
        "billNumber": 4047,
        "billType": "HB"
    },
    "effectiveDate": "2016-05-05",
    "year": 2016,
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


from this:

![image](https://raw.githubusercontent.com/dogweather/analyze-ors-amendment-haskell/master/fixtures/typical-pdf.png)

See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs).


# An intermediate step to improve flexibility

In the past, I've done this kind of coding in the same project as the rest of the application. E.g., here, it'd be a Ruby rake task because the app is in Rails.

But this new, separate repo decouples the data import process: instead of writing more Ruby code for my Rails app, the JSON data is a go-between format. In this way I can, e.g. use with other languages like Haskell when appropriate.
