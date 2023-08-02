# analyze-oregon-law

A command line app, `analyze`, which pulls in [an Oregon session law in PDF format](https://www.oregonlegislature.gov/bills_laws/lawsstatutes/2016orLaw0001.pdf):

![image](https://raw.githubusercontent.com/dogweather/analyze-ors-amendment-haskell/master/fixtures/typical-pdf.png)

and produces this metadata in JSON:


```bash
$ analyze 2016orLaw0001.pdf

{
    "summary": "Relating to speed limits on highways that traverse state lines; creating new provisions; amending ORS 811.111; and declaring an emergency.",
    "bill": {
        "billNumber": 4047,
        "billType": "HB"
    }, 
    "effectiveDate": "2016-03-01",
    "year": 2016,
    "affectedSections": {
        "repealed": [],
        "amended": [
            "811.111"
        ]
    }
}
```

A web app can easily import this and display it:

![image](https://raw.githubusercontent.com/dogweather/analyze-oregon-law-haskell/master/fixtures/end-result.png)

<img width="473" alt="Screenshot 2023-07-29 at 4 21 35 PM" src="https://github.com/public-law/oregon-law-parser/assets/150670/c6220f66-e370-4e7f-b4e9-704a4bdc540a">

See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs) for the top-level code.


# Improving flexibility via this intermediate step

In the past, this kind of coding was in the same project as the rest of the application. E.g., here, it'd be a Ruby rake task because the app is in Rails.

But this new, separate repo decouples the data import process: instead of writing more Ruby code for my Rails app, the JSON data is a go-between format. In this way I can, e.g. use with other languages like Haskell when appropriate.
