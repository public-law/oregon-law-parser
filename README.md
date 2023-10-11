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

<img width="586" alt="Screenshot 2023-10-10 at 8 32 32 PM" src="https://github.com/public-law/oregon-law-parser/assets/150670/29ebe973-53e7-48b9-9f5f-52cef04e8b0f">


Here [it is](https://oregon.public.law/statutes/ors_469.233) in production. 
See [Main.hs](https://github.com/dogweather/analyze-ors-amendment-haskell/blob/master/analyze/src/Main.hs) for the top-level code.


# Improving flexibility via this intermediate step

In the past, this kind of coding was in the same project as the rest of the application. E.g., here, it'd be a Ruby rake task because the app is in Rails.

But this new, separate repo decouples the data import process: instead of writing more Ruby code for my Rails app, the JSON data is a go-between format. In this way I can, e.g. use with other languages like Haskell when appropriate.
