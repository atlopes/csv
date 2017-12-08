# CSVProcessor Documentation

### Overview

With CSVProcessor, a CSV file can be imported in a similar way that the `IMPORT` command imports other file types (XLS, WRK, ...), with some added features.

* [Properties and Methods](pem.md "Properties and Methods")
* [Examples](examples.md "Examples")

### Quick guide

To enable the class
```foxpro
DO LOCFILE("csv.prg")
```

To instantiate an object
```foxpro
LOCAL CSV AS CSVProcessor

m.CSV = CREATEOBJECT("CSVProcessor")
```

The properties that control the CSV parsing must match the formatting of the source file. For instance, to import data from a CSV file that is delimited with tabs, the decimal point is represented by a comma, the dates are inserted according to the basic format of ISO 8601 (YYYYMMDD), and empty values represent NULL:
```foxpro
m.CSV.ValueSeparator = CHR(9)
m.CSV.DecimalPoint = ","
m.CSV.DatePattern = "%4Y%2M%2D"
m.CSV.NullValue = ""
```

To import the CSV data, and store the result in a specific new cursor
```foxpro
IF m.CSV.Import(GETFILE("csv"), "tempCSV") = 0
  SELECT tempCSV
  BROWSE
ELSE
  * an error occurred
ENDIF
```

If a date column in the CSV file was not properly imported as a date field, it's easy to check for the values that could not be parsed as a date. For instance, assuming that the data was imported into the cursor `tempCSV`, and the field name is `dateCreation` , looking for mismatched column values can be done this way:
```foxpro
SELECT dateCreation FROM tempCSV WHERE ISNULL(m.CSV.ScanDate(dateCreation, .F.))
```

Hopefully, this will help on setting an adequate date pattern. The same type of verification can be done with Logical and Numeric columns.