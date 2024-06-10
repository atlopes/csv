DO LOCFILE("csv.prg")

* demonstrates import of boxed data
LOCAL CSV AS CVSProcessor

m.CSV = CREATEOBJECT("CSVProcessor")

LOCAL Source AS String

TEXT TO m.Source NOSHOW
+-------------+------------+------------+---------------+
| Country     | City       | Population | Density (km2) |
+-------------+------------+------------+---------------+
| China       | Chongqing  | 32,054,159 |           389 |
| China       | Shanghai   | 24,870,895 |         3,922 |
| China       | Beijing    | 21,893,095 |         1,334 |
| China       | Chengdu    | 20,937,757 |         1,456 |
| Pakistan    | Karachi    | 20,382,881 |         5,774 |
| China       | Guangzhou  | 18,676,605 |         2,512 |
| China       | Shenzhen   | 17,494,398 |         8,534 |
| India       | Delhi      | 16,753,235 |        11,289 |
| Turkey      | Istanbul   | 15,519,267 |        11,289 |
| DR Congo    | Kinshasa   | 13,171,000 |         9,965 |
+-------------+------------+------------+---------------+
ENDTEXT

m.CSV.ThousandsSeparator = ","
m.CSV.ValueSeparator = "|"
m.CSV.InTrimmer = 3

* data is boxed, set parameters
m.CSV.BoxedData = .T.
m.CSV.BoxedRowDelimiters = "||"
m.CSV.BoxedSeparator = "+-+-+"

m.CSV.ImportString(m.Source, "LargestCitiesInTheWorld")

SELECT LargestCitiesInTheWorld
GO TOP

BROWSE NOWAIT
