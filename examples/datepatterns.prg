DO LOCFILE("csv.prg")

SET HOURS TO 24

* demonstrates the use of date patterns
LOCAL CSV AS CVSProcessor
LOCAL SourceCSV AS String

TEXT TO m.SourceCSV NOSHOW
Id,OccurrenceTime,Description
6731,10/23/2017 11:12:40 AM,"Power off, alarm not set"
6732,10/23/2017 11:13:21 AM,Restarted
6733,10/23/2017 00:07:54 PM,"Signal OK, entering quiet mode"

ENDTEXT

STRTOFILE(m.SourceCSV, "~temp.csv")

m.CSV = CREATEOBJECT("CSVProcessor")

* first read, with default patterns
m.CSV.Import("~temp.csv")

SELECT (m.CSV.CursorName)
BROWSE

MESSAGEBOX(TEXTMERGE("Pattern: <<m.CSV.DatetimePattern>> / Type of Occurencetime: <<TYPE('Occurrencetime')>>"))

* the Ocurrencetime column will be interpreted as a Datetime
m.CSV.DatetimePattern = "%2M/%2D/%4Y %2h:%2m:%2s %p"

m.CSV.Import("~temp.csv")

SELECT (m.CSV.CursorName)
BROWSE

MESSAGEBOX(TEXTMERGE("Pattern: <<m.CSV.DatetimePattern>> / Type of Occurencetime: <<TYPE('Occurrencetime')>>"))

ERASE ~temp.csv
