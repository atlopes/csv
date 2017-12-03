DO LOCFILE("csv.prg")

SET HOURS TO 24

* demonstrates the import of different character encodings
LOCAL CSV AS CVSProcessor
LOCAL SourceCSV AS String

m.CSV = CREATEOBJECT("CSVProcessor")

TEXT TO m.SourceCSV NOSHOW
Country,City,Population,Latitude,Longitude
BR,Belém,1452275,-1.455833,-48.503889
PT,Coimbra,143396,40.211111,-8.429167
ES,A Coruña,246056,43.365,-8.41

ENDTEXT

* save as UNICODE, with a Byte Order Mark
STRTOFILE(STRCONV(STRCONV(m.SourceCSV, 1), 5), "~temp.csv", 2)

* import the UNICODE file
m.CSV.Import("~temp.csv")

* show the file, just to check its encoding
MODIFY FILE ~temp.csv NOEDIT

* show the imported file
SELECT (m.CSV.CursorName)
BROWSE

ERASE ~temp.csv

* now, the same for UTF-8
STRTOFILE(STRCONV(STRCONV(m.SourceCSV, 1), 9), "~temp.csv", 4)
m.CSV.Import("~temp.csv")
MODIFY FILE ~temp.csv NOEDIT
SELECT (m.CSV.CursorName)
BROWSE

ERASE ~temp.csv
