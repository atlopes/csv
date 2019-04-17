DO LOCFILE("csv.prg")

* demonstrates the import of different character encodings
LOCAL CSV AS CVSProcessor
LOCAL SourceCSV AS String

m.CSV = CREATEOBJECT("CSVProcessor")

TEXT TO m.SourceCSV NOSHOW
Country,City,Population,Latitude,Longitude
BR,Belйm,1452275,-1.455833,-48.503889
PT,Coimbra,143396,40.211111,-8.429167
ES,A Coruсa,246056,43.365,-8.41

ENDTEXT

* save as UNICODE, with a Byte Order Mark
STRTOFILE(STRCONV(STRCONV(m.SourceCSV, 1), 5), "~temp.csv", 2)

* import the UNICODE file
m.CSV.Import("~temp.csv")

* show the file, just to check its encoding
MODIFY FILE ~temp.csv NOEDIT

* show the imported file
SELECT (m.CSV.CursorName)
BROWSE NOWAIT

MESSAGEBOX("Import UNICODE data, with BOM at the beginning of the file.")

ERASE ~temp.csv

* now, the same for UTF-8
STRTOFILE(STRCONV(STRCONV(m.SourceCSV, 1), 9), "~temp.csv", 4)
m.CSV.Import("~temp.csv")
MODIFY FILE ~temp.csv NOEDIT
SELECT (m.CSV.CursorName)
BROWSE NOWAIT

MESSAGEBOX("Import UTF-8 data, with BOM at the beginning of the file.")

ERASE ~temp.csv

* and UTF-8 again, now without BOM
STRTOFILE(STRCONV(STRCONV(m.SourceCSV, 1), 9), "~temp.csv")
m.CSV.Import("~temp.csv")
MODIFY FILE ~temp.csv NOEDIT
SELECT (m.CSV.CursorName)
BROWSE NOWAIT

MESSAGEBOX("Import UTF-8 data, encoding auto-detected, no BOM at the beginning of the file.")

ERASE ~temp.csv

* finally, import data with Cyrillic fragments encoded as Windows-1251
TEXT TO m.SourceCSV NOSHOW
Author_en,Author_ru,YearOfBirth,YearOfDeath,Work_en,Work_ru
"Dostoyevsky, Fyodor","Достојевски, Фјодор",1821,1881,Crime and punishement,Преступление и наказание
"Tolstoy, Leo","Толстой, Лев",1828,1910,War and peace,Война и мир
"Gorky, Maksim","Горький, Максим",1868,1936,Mother,Мать

ENDTEXT

* save it verbatim
STRTOFILE(m.SourceCSV, "~temp.csv")

m.CSV.UTF = 0
m.CSV.RegionalID = 1251
m.CSV.RegionalIDType = 1
m.CSV.CPTrans = .F.
m.CSV.Import("~temp.csv")

MODIFY FILE ~temp.csv NOEDIT
SELECT (m.CSV.CursorName)
BROWSE NOWAIT FONT "Arial", 9, 204 

MESSAGEBOX("Import Windows-1251 data.")

ERASE ~temp.csv
