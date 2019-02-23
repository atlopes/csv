DO LOCFILE("csv.prg")

* demonstrates import and export of binary data
LOCAL CSV AS CVSProcessor

m.CSV = CREATEOBJECT("CSVProcessor")

* a cursor with different type binary data in some of its columns
CREATE CURSOR BinaryCursor (Id Integer, ImgGeneral General, BinaryBlob Blob, ShortBinary Varbinary(200))

* put some data inside the cursor
APPEND BLANK
APPEND GENERAL ImgGeneral FROM (GETPICT())
REPLACE Id WITH 1, BinaryBlob WITH FILETOSTR(FORCEEXT(SYS(16), "fxp")), ShortBinary WITH STRCONV("1172ab001390ff", 16)

* other encoding values to try: hex and plain
m.CSV.BinaryEncoding = "base64"

* everything is exported, including the binary data, base64 encoded
m.CSV.Export("~temp.csv")

* show the exported file
MODIFY FILE ~temp.csv NOEDIT

* the same data is now going to be appended to the cursor
* the row will be duplicated, except for the General field that won't be reimported

m.CSV.WorkArea = "BinaryCursor"
m.CSV.Import("~temp.csv")

SELECT BinaryCursor
GO TOP

BROWSE NOWAIT

ERASE ~temp.csv
