DO LOCFILE("csv.prg")

SET HOURS TO 24

* demonstrates appending and field mapping methods CSV and a cursor
LOCAL CSV AS CVSProcessor
LOCAL SourceCSV AS String

* note that CSV columns and target cursor are not in the same order
TEXT TO m.SourceCSV NOSHOW
Identifier,Time,Status
6731,2017-10-23 11:12:40,"Power off, alarm not set"
6732,2017-10-23 11:13:21,Restarted
6733,2017-10-23 00:07:54,"Signal OK, entering quiet mode"

ENDTEXT

STRTOFILE(m.SourceCSV, "~temp.csv")

m.CSV = CREATEOBJECT("CSVProcessor")

* a) import, data not appended
SetCursor()
m.CSV.Import("~temp.csv", "TargetCursor")
SELECT (m.CSV.CursorName)
GO TOP
BROWSE NOWAIT
MESSAGEBOX("Import, data not appended.")

* from now on, CSV data will be appended to an existing cursor
m.CSV.Workarea = "TargetCursor"

* b) no mapping... columns won't match
SetCursor()
m.CSV.Import("~temp.csv")
SELECT (m.CSV.CursorName)
GO TOP
BROWSE NOWAIT
MESSAGEBOX("No mapping... columns don't match.")

* c) index mapping... CSV numbered column to Cursor named column, data will be ok
SetCursor()
m.CSV.FieldMapping.Add("Id")
m.CSV.FieldMapping.Add("OccurrenceTime")
m.CSV.FieldMapping.Add("Description")
m.CSV.Import("~temp.csv")
SELECT (m.CSV.CursorName)
GO TOP
BROWSE NOWAIT
MESSAGEBOX("Index mapping... CSV numbered columns to Cursor named columns, data is ok.")

* d) key mapping... CSV named column to Cursor named column, data will be ok
SetCursor()
m.CSV.FieldMapping.Remove(-1)
m.CSV.FieldMapping.Add("Id", "Identifier")
m.CSV.FieldMapping.Add("Description", "Status")
m.CSV.FieldMapping.Add("OccurrenceTime", "Time")
m.CSV.Import("~temp.csv")
SELECT (m.CSV.CursorName)
GO TOP
BROWSE NOWAIT
MESSAGEBOX("Key mapping... CSV named columns to Cursor named columns, data is ok.")

* e) field filtering... unmapped CSV column won't be imported
SetCursor()
m.CSV.FieldMapping.Remove("Status")
m.CSV.Import("~temp.csv")
SELECT (m.CSV.CursorName)
GO TOP
BROWSE NOWAIT
MESSAGEBOX("Field filtering... unmapped CSV column isn't imported.")

ERASE ~temp.csv

FUNCTION SetCursor ()

	CREATE CURSOR TargetCursor (ID Integer, Description Varchar(64), OccurrenceTime DateTime)

	* append won't touch this
	INSERT INTO TargetCursor VALUES (6730, "All systems ON", {^2017-10-23 18:00:01})

ENDFUNC
