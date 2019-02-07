CLEAR
CLOSE TABLES ALL

DO LOCFILE("csv.prg")

LOCAL CSV AS CSVProcessor
LOCAL Operation AS String
LOCAL Imported AS String

m.CSV = CREATEOBJECT("CSVProcessor")
m.CSV.Multiplecursors = .T.

m.Operation = "Reading CSV with 800 columns, all columns will be imported into as much cursors as needed..."

WAIT WINDOW m.Operation NOWAIT NOCLEAR
m.CSV.Import(LOCFILE("elementary_2015_16.csv"))

FOR EACH m.Imported IN m.CSV.MultipleCursorsNames
	SELECT (m.Imported)
	GO TOP
	BROWSE NOWAIT
ENDFOR

WAIT CLEAR

MESSAGEBOX(STRTRAN(m.Operation, "will be", "were"))

SET

