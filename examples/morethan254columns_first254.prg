CLEAR
CLOSE TABLES ALL

DO LOCFILE("csv.prg")

LOCAL CSV AS CSVProcessor
LOCAL Operation AS String

m.CSV = CREATEOBJECT("CSVProcessor")

m.Operation = "Reading CSV with 800 columns, only first 254 will be imported..."

WAIT WINDOW m.Operation NOWAIT NOCLEAR
m.CSV.Import(LOCFILE("elementary_2015_16.csv"))
BROWSE NOWAIT
WAIT CLEAR

MESSAGEBOX(STRTRAN(m.Operation, "will be", "were"))

SET
