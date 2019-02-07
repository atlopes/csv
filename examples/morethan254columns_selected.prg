CLEAR
CLOSE TABLES ALL

DO LOCFILE("csv.prg")

LOCAL CSV AS CSVProcessor
LOCAL Operation AS String

m.CSV = CREATEOBJECT("CSVProcessor")

CREATE CURSOR AutismData ;
	(State Varchar(32), District Varchar(32), Population Integer, Children6_10 Integer, Children11_13 Integer, ;
	Autism1 Integer, Autism2 Integer, Autism3 Integer, Autism4 Integer, Autism5 Integer, Autism6 Integer, Autism7 Integer, Autism8 Integer)

m.CSV.Workarea = "AutismData"
m.CSV.FieldMapping.Add("State", "STATE NAME")
m.CSV.FieldMapping.Add("District", "DISTRICT NAME")
m.CSV.FieldMapping.Add("Population", "TOTAL POULATION")
m.CSV.FieldMapping.Add("Children6_10", "AGE GROUP 6 TO 10 (TOT 6 10 15)")
m.CSV.FieldMapping.Add("Children11_13", "AGE GROUP 11 TO 13 (TOT 11 13 15)")
m.CSV.FieldMapping.Add("Autism1", "AUTISM (AUC1)")
m.CSV.FieldMapping.Add("Autism2", "AUTISM (AUC2)")
m.CSV.FieldMapping.Add("Autism3", "AUTISM (AUC3)")
m.CSV.FieldMapping.Add("Autism4", "AUTISM (AUC4)")
m.CSV.FieldMapping.Add("Autism5", "AUTISM (AUC5)")
m.CSV.FieldMapping.Add("Autism6", "AUTISM (AUC6)")
m.CSV.FieldMapping.Add("Autism7", "AUTISM (AUC7)")
m.CSV.FieldMapping.Add("Autism8", "AUTISM (AUC8)")
m.CSV.FieldMapping.Add("Autism9", "AUTISM (AUC9)")
* missing data will be empty
m.CSV.NullValue = .NULL.

m.Operation = "Reading CSV with 800 columns, selected columns will be imported into an existing cursor..."

WAIT WINDOW m.Operation NOWAIT NOCLEAR
m.CSV.Import(LOCFILE("elementary_2015_16.csv"))

SELECT AutismData
GO TOP
BROWSE NOWAIT

WAIT CLEAR

MESSAGEBOX(STRTRAN(m.Operation, "will be", "were"))

SET

