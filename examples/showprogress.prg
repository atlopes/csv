DO LOCFILE("csv.prg")

* demonstrates the use of a progress indicator during a lengthy CSV import
LOCAL CSV AS CSVProcessor
LOCAL ShowProgress AS Progressor

m.ShowProgress = CREATEOBJECT("Progressor")
m.CSV = CREATEOBJECT("CSVProcessor")
BINDEVENT(m.CSV, "ProcessStep", m.ShowProgress, "Done", 2)

* choose a lengthy csv file
m.CSV.Import(GETFILE())
BROWSE

DEFINE CLASS Progressor AS Session

	AlreadyDone = -1

	PROCEDURE Done (Phase AS Integer, Done AS Number, ToDo AS Number)

		LOCAL NowDone AS Integer

		m.NowDone = INT(m.Done / m.ToDo * 50)
		IF m.NowDone != This.AlreadyDone
			WAIT WINDOW TEXTMERGE("Phase <<m.Phase>>: <<PADR(REPLICATE('*', m.NowDone), 50, '-')>>") NOWAIT NOCLEAR
			This.AlreadyDone = m.NowDone
		ENDIF

	ENDPROC

	PROCEDURE Destroy
		WAIT CLEAR
	ENDPROC

ENDDEFINE
