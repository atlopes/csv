DO LOCFILE("csv.prg")

* demonstrates import from clipboard
LOCAL CSV AS CVSProcessor

m.CSV = CREATEOBJECT("CSVProcessor")

* put some data in the clipboard
MESSAGEBOX("Ctrl + C on some Excel data, and then OK to continue.", 64, "From clipboard CSV example")

IF EMPTY(_CLIPTEXT)
	MESSAGEBOX("Clipboard seems to be empty...", 48, "Failed example...")
	RETURN
ENDIF

* what Excel normally uses to separate cells when it copies a worksheet fragment to the clipboard
m.CSV.ValueSeparator = CHR(9)

LOCAL Result AS Integer

* import from _CLIPTEXT
m.Result = m.CSV.ImportString(_CLIPTEXT, "Clipboard")
IF m.Result != 0
	ERROR m.Result
ELSE
	BROWSE NOWAIT
ENDIF
