*!*	CSVProcessor

*!*	A VFP class to process CSV files

* dependencies
DO LOCFILE("namer.prg")

* install itself
IF !SYS(16) $ SET("Procedure")
	SET PROCEDURE TO (SYS(16)) ADDITIVE
ENDIF

#DEFINE SAFETHIS			ASSERT !USED("This") AND TYPE("This") == "O"

DEFINE CLASS CSVProcessor AS Custom

	* a name controller to set valid cursor and field names
	ADD OBJECT NameController AS Namer

	* properties related to the resulting cursor/table
	CursorName = ""

	* properties related to how data is stored in the CSV file
	* the CSV file has a header row?
	HeaderRow = .T.
	* number of rows to skip, at the beginning of the file
	SkipRows = 0
	* how values are separated
	ValueSeparator = ","
	* how values are delimited
	ValueDelimiter = '"'
	* the decimal point
	DecimalPoint = "."
	* value for .T. (.NULL., if no logical values)
	LogicalTrue = "T"
	* value for .F. (.NULL., if no logical values)
	LogicalFalse = "F"
	* how dates are formatted
	DatePattern = "%4Y-%2M-%2D"
	* how datetimes are formatted
	DateTimePattern = "%4Y-%2M-%2D %2h:%2m:%2s"
	* month names, if needed for date anda datetime scanning
	MonthNames = "Jan:1:Feb:2:Mar:3:Apr:4:May:5:Jun:6:Jul:7:Aug:8:Sep:9:Oct:10:Nov:11:Dec:12"
	* ante- and post-meridian signatures
	AnteMeridian = "AM"
	PostMeridian = "PM"
	* century years
	CenturyYears = 0
	* how are .NULL. values represented (can be a string, such as "NULL", or .NULL., in which cases they are replaced by empty values)
	NullValue = ""
	* sample size, to determine column data types (0 = all rows)
	SampleSize = 0

	* properties related to the file
	* file handle
	HFile = -1
	* UNICODE encoding
	UTF = 0
	* length and position
	FileLength = -1
	FilePosition = -1

	_MemberData = "<VFPData>" + ;
						'<memberdata name="antemeridian" type="property" display="AnteMeridian"/>' + ;
						'<memberdata name="cursorname" type="property" display="CursorName"/>' + ;
						'<memberdata name="centuryyears" type="property" display="CenturyYears"/>' + ;
						'<memberdata name="datepattern" type="property" display="DatePattern"/>' + ;
						'<memberdata name="datetimepattern" type="property" display="DatetimePattern"/>' + ;
						'<memberdata name="decimalpoint" type="property" display="DecimalPoint"/>' + ;
						'<memberdata name="filelength" type="property" display="FileLength"/>' + ;
						'<memberdata name="fileposition" type="property" display="FilePosition"/>' + ;
						'<memberdata name="headerrow" type="property" display="HeaderRow"/>' + ;
						'<memberdata name="hfile" type="property" display="HFile"/>' + ;
						'<memberdata name="logicalfalse" type="property" display="LogicalFalse"/>' + ;
						'<memberdata name="logicaltrue" type="property" display="LogicalTrue"/>' + ;
						'<memberdata name="monthnames" type="property" display="MonthNames"/>' + ;
						'<memberdata name="namecontroller" type="property" display="NameController"/>' + ;
						'<memberdata name="nullvalue" type="property" display="NullValue"/>' + ;
						'<memberdata name="postmeridian" type="property" display="PostMeridian"/>' + ;
						'<memberdata name="samplesize" type="property" display="SampleSize"/>' + ;
						'<memberdata name="skiprows" type="property" display="SkipRows"/>' + ;
						'<memberdata name="utf" type="property" display="UTF"/>' + ;
						'<memberdata name="valuedelimiter" type="property" display="ValueDelimiter"/>' + ;
						'<memberdata name="valueseparator" type="property" display="ValueSeparator"/>' + ;
						'<memberdata name="closefile" type="method" display="CloseFile"/>' + ;
						'<memberdata name="columntype" type="method" display="ColumnType"/>' + ;
						'<memberdata name="getline" type="method" display="GetLine"/>' + ;
						'<memberdata name="import" type="method" display="Import"/>' + ;
						'<memberdata name="openfile" type="method" display="OpenFile"/>' + ;
						'<memberdata name="processstep" type="method" display="ProcessStep"/>' + ;
						'<memberdata name="scandate" type="method" display="ScanDate"/>' + ;
					'</VFPData>'

	* Init
	* attach a VFP name processor to the name controller
	PROCEDURE Init
		This.NameController.AttachProcessor("VFPNamer", "vfp-names.prg")
	ENDPROC

	* clean up, on exit
	PROCEDURE Destroy
		This.CloseFile()
	ENDPROC

	* Import (Filename[, CursorName[, HostDatabase]])
	* import a CSV file into a cursor (or a database table)
	FUNCTION Import (Filename AS String, CursorName AS String, HostDatabase AS String) AS Integer

		SAFETHIS

		ASSERT (PCOUNT() < 3 OR VARTYPE(m.HostDatabase) == "C") AND (PCOUNT() < 2 OR VARTYPE(m.CursorName) == "C") ;
					AND VARTYPE(m.Filename) == "C" ;
				MESSAGE "String parameters expected."

		* what is read from the CVS
		LOCAL CSVFileContents AS String
		* separated by columns
		LOCAL ARRAY ColumnsData(1)
		* after being buffered
		LOCAL ARRAY ColumnsBuffer(1)

		* the name of the columns
		LOCAL ARRAY ColumnsNames(1)
		* and the field definitions
		LOCAL ARRAY CursorFields(1)
		* how many (real) columns there are
		LOCAL ColumnsCount AS Integer

		* the detected type of each column
		LOCAL Retype AS String
		* name and contents of a column
		LOCAL ColumnName AS String
		LOCAL BaseColumnName AS String
		LOCAL ColumnText AS String

		* the CSV file uses delimiters?
		LOCAL IsDelimited AS Boolean
		* this controls their use while reading a column
		LOCAL TrailDelimiters AS Integer

		* loop indexers
		LOCAL ExpIndex AS Integer
		LOCAL LineIndex AS Integer
		LOCAL ColLineIndex AS Integer
		LOCAL RowIndex AS Integer
		LOCAL ColumnIndex AS Integer

		* a temporary cursor that will receive the first import
		LOCAL Importer AS String

		* anything wrong will be trapped
		LOCAL ErrorHandler AS Exception

		* 0 = OK, -1 is error reading file, > 0 other type of errors
		LOCAL Result AS Integer

		* open the file
		IF !This.OpenFile(m.Filename)
			RETURN -1
		ENDIF

		* derive a name for the filename, if it was not passed as a parameter
		IF PCOUNT() = 1
			This.NameController.SetOriginalName(JUSTSTEM(m.FileName))
			m.CursorName = This.NameController.GetName()
			IF ISNULL(m.CursorName)
				RETURN -1
			ENDIF
		ENDIF

		* set it, anyway, in case the caller needs it
		This.CursorName = m.CursorName

		TRY

			* skip rows, if needed
			FOR m.RowIndex = 1 TO This.SkipRows
				This.GetLine()
			ENDFOR

			* get the column names (from the CSV file) or use a Col_XXX pattern

			* if the CSV files has headers
			IF This.HeaderRow
				* fetch column names in first line of the CSV file
				m.CSVFileContents = This.GetLine()

				DIMENSION m.CursorFields(ALINES(m.ColumnsNames, m.CSVFileContents, 1, This.ValueSeparator), 18)
				m.ColumnsCount = ALEN(m.ColumnsNames)
			ELSE

				* columns are not named, so create a dummy structure, with max number of 254 columns (the VFP limit)
				DIMENSION m.CursorFields(254, 18)
				DIMENSION m.ColumnsNames(254)
				FOR m.ColumnIndex = 1 TO 254
					m.ColumnsNames(m.ColumnIndex) = "Col_" + TRANSFORM(m.ColumnIndex, "@L 999")
				ENDFOR
				* the real column count will be read as data is imported
				m.ColumnsCount = 0

			ENDIF

			* clear the structure
			STORE "" TO m.CursorFields
			* fetch valid column names and check for name conformity
			FOR m.ColumnIndex = 1 TO MIN(ALEN(m.ColumnsNames), 254)

				* names must be validated if they come from the CSV file
				IF This.HeaderRow
					This.NameController.SetOriginalName(m.ColumnsNames(m.ColumnIndex))
					m.ColumnName = This.NameController.GetName()
					* check for repetitions
					IF m.ColumnIndex > 1
						m.ExpIndex = 1
						m.BaseColumnName = m.ColumnName
						DO WHILE ASCAN(m.ColumnsNames, m.ColumnName, 1, m.ColumnIndex - 1, 1, 1 + 2 + 4) != 0
							m.ColumnName = m.BaseColumnName + "_" + LTRIM(STR(m.ExpIndex, 10, 0))
							m.ExpIndex = m.ExpIndex + 1
						ENDDO
					ENDIF
				ELSE
					m.ColumnName = m.ColumnsNames(m.ColumnIndex)
				ENDIF

				* the name is valid and unique: prepare a field definition, starting by the name
				m.ColumnsNames(m.ColumnIndex) = m.ColumnName
				m.CursorFields(m.ColumnIndex, 1) = m.ColumnName
				* the type (Memo, to hold anything)
				m.CursorFields(m.ColumnIndex, 2) = "M"
				* nocptrans and accepting .NULL.
				m.CursorFields(m.ColumnIndex, 5) = .T.
				m.CursorFields(m.ColumnIndex, 6) = .T.
				* dimension, precision, etc., set to zero
				STORE 0 TO m.CursorFields(m.ColumnIndex, 3), m.CursorFields(m.ColumnIndex, 4), ;
					m.CursorFields(m.ColumnIndex, 17), m.CursorFields(m.ColumnIndex, 18)
			ENDFOR

			* get a name for the import cursor, based on the cursor name
			m.ExpIndex = 1
			m.Importer = "_" + m.CursorName
			DO WHILE USED(m.Importer)
				m.Importer = "_" + m.CursorName + "_" + LTRIM(STR(m.ExpIndex, 10, 0))
				m.ExpIndex = m.ExpIndex + 1
			ENDDO
			* a structure is at hand, the cursor may be created
			CREATE CURSOR (m.Importer) FROM ARRAY m.CursorFields

			* if a delimiter was set, values can be delimited
			m.IsDelimited = LEN(NVL(This.ValueDelimiter, "")) > 0

			* starting to import...
			* phase 1: read the data in the CSV file

			* this will point to the column that is being filled with data
			m.ColumnIndex = 1
			DIMENSION m.ColumnsData(ALEN(m.ColumnsNames))
			STORE "" TO m.ColumnsData
			m.CSVFileContents = This.GetLine()

			* until there is nothing left to read from the CSV file
			DO WHILE !ISNULL(m.CSVFileContents)

				* buffer the data from the line, separated (may be reassembled, later on, if needed)
				ALINES(m.ColumnsBuffer, m.CSVFileContents, 2, This.ValueSeparator)
				* this will point to the CSV column that is being read 
				m.ColLineIndex = 1

				* while both indexes have something to look into
				DO WHILE m.ColumnIndex <= ALEN(m.ColumnsNames) AND m.ColLineIndex <= ALEN(m.ColumnsBuffer)

					* update the column count, if we have now an extra column
					IF !This.HeaderRow AND m.ColumnIndex > m.ColumnsCount
						m.ColumnsCount = m.ColumnIndex
					ENDIF

					* the (partial or complete) value from the CSV field
					m.ColumnText = m.ColumnsBuffer(m.ColLineIndex)
					* add it to the fetched value
					m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + m.ColumnText

					* found a delimited field?
					IF m.IsDelimited AND LEFT(m.ColumnsData(m.ColumnIndex), 1) == This.ValueDelimiter

						m.TrailDelimiters = 0
						* check on the case that the field may end wth a bunch of delimiters...
						IF LEN(m.ColumnsData(m.ColumnIndex)) > 1
							DO WHILE RIGHT(m.ColumnText, 1) == This.ValueDelimiter
								m.TrailDelimiters = m.TrailDelimiters + 1
								m.ColumnText = LEFT(m.ColumnText, LEN(m.ColumnText) - 1)
							ENDDO
						ENDIF

						* if the field ended with a delimiter
						IF RIGHT(m.ColumnsData(m.ColumnIndex), 1) == This.ValueDelimiter AND (m.TrailDelimiters / 2) != INT(m.TrailDelimiters / 2)
							* remove the delimiters from the column data, at the beginning and at the end of the field
							m.ColumnsData(m.ColumnIndex) = SUBSTR(m.ColumnsData(m.ColumnIndex), LEN(This.ValueDelimiter) + 1, LEN(m.ColumnsData(m.ColumnIndex)) - (LEN(This.ValueDelimiter) + 1))
							* and also in the middle
							m.ColumnsData(m.ColumnIndex) = STRTRAN(m.ColumnsData(m.ColumnIndex), REPLICATE(This.ValueDelimiter, 2), This.ValueDelimiter)
						ELSE
							* if not, it was a separator that broke the columns, so add it
							IF m.ColLineIndex < ALEN(m.ColumnsBuffer)
								m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + This.ValueSeparator
							ENDIF
							* and continue to fill the current data column from the next CSV column
							m.ColLineIndex = m.ColLineIndex + 1
							LOOP
						ENDIF
					ENDIF

					* fetch more columns...
					IF m.ColLineIndex < ALEN(m.ColumnsBuffer)
						m.ColumnIndex = m.ColumnIndex + 1
					ENDIF
					m.ColLineIndex = m.ColLineIndex + 1
				ENDDO

				* if there are set columns, and they were not completely fetched from the previous line,
				* there is a line break that must be inserted, and the rest of the column, and of the columns,
				* to be imported from the next line(s)
				IF This.HeaderRow AND m.ColumnIndex < ALEN(m.ColumnsNames)

					m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + CHR(13) + CHR(10)

				ELSE

					* the line is completely read
					FOR m.ColumnIndex = 1 TO ALEN(m.ColumnsNames)
						* .NULL.ify, if needed
						IF NVL(m.ColumnsData(m.ColumnIndex) == This.NullValue, .F.)
							m.ColumnsData(m.ColumnIndex) = .NULL.
						ENDIF
					ENDFOR

					* insert the data
					APPEND BLANK
					GATHER FROM m.ColumnsData MEMO

					* and reset the row
					m.ColumnIndex = 1
					STORE "" TO m.ColumnsData

				ENDIF

				* signal another line read
				RAISEEVENT(This, "ProcessStep", 1, This.FilePosition, This.FileLength)

				* and step to the next one
				m.CSVFileContents = This.GetLine()

			ENDDO

			* the CSV file can be closed
			This.CloseFile()

			* phase 2: set the type of the fields

			* reset the fields definitions
			DIMENSION m.CursorFields(m.ColumnsCount, 18)
			DIMENSION m.ColumnsNames(m.ColumnsCount)

			* determine the type and length of each column
			FOR m.ColumnIndex = 1 TO m.ColumnsCount
				* change the Memo to something else
				m.Retype = This.ColumnType(m.Importer, m.ColumnsNames(m.ColumnIndex))
				DO CASE
				* Integer
				CASE m.Retype == "I"
					m.CursorFields(m.ColumnIndex, 2) = "I"
					m.CursorFields(m.ColumnIndex, 3) = 4
				* Logical
				CASE m.Retype == "L"
					m.CursorFields(m.ColumnIndex, 2) = "L"
					m.CursorFields(m.ColumnIndex, 3) = 1
				* Date
				CASE m.Retype == "D"
					m.CursorFields(m.ColumnIndex, 2) = "D"
					m.CursorFields(m.ColumnIndex, 3) = 4
				* Datetime
				CASE m.Retype == "T"
					m.CursorFields(m.ColumnIndex, 2) = "T"
					m.CursorFields(m.ColumnIndex, 3) = 8
				* Double
				CASE m.Retype == "B"
					m.CursorFields(m.ColumnIndex, 2) = "B"
					m.CursorFields(m.ColumnIndex, 3) = 8
					m.CursorFields(m.ColumnIndex, 4) = 4
				* Varchar()
				CASE LEFT(m.Retype, 1) == "V"
					m.CursorFields(m.ColumnIndex, 2) = "V"
					m.CursorFields(m.ColumnIndex, 3) = EVL(VAL(SUBSTR(m.Retype, 2)), 1)
				* or leave it as a Memo
				ENDCASE

				* signal the step
				RAISEEVENT(This, "ProcessStep", 2, m.ColumnIndex, ALEN(m.ColumnsNames))

			ENDFOR

			IF USED(m.CursorName)
				USE IN (m.CursorName)
			ENDIF
			* create a cursor
			IF PCOUNT() < 3
				CREATE CURSOR (m.CursorName) FROM ARRAY m.CursorFields
			ELSE
				* or a table of a database
				SET DATABASE TO (m.ToDatabase)
				IF INDBC(m.CursorName, "TABLE")
					DROP TABLE (m.CursorName)
				ENDIF
				CREATE TABLE (m.CursorName) FROM ARRAY m.CursorFields
			ENDIF

			* phase 3: move the imported data to the cursor
			SELECT (m.Importer)
			SCAN
				* move to an array
				SCATTER MEMO TO m.ColumnsData

				* evaluate the memo, and reset the value with its (new) data type
				FOR m.ColumnIndex = 1 TO ALEN(m.ColumnsData)
					m.ColumnText = m.ColumnsData(m.ColumnIndex)
					DO CASE
					CASE ISNULL(m.ColumnText)
						&& do nothing, NULL will be passed to the cursor
					CASE m.CursorFields(m.ColumnIndex, 2) $ "IB"
						m.ColumnsData(m.ColumnIndex) = VAL(CHRTRAN(m.ColumnText, This.DecimalPoint, SET("Point")))
					CASE m.CursorFields(m.ColumnIndex, 2) == "L"
						m.ColumnsData(m.ColumnIndex) = UPPER(m.ColumnText) == UPPER(This.LogicalTrue)
					CASE m.CursorFields(m.ColumnIndex, 2) $ "DT"
						m.ColumnsData(m.ColumnIndex) = NVL(This.ScanDate(m.ColumnText, m.CursorFields(m.ColumnIndex, 2) == "T"), {})
					OTHERWISE
						m.ColumnsData(m.ColumnIndex) = m.ColumnText
					ENDCASE
				ENDFOR

				* the data is finally moved into the cursor
				SELECT (m.CursorName)
				APPEND BLANK
				GATHER MEMO FROM m.ColumnsData

				* signal the step
				RAISEEVENT(This, "ProcessStep", 3, RECNO(m.Importer), RECCOUNT(m.Importer))
			ENDSCAN

			* clean up
			USE IN (m.Importer)
			SELECT (m.CursorName)

			* everything was ok
			m.Result = 0

		CATCH TO m.ErrorHandler

			This.CloseFile()

			* something went wrong...
			m.Result = m.ErrorHandler.ErrorNo

		ENDTRY

		RETURN m.Result

	ENDFUNC

	* OpenFile (Filename)
	* open a file and set its properties
	FUNCTION OpenFile (Filename AS String) AS Boolean

		SAFETHIS

		ASSERT VARTYPE(m.Filename) == "C" MESSAGE "String parameter expected."

		LOCAL BOM AS String

		This.CloseFile()

		This.HFile = FOPEN(m.Filename)
		IF This.HFile != -1

			* get the file length
			This.FileLength = FSEEK(This.HFile, 0, 2)

			* and now the encoding (ANSI or some form of UNICODE)
			FSEEK(This.HFile, 0, 0)
			m.BOM = FREAD(This.HFile, 2)

			DO CASE
			* UNICODE LE
			CASE m.BOM == "" + 0hFFFE
				This.UTF = 1
				FSEEK(This.HFile, 1, 0)
			* UNICODE BE
			CASE m.BOM == "" + 0hFEFF
				This.UTF = 2
			* UTF-8?
			CASE m.BOM == "" + 0hEFBB AND FREAD(This.HFile, 1) == "" + 0hBF
				This.UTF = 3
			* assume ANSI
			OTHERWISE
				FSEEK(This.HFile, 0, 0)
				This.UTF = 0
			ENDCASE

			* where the read pointer is
			This.FilePosition = FSEEK(This.HFile, 0, 1)

		ENDIF

		RETURN This.HFile != -1

	ENDFUNC

	* GetLine()
	* get a line from the CSV file
	FUNCTION GetLine () AS String

		LOCAL FileContents AS String
		LOCAL CharIndex AS Integer
		LOCAL TempChar AS Character

		* signal end of file
		IF FEOF(This.HFile)
			This.FilePosition = This.FileLength
			RETURN .NULL.
		ENDIF

		* read a line from the file stream
		m.FileContents = FGETS(This.HFile, 8192)
		This.FilePosition = FSEEK(This.HFile, 0, 1)

		* word-length UNICODE characters leave a single NUL character in a partial CRLF sequence
		* 00 0D ->00<- 0A [characters of the new line] or 0D ->00<- 0A 00 [characters of the new line]
		IF INLIST(This.UTF, 1, 2) AND m.FileContents == CHR(0)
			* if so, read the line corresponding to the LF
			m.FileContents = FGETS(This.HFile, 8192)

			* if nothing more, signal EOF
			IF FEOF(This.HFile)
				RETURN .NULL.
			ENDIF
		ENDIF

		* unencode the UNICODE transformation, if needed
		DO CASE
		CASE This.UTF = 1
			* for UNICODE LE, skip the first character (the rest of the NL from the previous line, or the rest of the BOM, in the first)
			* and convert them
			m.FileContents = STRCONV(STRCONV(SUBSTR(m.FileContents, 2), 6), 2)

		CASE This.UTF = 2
			* for UNICODE BE, trim the last NUL character that is part of the NL sequence
			m.FileContents = LEFT(m.FileContents, LEN(m.FileContents) -1)
			* and swap little and big endians
			FOR m.CharIndex = 1 TO LEN(m.FileContents) STEP 2
				m.FileContents = STUFF(m.FileContents, ;
												m.CharIndex, 2, ;
												SUBSTR(m.FileContents, m.CharIndex + 1, 1) + SUBSTR(m.FileContents, m.CharIndex, 1))
			ENDFOR
			* the characters are now little endians, so convert them
			m.FileContents = STRCONV(STRCONV(m.FileContents, 6), 2)

		CASE This.UTF = 3
			* for UTF-8, use the full string
			m.FileContents = STRCONV(STRCONV(m.FileContents, 11), 2)
		ENDCASE

		RETURN m.FileContents

	ENDFUNC

	* CloseFile()
	* close the open CSV file
	PROCEDURE CloseFile

		IF This.HFile != -1
			FCLOSE(This.HFile)
			STORE - 1 TO This.HFile, This.FileLength, This.FilePosition
		ENDIF

	ENDPROC

	* ColumnType (CursornName, ColumnName)
	* calculate a field data type
	HIDDEN FUNCTION ColumnType (CursorName AS String, ColumnName AS String) AS String

		LOCAL ColumnType AS String
		LOCAL ColumnValue AS String
		LOCAL SampleSize AS Integer
		LOCAL ARRAY AdHoc(1)

		* Memo if max length of column is greater than 254
		SELECT MAX(LEN(NVL(EVALUATE(m.ColumnName), ""))) FROM (m.CursorName) INTO ARRAY AdHoc
		IF m.AdHoc > 254
			RETURN "M"
		ENDIF
		* Varchar(10) if all rows are empty or null
		IF m.AdHoc = 0
			RETURN "V10"
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Datetime
		m.ColumnType = "T"
		SCAN FOR !ISNULL(EVALUATE(m.ColumnName)) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .T.))
				* check if Date
				m.ColumnType = "D"
				EXIT
			ENDIF
			m.SampleSize = MIN(m.SampleSize - 1, -1)
		ENDSCAN
		IF m.ColumnType == "T"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Date
		SCAN FOR !ISNULL(EVALUATE(m.ColumnName)) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .F.))
				* check if logical
				m.ColumnType = "L"
				EXIT
			ENDIF
			m.SampleSize = MIN(m.SampleSize - 1, -1)
		ENDSCAN
		IF m.ColumnType == "D"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Logical
		SCAN FOR !ISNULL(EVALUATE(m.ColumnName)) AND m.SampleSize >= 0
			IF !(UPPER(EVALUATE(m.ColumnName)) == This.LogicalFalse) AND !(UPPER(EVALUATE(m.ColumnName)) == This.LogicalTrue)
				* check if Integer
				m.ColumnType = "I"
				EXIT
			ENDIF
			m.SampleSize = MIN(m.SampleSize - 1, -1)
		ENDSCAN
		IF m.ColumnType == "L"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Number
		SCAN FOR !ISNULL(EVALUATE(m.ColumnName)) AND m.SampleSize >= 0
			IF TYPE(CHRTRAN(EVALUATE(m.ColumnName), This.DecimalPoint, ".")) != "N"
				* check if Character
				m.ColumnType = "C"
				EXIT
			ENDIF
			m.SampleSize = MIN(m.SampleSize - 1, -1)
		ENDSCAN
		* but, if Number, check if Integer or Double
		IF m.ColumnType == "I"
			m.SampleSize = This.SampleSize
			SCAN FOR !ISNULL(EVALUATE(m.ColumnName)) AND m.SampleSize >= 0
				IF This.DecimalPoint $ EVALUATE(m.ColumnName) OR ABS(VAL(EVALUATE(m.ColumnName))) > 2147483647
					m.ColumnType = "B"
					EXIT
				ENDIF
				m.SampleSize = MIN(m.SampleSize - 1, -1)
			ENDSCAN
			RETURN m.ColumnType
		ENDIF

		* every other types failed, get the max length of the character field and set a Varchar() with it
		SELECT MAX(LEN(EVALUATE(m.ColumnName))) FROM (m.CursorName) INTO ARRAY AdHoc
		RETURN "V" + LTRIM(STR(m.AdHoc, 3, 0))

	ENDFUNC

	* ScanDate (Source[, IsTime])
	* scan a string and check if it is a date, against a defined pattern (return .NULL. if no date or datetime, as modelled)
	FUNCTION ScanDate (Source AS String, IsTime AS Boolean) AS DateOrDatetime

		SAFETHIS

		ASSERT VARTYPE(m.Source) == "C" AND VARTYPE(m.IsTime) == "L" ;
			MESSAGE "String and boolean parameters expected."

		IF ISNULL(m.Source)
			RETURN .NULL.
		ENDIF

		* the pattern, as being checked
		LOCAL Pattern AS String
		LOCAL ChPattern AS Character

		* the source, as being scanned
		LOCAL Scanned AS String
		LOCAL ChSource AS Character
		LOCAL ScanPart AS String

		* date and time parts
		LOCAL IsPart AS Boolean
		LOCAL PartYear, PartMonth, PartDay, PartHour, PartMinute, PartSeconds AS Integer
		LOCAL PartMeridian AS Boolean

		* add hours to 12 Hours format
		LOCAL AddHours AS Integer

		m.Pattern = IIF(m.IsTime, This.DateTimePattern, This.DatePattern)
		STORE - 1 TO m.PartYear, m.PartMonth, m.PartDay, m.PartHour, m.PartMinute, m.PartSeconds
		m.PartMeridian = .F.
		m.AddHours = 0

		m.Scanned = m.Source

		* while the pattern has not reached the end
		DO WHILE LEN(m.Pattern) > 0

			m.ChSource = LEFT(m.Scanned, 1)

			m.IsPart = .F.
			m.ChPattern = LEFT(m.Pattern, 1)

			* found a pattern part
			IF m.ChPattern == "%"
				* store it, to process it next
				m.Pattern= SUBSTR(m.Pattern, 2)
				m.ChPattern = LEFT(m.Pattern, 1)
				m.IsPart = .T.
			ENDIF
			m.Pattern= SUBSTR(m.Pattern, 2)

			DO CASE
			* if not a pattern part, source and literal characters in the pattern must match
			CASE !m.IsPart
				IF !(m.ChSource == m.ChPattern)
					RETURN .NULL.
				ENDIF
				m.Scanned = SUBSTR(m.Scanned, 2)

			* %% = %
			CASE m.ChPattern == "%"
				IF !m.ChSource == "%"
					RETURN .NULL.
				ENDIF
				m.Scanned = SUBSTR(m.Scanned, 2)

			OTHERWISE

				* a digit sets a part with fixed length (for instance, %4Y)
				IF ISDIGIT(m.ChPattern)
					m.ScanPart = LEFT(m.Scanned, VAL(m.ChPattern))
					m.Scanned = SUBSTR(m.Scanned, VAL(m.ChPattern) + 1)
					m.ChPattern = LEFT(m.Pattern, 1)
					m.Pattern = SUBSTR(m.Pattern, 2)
				ELSE
					* if not fixed, the part ends at the next literal character (or end of source string)
					m.ScanPart = STREXTRACT(m.Scanned, "", LEFT(m.Pattern, 1), 1, 2)
					m.Scanned = SUBSTR(m.Scanned, LEN(m.ScanPart) + 1)
				ENDIF

				DO CASE
				* %Y = year
				CASE m.ChPattern == "Y"
					m.PartYear = VAL(m.ScanPart)

				* %M = month number
				CASE m.ChPattern == "M"
					m.PartMonth = VAL(m.ScanPart)

				* %N = month name
				CASE m.ChPattern == "N"
					m.PartMonth = VAL(STREXTRACT(This.MonthNames, m.ScanPart + ":", ":", 1, 3))

				* %D = day
				CASE m.ChPattern == "D"
					m.PartDay = VAL(m.ScanPart)

				* %h = hours
				CASE m.ChPattern == "h"
					m.PartHour = VAL(m.ScanPart)

				* %m = minutes
				CASE m.ChPattern == "m"
					m.PartMinute = VAL(m.ScanPart)

				* %s = seconds
				CASE m.ChPattern == "s"
					m.PartSeconds = VAL(m.ScanPart)

				* %p = meridian signature
				CASE m.ChPattern == "p"
					m.PartMeridian = .T.
					IF m.ScanPart == This.AnteMeridian
						m.AddHours = 0
					ELSE
						IF m.ScanPart == This.PostMeridian
							m.AddHours = 12
						ELSE
							RETURN .NULL.
						ENDIF
					ENDIF

				* %? = ignore
				CASE m.ChPattern == "?"
					* just ignore

				* wrong pattern, return .NULL.
				OTHERWISE
					RETURN .NULL.
				ENDCASE
			ENDCASE

		ENDDO

		* nothing left to scan
		IF LEN(m.Scanned) > 0
			RETURN .NULL.
		ENDIF

		* try to return a date or a datetime
		IF !m.IsTime
			IF TYPE("DATE(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay)") == "D"
				RETURN DATE(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay)
			ENDIF
		ELSE
			IF m.PartMeridian
				IF m.AddHours = 0 AND m.PartHour = 12
					m.PartHour = 0
				ELSE
					m.PartHour = m.PartHour + m.AddHours
				ENDIF
			ENDIF
			IF TYPE("DATETIME(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay, m.PartHour, m.PartMinute, m.PartSeconds)") == "T"
				RETURN DATETIME(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay, m.PartHour, m.PartMinute, m.PartSeconds)
			ENDIF
		ENDIF

		* the parts could not evaluate to a date or a datetime
		RETURN .NULL.

	ENDFUNC

	* ProcessStep (Phase, Done, ToDo)
	* a event signaling a step on the CSV import processing
	PROCEDURE ProcessStep (Phase AS Integer, Done AS Number, ToDo AS Number)
	ENDPROC

ENDDEFINE
