*!*	CSVProcessor

*!*	A VFP class to process CSV files

* dependencies
IF _VFP.StartMode = 0
	DO LOCFILE("namer.prg")
ELSE
	DO namer.prg
ENDIF

* install itself
IF !SYS(16) $ SET("Procedure")
	SET PROCEDURE TO (SYS(16)) ADDITIVE
ENDIF

#DEFINE SAFETHIS			ASSERT !USED("This") AND TYPE("This") == "O"

#DEFINE CRLF				CHR(13) + CHR(10)

#DEFINE MAXCOLUMNS		254
#DEFINE MAXCHARSIZE		254
#DEFINE COLUMNDEFSIZE	18

DEFINE CLASS CSVProcessor AS Custom

	* a name controller to set valid cursor and field names
	ADD OBJECT NameController AS Namer

	* properties related to the resulting cursor(s)/table(s)
	CursorName = ""
	* set this property to append to an existing cursor/table
	WorkArea = ""
	* how to react when table already exists in the database
	DropExistingTable = .F.
	* Cursor fields / CSV Columns mapping collection
	ADD OBJECT FieldMapping AS Collection
	* multiple cursor support
	MultipleCursors = .F.
	ADD OBJECT MultipleCursorsNames AS Collection

	* properties related to how data is stored in the CSV file
	* the CSV file has a header row?
	HeaderRow = .T.
	* number of rows to skip, at the beginning of the file
	SkipRows = 0
	* how values are separated
	ValueSeparator = ","
	* how values are delimited
	ValueDelimiter = '"'
	* inline newlines may be delimited
	InlineDelimitedNewLine = .F.
	* how newlines are inserted in a value (.NULL. if newlines are not transformed)
	NewLine = .NULL.
	* the decimal point
	DecimalPoint = "."
	* Code page translation status while creating columns
	CPTrans = .T.
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
	* trim exported values?
	Trimmer = .T.
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
						'<memberdata name="cptrans" type="property" display="CPTrans"/>' + ;
						'<memberdata name="datepattern" type="property" display="DatePattern"/>' + ;
						'<memberdata name="datetimepattern" type="property" display="DatetimePattern"/>' + ;
						'<memberdata name="decimalpoint" type="property" display="DecimalPoint"/>' + ;
						'<memberdata name="dropexistingtable" type="property" display="DropExistingTable"/>' + ;
						'<memberdata name="fieldmapping" type="property" display="FieldMapping"/>' + ;
						'<memberdata name="filelength" type="property" display="FileLength"/>' + ;
						'<memberdata name="fileposition" type="property" display="FilePosition"/>' + ;
						'<memberdata name="headerrow" type="property" display="HeaderRow"/>' + ;
						'<memberdata name="hfile" type="property" display="HFile"/>' + ;
						'<memberdata name="inlinedelimitednewline" type="property" display="InlineDelimitedNewLine"/>' + ;
						'<memberdata name="logicalfalse" type="property" display="LogicalFalse"/>' + ;
						'<memberdata name="logicaltrue" type="property" display="LogicalTrue"/>' + ;
						'<memberdata name="monthnames" type="property" display="MonthNames"/>' + ;
						'<memberdata name="multiplecursors" type="property" display="MultipleCursors"/>' + ;
						'<memberdata name="multiplecursorsnames" type="property" display="MultipleCursorsNames"/>' + ;
						'<memberdata name="namecontroller" type="property" display="NameController"/>' + ;
						'<memberdata name="newline" type="property" display="NewLine"/>' + ;
						'<memberdata name="nullvalue" type="property" display="NullValue"/>' + ;
						'<memberdata name="postmeridian" type="property" display="PostMeridian"/>' + ;
						'<memberdata name="samplesize" type="property" display="SampleSize"/>' + ;
						'<memberdata name="skiprows" type="property" display="SkipRows"/>' + ;
						'<memberdata name="trimmer" type="property" display="Trimmer"/>' + ;
						'<memberdata name="utf" type="property" display="UTF"/>' + ;
						'<memberdata name="valuedelimiter" type="property" display="ValueDelimiter"/>' + ;
						'<memberdata name="valueseparator" type="property" display="ValueSeparator"/>' + ;
						'<memberdata name="workarea" type="property" display="WorkArea"/>' + ;
						'<memberdata name="appendtofile" type="method" display="AppendToFile"/>' + ;
						'<memberdata name="closefile" type="method" display="CloseFile"/>' + ;
						'<memberdata name="createfile" type="method" display="CreateFile"/>' + ;
						'<memberdata name="columntype" type="method" display="ColumnType"/>' + ;
						'<memberdata name="encodevalue" type="method" display="EncodeValue"/>' + ;
						'<memberdata name="getline" type="method" display="GetLine"/>' + ;
						'<memberdata name="export" type="method" display="Export"/>' + ;
						'<memberdata name="import" type="method" display="Import"/>' + ;
						'<memberdata name="openfile" type="method" display="OpenFile"/>' + ;
						'<memberdata name="processstep" type="method" display="ProcessStep"/>' + ;
						'<memberdata name="putline" type="method" display="PutLine"/>' + ;
						'<memberdata name="outputdate" type="method" display="OutputDate"/>' + ;
						'<memberdata name="outputlogical" type="method" display="OutputLogical"/>' + ;
						'<memberdata name="outputnumber" type="method" display="OutputNumber"/>' + ;
						'<memberdata name="scandate" type="method" display="ScanDate"/>' + ;
						'<memberdata name="scanlogical" type="method" display="ScanLogical"/>' + ;
						'<memberdata name="scannumber" type="method" display="ScanNumber"/>' + ;
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

		* what is read from the CSV
		LOCAL CSVFileContents AS String
		* separated by columns
		LOCAL ARRAY ColumnsData(1)
		LOCAL ARRAY ImporterData(1)
		* after being buffered
		LOCAL ARRAY ColumnsBuffer(1)
		* and sent to a target
		LOCAL TargetData AS Object
		LOCAL TargetColumn AS String
		LOCAL TargetName AS String

		* the name of the columns
		LOCAL ARRAY ColumnsNames(1)
		* the identifier (by position or name)
		LOCAL ARRAY CSVColumns(1)
		* the columns that are to be considered for importing
		LOCAL ARRAY ActiveColumns(1)
		* the field definitions
		LOCAL ARRAY CursorFields(1)
		LOCAL ARRAY ImporterFields(1)
		* how many (real) columns there are
		LOCAL ColumnsCount AS Integer
		LOCAL ImporterColumnsCount AS Integer
		LOCAL ImporterSegment AS Integer

		* the detected type of each column
		LOCAL Retype AS String
		* name and contents of a column
		LOCAL ColumnName AS String
		LOCAL BaseColumnName AS String
		LOCAL ColumnText AS String

		* the CSV file uses delimiters?
		LOCAL IsDelimited AS Boolean
		* these control their use while reading a column
		LOCAL TrailDelimiters AS Integer
		LOCAL InsideDelimiters AS Boolean

		* loop indexers
		LOCAL ImporterIndex AS Integer
		LOCAL LineIndex AS Integer
		LOCAL ColLineIndex AS Integer
		LOCAL RowIndex AS Integer
		LOCAL ColumnIndex AS Integer
		LOCAL NameIndex AS Integer
		LOCAL ExpIndex AS Integer

		* the temporary cursors that will receive the first import
		LOCAL ARRAY Importer(1)
		LOCAL ImporterCount AS Integer

		* creation flag
		LOCAL CreateCursor AS Boolean

		* anything wrong will be trapped
		LOCAL ErrorHandler AS Exception

		* 0 = OK, -1 is error reading file, > 0 other type of errors
		LOCAL Result AS Integer

		* open the file
		IF !This.OpenFile(m.Filename)
			RETURN -1
		ENDIF

		m.CreateCursor = .T.

		* derive a name for the cursor, if it was not passed as a parameter
		IF PCOUNT() = 1
			* if no work area was set, get the cursor name from the filename
			IF EMPTY(This.WorkArea)
				This.NameController.SetOriginalName(JUSTSTEM(m.FileName))
				m.CursorName = This.NameController.GetName()
			ELSE
			* otherwise, the cursor exists (that is, it must exist)
				m.CursorName = EVL(ALIAS(SELECT(This.WorkArea)), .NULL.)
				m.CreateCursor = .F.
			ENDIF
			IF ISNULL(m.CursorName)
				RETURN -1
			ENDIF
		ENDIF

		* set it, anyway, in case the caller needs it
		This.CursorName = m.CursorName

		TRY

			m.Importer(1) = .NULL.

			* skip rows, if needed
			FOR m.RowIndex = 1 TO This.SkipRows
				This.GetLine()
			ENDFOR

			* get the column names (from the CSV file) or use a Col_X pattern

			* if the CSV files has headers
			IF This.HeaderRow
				* fetch column names in first line of the CSV file
				m.CSVFileContents = This.GetLine()

				DIMENSION m.CursorFields(ALINES(m.ColumnsNames, m.CSVFileContents, 1, This.ValueSeparator), COLUMNDEFSIZE)
				m.ColumnsCount = ALEN(m.ColumnsNames)
				ACOPY(m.ColumnsNames, m.CSVColumns)

			ELSE

				* columns are not named, so create a dummy structure with max number of 254 columns (the VFP limit), for now
				DIMENSION m.CursorFields(MAXCOLUMNS, COLUMNDEFSIZE)
				DIMENSION m.ColumnsNames(MAXCOLUMNS)
				FOR m.NameIndex = 1 TO MAXCOLUMNS
					m.ColumnsNames(m.NameIndex) = "Col_" + LTRIM(STR(m.NameIndex, 3, 0))
				ENDFOR
				* the real column count will be read as data is imported
				m.ColumnsCount = 0

			ENDIF

			* clear the structure
			STORE "" TO m.CursorFields
			* fetch valid column names and check for name conformity
			FOR m.ColumnIndex = 1 TO ALEN(m.ColumnsNames)

				* names must be validated if they come from the CSV file
				IF This.HeaderRow
					* remove the delimiter, if needed
					m.ColumnName = ALLTRIM(m.ColumnsNames(m.ColumnIndex), 0, " ", NVL(This.ValueDelimiter, ""))
					* check the name against the VFP name controller
					This.NameController.SetOriginalName(m.ColumnName)
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
				m.CursorFields(m.ColumnIndex, 6) = !This.CPTrans
				* dimension, precision, etc., set to zero
				STORE 0 TO m.CursorFields(m.ColumnIndex, 3), m.CursorFields(m.ColumnIndex, 4), ;
					m.CursorFields(m.ColumnIndex, 17), m.CursorFields(m.ColumnIndex, 18)
			ENDFOR

			* get the name for the import cursor(s), based on the cursor name
			DIMENSION Importer(CEILING(ALEN(m.ColumnsNames) / MAXCOLUMNS))
			m.ImporterSegment = 1

			* create as many cursors that are needed to import the data
			FOR m.ImporterIndex = 1 TO ALEN(m.Importer)
				* get a name for the (eventually segmented) cursor
				m.Importer(m.ImporterIndex) = This._GetCursorName(m.CursorName, m.ImporterIndex)
				* get the number of columns for the cursor (254 or less, if in the last segment)
				m.ImporterColumnsCount = MIN((ALEN(m.ColumnsNames) - m.ImporterSegment) + 1, MAXCOLUMNS)
				* segment the overall structure for an importer
				DIMENSION m.ImporterFields(1)
				ACOPY(m.CursorFields, m.ImporterFields, (m.ImporterSegment - 1) * COLUMNDEFSIZE + 1, m.ImporterColumnsCount * COLUMNDEFSIZE)
				DIMENSION m.ImporterFields(m.ImporterColumnsCount, COLUMNDEFSIZE)
				* a structure is at hand, the cursor may be created
				CREATE CURSOR (m.Importer(m.ImporterIndex)) FROM ARRAY m.ImporterFields
				m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS
			ENDFOR

			* if a delimiter was set, values can be delimited
			m.IsDelimited = LEN(NVL(This.ValueDelimiter, "")) > 0
			m.InsideDelimiters = .F.

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
				DO WHILE (m.ColumnIndex <= ALEN(m.ColumnsNames) AND m.ColLineIndex <= ALEN(m.ColumnsBuffer)) OR ;
						(!This.HeaderRow AND m.ColumnIndex > ALEN(m.ColumnsNames) AND m.ColLineIndex <= ALEN(m.ColumnsBuffer))

					* update the column count, if we have now an extra column
					IF !This.HeaderRow AND m.ColumnIndex > m.ColumnsCount
						m.ColumnsCount = m.ColumnIndex
					ENDIF

					* this is the case where an headerless CSV file requires the creation of a new importer cursor
					IF m.ColumnIndex > ALEN(m.ColumnsNames)
						DIMENSION m.ColumnsNames(m.ColumnsCount + MAXCOLUMNS - 1)
						DIMENSION m.CursorFields(ALEN(m.ColumnsNames), COLUMNDEFSIZE)
						FOR m.NameIndex = m.ColumnsCount TO ALEN(m.ColumnsNames)
							m.ColumnsNames(m.NameIndex) = "Col_" + LTRIM(STR(m.NameIndex, 3, 0))
							m.CursorFields(m.NameIndex, 1) = m.ColumnsNames(m.NameIndex)
							m.CursorFields(m.NameIndex, 2) = "M"
							m.CursorFields(m.NameIndex, 5) = .T.
							m.CursorFields(m.NameIndex, 6) = !This.CPTrans
							STORE 0 TO m.CursorFields(m.NameIndex, 3), m.CursorFields(m.NameIndex, 4), ;
								m.CursorFields(m.NameIndex, 17), m.CursorFields(m.NameIndex, 18)
						ENDFOR
						m.ImporterIndex = ALEN(m.Importer) + 1
						DIMENSION m.Importer(m.ImporterIndex)
						m.Importer(m.ImporterIndex) = This._GetCursorName(m.CursorName, m.ImporterIndex)
						DIMENSION m.ImporterFields(MAXCOLUMNS, COLUMNDEFSIZE)
						ACOPY(m.CursorFields, m.ImporterFields, (m.ColumnsCount - 1) * COLUMNDEFSIZE + 1, MAXCOLUMNS * COLUMNDEFSIZE)
						CREATE CURSOR (m.Importer(m.ImporterIndex)) FROM ARRAY m.ImporterFields
					ENDIF

					* the (partial or complete) value from the CSV field
					m.ColumnText = m.ColumnsBuffer(m.ColLineIndex)
					* if it includes transformed newlines, change them back into real newlines
					IF !ISNULL(This.NewLine)
						m.ColumnText = STRTRAN(m.ColumnText, This.NewLine, CRLF)
					ENDIF
					* add it to the fetched value
					m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + m.ColumnText

					* found a delimited field?
					IF m.IsDelimited AND LEFT(m.ColumnsData(m.ColumnIndex), LEN(This.ValueDelimiter)) == This.ValueDelimiter

						m.InsideDelimiters = .T.

						m.TrailDelimiters = 0
						* check on the case where the field may end wth a bunch of delimiters...
						IF LEN(m.ColumnsData(m.ColumnIndex)) > 1
							DO WHILE RIGHT(m.ColumnText, LEN(This.ValueDelimiter)) == This.ValueDelimiter
								m.TrailDelimiters = m.TrailDelimiters + 1
								m.ColumnText = LEFT(m.ColumnText, LEN(m.ColumnText) - LEN(This.ValueDelimiter))
							ENDDO
						ENDIF

						DO CASE
						* empty delimited field..
						CASE EMPTY(m.ColumnText) AND m.TrailDelimiters = 2
							m.ColumnsData(m.ColumnIndex) = ""
							m.InsideDelimiters = .F.
	
						* if the field ended with a delimiter
						CASE RIGHT(m.ColumnsData(m.ColumnIndex), LEN(This.ValueDelimiter)) == This.ValueDelimiter AND (m.TrailDelimiters / 2) != INT(m.TrailDelimiters / 2)
							* remove the delimiters from the column data, at the beginning and at the end of the field
							m.ColumnsData(m.ColumnIndex) = SUBSTR(m.ColumnsData(m.ColumnIndex), LEN(This.ValueDelimiter) + 1, LEN(m.ColumnsData(m.ColumnIndex)) - (LEN(This.ValueDelimiter) + 1))
							* and also in the middle
							m.ColumnsData(m.ColumnIndex) = STRTRAN(m.ColumnsData(m.ColumnIndex), REPLICATE(This.ValueDelimiter, 2), This.ValueDelimiter)
							m.InsideDelimiters = .F.

						OTHERWISE
							* if not, it was a separator that broke the columns, so add it
							IF m.ColLineIndex < ALEN(m.ColumnsBuffer)
								m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + This.ValueSeparator
							ENDIF
							* and continue to fill the current data column from the next CSV column
							m.ColLineIndex = m.ColLineIndex + 1
							LOOP
						ENDCASE
					ENDIF

					* fetch more columns...
					IF m.ColLineIndex < ALEN(m.ColumnsBuffer)
						IF !m.InsideDelimiters
							m.ColumnIndex = m.ColumnIndex + 1
						ELSE
							* SET STEP ON
						ENDIF
					ENDIF
					m.ColLineIndex = m.ColLineIndex + 1
				ENDDO

				* if there are set columns, and they were not completely fetched from the previous line,
				* there is a line break that must be inserted, and the rest of the column, and of the columns,
				* to be imported from the next line(s)
				IF This.HeaderRow AND (m.ColumnIndex < ALEN(m.ColumnsNames) OR m.InsideDelimiters)

					m.ColumnsData(m.ColumnIndex) = m.ColumnsData(m.ColumnIndex) + CRLF

				ELSE

					* the line is completely read
					FOR m.ColumnIndex = 1 TO ALEN(m.ColumnsNames)
						* .NULL.ify, if needed
						IF m.ColumnsData(m.ColumnIndex) == This.NullValue
							m.ColumnsData(m.ColumnIndex) = .NULL.
						ELSE
							* remove delimited newlines 
							IF This.InlineDelimitedNewLine AND ;
									This.ValueDelimiter + CRLF + This.ValueDelimiter $ m.ColumnsData(m.ColumnIndex)
								m.ColumnsData(m.ColumnIndex) = STRTRAN(m.ColumnsData(m.ColumnIndex), ;
									This.ValueDelimiter + CRLF + This.ValueDelimiter, CRLF)
							ENDIF
						ENDIF
					ENDFOR

					* insert the data into the import cursor(s)
					m.ImporterSegment = 1
					FOR m.ImporterIndex = 1 TO ALEN(m.Importer)
						SELECT (m.Importer(m.ImporterIndex))
						APPEND BLANK
						* select a bunch from the CSV columns to import into the cursor(s)
						DIMENSION m.ImporterData(MIN(MAXCOLUMNS, ALEN(m.ColumnsData) - m.ImporterSegment + 1))
						ACOPY(m.ColumnsData, m.ImporterData, m.ImporterSegment, ALEN(m.ImporterData))
						GATHER FROM m.ImporterData MEMO
						m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS
					ENDFOR

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
			DIMENSION m.CursorFields(m.ColumnsCount, COLUMNDEFSIZE)
			DIMENSION m.ColumnsNames(m.ColumnsCount)

			* determine the type and length of each column
			FOR m.ColumnIndex = 1 TO m.ColumnsCount

				m.ImporterIndex = INT((m.ColumnIndex - 1) / MAXCOLUMNS) + 1

				* change the Memo to something else, if needed / possible
				TRY
					DO CASE
					CASE m.CreateCursor
						m.Retype = This.ColumnType(m.Importer(m.ImporterIndex), m.ColumnsNames(m.ColumnIndex))
					CASE This.FieldMapping.Count = 0
						m.Retype = TYPE(This.WorkArea + "." + FIELD(m.ColumnIndex, This.WorkArea))
					CASE EMPTY(This.FieldMapping.GetKey(1))
						m.Retype = TYPE(This.WorkArea + "." + FIELD(This.FieldMapping.Item(m.ColumnIndex), This.WorkArea))
					OTHERWISE
						m.Retype = TYPE(This.WorkArea + "." + FIELD(This.FieldMapping.Item(m.CSVColumns(m.ColumnIndex)), This.WorkArea))
					ENDCASE
				CATCH
					m.Retype = "U"
				ENDTRY
					
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
				CASE m.Retype == "B" OR m.Retype == "Y" OR m.Retype == "N"
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

			IF m.CreateCursor

				* if returning a single cursor or importing into a database table...
				IF !This.MultipleCursors OR PCOUNT() = 3

					* consider no more than 254 CSV columns if the cursor was being created
					DIMENSION m.CursorFields(MIN(m.ColumnsCount, MAXCOLUMNS), COLUMNDEFSIZE)
					m.ImporterCount = 1

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
							* if it exists and must not be dropped, assume it's properly prepared for import
							IF This.DropExistingTable
								DROP TABLE (m.CursorName)
								CREATE TABLE (m.CursorName) FROM ARRAY m.CursorFields
							ENDIF
						ELSE
							CREATE TABLE (m.CursorName) FROM ARRAY m.CursorFields
						ENDIF
					ENDIF

				* if returning multiple cursors
				ELSE

					m.ImporterCount = ALEN(m.Importer)
					This.MultipleCursorsNames.Remove(-1)
					m.ImporterSegment = 1
					* create all required cursors to receive the CSV data
					FOR m.ImporterIndex = 1 TO m.ImporterCount
						* find an available name for it
						m.TargetName = This._GetCursorName(m.CursorName, m.ImporterIndex, .T.)
						This.MultipleCursorsNames.Add(m.TargetName)
						* and prepare the segmented structure (part of the cursor fields definitin)
						m.ImporterColumnsCount = MIN((ALEN(m.ColumnsNames) - ((m.ImporterIndex - 1) * MAXCOLUMNS)), MAXCOLUMNS)
						DIMENSION m.ImporterFields(1)
						ACOPY(m.CursorFields, m.ImporterFields, m.ImporterSegment, m.ImporterColumnsCount * COLUMNDEFSIZE)
						DIMENSION m.ImporterFields(m.ImporterColumnsCount, COLUMNDEFSIZE)
						* create the cursor and continue
						CREATE CURSOR (m.TargetName) FROM ARRAY m.ImporterFields
						m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS * COLUMNDEFSIZE
					ENDFOR

				ENDIF

			ELSE

				* consider all importer cursors when appending to an existing cursor
				m.ImporterCount = ALEN(m.Importer)

			ENDIF			

			* phase 3: move the imported data to the cursor(s)
			DIMENSION m.ActiveColumns(m.ColumnsCount)
			STORE .T. TO m.ActiveColumns

			* the first import cursor will be used as the reference (by RECNO()) for all import cursors
			* if there are more than one
			SELECT (m.Importer(1))
			SCAN

				m.RowIndex = RECNO()
				m.ImporterSegment = 0

				FOR m.ImporterIndex = 1 TO m.ImporterCount

					SELECT (m.Importer(m.ImporterIndex))
					GO (m.RowIndex)

					* move importer data into an array
					DIMENSION m.ColumnsData(1)
					SCATTER MEMO TO m.ColumnsData

					* if appending, importer data will go to the cursor already created
					IF !m.CreateCursor AND m.ImporterIndex = 1
						SELECT (m.CursorName)
						SCATTER MEMO BLANK NAME m.TargetData
					ENDIF

					* evaluate the memo, and reset the value with its (new) data type
					FOR m.ColumnIndex = 1 TO ALEN(m.ColumnsData)

						* skip the column if it has been deactivated
						IF !m.ActiveColumns(m.ColumnIndex + m.ImporterSegment)
							LOOP
						ENDIF

						m.ColumnText = m.ColumnsData(m.ColumnIndex)

						TRY
							DO CASE
							CASE m.CreateCursor
								m.TargetColumn = IIF(m.ColumnIndex + m.ImporterSegment <= m.ColumnsCount, "m.ColumnsData(m.ColumnIndex)", "")
							CASE This.FieldMapping.Count = 0
								m.TargetColumn = "m.TargetData." + FIELD(m.ColumnIndex, This.WorkArea)
							CASE EMPTY(This.FieldMapping.GetKey(1))
								m.TargetColumn = "m.TargetData." + FIELD(This.FieldMapping.Item(m.ColumnIndex + m.ImporterSegment), This.WorkArea)
							OTHERWISE
								m.TargetColumn = "m.TargetData." + FIELD(This.FieldMapping.Item(m.CSVColumns(m.ColumnIndex + m.ImporterSegment)), This.WorkArea)
							ENDCASE
						CATCH
							m.TargetColumn = ""
						ENDTRY

						DO CASE
						CASE EMPTY(m.TargetColumn) OR m.TargetColumn == "m.TargetData." OR TYPE(m.TargetColumn) == "U"
							* field not mapped, source column may be deactivated
							m.ActiveColumns(m.ColumnIndex + m.ImporterSegment) = .F.
						CASE ISNULL(m.ColumnText)
							&TargetColumn. = .NULL.
						CASE m.CursorFields(m.ColumnIndex, 2) $ "IB"
							&TargetColumn. = NVL(This.ScanNumber(m.ColumnText), 0)
						CASE m.CursorFields(m.ColumnIndex, 2) == "L"
							&TargetColumn. = NVL(This.ScanLogical(m.ColumnText), .F.)
						CASE m.CursorFields(m.ColumnIndex, 2) $ "DT"
							&TargetColumn. = NVL(This.ScanDate(m.ColumnText, m.CursorFields(m.ColumnIndex, 2) == "T"), {})
						OTHERWISE
							&TargetColumn. = m.ColumnText
						ENDCASE
					ENDFOR

					* the data is finally moved into the cursor(s)
					IF !This.MultipleCursors OR !m.CreateCursor
						SELECT (m.CursorName)
						IF m.ImporterIndex = 1
							APPEND BLANK
						ENDIF
					ELSE
						SELECT (This.MultipleCursorsNames(m.ImporterIndex))
						APPEND BLANK
					ENDIF
					IF m.CreateCursor
						GATHER MEMO FROM m.ColumnsData
					ELSE
						GATHER MEMO NAME m.TargetData
					ENDIF

					m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS

				ENDFOR

				* signal the step
				RAISEEVENT(This, "ProcessStep", 3, m.RowIndex, RECCOUNT(m.Importer(1)))

			ENDSCAN

			* clean up
			FOR m.ImporterIndex = 1 TO ALEN(m.Importer)
				USE IN SELECT(m.Importer(m.ImporterIndex))
			ENDFOR
			IF !This.MultipleCursors OR !m.CreateCursor
				SELECT (m.CursorName)
			ELSE
				SELECT (This.MultipleCursorsNames(1))
			ENDIF
			IF m.CreateCursor
				GO TOP
			ENDIF

			* everything was ok
			m.Result = 0

		CATCH TO m.ErrorHandler

			This.CloseFile()

			IF !ISNULL(m.Importer(1))
				FOR m.ImporterIndex = 1 TO ALEN(m.Importer)
					USE IN SELECT(m.Importer(m.ImporterIndex))
				ENDFOR
			ENDIF

			* something went wrong...
			m.Result = m.ErrorHandler.ErrorNo

		ENDTRY

		RETURN m.Result

	ENDFUNC

	* Export (Filename[, AllRecords[, Append]])
	* export a cursor to a CSV file
	FUNCTION Export (Filename AS String, AllRecords AS Boolean, Append AS Boolean) AS Integer

		SAFETHIS

		ASSERT VARTYPE(m.Filename) + VARTYPE(m.AllRecords) + VARTYPE(m.Append) == "CLL" ;
			MESSAGE "String and boolean parameters expected."

		LOCAL WArea AS String
		LOCAL LastWArea AS Integer
		LOCAL CurrentRecno AS Integer
		LOCAL CSVFileContents AS String
		LOCAL ColumnIndex AS Integer
		LOCAL ColumnValue AS String
		LOCAL ColumnData AS Expression
		LOCAL RowIndex AS Integer
		LOCAL OutputFields AS Collection

		LOCAL ErrorHandler AS Exception
		LOCAL Result AS Integer

		* create the file or open for append
		IF (!m.Append AND !This.CreateFile(m.Filename)) OR (m.Append AND !This.AppendToFile(m.Filename))
			RETURN -1
		ENDIF

		TRY

			m.LastWArea = SELECT()

			* select the cursor (if none set, use the current area)
			m.WArea = EVL(This.WorkArea, ALIAS())

			* after being exported, the record pointer will be restored
			m.CurrentRecno = RECNO(m.WArea)

			* a collection keyed by field name, having for value the CSV column name
			m.OutputFields = CREATEOBJECT("Collection")
			* use the field mapping collection to map or filter the columns to export
			IF This.FieldMapping.Count != 0
				FOR m.ColumnIndex = 1 TO This.FieldMapping.Count
					m.OutputFields.Add(EVL(This.FieldMapping.GetKey(m.ColumnIndex), This.FieldMapping.Item(m.ColumnIndex)), This.FieldMapping.Item(m.ColumnIndex))
				ENDFOR
			ELSE
				* otherwise, all fields will be exported with the same column name
				FOR m.ColumnIndex = 1 TO FCOUNT(m.WArea)
					m.OutputFields.Add(FIELD(m.ColumnIndex, m.WArea, 0), FIELD(m.ColumnIndex, m.WArea, 0))
				ENDFOR
			ENDIF

			* skip rows, if needed
			FOR m.RowIndex = 1 TO This.SkipRows
				This.PutLine("")
			ENDFOR

			* if there is a header row
			IF This.HeaderRow

				m.CSVFileContents = ""

				* export the column names
				FOR m.ColumnIndex = 1 TO m.OutputFields.Count
					m.ColumnValue = This.EncodeValue(m.OutputFields.Item(m.ColumnIndex))
					m.CSVFileContents = m.CSVFileContents + IIF(m.ColumnIndex > 1, This.ValueSeparator, "") + m.ColumnValue
				ENDFOR

				This.PutLine(m.CSVFileContents)

			ENDIF

			SELECT (m.WArea)
			* if all records are to be exported, start at the beginnig, otherwise start at the curremt position
			IF m.AllRecords
				GO TOP
			ENDIF

			* and from there on...
			SCAN REST

				* the row contents
				m.CSVFileContents = ""

				* go through all output fields (set previously)
				FOR m.ColumnIndex = 1 TO m.OutputFields.Count

					* identifiy the field that will be used as source
					m.ColumnData = m.WArea + "." + m.OutputFields.GetKey(m.ColumnIndex)
					* and set the output value, depending on the source data type
					DO CASE
					CASE TYPE(m.ColumnData) $ "NY"
						m.ColumnValue = This.OutputNumber(EVALUATE(m.ColumnData))
					CASE TYPE(m.ColumnData) == "L"
						m.ColumnValue = This.OutputLogical(EVALUATE(m.ColumnData))
					CASE TYPE(m.ColumnData) $ "DT"
						m.ColumnValue = This.OutputDate(EVALUATE(m.ColumnData))
					OTHERWISE
						m.ColumnValue = TRANSFORM(NVL(EVALUATE(m.ColumnData), NVL(This.NullValue, "")))
					ENDCASE

					* finally, encode the value
					m.ColumnValue = This.EncodeValue(m.ColumnValue)
					* and add to the row contents
					m.CSVFileContents = m.CSVFileContents + IIF(m.ColumnIndex > 1, This.ValueSeparator, "") + m.ColumnValue
				ENDFOR

				* finally, write the row contents into the file
				This.PutLine(m.CSVFileContents)

			ENDSCAN

			* restore the record pointer, if possible
			IF BETWEEN(m.CurrentRecno, 1, RECCOUNT(m.WArea))
				GO RECORD m.CurrentRecno IN m.WArea
			ENDIF

			SELECT (m.LastWArea)

			* close the file
			This.CloseFile()

			m.Result = 0

		CATCH TO m.ErrorHandler

			This.CloseFile()
			m.Result = m.ErrorHandler.ErrorNo

		ENDTRY

		RETURN m.Result

	ENDFUNC

	* EncodeValue (Unencoded)
	* encode the value, and protect it from ambiguity
	FUNCTION EncodeValue (Unencoded AS String) AS String

		LOCAL Encoded AS String

		* if requested, trim the value
		m.Encoded = IIF(This.Trimmer, ALLTRIM(m.Unencoded), m.Unencoded)
		* and transform newlines
		IF !ISNULL(This.NewLine)
			m.Encoded = STRTRAN(m.Encoded, CRLF, This.NewLine)
		ENDIF
		* double the delimiters, if present
		m.Encoded = STRTRAN(m.Encoded, This.ValueDelimiter, REPLICATE(This.ValueDelimiter, 2))
		* if the value includes the separator or CR or LF, surround the value with the value delimiter
		IF This.ValueSeparator $ m.Encoded OR CHR(13) $ m.Encoded OR CHR(10) $ m.Encoded
			m.Encoded = This.ValueDelimiter + m.Encoded + This.ValueDelimiter
		ENDIF

		RETURN m.Encoded

	ENDFUNC

	* OpenFile (Filename)
	* open a file and set its properties
	FUNCTION OpenFile (Filename AS String) AS Boolean

		SAFETHIS

		ASSERT VARTYPE(m.Filename) == "C" MESSAGE "String parameter expected."

		LOCAL BOM AS String
		LOCAL TempBuffer AS String

		This.CloseFile()

		TRY
			m.TempBuffer = FILETOSTR(m.Filename)
		CATCH
			m.TempBuffer = .NULL.
		ENDTRY

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
			* UTF-8 no BOM?
			CASE !ISNULL(m.TempBuffer) AND !(LEN(STRCONV(m.TempBuffer, 9)) == LEN(m.TempBuffer)) AND STRCONV(STRCONV(m.TempBuffer, 12), 10) == m.TempBuffer
				This.UTF = 4
				FSEEK(This.HFile, 0, 0)
			* leave the UTF property as it was set
			OTHERWISE
				FSEEK(This.HFile, 0, 0)
			ENDCASE

			* where the read pointer is
			This.FilePosition = FSEEK(This.HFile, 0, 1)

		ENDIF

		RETURN This.HFile != -1

	ENDFUNC

	* CreateFile (Filename)
	* create a file
	FUNCTION CreateFile (Filename AS String) AS Boolean

		SAFETHIS

		ASSERT VARTYPE(m.Filename) == "C" MESSAGE "String parameter expected."

		This.CloseFile()

		This.HFile = FCREATE(m.Filename)
		IF This.HFile != -1

			* prepare a BOM, depending on the UTF property setting

			DO CASE
			* UNICODE LE
			CASE This.UTF = 1
				FWRITE(This.HFile, "" + 0hFFFE)
			* UNICODE BE
			CASE This.UTF = 2
				FWRITE(This.HFile, "" + 0hFEFF)
			* UTF-8?
			CASE This.UTF = 3
				FWRITE(This.HFile, "" + 0hEFBBBF)
			* for ANSI or no BOM, just let it be
			ENDCASE

		ENDIF

		RETURN This.HFile != -1

	ENDFUNC

	* AppendToFile (Filename)
	* open a file for appending
	FUNCTION AppendToFile (Filename AS String) AS Boolean

		SAFETHIS

		ASSERT VARTYPE(m.Filename) == "C" MESSAGE "String parameter expected."

		LOCAL ARRAY FileExist(1)

		IF ADIR(m.FileExist, m.Filename) = 0
			RETURN This.CreateFile(m.Filename)
		ENDIF

		This.CloseFile()

		This.HFile = FOPEN(m.Filename, 12)
		IF This.HFile != -1 AND FSEEK(This.HFile, 0, 2) = 0

			* prepare a BOM, depending on the UTF property setting

			DO CASE
			* UNICODE LE
			CASE This.UTF = 1
				FWRITE(This.HFile, "" + 0hFFFE)
			* UNICODE BE
			CASE This.UTF = 2
				FWRITE(This.HFile, "" + 0hFEFF)
			* UTF-8?
			CASE This.UTF = 3
				FWRITE(This.HFile, "" + 0hEFBBBF)
			* for ANSI or no BOM, just let it be
			ENDCASE

		ENDIF

		RETURN This.HFile != -1

	ENDFUNC

	* GetLine()
	* get a line from the CSV file
	FUNCTION GetLine () AS String

		SAFETHIS

		LOCAL FileContents AS String
		LOCAL LinePart AS String
		LOCAL CharIndex AS Integer
		LOCAL TempChar AS Character

		* signal end of file
		IF FEOF(This.HFile)
			This.FilePosition = This.FileLength
			RETURN .NULL.
		ENDIF

		* read a line from the file stream
		m.FileContents = ""
		m.LinePart = FGETS(This.HFile, 8192)
		DO WHILE LEN(m.LinePart) = 8192 AND !FEOF(This.HFile)
			m.FileContents = m.FileContents + m.LinePart
			m.LinePart = FGETS(This.HFile, 8192)
		ENDDO
		m.FileContents = m.FileContents + m.LinePart
			
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

		CASE INLIST(This.UTF, 3, 4)
			* for UTF-8, use the full string
			* but check approximations to quotes in the conversion, first, and protect the result by doubling the result character
			IF This.ValueDelimiter == '"'
				m.FileContents = STRTRAN(m.FileContents, '”', '""')
			ENDIF
			m.FileContents = STRCONV(STRCONV(m.FileContents, 11), 2)
		ENDCASE

		RETURN m.FileContents

	ENDFUNC

	* PutLine()
	* put a line into the CSV file
	FUNCTION PutLine (Contents AS String) AS Boolean

		SAFETHIS

		LOCAL FileContents AS String
		LOCAL CharIndex AS Integer
		LOCAL TempChar AS Character

		* the line ends with a CRLF combination
		m.FileContents = m.Contents + CRLF

		DO CASE
		* UNICODE?
		CASE INLIST(This.UTF, 1, 2)
			* convert to UNICODE
			m.FileContents = STRCONV(STRCONV(m.FileContents, 1), 5)

			IF This.UTF = 2		&& UNICODE BE? Exchange high order with low order bytes
				FOR m.CharIndex = 1 TO LEN(m.FileContents) STEP 2
					m.FileContents = STUFF(m.FileContents, ;
												m.CharIndex, 2, ;
												SUBSTR(m.FileContents, m.CharIndex + 1, 1) + SUBSTR(m.FileContents, m.CharIndex, 1))
				ENDFOR
			ENDIF

		* UFT-8?
		CASE INLIST(This.UTF, 3, 4)
			* convert to UTF-8
			m.FileContents = STRCONV(STRCONV(m.FileContents, 1), 9)
		ENDCASE

		* write the line
		RETURN FWRITE(This.HFile, m.FileContents) = LEN(m.FileContents)

	ENDFUNC

	* CloseFile()
	* close the open CSV file
	PROCEDURE CloseFile

		SAFETHIS

		IF This.HFile != -1
			FCLOSE(This.HFile)
			STORE - 1 TO This.HFile, This.FileLength, This.FilePosition
		ENDIF

	ENDPROC

	* ColumnType (CursornName, ColumnName)
	* calculate a field data type
	HIDDEN FUNCTION ColumnType (CursorName AS String, ColumnName AS String) AS String

		LOCAL ColumnType AS String
		LOCAL SampleSize AS Integer
		LOCAL NumberValue AS Number
		LOCAL ARRAY AdHoc(1)

		SELECT (m.CursorName)

		* Memo if max length of column is greater than 254
		SELECT MAX(LEN(NVL(EVALUATE(m.ColumnName), ""))) FROM (m.CursorName) INTO ARRAY AdHoc
		IF m.AdHoc > MAXCHARSIZE
			RETURN "M"
		ENDIF
		* Varchar(10) if all rows are empty or null
		IF m.AdHoc = 0
			RETURN "V10"
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Datetime
		m.ColumnType = "T"
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .T.))
				* check if Date
				m.ColumnType = "D"
				EXIT
			ENDIF
			m.SampleSize = m.SampleSize - IIF(m.SampleSize > 1, 1, IIF(m.SampleSize = 1, 2, 0))
		ENDSCAN
		IF m.ColumnType == "T"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Date
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .F.))
				* check if logical
				m.ColumnType = "L"
				EXIT
			ENDIF
			m.SampleSize = m.SampleSize - IIF(m.SampleSize > 1, 1, IIF(m.SampleSize = 1, 2, 0))
		ENDSCAN
		IF m.ColumnType == "D"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Logical
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			IF ISNULL(This.ScanLogical(EVALUATE(m.ColumnName)))
				* check if Integer
				m.ColumnType = "I"
				EXIT
			ENDIF
			m.SampleSize = m.SampleSize - IIF(m.SampleSize > 1, 1, IIF(m.SampleSize = 1, 2, 0))
		ENDSCAN
		IF m.ColumnType == "L"
			RETURN m.ColumnType
		ENDIF

		m.SampleSize = This.SampleSize
		* if any value is not Number
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			m.NumberValue = This.ScanNumber(EVALUATE(m.ColumnName))
			IF ISNULL(m.NumberValue)
				* it is a character
				m.ColumnType = "V"
				EXIT
			ENDIF
			* but, if Number, check if Integer or Double
			IF m.ColumnType == "I" AND (m.NumberValue != INT(m.NumberValue) OR ABS(m.NumberValue) > 2147483647)
				m.ColumnType = "B"
			ENDIF
			m.SampleSize = m.SampleSize - IIF(m.SampleSize > 1, 1, IIF(m.SampleSize = 1, 2, 0))
		ENDSCAN
		IF m.ColumnType $ "IB"
			RETURN m.ColumnType
		ENDIF

		* every other types failed, get the max length of the character field and set a Varchar() with it
		SELECT MAX(LEN(EVALUATE(m.ColumnName))) FROM (m.CursorName) INTO ARRAY AdHoc
		RETURN m.ColumnType + LTRIM(STR(m.AdHoc, 3, 0))

	ENDFUNC

	* ScanNumber (Source)
	* scan a string and check if it represents a number
	FUNCTION ScanNumber (Source AS String) AS Number

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "CX" ;
			MESSAGE "String parameter expected."

		LOCAL CleanSource AS String
		LOCAL CleanSource2 AS String
		LOCAL Symbols AS String

		IF ISNULL(m.Source) OR TYPE(CHRTRAN(m.Source, This.DecimalPoint, ".")) != "N"
			RETURN .NULL.
		ENDIF

		m.CleanSource = ALLTRIM(m.Source)
		m.CleanSource2 = SUBSTR(m.CleanSource, 2)
		m.Symbols = CHRTRAN(m.CleanSource, "0123456789+-eE" + This.DecimalPoint, "")
		IF LEN(m.Symbols) > 0 OR ;
				("-" $ m.CleanSource2 AND ATC("e", m.CleanSource) != AT("-", m.CleanSource2)) OR ;
				("+" $ m.CleanSource2 AND ATC("e", m.CleanSource) != AT("+", m.CleanSource2))
			RETURN .NULL.
		ENDIF
		
		RETURN VAL(CHRTRAN(m.Source, This.DecimalPoint, SET("Point")))

	ENDFUNC

	* OutputNumber (Source)
	* output a number
	FUNCTION OutputNumber (Source AS Number) AS String

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "NX" ;
			MESSAGE "Number parameter expected."

		IF ISNULL(m.Source)
			RETURN NVL(This.NullValue, "")
		ENDIF

		RETURN CHRTRAN(TRANSFORM(m.Source), SET("Point"), This.DecimalPoint)

	ENDFUNC

	* ScanLogical (Source)
	* scan a string and check if it represents a logical value
	FUNCTION ScanLogical (Source AS String) AS Boolean

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "CX" ;
			MESSAGE "String parameter expected."

		DO CASE
		CASE ISNULL(m.Source)
			RETURN .NULL.
		CASE UPPER(m.Source) == This.LogicalFalse
			RETURN .F.
		CASE UPPER(m.Source) == This.LogicalTrue
			RETURN .T.
		OTHERWISE
			RETURN .NULL.
		ENDCASE

	ENDFUNC

	* OutputLogical (Source)
	* output a logical value
	FUNCTION OutputLogical (Source AS Boolean) AS String

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "LX" ;
			MESSAGE "Logical parameter expected."

		DO CASE
		CASE ISNULL(m.Source)
			RETURN NVL(This.NullValue, "")
		CASE m.Source
			RETURN NVL(This.LogicalTrue, "True")
		OTHERWISE
			RETURN NVL(This.LogicalFalse, "False")
		ENDCASE

	ENDFUNC

	* ScanDate (Source[, IsTime])
	* scan a string and check if it represents a date, against a defined pattern (return .NULL. if no date or datetime, as modelled)
	FUNCTION ScanDate (Source AS String, IsTime AS Boolean) AS DateOrDatetime

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "CX" AND VARTYPE(m.IsTime) == "L" ;
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

		* the result
		LOCAL Result AS DateOrDatetime

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
				* store it, to process next
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

		* something left to scan?
		IF LEN(m.Scanned) > 0
			RETURN .NULL.
		ENDIF

		* try to return a date or a datetime
		TRY
			IF !m.IsTime
				m.Result = DATE(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay)
			ELSE
				IF m.PartMeridian
					IF m.AddHours = 0 AND m.PartHour = 12
						m.PartHour = 0
					ELSE
						m.PartHour = m.PartHour + m.AddHours
					ENDIF
				ENDIF
				m.Result = DATETIME(m.PartYear + This.CenturyYears, m.PartMonth, m.PartDay, m.PartHour, m.PartMinute, m.PartSeconds)
			ENDIF
		CATCH
			* the parts could not evaluate to a date or a datetime
			m.Result = .NULL.
		ENDTRY

		RETURN m.Result

	ENDFUNC

	* OutputDate (Source)
	* output a date or datetime
	FUNCTION OutputDate (Source AS DateOrDatetime) AS String

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "DTX" ;
			MESSAGE "Date or Datetime parameter expected."

		IF ISNULL(m.Source)
			RETURN NVL(This.NullValue, "")
		ENDIF

		IF EMPTY(m.Source)
			RETURN ""
		ENDIF

		* the pattern, as being checked
		LOCAL Pattern AS String
		LOCAL ChPattern AS Character

		* date and time parts
		LOCAL IsPart AS Boolean
		LOCAL Mask AS String
		LOCAL DatePart AS Integer
		LOCAL PartMeridian AS Boolean

		* the result
		LOCAL Result AS String
		LOCAL ResultAltPM AS String
		LOCAL PM AS Boolean
		LOCAL Added AS String
		LOCAL AddedHours AS Boolean

		m.Pattern = IIF(VARTYPE(m.Source) == "T", This.DateTimePattern, This.DatePattern)
		m.PartMeridian = .F.
		m.AddHours = 0

		m.Result = ""
		m.ResultAltPM = .NULL.
		m.PM = .F.

		* while the pattern has not reached the end
		DO WHILE LEN(m.Pattern) > 0

			m.AddedHours = .F.

			m.IsPart = .F.
			m.ChPattern = LEFT(m.Pattern, 1)

			* found a pattern part
			IF m.ChPattern == "%"
				* store it, to process next
				m.Pattern = SUBSTR(m.Pattern, 2)
				m.ChPattern = LEFT(m.Pattern, 1)
				m.IsPart = .T.
			ENDIF
			m.Pattern = SUBSTR(m.Pattern, 2)

			DO CASE

			* if not a pattern part, output the character in the pattern
			CASE !m.IsPart
				m.Added = m.ChPattern

			* %% = %
			CASE m.ChPattern == "%"
				m.Added = "%"

			OTHERWISE

				* a digit sets a part with fixed length (for instance, %4Y)
				IF ISDIGIT(m.ChPattern)
					m.Mask = "@L " + REPLICATE("9", VAL(m.ChPattern))
					m.ChPattern = LEFT(m.Pattern, 1)
					m.Pattern = SUBSTR(m.Pattern, 2)
				ELSE
					m.Mask = ""
				ENDIF

				DO CASE
				* %Y = year
				CASE m.ChPattern == "Y"
					m.DatePart = YEAR(m.Source) - This.CenturyYears

				* %M = month number
				CASE m.ChPattern == "M"
					m.DatePart = MONTH(m.Source)

				* %N = month name
				CASE m.ChPattern == "N"
					m.DatePart = -1
					m.Added = STREXTRACT(":" + This.MonthNames + ":", ":", ":" + LTRIM(STR(MONTH(m.Source), 2, 0)) + ":") 

				* %D = day
				CASE m.ChPattern == "D"
					m.DatePart = DAY(m.Source)

				* %h = hours
				CASE m.ChPattern == "h"
					m.DatePart = HOUR(m.Source)
					IF m.DatePart >= 12 AND ISNULL(m.ResultAltPM)
						m.ResultAltPM = m.Result
						m.PM = .T.
						m.AddedHours = .T.
					ENDIF

				* %m = minutes
				CASE m.ChPattern == "m"
					m.DatePart = MINUTE(m.Source)

				* %s = seconds
				CASE m.ChPattern == "s"
					m.DatePart = SEC(m.Source)

				* %p = meridian signature
				CASE m.ChPattern == "p"
					m.DatePart = -1
					IF m.PM
						m.Added = This.PostMeridian
						m.Result = m.ResultAltPM
						m.ResultAltPM = .NULL.
					ELSE
						m.Added = This.AnteMeridian
					ENDIF

				* %? = ignore
				CASE m.ChPattern == "?"
					* just ignore
					m.DatePart = -1
					m.Added = ""

				* wrong pattern, return .NULL.
				OTHERWISE
					RETURN .NULL.
				ENDCASE

				* construct the date part, if it wasn't already set
				IF m.DatePart != -1
					IF m.AddedHours
						m.DatePart = m.DatePart - 12
					ENDIF
					IF EMPTY(m.Mask)
						m.Added = LTRIM(STR(m.DatePart, 4, 0))
					ELSE
						m.Added = TRANSFORM(m.DatePart, m.Mask)
					ENDIF
				ENDIF

			ENDCASE

			* add to the result and, if active, to the alternative PM result
			m.Result = m.Result + m.Added
			IF !ISNULL(m.ResultAltPM)
				m.ResultAltPM = m.ResultAltPM + m.Added
			ENDIF

		ENDDO

		RETURN m.Result

	ENDFUNC

	* ProcessStep (Phase, Done, ToDo)
	* a event signaling a step on the CSV import processing
	PROCEDURE ProcessStep (Phase AS Integer, Done AS Number, ToDo AS Number)
	ENDPROC

	* get a name for the import cursor
	HIDDEN FUNCTION _GetCursorName (BaseName AS String, CursorIndex AS Integer, IsTarget AS Boolean)

		LOCAL ExpIndex AS Integer
		LOCAL CursorName AS String
		LOCAL ImporterPrefix AS String

		m.ImporterPrefix = IIF(m.IsTarget, "", "_")

		m.ExpIndex = 1
		m.CursorName = TEXTMERGE("<<m.ImporterPrefix>><<m.BaseName>>_<<INT(m.CursorIndex)>>")
		DO WHILE USED(m.CursorName)
			m.CursorName = TEXTMERGE("<<m.ImporterPrefix>><<m.BaseName>>_<<INT(m.CursorIndex)>>_<<INT(m.ExpIndex)>>")
			m.ExpIndex = m.ExpIndex + 1
		ENDDO

		RETURN m.CursorName
	ENDFUNC

ENDDEFINE
