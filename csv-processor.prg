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

#DEFINE CRLF				"" + 0h0d0a

#DEFINE MAXCOLUMNS		254
#DEFINE MAXCHARSIZE		254
#DEFINE COLUMNDEFSIZE	18

DEFINE CLASS _CSVProcessor AS Custom

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
	* CSV Columns type collection
	ADD OBJECT FieldTypes AS Collection
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
	* thousands separator (.NULL. if numbers don't have separators)
	ThousandsSeparator = .NULL.
	* number unmask before scanning
	NumberUnmask = .F.

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
	* Regular expressions pattern alternatives
	RXDatePattern = "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
	RXDateReformatter = "{^$1-$2-$3}"
	RXDateTimePattern = "([0-9]{4})-([0-1][0-9])-([0-3][0-9])[ T]([0-2][0-9]):([0-5][0-9]):([0-5][0-9])"
	RXDateTimeReformatter = "{^$1-$2-$3 $4:$5:$6}"

	* encoding for binary fields - General, Varbinary, Blob - (hex|base64|plain)
	BinaryEncoding = "hex"

	* how are .NULL. values represented (can be a string, such as "NULL", or .NULL., in which cases they are replaced by empty values)
	NullValue = ""
	* also, nullify empty values?
	EmptyIsNull = .F.
	* trim exported values?
	Trimmer = .T.
	* trim imported values? (0 = no, 1 = left, 2 = right, 3 = both)
	InTrimmer = 0

	* sample size, to determine column data types (0 = all rows)
	SampleSize = 0

	* Regular expression engine
	RegExpr = .NULL.
	* activate regular expression engine for data types: for now, only D and T are supported
	RegularExpressionScanner = ""

	* properties related to the file
	* file handle
	HFile = -1
	* UNICODE encoding
	UTF = 0
	* codepage or locale ID
	RegionalID = 0
	RegionalIDType = 0
	SetCodepage = .F.
	* length and position
	FileLength = -1
	FilePosition = -1

	_MemberData = "<VFPData>" + ;
						'<memberdata name="antemeridian" type="property" display="AnteMeridian"/>' + ;
						'<memberdata name="binaryencoding" type="property" display="BinaryEncoding"/>' + ;
						'<memberdata name="cursorname" type="property" display="CursorName"/>' + ;
						'<memberdata name="centuryyears" type="property" display="CenturyYears"/>' + ;
						'<memberdata name="cptrans" type="property" display="CPTrans"/>' + ;
						'<memberdata name="datepattern" type="property" display="DatePattern"/>' + ;
						'<memberdata name="datetimepattern" type="property" display="DatetimePattern"/>' + ;
						'<memberdata name="decimalpoint" type="property" display="DecimalPoint"/>' + ;
						'<memberdata name="dropexistingtable" type="property" display="DropExistingTable"/>' + ;
						'<memberdata name="emptyisnull" type="property" display="EmptyIsNull"/>' + ;
						'<memberdata name="fieldmapping" type="property" display="FieldMapping"/>' + ;
						'<memberdata name="filelength" type="property" display="FileLength"/>' + ;
						'<memberdata name="fileposition" type="property" display="FilePosition"/>' + ;
						'<memberdata name="headerrow" type="property" display="HeaderRow"/>' + ;
						'<memberdata name="hfile" type="property" display="HFile"/>' + ;
						'<memberdata name="inlinedelimitednewline" type="property" display="InlineDelimitedNewLine"/>' + ;
						'<memberdata name="intrimmer" type="property" display="InTrimmer"/>' + ;
						'<memberdata name="logicalfalse" type="property" display="LogicalFalse"/>' + ;
						'<memberdata name="logicaltrue" type="property" display="LogicalTrue"/>' + ;
						'<memberdata name="monthnames" type="property" display="MonthNames"/>' + ;
						'<memberdata name="multiplecursors" type="property" display="MultipleCursors"/>' + ;
						'<memberdata name="multiplecursorsnames" type="property" display="MultipleCursorsNames"/>' + ;
						'<memberdata name="namecontroller" type="property" display="NameController"/>' + ;
						'<memberdata name="newline" type="property" display="NewLine"/>' + ;
						'<memberdata name="nullvalue" type="property" display="NullValue"/>' + ;
						'<memberdata name="numberunmask" type="property" display="NumberUnmask"/>' + ;
						'<memberdata name="postmeridian" type="property" display="PostMeridian"/>' + ;
						'<memberdata name="regexpr" type="property" display="RegExpr"/>' + ;
						'<memberdata name="regionalid" type="property" display="RegionalID"/>' + ;
						'<memberdata name="regionalidtype" type="property" display="RegionalIDType"/>' + ;
						'<memberdata name="regularexpressionscanner" type="property" display="RegularExpressionScanner"/>' + ;
						'<memberdata name="rxdatepattern" type="property" display="RXDatePattern"/>' + ;
						'<memberdata name="rxdatereformatter" type="property" display="RXDateReformatter"/>' + ;
						'<memberdata name="rxdatetimepattern" type="property" display="RXDateTimePattern"/>' + ;
						'<memberdata name="rxdatetimereformatter" type="property" display="RXDateTimeReformatter"/>' + ;
						'<memberdata name="samplesize" type="property" display="SampleSize"/>' + ;
						'<memberdata name="setcodepage" type="property" display="SetCodepage"/>' + ;
						'<memberdata name="skiprows" type="property" display="SkipRows"/>' + ;
						'<memberdata name="thousandsseparator" type="property" display="ThousandsSeparator"/>' + ;
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
						'<memberdata name="openfile" type="method" display="OpenFile"/>' + ;
						'<memberdata name="processstep" type="method" display="ProcessStep"/>' + ;
						'<memberdata name="putline" type="method" display="PutLine"/>' + ;
						'<memberdata name="outputdate" type="method" display="OutputDate"/>' + ;
						'<memberdata name="outputlogical" type="method" display="OutputLogical"/>' + ;
						'<memberdata name="outputnumber" type="method" display="OutputNumber"/>' + ;
						'<memberdata name="preencodebinaryvalue" type="method" display="PreEncodeBinaryValue"/>' + ;
						'<memberdata name="restoredefaultproperties" type="method" display="RestoreDefaultProperties"/>' + ;
						'<memberdata name="scanbinary" type="method" display="ScanBinary"/>' + ;
						'<memberdata name="scandate" type="method" display="ScanDate"/>' + ;
						'<memberdata name="scanlogical" type="method" display="ScanLogical"/>' + ;
						'<memberdata name="scannumber" type="method" display="ScanNumber"/>' + ;
					'</VFPData>'

	* Init
	* attach a VFP name processor to the name controller
	PROCEDURE Init
		IF EMPTY(This.NameController.AttachProcessor("VFPNamer", "vfp-names.prg"))
			RETURN .F.	&& but fail instantiation if the processor could not be attached
		ENDIF

		This.RegExpr = CREATEOBJECT("VBScript.RegExp")		&& instantiate a regular expression engine

	ENDPROC

	* clean up, on exit
	PROCEDURE Destroy
		This.CloseFile()
	ENDPROC

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

	* PreEncodeBinaryValue (Unencoded)
	* prepare a Binary value for encoding
	FUNCTION PreEncodeBinaryValue (Unencoded AS String) AS String

		LOCAL Encoded AS String

		DO CASE
		CASE This.BinaryEncoding == "hex"
			m.Encoded = STRCONV("" + m.Unencoded, 15)
		CASE This.BinaryEncoding == "base64"
			m.Encoded = STRCONV("" + m.Unencoded, 13)
		CASE This.BinaryEncoding == "plain"
			m.Encoded = "" + m.Unencoded
		OTHERWISE
			m.Encoded = m.Unencoded
		ENDCASE

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
			STRCONV(m.TempBuffer, 9)
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
			CASE m.BOM == 0hFFFE
				This.UTF = 1
				FSEEK(This.HFile, 1, 0)
			* UNICODE BE
			CASE m.BOM == 0hFEFF
				This.UTF = 2
			* UTF-8?
			CASE m.BOM == 0hEFBB AND FREAD(This.HFile, 1) == 0hBF
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
				FWRITE(This.HFile, 0hFFFE)
			* UNICODE BE
			CASE This.UTF = 2
				FWRITE(This.HFile, 0hFEFF)
			* UTF-8?
			CASE This.UTF = 3
				FWRITE(This.HFile, 0hEFBBBF)
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
				FWRITE(This.HFile, 0hFFFE)
			* UNICODE BE
			CASE This.UTF = 2
				FWRITE(This.HFile, 0hFEFF)
			* UTF-8?
			CASE This.UTF = 3
				FWRITE(This.HFile, 0hEFBBBF)
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
		LOCAL Conversion AS Integer
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
			m.Conversion = 6
			m.FileContents = SUBSTR(m.FileContents, 2)

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
			m.Conversion = 6

		CASE INLIST(This.UTF, 3, 4)
			* for UTF-8, use the full string
			* but check approximations to quotes in the conversion, first, and protect the result by doubling the result character
			IF This.ValueDelimiter == '"'
				m.FileContents = STRTRAN(m.FileContents, '”', '""')
			ENDIF
			m.Conversion = 11

		OTHERWISE
			m.Conversion = 0

		ENDCASE

		IF m.Conversion != 0
			IF This.RegionalID != 0
				m.FileContents = STRCONV(m.FileContents, m.Conversion, This.RegionalID, This.RegionalIDType)
			ELSE
				m.FileContents = STRCONV(m.FileContents, m.Conversion)
			ENDIF
			m.FileContents = STRCONV(m.FileContents, 2)
		ENDIF

		RETURN m.FileContents

	ENDFUNC

	* GetLineContents()
	* get the contents of a logical CSV line (which may spread for several actual lines)
	FUNCTION GetLineContents () AS Collection

		SAFETHIS

		LOCAL Contents AS Collection
		LOCAL FileContents AS String
		LOCAL ARRAY ColumnsBuffer(1)
		LOCAL ColumnText AS String
		LOCAL ColLineIndex AS Integer
		LOCAL Pending AS String
		LOCAL IsDelimited AS Boolean
		LOCAL InsideDelimiters AS Boolean
		LOCAL TrailDelimiters AS Integer

		m.Contents = CREATEOBJECT("Collection")

		m.FileContents = This.GetLine()
		* get the separator, if it's not set yet
		This._GetSeparator(m.FileContents)

		* if a delimiter was set, values can be delimited
		m.IsDelimited = LEN(NVL(This.ValueDelimiter, "")) > 0
		m.InsideDelimiters = .F.
		m.Pending = ""

		DO WHILE !ISNULL(m.FileContents)

			* get a crude separation
			ALINES(m.ColumnsBuffer, m.FileContents, 2, This.ValueSeparator)
			m.ColLineIndex = 1

			* while there is column to look into
			DO WHILE m.ColLineIndex <= ALEN(m.ColumnsBuffer)

				* the (partial or complete) value from the CSV field
				m.ColumnText = m.ColumnsBuffer(m.ColLineIndex)
				* if it includes transformed newlines, change them back into real newlines
				IF !ISNULL(This.NewLine)
					m.ColumnText = STRTRAN(m.ColumnText, This.NewLine, CRLF)
				ENDIF
				* add it to the fetched value
				m.Pending = m.Pending + m.ColumnText

				* found a delimited field?
				IF m.IsDelimited AND LEFT(m.Pending, LEN(This.ValueDelimiter)) == This.ValueDelimiter

					m.InsideDelimiters = .T.

					m.TrailDelimiters = 0
					* check the case where the field may end wth a bunch of delimiters...
					IF LEN(m.Pending) > 1
						DO WHILE RIGHT(m.ColumnText, LEN(This.ValueDelimiter)) == This.ValueDelimiter
							m.TrailDelimiters = m.TrailDelimiters + 1
							m.ColumnText = LEFT(m.ColumnText, LEN(m.ColumnText) - LEN(This.ValueDelimiter))
						ENDDO
					ENDIF

					DO CASE
					* empty delimited field..
					CASE EMPTY(m.ColumnText) AND m.TrailDelimiters = 2
						m.Pending = ""
						m.InsideDelimiters = .F.

					* if the field ended with a delimiter
					CASE RIGHT(m.Pending, LEN(This.ValueDelimiter)) == This.ValueDelimiter AND (m.TrailDelimiters / 2) != INT(m.TrailDelimiters / 2)
						* remove the delimiters from the column data, at the beginning and at the end of the field
						m.Pending = SUBSTR(m.Pending, LEN(This.ValueDelimiter) + 1, LEN(m.Pending) - (LEN(This.ValueDelimiter) + 1))
						* and also in the middle
						m.Pending = STRTRAN(m.Pending, REPLICATE(This.ValueDelimiter, 2), This.ValueDelimiter)
						m.InsideDelimiters = .F.

					OTHERWISE
						* if not, it was a separator that broke the columns, so add it
						IF m.ColLineIndex < ALEN(m.ColumnsBuffer)
							m.Pending = m.Pending + This.ValueSeparator
						ENDIF
						* and continue to fill the current data column from the next CSV column
						m.ColLineIndex = m.ColLineIndex + 1
						LOOP
					ENDCASE
				ENDIF

				* fetch more columns if the last one was completely fetched...
				IF m.ColLineIndex <= ALEN(m.ColumnsBuffer) AND !m.InsideDelimiters
					IF BITAND(This.InTrimmer, 0x01) != 0
						m.Pending = LTRIM(m.Pending)
					ENDIF
					IF BITAND(This.InTrimmer, 0x02) != 0
						m.Pending = RTRIM(m.Pending)
					ENDIF
					m.Contents.Add(m.Pending)
					m.Pending = ""
				ENDIF
				m.ColLineIndex = m.ColLineIndex + 1
			ENDDO

			* if the last column was not completely read, we will try with the next line
			IF m.InsideDelimiters

				m.Pending = m.Pending + CRLF
				m.FileContents = This.GetLine()

			ELSE

				EXIT		&& done, the logical CSV line was completely read

			ENDIF

		ENDDO

		* the contents of each column is stored in the collection items
		RETURN m.Contents

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
		* prepare a UNICODE conversion, if necessary
		IF This.UTF != 0
			m.FileContents = STRCONV(m.FileContents, 1)
		ENDIF

		DO CASE
		* UNICODE?
		CASE INLIST(This.UTF, 1, 2)
			* convert to UNICODE
			IF This.RegionalID != 0
				m.FileContents = STRCONV(m.FileContents, 5, This.RegionalID, This.RegionalIDType)
			ELSE
				m.FileContents = STRCONV(m.FileContents, 5)
			ENDIF

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
			IF This.RegionalID != 0
				m.FileContents = STRCONV(m.FileContents, 9, This.RegionalID, This.RegionalIDType)
			ELSE
				m.FileContents = STRCONV(m.FileContents, 9)
			ENDIF
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
	PROTECTED FUNCTION ColumnType (CursorName AS String, ColumnName AS String) AS String

		LOCAL ColumnType AS String
		LOCAL SampleSize AS Integer
		LOCAL NumberValue AS Number
		LOCAL ARRAY AdHoc(1)
		LOCAL UseRX AS Logical

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
		m.UseRX = "T" $ This.RegularExpressionScanner
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .T., m.UseRX))
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
		m.UseRX = "D" $ This.RegularExpressionScanner
		* if any value is not Date
		SCAN FOR !(ISNULL(EVALUATE(m.ColumnName)) OR (ISNULL(This.NullValue) AND EVALUATE(m.ColumnName) == "")) AND m.SampleSize >= 0
			IF ISNULL(This.ScanDate(EVALUATE(m.ColumnName), .F., m.UseRX))
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

		LOCAL NoThousandsSource AS String
		LOCAL CleanSource AS String
		LOCAL CleanSource2 AS String
		LOCAL Symbols AS String

		IF ISNULL(m.Source)
			RETURN .NULL.
		ENDIF

		* remove thousands separators, according to setting
		m.NoThousandsSource = IIF(ISNULL(This.ThousandsSeparator), m.Source, CHRTRAN(m.Source, This.ThousandsSeparator, ""))
		* unmask to numeric digits, according to setting
		IF This.NumberUnmask
			m.NoThousandsSource = CHRTRAN(m.NoThousandsSource, CHRTRAN(m.NoThousandsSource, "-0123456789" + This.DecimalPoint, ""), "")
		ENDIF
		IF TYPE(CHRTRAN(m.NoThousandsSource, This.DecimalPoint, ".")) != "N"
			RETURN .NULL.
		ENDIF

		m.CleanSource = ALLTRIM(m.NoThousandsSource)
		m.CleanSource2 = SUBSTR(m.CleanSource, 2)
		m.Symbols = CHRTRAN(m.CleanSource, "0123456789+-eE" + This.DecimalPoint, "")
		IF LEN(m.Symbols) > 0 OR ;
				("-" $ m.CleanSource2 AND ATC("e", m.CleanSource) != AT("-", m.CleanSource2)) OR ;
				("+" $ m.CleanSource2 AND ATC("e", m.CleanSource) != AT("+", m.CleanSource2))
			RETURN .NULL.
		ENDIF
		
		RETURN VAL(CHRTRAN(m.NoThousandsSource, This.DecimalPoint, SET("Point")))

	ENDFUNC

	* OutputNumber (Source)
	* output a number
	FUNCTION OutputNumber (Source AS Number) AS String

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "NX" ;
			MESSAGE "Number parameter expected."

		LOCAL Output AS String
		LOCAL PostDecimal AS String
		LOCAL PreDecimal AS Number

		IF ISNULL(m.Source)
			RETURN NVL(This.NullValue, "")
		ENDIF

		m.Output = CHRTRAN(TRANSFORM(m.Source), SET("Point"), This.DecimalPoint) 
		IF ISNULL(This.ThousandsSeparator) OR ATC("e", m.Output) != 0 OR ABS(m.Source) < 1000
			RETURN ALLTRIM(m.Output)
		ENDIF

		IF This.DecimalPoint $ m.Output
			m.PreDecimal = VAL(LEFT(m.Output, AT(This.DecimalPoint, m.Output) - 1))
			m.PostDecimal = SUBSTR(m.Output, AT(This.DecimalPoint, m.Output))
		ELSE
			m.PreDecimal = m.Source
			m.PostDecimal = ""
		ENDIF

		RETURN ALLTRIM(CHRTRAN(TRANSFORM(m.PreDecimal, REPLICATE("###,", 10) + "###"), SET("Separator"), This.ThousandsSeparator)) + m.PostDecimal
		
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
	FUNCTION ScanDate (Source AS String, IsTime AS Boolean, UseRegularExpression AS Logical) AS DateOrDatetime

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "CX" AND VARTYPE(m.IsTime) == "L" ;
			MESSAGE "String and boolean parameters expected."

		IF ISNULL(m.Source)
			RETURN .NULL.
		ENDIF

		* the result
		LOCAL Result AS DateOrDatetime

		* if scanning uses a regular expression
		IF m.UseRegularExpression

			TRY
				* try to get a datetime
				IF m.IsTime
					This.RegExpr.Pattern = This.RXDateTimePattern
					m.Result = EVALUATE(This.RegExpr.Replace(m.Source, This.RXDateTimeReformatter))
					IF VARTYPE(m.Result) != "T" OR EMPTY(m.Result)
						m.Result = .NULL.
					ENDIF
				* or a date
				ELSE
					This.RegExpr.Pattern = This.RXDatePattern
					m.Result = EVALUATE(This.RegExpr.Replace(m.Source, This.RXDateReformatter))
					IF VARTYPE(m.Result) != "D" OR EMPTY(m.Result)
						m.Result = .NULL.
					ENDIF
				ENDIF
			CATCH
				m.Result = .NULL.
			ENDTRY

			RETURN m.Result
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

				DO CASE
				* a digit sets a part with fixed length (for instance, %4Y)
				CASE ISDIGIT(m.ChPattern)
					m.ScanPart = LEFT(m.Scanned, VAL(m.ChPattern))
					m.Scanned = SUBSTR(m.Scanned, VAL(m.ChPattern) + 1)
					m.ChPattern = LEFT(m.Pattern, 1)
					m.Pattern = SUBSTR(m.Pattern, 2)

				* for contiguous pattern parts, skip characters to be scanned depending on part type
				CASE LEFT(m.Pattern, 1) == "%"
					m.ScanPart = ""
					DO CASE
					* numeric parts
					CASE m.ChPattern $ "YMDhms"
						DO WHILE ISDIGIT(LEFT(m.Scanned, 1))
							m.ScanPart = m.ScanPart + LEFT(m.Scanned, 1)
							m.Scanned = SUBSTR(m.Scanned, 2)
						ENDDO
					* text parts
					CASE m.ChPattern == "N"
						DO WHILE !ISDIGIT(LEFT(m.Scanned, 1)) AND !EMPTY(m.Scanned)
							m.ScanPart = m.ScanPart + LEFT(m.Scanned, 1)
							m.Scanned = SUBSTR(m.Scanned, 2)
						ENDDO
					* meridian parts
					CASE m.ChPattern == "p"
						m.ScanPart = LEFT(m.Scanned, LEN(This.AnteMeridian))
						m.Scanned = SUBSTR(m.Scanned, LEN(This.AnteMeridian) + 1)
					* just advance one character....
					OTHERWISE
						m.ScanPart = LEFT(m.Scanned, 1)
						m.Scanned = SUBSTR(m.Scanned, 2)
					ENDCASE

				OTHERWISE
					* if not fixed or contiguous, the part ends at the next literal character (or end of source string)
					m.ScanPart = STREXTRACT(m.Scanned, "", LEFT(m.Pattern, 1), 1, 2)
					m.Scanned = SUBSTR(m.Scanned, LEN(m.ScanPart) + 1)
				ENDCASE

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
					m.Added = STREXTRACT(":" + This.MonthNames + ":", ":", ":", MONTH(m.Source) * 2 - 1) 

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

	* ScanBinary (Source)
	* scan an encoded string and check if it's a valid encoded Blob
	FUNCTION ScanBinary (Source AS String) AS Blob

		SAFETHIS

		ASSERT VARTYPE(m.Source) $ "CX" ;
			MESSAGE "String parameter expected."

		LOCAL Decoded AS String
		LOCAL Success AS Boolean

		DO CASE
		CASE ISNULL(m.Source)
			RETURN .NULL.
		CASE This.BinaryEncoding == "hex"
			m.Decoded = STRCONV(m.Source, 16)
			m.Success = STRCONV(m.Decoded, 15) == UPPER(m.Source)
		CASE This.BinaryEncoding == "base64"
			m.Decoded = STRCONV(m.Source, 14)
			m.Success = STRCONV(m.Decoded, 13) == m.Source
		OTHERWISE
			m.Decoded = m.Source
			m.Success = .T.
		ENDCASE

		RETURN IIF(m.Success, CAST(m.Decoded AS Blob), .NULL.)

	ENDFUNC

	* ProcessStep (Phase, Done, ToDo)
	* a event signaling a step on the CSV import processing
	PROCEDURE ProcessStep (Phase AS Integer, Done AS Number, ToDo AS Number)
	ENDPROC

	* RestoreDefaultProperties ()
	* reset the properties values to their default
	PROCEDURE RestoreDefaultProperties

		SAFETHIS

		LOCAL ARRAY Properties(1)
		LOCAL PropertyName AS String, RestorePropertyName AS String
		LOCAL PropertyIndex AS Integer
		LOCAL ArrayName AS String
		LOCAL ItemIndex AS Integer
		LOCAL ItemRestored
		LOCAL Restore AS CSVProcessor

		* default values will come from a new instance
		m.Restore = CREATEOBJECT(This.Class)

		* go through all non-base class properties
		FOR m.PropertyIndex = 1 TO AMEMBERS(m.Properties, m.Restore, 0, "U")

			m.PropertyName = m.Properties(m.PropertyIndex)
			m.RestorePropertyName = "m.Restore." + m.PropertyName

			* if not an array,
			IF TYPE(m.RestorePropertyName, 1) != "A"
				* just fetch the value
				STORE EVALUATE(m.RestorePropertyName) TO ("This." + m.PropertyName)
			ELSE
				* otherwise, redimension it and fetch individual elements
				m.ArrayName = "This." + m.PropertyName
				DIMENSION &ArrayName.(ALEN(SUBSTR(m.RestorePropertyName, 3)))
				FOR m.ItemIndex = 1 TO ALEN(SUBSTR(m.RestorePropertyName, 3))
					STORE EVALUATE(m.RestorePropertyName + "(" + TRANSFORM(m.ItemIndex) + ")") ;
						TO (m.ArrayName + "(" + TRANSFORM(m.ItemIndex) + ")")
				ENDFOR
			ENDIF
		ENDFOR

		* what was done to arrays, repeat with collections
		FOR m.PropertyIndex = 1 TO m.Restore.ControlCount
			IF m.Restore.Controls(m.PropertyIndex).BaseClass == "Collection"
				This.Controls(m.PropertyIndex).Remove(-1)
				FOR EACH m.ItemRestored IN m.Restore.Controls(m.PropertyIndex)
					This.Controls(m.PropertyIndex).Add(m.ItemRestored)
				ENDFOR
			ENDIF
		ENDFOR

	ENDPROC

	* get a name for the import cursor
	PROTECTED FUNCTION _GetCursorName (BaseName AS String, CursorIndex AS Integer, IsTarget AS Boolean)

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

	* create a cursor / table
	PROTECTED FUNCTION _CreateCursor (CursorName AS String, CursorStructure AS Array, IsTable AS Logical)

		DO CASE
		CASE !m.IsTable AND (!This.SetCodepage OR This.RegionalIDType != 1)
			CREATE CURSOR (m.CursorName) FROM ARRAY CursorStructure
		CASE !m.IsTable
			CREATE CURSOR (m.CursorName) CODEPAGE = (This.RegionalID) FROM ARRAY CursorStructure
		CASE !This.SetCodepage OR This.RegionalIDType != 1
			CREATE TABLE (m.CursorName) FROM ARRAY CursorStructure
		OTHERWISE
			CREATE TABLE (m.CursorName) CODEPAGE = (This.RegionalID) FROM ARRAY CursorStructure
		ENDCASE

	ENDFUNC

	* set the separator, if not given
	PROTECTED FUNCTION _GetSeparator (FirstLine AS String)

		LOCAL ValueSeparators AS String
		LOCAL VSIndex AS Integer
		LOCAL VSIndexFound AS Integer
		LOCAL Previous AS Integer
		LOCAL Current AS Integer

		IF ISNULL(This.ValueSeparator)

			* defaults to ","
			m.ValueSeparators = ",;" + CHR(9)
			m.VSIndexFound = 1
			m.Previous = 0

			IF !ISNULL(m.FirstLine)
				* find the most frequent of possible separators that are found in first line
				FOR m.VSIndex = 1 TO LEN(m.ValueSeparators)
					m.Current = OCCURS(SUBSTR(m.ValueSeparators, m.VSIndex, 1), m.FirstLine)
					IF m.Current > m.Previous
						m.VSIndexFound = m.VSIndex
					ENDIF
				ENDFOR
			ENDIF

			* that's the one that will be set
			This.ValueSeparator = SUBSTR(m.ValueSeparators, m.VSIndexFound, 1)
		ENDIF

	ENDFUNC

ENDDEFINE
