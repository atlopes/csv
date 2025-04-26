*!*	CSVProcessor

*!*	A VFP class to process CSV files

* dependencies
DO ADDBS(JUSTPATH(SYS(16))) + "csv-processor.prg"

* install itself
IF !SYS(16) $ SET("Procedure")
	SET PROCEDURE TO (SYS(16)) ADDITIVE
ENDIF

#DEFINE SAFETHIS			ASSERT !USED("This") AND TYPE("This") == "O"

#DEFINE CRLF				"" + 0h0d0a

#DEFINE MAXCOLUMNS		254
#DEFINE MAXCHARSIZE		254
#DEFINE MAXCOLNAMESIZE	254
#DEFINE COLUMNDEFSIZE	18

DEFINE CLASS CSVProcessor AS _CSVProcessor

	_MemberData = "<VFPData>" + ;
						'<memberdata name="export" type="method" display="Export"/>' + ;
						'<memberdata name="import" type="method" display="Import"/>' + ;
						'<memberdata name="importstring" type="method" display="ImportString"/>' + ;
					'</VFPData>'

	* Import (Filename[, CursorName[, HostDatabase]])
	* import a CSV file into a cursor (or a database table)
	FUNCTION Import (Filename AS String, CursorName AS String, HostDatabase AS String) AS Integer

		SAFETHIS

		ASSERT (PCOUNT() < 3 OR VARTYPE(m.HostDatabase) == "C") AND (PCOUNT() < 2 OR VARTYPE(m.CursorName) == "C") ;
					AND VARTYPE(m.Filename) == "C" ;
				MESSAGE "String parameters expected."

		* what is read from the CSV
		LOCAL CSVFileContents AS Collection
		* separated by columns
		LOCAL ARRAY ColumnsData(1)
		LOCAL ARRAY ImporterData(1)
		* and sent to a target
		LOCAL TargetData AS Object
		LOCAL TargetColumn AS String
		LOCAL TargetName AS String
		LOCAL ARRAY TargetFields(1)

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
		LOCAL ARRAY UniqueBaseNames[1], UniqueBaseIndexes[1]
		LOCAL UniqueIndex AS Integer
		LOCAL UniqueSuffix AS String
		LOCAL ColumnText AS String

		* initial value and row separators (may be set automatically)
		LOCAL InitialValueSeparator AS String, InitialRowSeparator AS String

		* loop indexers
		LOCAL ImporterIndex AS Integer
		LOCAL LineIndex AS Integer
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

		m.InitialValueSeparator = This.ValueSeparator
		m.InitialRowSeparator = This.RowSeparator

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
				m.CSVFileContents = This.GetLineContents()

				m.ColumnsCount = m.CSVFileContents.Count
				DIMENSION ColumnsNames(m.ColumnsCount)
				FOR m.ColumnIndex = 1 TO m.ColumnsCount
					m.ColumnsNames(m.ColumnIndex) = STRTRAN(m.CSVFileContents.Item(m.ColumnIndex), CRLF, " ")
				ENDFOR
				DIMENSION m.CursorFields(m.ColumnsCount, COLUMNDEFSIZE)
				ACOPY(m.ColumnsNames, m.CSVColumns)

			ELSE

				* columns are not named, so create a dummy structure with max number of 254 columns (the VFP limit), for now
				DIMENSION m.CursorFields(MAXCOLUMNS, COLUMNDEFSIZE)
				DIMENSION m.ColumnsNames(MAXCOLUMNS)
				FOR m.NameIndex = 1 TO MAXCOLUMNS
					m.ColumnsNames(m.NameIndex) = "Col_" + LTRIM(STR(m.NameIndex))
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
					* check the name against the VFP name controller
					This.NameController.SetOriginalName(m.ColumnsNames(m.ColumnIndex))
					m.ColumnName = This.NameController.GetName()
					* check for repetitions in the second and following columns
					IF m.ColumnIndex > 1
						m.BaseColumnName = m.ColumnName
						* when the column name was used in one of the already processed columns
						IF ASCAN(m.ColumnsNames, m.ColumnName, 1, m.ColumnIndex - 1, 1, 1 + 2 + 4) != 0
							* look for a unique base name, create one if not found
							m.UniqueIndex = ASCAN(m.UniqueBaseNames, m.ColumnName, -1, -1, -1, 1 + 2 + 4)
							IF m.UniqueIndex == 0
								m.UniqueIndex = ALEN(m.UniqueBaseNames) + 1
								DIMENSION m.UniqueBaseNames[m.UniqueIndex], m.UniqueBaseIndexes[m.UniqueIndex]
								m.UniqueBaseNames = m.BaseColumnName
								m.UniqueBaseIndexes[m.UniqueIndex] = 1
							ENDIF
							* use its incremental suffix to distinguish columns with the same base name
							m.ExpIndex = m.UniqueBaseIndexes[m.UniqueIndex]
							DO WHILE .T.
								m.UniqueSuffix = "_" + LTRIM(STR(m.ExpIndex))
								m.ColumnName = m.BaseColumnName + m.UniqueSuffix
								* but make sure it fits in the allowed name length
								IF LEN(m.ColumnName) > MAXCOLNAMESIZE
									m.ColumnName = LEFT(m.BaseColumnName, MAXCOLNAMESIZE - LEN(m.UniqueSuffix)) + m.UniqueSuffix
								ENDIF
								* increment for next usage
								m.ExpIndex = m.ExpIndex + 1
								* if the new suffixed name is unique, we're done, a valid and unique name is available
								IF ASCAN(m.ColumnsNames, m.ColumnName, 1, m.ColumnIndex - 1, 1, 1 + 2 + 4) == 0
									EXIT
								ENDIF
							ENDDO
							* store the next suffix index
							m.UniqueBaseIndexes[m.UniqueIndex] = m.ExpIndex
						ENDIF
					ELSE
						* prepare the unique base names to be used in uniqueness validation
						DIMENSION m.UniqueBaseNames[1], m.UniqueBaseIndexes[1]
						m.UniqueBaseNames[1] = m.BaseColumnName
						m.UniqueBaseIndexes[1] = 1
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
				This._CreateCursor(m.Importer(m.ImporterIndex), @m.ImporterFields)
				m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS
			ENDFOR

			* starting to import...
			* phase 1: read the data from the CSV file

			* this will point to the column that is being filled with data
			m.ColumnIndex = 1
			DIMENSION m.ColumnsData(ALEN(m.ColumnsNames))
			STORE "" TO m.ColumnsData

			m.CSVFileContents = This.GetLineContents()

			* until there is nothing left to read from the CSV file
			DO WHILE m.CSVFileContents.Count > 0

				STORE "" TO m.ColumnsData

				FOR m.ColumnIndex = 1 TO m.CSVFileContents.Count

					* update the column count, if we have now an extra column
					IF !This.HeaderRow AND m.ColumnIndex > m.ColumnsCount
						m.ColumnsCount = m.ColumnIndex
					ENDIF

					* this is the case where an headerless CSV file requires the creation of a new importer cursor
					IF !This.HeaderRow AND m.ColumnIndex > ALEN(m.ColumnsNames)
						DIMENSION m.ColumnsNames(m.ColumnsCount + MAXCOLUMNS - 1)
						DIMENSION m.CursorFields(ALEN(m.ColumnsNames), COLUMNDEFSIZE)
						DIMENSION m.ColumnsData(ALEN(m.ColumnsNames))
						FOR m.NameIndex = m.ColumnsCount TO ALEN(m.ColumnsNames)
							m.ColumnsNames(m.NameIndex) = "Col_" + LTRIM(STR(m.NameIndex))
							m.CursorFields(m.NameIndex, 1) = m.ColumnsNames(m.NameIndex)
							m.CursorFields(m.NameIndex, 2) = "M"
							m.CursorFields(m.NameIndex, 5) = .T.
							m.CursorFields(m.NameIndex, 6) = !This.CPTrans
							STORE 0 TO m.CursorFields(m.NameIndex, 3), m.CursorFields(m.NameIndex, 4), ;
								m.CursorFields(m.NameIndex, 17), m.CursorFields(m.NameIndex, 18)
							STORE "" TO m.CursorFields(m.NameIndex, 7), m.CursorFields(m.NameIndex, 8), ;
								m.CursorFields(m.NameIndex, 9), m.CursorFields(m.NameIndex, 10), ;
								m.CursorFields(m.NameIndex, 11), m.CursorFields(m.NameIndex, 12), ;
								m.CursorFields(m.NameIndex, 13), m.CursorFields(m.NameIndex, 14), ;
								m.CursorFields(m.NameIndex, 15), m.CursorFields(m.NameIndex, 16)
						ENDFOR
						m.ImporterIndex = ALEN(m.Importer) + 1
						DIMENSION m.Importer(m.ImporterIndex)
						m.Importer(m.ImporterIndex) = This._GetCursorName(m.CursorName, m.ImporterIndex)
						DIMENSION m.ImporterFields(MAXCOLUMNS, COLUMNDEFSIZE)
						ACOPY(m.CursorFields, m.ImporterFields, (m.ColumnsCount - 1) * COLUMNDEFSIZE + 1, MAXCOLUMNS * COLUMNDEFSIZE)
						This._CreateCursor(m.Importer(m.ImporterIndex), @m.ImporterFields)
					ENDIF

					* put the data into the columns that were set
					IF m.ColumnIndex <= m.ColumnsCount
						m.ColumnsData(m.ColumnIndex) = m.CSVFileContents.Item(m.ColumnIndex)
					ENDIF

				ENDFOR

				* the line is completely read
				FOR m.ColumnIndex = 1 TO m.ColumnsCount
					* set as an empty string if uninitialized array element
					IF VARTYPE(m.ColumnsData(m.ColumnIndex)) == "L"
						m.ColumnsData(m.ColumnIndex) = ""
					ENDIF
					* .NULL.ify, if needed
					IF m.ColumnsData(m.ColumnIndex) == This.NullValue OR (LEN(m.ColumnsData(m.ColumnIndex)) = 0 AND This.EmptyIsNull)
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

				* signal another line read
				RAISEEVENT(This, "ProcessStep", 1, This.FilePosition, This.FileLength)

				* and step to the next one
				m.CSVFileContents = This.GetLineContents()

			ENDDO

			* the CSV file can be closed
			This.CloseFile()

			* phase 2: set the type of the fields

			* reset the fields definitions
			DIMENSION m.CursorFields(m.ColumnsCount, COLUMNDEFSIZE)
			DIMENSION m.ColumnsNames(m.ColumnsCount)

			* prepare the optimization of the reading process (irrelevant CSV columns will be deactived as soon as possible)
			DIMENSION m.ActiveColumns(m.ColumnsCount)
			* but at first they will all be active
			STORE .T. TO m.ActiveColumns

			* determine the type and length of each column
			FOR m.ColumnIndex = 1 TO m.ColumnsCount

				m.ImporterIndex = INT((m.ColumnIndex - 1) / MAXCOLUMNS) + 1

				* change the Memo to something else, if needed / possible
				TRY
					DO CASE
					CASE m.CreateCursor
						m.Retype = ""
						TRY
							IF This.FieldTypes.Count > 0
								IF EMPTY(This.FieldTypes.GetKey(1))
									m.Retype = This.FieldTypes.Item(m.ColumnIndex)
								ELSE
									m.Retype = This.FieldTypes.Item(m.CSVColumns(m.ColumnIndex))
								ENDIF
							ENDIF
						CATCH
							m.Retype = ""
						ENDTRY
						m.Retype = EVL(m.Retype, This.ColumnType(m.Importer(m.ImporterIndex), m.ColumnsNames(m.ColumnIndex)))
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
				CASE m.Retype $ "BYN"
					m.CursorFields(m.ColumnIndex, 2) = "B"
					m.CursorFields(m.ColumnIndex, 3) = 8
					m.CursorFields(m.ColumnIndex, 4) = 4
				* Char() or Varchar()
				CASE LEFT(m.Retype, 1) == "V"
					m.CursorFields(m.ColumnIndex, 2) = "V"
					m.CursorFields(m.ColumnIndex, 3) = EVL(VAL(SUBSTR(m.Retype, 2)), 10)
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
						This._CreateCursor(m.CursorName, @m.CursorFields)
					ELSE
						* or a table of a database
						SET DATABASE TO (m.ToDatabase)
						IF INDBC(m.CursorName, "TABLE")
							* if it exists and must not be dropped, assume it's properly prepared for import
							IF This.DropExistingTable
								DROP TABLE (m.CursorName)
								This._CreateCursor(m.CursorName, @m.CursorFields, .T.)
							ENDIF
						ELSE
							This._CreateCursor(m.CursorName, @m.CursorFields, .T.)
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
						This._CreateCursor(m.TargetName, @m.ImporterFields)
						m.ImporterSegment = m.ImporterSegment + MAXCOLUMNS * COLUMNDEFSIZE
					ENDFOR

				ENDIF

			ELSE

				* consider all importer cursors when appending to an existing cursor
				m.ImporterCount = ALEN(m.Importer)

			ENDIF			

			* phase 3: move the imported data to the cursor(s)

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
						* fix a problem with Varbinary(x) - VFP generates SPACE(x), it should be ""
						FOR m.ColumnIndex = 1 TO AMEMBERS(m.TargetFields, m.TargetData)
							m.TargetColumn = "m.TargetData." + m.TargetFields(m.ColumnIndex)
							IF TYPE(m.TargetColumn) == "Q"
								STORE 0h TO (m.TargetColumn)
							ENDIF
						ENDFOR
					ENDIF

					* evaluate the memo, and reset the value with its (new) data type
					FOR m.ColumnIndex = 1 TO IIF(m.ImporterIndex < m.ImporterCount, ALEN(m.ColumnsData), EVL(m.ColumnsCount % MAXCOLUMNS, MAXCOLUMNS))

						* skip the column if it has been deactivated or deleted
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
						CASE EMPTY(m.TargetColumn) OR m.TargetColumn == "m.TargetData." OR TYPE(m.TargetColumn) $ "UG" OR m.ColumnIndex > ALEN(m.CursorFields, 1)
							* field not mapped, does not exist, or it's of General type: source column may be deactivated
							m.ActiveColumns(m.ColumnIndex + m.ImporterSegment) = .F.
						CASE ISNULL(m.ColumnText)
							STORE .NULL. TO (m.TargetColumn)
						CASE m.CursorFields(m.ColumnIndex, 2) $ "IB"
							STORE NVL(This.ScanNumber(m.ColumnText), 0) TO (m.TargetColumn)
						CASE m.CursorFields(m.ColumnIndex, 2) == "L"
							STORE NVL(This.ScanLogical(m.ColumnText), .F.) TO (m.TargetColumn)
						CASE m.CursorFields(m.ColumnIndex, 2) $ "DT"
							STORE NVL(This.ScanDate(m.ColumnText, m.CursorFields(m.ColumnIndex, 2) == "T", m.CursorFields(m.ColumnIndex, 2) $ This.RegularExpressionScanner), {}) TO (m.TargetColumn)
						CASE TYPE(m.TargetColumn) $ "WQ"
							STORE NVL(This.ScanBinary(m.ColumnText), "") TO (m.TargetColumn)
						OTHERWISE
							STORE m.ColumnText TO (m.TargetColumn) 
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

			TRY
				IF !ISNULL(m.Importer(1))
					FOR m.ImporterIndex = 1 TO ALEN(m.Importer)
						USE IN SELECT(m.Importer(m.ImporterIndex))
					ENDFOR
				ENDIF
			CATCH
			ENDTRY

			* something went wrong...
			m.Result = m.ErrorHandler.ErrorNo

		ENDTRY

		* restore value and row separators, for the cases where they may have been set automatically 
		This.ValueSeparator = m.InitialValueSeparator
		This.RowSeparator = m.InitialRowSeparator

		RETURN m.Result

	ENDFUNC

	* ImportString (Source[, CursorName[, HostDatabase]])
	* import a CSV formatted source string into a cursor (or a database table)
	FUNCTION ImportString (Source AS String, CursorName AS String, HostDatabase AS String) AS Integer

		SAFETHIS

		ASSERT (PCOUNT() < 3 OR VARTYPE(m.HostDatabase) == "C") AND (PCOUNT() < 2 OR VARTYPE(m.CursorName) == "C") ;
					AND VARTYPE(m.Source) == "C" ;
				MESSAGE "String parameters expected."

		LOCAL TempCSV AS String
		LOCAL Result AS Integer
		LOCAL Trapper AS Exception

		TRY
			m.TempCSV = ""
			DO WHILE EMPTY(m.TempCSV) OR FILE(m.TempCSV)
				m.TempCSV = TEXTMERGE("<<ADDBS(SYS(2023))>>~tmp<<SYS(2015)>>.csv")
			ENDDO
			STRTOFILE(m.Source, m.TempCSV, 0)
			DO CASE
			CASE PCOUNT() = 1
				m.Result = This.Import(m.TempCSV)
			CASE PCOUNT() = 2
				m.Result = This.Import(m.TempCSV, m.CursorName)
			OTHERWISE
				m.Result = This.Import(m.TempCSV, m.CursorName, m.HostDatabase)
			ENDCASE
			ERASE (m.TempCSV)
		CATCH TO m.Trapper
			m.Result = m.Trapper.ErrorNo
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
					m.CSVFileContents = m.CSVFileContents + IIF(m.ColumnIndex > 1, NVL(This.ValueSeparator, ","), "") + m.ColumnValue
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
*** DH 2024-10-14: handle N and Y separately: need to convert Y to N so don't get $ in output
*					CASE TYPE(m.ColumnData) $ "NY"
					CASE TYPE(m.ColumnData) = "N"
						m.ColumnValue = This.OutputNumber(EVALUATE(m.ColumnData))
					CASE TYPE(m.ColumnData) = "Y"
						m.ColumnValue = This.OutputNumber(mton(EVALUATE(m.ColumnData)))
*** DH 2024-10-14: end of change
					CASE TYPE(m.ColumnData) == "L"
						m.ColumnValue = This.OutputLogical(EVALUATE(m.ColumnData))
					CASE TYPE(m.ColumnData) $ "DT"
						m.ColumnValue = This.OutputDate(EVALUATE(m.ColumnData))
					CASE TYPE(m.ColumnData) == "G"
						m.ColumnValue = This.PreEncodeBinaryValue(CAST(&ColumnData. AS Blob))
					CASE TYPE(m.ColumnData) $ "WQ"
						m.ColumnValue = This.PreEncodeBinaryValue(&ColumnData.)
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

ENDDEFINE
