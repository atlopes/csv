# CSVProcessor Documentation

Go to [Overview](DOCUMENTATION.md "Overview"), or [Examples](examples.md "Examples").

### Properties

| Name | Type | Information |
| ---- | ---- | ----------- |
| AnteMeridian | C | The ante-meridian signature (defaults to "AM"). |
| BinaryEncoding | C | The encoding of binary data - hex, base64, or plain (defaults to "hex"). |
| CenturyYears | N | Years to add to imported CSV dates (defaults to 0). |
| CPTrans | L | Code page translation setting for new text columns (defaults to .T.) |
| CursorName | C | The name of the result cursor. |
| DatePattern | C | The pattern of Date values in the CSV file (defaults to "%4Y-%2M-%2D"). |
| DatetimePattern | C | The pattern of Datetime values in the CSV file (defaults to "%4Y-%2M-%2D %2h:%2m:%2s"). |
| DecimalPoint | C | The representation of decimal point in the CSV file (defaults to "."). |
| DropExistingTable | L | If importing into a database, drop existing table if the target table already exists (defaults to .F.). |
| FieldMapping | O | A collection of cursor field names, mapped (by indexed position or by key) to the columns in the CSV file. Used in append mode. |
| FieldTypes | O | A collection of cursor field types, mapped (by indexed position or by key) to the columns in the CSV file. Field types can be one of `ILDTBYN` or `Vnnn` (where 0 < nnn < 255). Used in import mode. |
| FileLength | N | The total length of the CSV file, in bytes. |
| FilePosition | N | The current position of the CSV file, while being read. |
| HeaderRow | L | The presence of a row with the header for columns (defaults to .T.). |
| HFile | N | The low level file handle. |
| InlineDelimitedNewLine | L | Inline newlines inside a record may be delimited with the value delimiter. |
| LogicalFalse | C | The representation for .F. (defaults to "F") |
| LogicalTrue | C | The representation for .T. (defaults to "T") |
| MonthNames | C | The name of months used in the date or datetime values (defaults to "Jan:1:Feb:2:Mar:3:Apr:4:May:5:Jun:6:Jul:7:Aug:8:Sep:9:Oct:10:Nov:11:Dec:12") |
| MultipleCursors | L | Import into as many cursors as needed if the number of columns exceeds single cursor capacity (defaults to .F.). |
| MultipleCursorsNames | O | Collection of the names of cursors imported in multiple cursors mode. |
| NameController | O | The Namer object that control the naming of fields and cursor. |
| NewLine | C | How newlines are inserted in an imported or exported value (defaults to .NULL., newlines are not transformed) |
| NullValue | C | The representation of .NULL. values (defaults to ""). Can be a string, such as "NULL", or .NULL., in which cases empty values are kept as such. |
| PostMeridian | C | The post-meridian signature (defaults to "PM"). |
| RegionalID | N | The regional identifier used for Unicode to ANSI charset translation support (defaults to 0, meaning identifier not set). See STRCONV() help for details and possible values. |
| RegionalIDType | N | The type of regional identifier used for Unicode to ANSI charset translation support (defaults to 0, meaning identifier type not set). See STRCONV() help for details and possible values. |
| SampleSize | N | The number of rows used to determine the column data type (defaults to 0, meaning all rows). |
| SetCodepage | L | Sets the codepage of created cursors, as defined by `RegionalID` and `RegionalIDType` properties (defaults to .F.). |
| SkipRows | N | Number of rows skipped before starting the import (defaults to 0) |
| ThousandsSeparator | C | Thousands separator symbol (.NULL. if numbers don't have separators) |
| Trimmer | L | Trims exported values (defaults to .T.) |
| UTF | N | The UNICODE encoding (0 = ANSI, 1 = LittleEndian, 2 = BigEndian, 3 = UTF8, 4 = UTF8 no BOM) |
| ValueDelimiter | C | The character used to delimit fields that may include value separators (defaults to '"'). |
| ValueSeparator | C | The character used to separate values (defaults to ","). If `.NULL.`, the separator will be guessed from the header row. |
| WorkArea | C/N | Workarea of the cursor that will be appended (defaults to empty, meaning no append). |

### Methods

External:

#### `Import (Filename[, CursorName[, Database]]) AS Integer`
Imports a CSV file into a cursor (name comes from `m.Filename` if `m.CursorName` is not given), or into a new table of a `m.Database`. If no `m.CursorName`is given and `WorkArea` is not empty, the data is appended to the cursor referenced by `WorkArea` (that is, set `WorkArea` to enter append mode).
Returns 0 if successful, -1 if the file could not be located, or > 0 for a VFP error number.

#### `ImportString (Source[, CursorName[, Database]]) AS Integer`
Imports a CSV formatted memory string into a cursor. Wraps a call to `Import()` by creating a temporary file that holds the string contents.

#### `Export (Filename[, AllRecords[, Append]]) AS Integer`
Exports a cursor to a CSV file. If `This.WorkArea` is empty, the data is exported from the current work area. If `m.AllRecords` is .T., all records from the cursor are exported, otherwise export starts at the current record position. If `m.Append` is .T., exported data is appended to the CSV file (otherwise, the file is overwritten).
Returns 0 if successful, -1 if the file could not be opened for writing, or > 0 for a VFP error number.

#### `PreEncodeBinaryValue (Unencoded) AS String`
Prepares binary data for output, according to `BinaryEncoding`.

#### `ProcessStep (Phase, Done, ToDo)`
Event issued when the importer goes to another step (`m.Phase` can be 0 for CSV file reading, 1 for data type checking, and 2 for cursor filling).

#### `RestoreDefaultProperties ()`
Reset the properties to their default values.

#### `ScanBinary (Source) AS Blob`
Scans an encoded `m.Source`. Returns .NULL. if `m.Source` does not encodes binary data according to `BinaryEncoding`.

#### `ScanDate (Source[, IsTime]) AS DateOrDatetime`
Scans a formatted date (or datetime) `m.Source`. Returns .NULL. if `m.Source` does not match the date patterns.

#### `ScanLogical (Source) AS Boolean`
Scans a formatted logical `m.Source`. Returns .NULL. if `m.Source` does not match the representation of the logical values.

#### `ScanNumber (Source) AS Number`
Scans a formatted numeric `m.Source`. Returns .NULL. if `m.Source` does not represent a numeric value.

#### `EncodeValue (Unencoded) AS String`
Encodes a value, while verifying it does not conflict with delimiters and separators. 

#### `OutputDate (Source) AS String`
Outputs a date according to the format defined by `DatePattern` or `DatetimePattern`.

#### `OutputLogical (Source) AS String`
Outputs a logical according to the format defined by `LogicalFalse` and `LogicalTrue` properties.

#### `OutputNumber (Source) AS String`
Outputs a number.

Internal:

#### `CloseFile ()`
Closes the CSV file.

#### `ColumnType (CursorName, ColumnName) AS String`
Returns the type of the column (M, V*nn*, I, B, L, D, or T).

#### `GetLine () AS String`
Reads a line from the CSV file (returns .NULL. on EOF).

#### `OpenFile (Filename) AS Boolean`
Opens a CSV file (.T., on success).

#### `CreateFile (Filename) AS Boolean`
Creates a CSV file (.T. on success)

#### `AppendToFile (Filename) AS Boolean`
Opens a CSV file for appending (.T., on success).

#### `PutLine (Contents) AS Boolean`
Writes a line in the CSV file.
