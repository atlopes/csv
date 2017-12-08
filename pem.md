# CSVProcessor Documentation

Go to [Overview](DOCUMENTATION.md "Overview"), or [Examples](examples.md "Examples").

### Properties

| Name | Type | Information |
| ---- | ---- | ----------- |
| AnteMeridian | C | The ante-meridian signature (defaults to "AM"). |
| CenturyYears | N | Years to add to imported CSV dates (defaults to 0). |
| CursorName | C | The name of the result cursor. |
| DatePattern | C | The pattern of Date values in the CSV file (defaults to "%4Y-%2M-%2D"). |
| DatetimePattern | C | The pattern of Datetime values in the CSV file (defaults to "%4Y-%2M-%2D %2h:%2m:%2s"). |
| DecimalPoint | C | The representation of decimal point in the CSV file (defaults to "."). |
| FieldMapping | O | A collection of cursor field names, mapped (by indexed position or by key) to the columns in the CSV file. Used in append mode. |
| FileLength | N | The total length of the CSV file, in bytes. |
| FilePosition | N | The current position of the CSV file, while being read. |
| HeaderRow | L | The presence of a row with the header for columns (defaults to .T.). |
| HFile | N | The low level file handle. |
| LogicalFalse | C | The representation for .F. (defaults to "F") |
| LogicalTrue | C | The representation for .T. (defaults to "T") |
| MonthNames | C | The name of months used in the date or datetime values (defaults to "Jan:1:Feb:2:Mar:3:Apr:4:May:5:Jun:6:Jul:7:Aug:8:Sep:9:Oct:10:Nov:11:Dec:12") |
| NameController | O | The Namer object that control the naming of fields and cursor. |
| NullValue | C | The representation of .NULL. values (defaults to ""). Can be a string, such as "NULL", or .NULL., in which cases empty values are kept as such. |
| PostMeridian | C | The post-meridian signature (defaults to "PM"). |
| SampleSize | N | The number of rows used to determine the column data type (defaults to 0, meaning all rows). |
| SkipRows | N | Number of rows skipped before starting the import (defaults to 0) |
| UTF | N | The UNICODE encoding (0 = ANSI, 1 = LittleEndian, 2 = BigEndian, 3 = UTF8) |
| ValueDelimiter | C | The character used to delimit fields that may include value separators (defaults to '"'). |
| ValueSeparator | C | The character used to separate values (defaults to ","). |
| WorkArea | C/N | Workarea of the cursor that will be appended (defaults to empty, meaning no append). |

### Methods

External:

#### `Import (Filename[, CursorName[, Database]]) AS Integer`
Imports a CSV file into a cursor (name comes from `m.Filename` if `m.CursorName` is not given), or into a new table of a `m.Database`. If no `m.CursorName`is given and `WorkArea` is not empty, the data is appended to the cursor referenced by `WorkArea`.
Returns 0 if successful, -1 if the file could not be located, or > 0 for a VFP error number.

#### `ProcessStep (Phase, Done, ToDo)`
Event issued when the importer goes to another step (`m.Phase` can be 0 for CVS file reading, 1 for data type checking, and 2 for cursor filling).

#### `ScanDate (Source[, IsTime]) AS DateOrDatetime`
Scans a formatted date (or datetime) `m.Source`. Returns .NULL. if `m.Source` does not match the date patterns.

#### `ScanLogical (Source) AS Boolean`
Scans a formatted logical `m.Source`. Returns .NULL. if `m.Source` does not match the representation of the logical values.

#### `ScanNumber (Source) AS Number`
Scans a formatted numeric `m.Source`. Returns .NULL. if `m.Source` does not represent a numeric value.

Internal:

#### `CloseFile ()`
Closes the CSV file.

#### `ColumnType (CursorName, ColumnName) AS String`
Returns the type of the column (M, V*nn*, I, B, L, D, or T).

#### `GetLine () AS String`
Reads a line from the CSV file (returns .NULL. on EOF).

#### `OpenFile (Filename) AS Boolean`
Opens a CSV file (.T., on success).
