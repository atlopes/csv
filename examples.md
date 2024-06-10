# CSVProcessor Documentation

Go to [Overview](DOCUMENTATION.md "Overview"), or [Properties/Events/Methods](pem.md "PEM")

### Examples

**Date patterns**

Demonstrates the use of Date patterns.

Patterns can be built using the following components:

- %Y = year

- %M = month (number)

- %N = month (name)

- %D = day

- %h = hours

- %m = minutes

- %s = seconds

- %p = meridian

- %? = anything

The components can be preceded by a digit 1-9, indicating a fixed width (for instance, %2D states that a day takes 2 digits, even when less than 10).

To represent %, use %%.

[Source](examples/datepatterns.prg "Source")

**Character encoding**

Demonstrates the import of the same data in different character encodings.

[Source](examples/encodings.prg "Source")

**Show import progress**

How to set up a progress indicator for a lengthy import, using `BINDEVENT()`.

[Source](examples/showprogress.prg "Source")

**Field mapping and filtering**

Demonstrates appending, and how to map between CSV columns and cursor fields (including filtering in CSV data).

[Source](examples/fieldmapping.prg "Source")

**Export to a CSV file**

Demonstrates exporting, including appending to build a segmented CSV file.

[Source](examples/export.prg "Source")

**Import a CSV file with more than 254 columns**

Demonstrates how to import data from a CSV file with more columns than VFP's limit.

a) Default behavior: read only the first 254 columns.

[Source](examples/morethan254columns_first254.prg "Source")

b) Read all columns by importing into as many cursors as needed. Key points: set `MultipleCursors` to .T. and get the names of the cursors from the `MultipleCursorsNames` collection.

[Source](examples/morethan254columns_all.prg "Source")

c) Read selected columns by appending data to a cursor. Key points: create a cursor to receive the data, set `WorkArea`, and map the cursor columns to the CSV columns by `FieldMapping` the required data.

[Source](examples/morethan254columns_selected.prg "Source")

**Import and export binary data**

Demonstrates how to import or export binary data.

Blob, varbinary, and general fields may be exported using hexadecimal, Base64, or plain encoding.

Blob and varbinary fields may be imported using the same set of encodings.

[Source](examples/binarydata.prg "Source")

**Import from clipboard**

Demonstrates how to import from a memory string.

Uses the `ImportString()` wrapper method. To run the example, have an Excel worksheet open at hand and copy its contents into the clipboard.

[Source](examples/fromclipboard.prg "Source")

**Import boxed data**

Demonstrates how to import boxed data, such as in a table, instead of simple separated values.

[Source](examples/importboxeddata.prg "Source")
