# CSVProcessor #

A VFP class to process CSV files.

### Overview ###

* Imports data from CSV files
* Automatically generates a cursor (or a table in a database)
* Also accepts headerless CSV files 
* Reads Memo-like fields, including multiline fields
* Detects also Integer, Double, Logical, Varchar(), Date, and Datetime data
* Date and Datetime source data defined by format patterns
* Sensitive to UNICODE encodings (LittleEndian, BigEndian, and UTF-8)

### Using ###

* See [UNLICENSE](UNLICENSE.md).
* In a project, include csv.prg and its dependencies
* `DO LOCFILE('csv.prg')` to put the class in scope
* Create an object, and use it (see [DOCUMENTATION](DOCUMENTATION.md) for more info).

### Dependencies ###

* `CSVProcessor` depends on the [Namer](https://bitbucket.org/atlopes/names "Namer") class.

### Contributing ###

* Test, use, fork, improve.
* Review, suggest, and comment.

### To-Do ###

* Field filtering...
* Append...
* Export...

### Talk, talk, talk... ###

* atlopes, found here, at [LevelExtreme](https://www.levelextreme.com) (former UT), [Foxite](https://www.foxite.com), and[Tek-Tips](https://www.tek-tips.com).