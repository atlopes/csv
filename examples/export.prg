DO LOCFILE("csv.prg")

* demonstrates export

CREATE CURSOR HeaderInformation (InfoType Varchar(40), InfoValue Memo)

INSERT INTO HeaderInformation VALUES ("Type", "Music")
INSERT INTO HeaderInformation VALUES ("Genre", "Pop, folk, rock, fusion")
INSERT INTO HeaderInformation VALUES ("Medium", "Vinyl record")

CREATE CURSOR Discography (Id_Record Integer, Artist Varchar(40), Title Varchar(40), DateRelease Date, Duration Integer)

INSERT INTO Discography VALUES (1, "Frank Zappa", "Sleep Dirt", {^1979-01-19}, 2353)
INSERT INTO Discography VALUES (2, "The Waterboys", "Fisherman's Blues", {^1988-10-17}, 3277)
INSERT INTO Discography VALUES (3, "Eugenio Finardi", "Sugo", {^1976-01-01}, 2309)

GO TOP

LOCAL CSV AS CSVProcessor

m.CSV = CREATEOBJECT("CSVProcessor")

ERASE ~temp.csv

* the first cursor to be exported will be the file header
m.CSV.WorkArea = "HeaderInformation"
m.CSV.HeaderRow = .F.
m.CSV.Export("~temp.csv", .T.)

* now, the main cursor
m.CSV.WorkArea = "Discography"
* prepare the headers for the columns
m.CSV.FieldMapping.Add("Id_Record", "ID#")
* note: not in the same order of the cursor structure
m.CSV.FieldMapping.Add("Title", "Album title")
m.CSV.FieldMapping.Add("Artist", "Artist(s)")
m.CSV.FieldMapping.Add("DateRelease", "Release on")
m.CSV.FieldMapping.Add("Duration", "Duration (in seconds)")
m.CSV.HeaderRow = .T.
* a two line space
m.CSV.SkipRows = 2
* append to the previous export
m.CSV.Export("~temp.csv", .T., .T.)

* the result
MODIFY FILE ~temp.csv NOEDIT
