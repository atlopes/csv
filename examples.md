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
The components can be preceded by a digit 1-9, indicating a fixed width (for instance, %2D states that a day takes 2 digits, even if less than 10).

To represent %, use %%.

[Source](examples/datepatterns.prg "Source")

**Character encoding**

Demonstrates the import of the same data in different character encodings.

[Source](examples/encodings.prg "Source")

**Show import progress**

How to set up a progress indicator for a lengthy import, using `BINDEVENT()`.

[Source](examples/showprogress.prg "Source")
