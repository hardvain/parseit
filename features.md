* Abstract char into token

## Parsers
newline - parses '\n'
carriageReturn - parses '\r'
crlf - parses '\r\n'
eol - parses crlf or newline
tab - parses '\t'
space - skip zero or more whitespace chars
space1 - skip one or more zero whitespace chars
controlChar - Parse a control character (a non-printing character of the Latin-1 subset of Unicode).
spaceChar - Parse a Unicode space character, and the control characters: tab, newline, carriage return, form feed, and vertical tab.
upperChar - Parse an upper-case or title-case alphabetic Unicode character.
lowerChar - Parse a lower-case alphabetic Unicode character.
letterChar - Parse an alphabetic Unicode character: lower-case, upper-case, or title-case letter, or a letter of case-less scripts/modifier letter.
alphaNumChar - Parse an alphabetic or numeric digit Unicode characters. Note that the numeric digits outside the ASCII range are parsed by this parser but not by digitChar. Such digits may be part of identifiers but are not used by the printer and reader to represent numbers.
printChar - Parse a printable Unicode character: letter, number, mark, punctuation, symbol or space.
digitChar - Parse an ASCII digit, i.e between “0” and “9”.
octDigitChar - Parse an octal digit, i.e. between “0” and “7”.
hexDigitChar - Parse a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”, or “A” and “F”.
markChar - Parse a Unicode mark character (accents and the like), which combines with preceding characters.
numberChar - Parse a Unicode numeric character, including digits from various scripts, Roman numerals, etc.
punctuationChar - Parse a Unicode punctuation character, including various kinds of connectors, brackets and quotes.
symbolChar - Parse a Unicode symbol characters, including mathematical and currency symbols.
separatorChar - Parse a Unicode space and separator characters.
asciiChar - Parse a character from the first 128 characters of the Unicode character set, corresponding to the ASCII character set.
latin1Char - Parse a character from the first 256 characters of the Unicode character set, corresponding to the ISO 8859-1 (Latin-1) character set.
char - parses a single character c
char' - The same as char but case-insensitive. This parser returns the actually parsed character preserving its case.
anyChar - This parser succeeds for any character. Returns the parsed character.
notChar - Match any character but the given one. It's a good idea to attach a label to this parser manually.
oneOf - oneOf cs succeeds if the current character is in the supplied collection of characters cs. Returns the parsed character. Note that this parser cannot automatically generate the “expected” component of error message, so usually you should label it manually with label or (<?>).
noneOf - As the dual of oneOf, noneOf cs succeeds if the current character not in the supplied list of characters cs. Returns the parsed character. Note that this parser cannot automatically generate the “expected” component of error message, so usually you should label it manually with label or (<?>).
satisfy - The parser satisfy f succeeds for any character for which the supplied function f returns True. Returns the character that is actually parsed.
string - string s parses a sequence of characters given by s. Returns the parsed string (i.e. s).
string' - The same as string, but case-insensitive. On success returns string cased as actually parsed input.




