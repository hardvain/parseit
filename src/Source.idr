module Source

public
interface Source a where
  next : a -> (Maybe Char, a)
  empty : a

public
Source String where
  next input = case unpack input of
                    [] => (Nothing, input)
                    (x::xs) => (Just x, pack xs)
  empty = ""

LineNo : Type
LineNo = Int

ColumnNo : Type
ColumnNo = Int

SourceName : Type
SourceName = String

SourcePosition : Type
SourcePosition = (SourceName, LineNo, ColumnNo)
