Prim
=============
type Parsec s u = ParsecT s u Identity

data State s u = State {
      stateInput :: s,
      statePos   :: !SourcePos,
      stateUser  :: !u
    }
    deriving ( Typeable )

-- | ParserT monad transformer and Parser type

-- | @ParsecT s u m a@ is a parser with stream type @s@, user state type @u@,
-- underlying monad @m@ and return type @a@.  Parsec is strict in the user state.
-- If this is undesirable, simply use a data type like @data Box a = Box a@ and
-- the state type @Box YourStateType@ to add a level of indirection.

newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 State s u
              -> (a -> State s u -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> State s u -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
              -> m b
             }
===============
Error
===============
data ParseError = ParseError !SourcePos [Message]
    deriving ( Typeable )