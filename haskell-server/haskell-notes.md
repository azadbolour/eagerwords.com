
- In development workflow use stack repl and load files into it 
  as they change for quick compilation. It is much faster than
  doing stack builds for uncovering compilation issues.

  - stack repl --test --ghci-options -isrc --ghci-options -itest
  - :l Bolour.Util.MiscUtil
  - :r -- for reload
  - :l BoardGame.Server.Service.GameServiceSpec
  - hspec spec

- To read large text files, use lazy byte strings, i.e., the functions provided by: 
  Data.ByteString.Lazy.Char8. 
  
  There are two differences from using the base functions for reading Strings
  that lead to major savings in time and memory footprint for reading. First,
  there is no need to convert to unicode characters. Second, and perhaps more
  importantly, lazy byte string reading of a file works in a streaming fashion
  (basically using an iterator), avoiding the need to instantiate the entire
  file in memory while it is processed.

  This can save a great deal of memory and time even in cases where the entire 
  data needs to be saved at the end but is manipulated somehow on the way in. 
  
  Just avoid converting to String for as long as possible. It is best to use
  byte strings in the final data structure for the lines, words, etc., of the
  file, and just work with byte strings. Functions that take a string argument
  for searching or otherwise working with the data structure, can convert the 
  string argument to byte string first.

  In this application, reading the masked words file as strings took an 
  inordinate amount of time and memory. Converting the reads and the final Set
  of masked words to byte string reduced memory use by a factor of 4, and 
  time by a couple of orders of magnitude.

- `sequence_ $ print <$> outputLines`

  Just returns: m () - useful in spec tests.

- Debugging technique - when a function from a module under test is 
  called from a test and its behavior is leading to a test failure,
  copy the implementaiton of that function from the module under test
  to the test itself, so that you can easily add print statements
  without affecting the module under test itself. 

- Stack traces.

  See  https://wiki.haskell.org/Debugging.
  ghc Crash.hs -prof -fprof-auto -fprof-cafs && ./Crash +RTS -xc
  But stack only has --profile.
  See profile-bld.sh and profile-run.sh.

  Best way to get stack trace from tests?

  For now I just copy the test code into a main program at trial/TempTrial.hs,
  whose executable is called 'temp-trial'. Then build and run with profiling.

- Named constructor parameters.

    let config = Config {
        Config.pool = myPool,
        Config.env = myEnv
        }

- Avoiding qualification of field names by using pattern matching:

  ```
  {-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE DisambiguateRecordFields #-}
  {-# LANGUAGE RecordWildCards #-}
  import qualified BoardGame.Common.Domain.GridPiece as GridPiece
  setGridPiece grid (GridPiece.GridPiece {piece, gridPoint}) = setGridCell grid gridPoint (Just piece)
  ```

  Need DisambigauteRecordFields for this. Not sure why you have to qualify the data constructor.
  But you do. NamedFieldPuns means that you don't have to have both the field name and a 
  variable name that refers to it - the field name and variable names are the same.

- Decided not to use Data.Matrix to represent grid because it does not provide accurate
  errors, e.g., for index out of range errors. Also it uses 1-based indexing, whereas the rest 
  of this application uses 0-based indexing.

- To use a specific test suite, create a main program for it like test/InFocus/Main.hs,
  and add your specific tests to it as exemplified. Then create the test suite in cabal,
  adding the main you created as its main, and the tests in the suite as other-modules
  (see .cabal file for deatils). To run, see run-test-suite.sh.

- Top-level definitions are always functions that can get called 
  multiple times with the same parameter. If you need to to do something
  time-consuming and cache its results, you need to create a data structure
  that contains the results and also contains functions that are closed on that result.
  See WordDictionary.hs.

- defaultInitialGridPieces :: MonadIO m => Board -> m [GridPiece]

  Compare this with Java interface - not making a commitment to any particular monad.

- Returning raw IO monad from a function is analogous to returning an ArrayList
  from a Java method. Better return interface List. Better return m where MonadIO m
  is the context.

- GridValue.GridValue {value = piece, point}) = Board.setBoardCell bd point piece

  Assignemnt is field = variable - here field name is 'value' but it is areally a piece.

- If a function has special treatment for null list, just use a separate 
  definition clause for the [] case. Using if makes it less readable.

- ghci -XTupleSections 
  (, []) <$> Just "A"

  Just ("A",[])

- class Monad m => MonadError m 

    throwError :: e -> m a
    catchError :: ExceptT e m a -> (e -> ExceptT e m a) -> Except e m a

  If you are doing something inside the IO monad that returns an either
  so that the result is IO Either left right, encapsulate it in ExceptT
  so that first of all you have a monad to work with, and second of all
  you can use catchError in callers. ExceptT is an instance of MonadError.

- Doing a monadic computation requires each step in the do block to return a
  monad. Rather than using a specific monad, you can make the do block as
  general as possible by considering what monad producers are used in the do
  block and restricting the monad to classes that define those functions
  by providing context bounds for the monad.

  This is just Haskell's equivalent of interface-based functions
  in object-oriented programming. MTL implements lots of interfaces
  to use in this way for monad transformers.
  
- An easy way to conceptualize our transformer stack is to think of every 
  function you create as having the following capabilities:

  - In addition to its normal input arguments, it has access to additional 
     _implicit_ or hidden arguments that are passed down to it 
     automatically (the reader part).

  - Its output return type is augmented by an error type, so you have
    the option of either returning normal results (Right) or abnormal
    results (Left) (the except/either part).

  - In addition to the above input and output enhancements, your 
    functions also take another hidden input, an accumulator, and return 
    a possibly transformed accumulator, with some appended data (the writer
    or logger part).

  The transformer machinery allows such functions to be easily 
  composed, so that if you follow one with another the inputs
  and outputs are passed, and returned by the framework.

- Exporting Cache (capacity) exports the data type and its field
  but not the data constructor.

- Ad-hoc polymorphism for fields of a data structure.

  The standard Haskell mechanism for using a type class field
  in a data structure looks requires the data type to be 
  parameterized by a type variable:

  ```
  data PieceGenerator pieceGenerator => Game pieceGenerator = Game {
    ...
    generator :: pieceGenerator 
    ....
  }
  ```

  But this representation is problematic as it forces every use of the type Game
  anywhere in the code base to have a pieceGenerator type parameter. 
  
  Refactoring a conceretely-typed field to an ad-hoc polymorphic field then can
  become a nightmare as even other data structures that have Game as a field
  will need the type variable added. 
  
  More importantly, the piece generator may be a private field of Game, or in
  general be of no concern to a client of Game.  And adding in a then
  meaningless type parameter to Game in these clients just pollutes the code
  base.

  The solution is to use the GHC extension ExistentialQuantification.  With this
  extension in scope, the data definition can avoid using an explicit type
  parameter for the generic type of a field by using the following syntax:

  ```
  data Game = forall PieceGenerator . pieceGenerator => Game = Game {
    ...
    generator :: pieceGenerator 
    ....
  }
  ```

  and the data type remains unparameterized. 
  
  The use of the universal quantifier 'forall' for something that is called
  'existential quantification' is confusing to me. The way I understand it is
  that for every 'existing' type that satisfies the constraint [i.e., ad-hoc
  polymorphic instance of type class] the compiler generates a different
  concrete type.  
  
  On the other hand, by default, the [implicit] 'forall' apply to the entire
  data definition and defines a type-level function [parameterically polymorphic
  data structure that works on every type] whose every use must provide actual
  type parameters.

  See https://wiki.haskell.org/Existential\_type and 
  https://wiki.haskell.org/OOP\_vs\_type\_classes.

- How far down to push the main monad transformer?

  To keep lower-level code as general as possible, it is better not to make it
  dependent on the particular monad transformer stack used by a given main
  program. Rather, figure out which monad interfaces a particular function
  actually needs, and use a monad type parameter that is constrained by the type
  classes of just those interfaces.

  That way the lower-level functions become re-usable by different transformer
  stack, as long as the stack implements the given interfaces.
  
  Here is an example from my DAO layer.

  ```
  addPlay :: (MonadError GameError m, MonadIO m) =>
       Config
    -> PlayRow
    -> m EntityId
  ```

  My main transformer stack includes a reader and a logger. But I decided to 
  do the 'reading' at a higher level and just pass down the part of the 
  environment that is relevant to the DAO layer (namely Config), and otherwise
  just use a monad type parameter that only knows about the IO and the Either 
  effects. 

- Public versus private fields. 

  Decide what fields of a data structure are public and only export those.

- Note that the representation of objects in JSON via aeson depends on field
  names only, and not on constructors or types.

- Servant client - http://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html

- Aeson: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json

- Program resources in Haskell.

  In project.cabal you can set data-files as the location of program resources:

      `data-files:     data/*.txt`

  Cabal then automatically creates a module called `Paths_project`, which 
  you can import and use like this:

  `
  import qualified Paths_project as ResourcePaths
  rootDir <- ResourcePaths.getDataDir -- IO String
  `

  Then you can read files from there, e.g., rootDir/data/some.txt.

## Extensions

- NamedFieldPuns - Pattern match via: Constructor {field1, field2, ... }
  The variables field1, field2, ... are assigned the corresponding fields.
  Obviates the need for explicit association of variables to fields when
  they have the same name.

- DisambiguateRecordFields - The fields in a pattern match do not have to be
  qualified, even if they come from a qualified import, as long as the
  constructor used in the pattern match in qualified.

- QuasiQuotes - Allow quasi-quote macros.

- DeriveGenerics - Allow the compiler to derive instances of Generic
  for data types. Generic provides encoding and decoding functions to a generic 
  representation of data structures, basically a generalization of HList to 
  any data structure.

- RecordWildCards - Allows use of ellipses '..' to represent record fields
  implicitly. Constructor { a = 1, ..} - .. implicitly brings other fields
  into scope.

- FlexibleContexts - Allow more general contexts in a class definition - 
  the parameters of a super-class [aka context class] can be terms rather
  than just type variables.

- GeneralizedNewtypeDeriving - Allows instances definitions for a data type 
  T to be inherited by a newtype based on T. So if T is an instance of class
  C, the newtype can just derive C, and inherit T's dictionary for C's
  functions.

## REPL

- x = 1 + 2 :: Int

- x :: Int; x = 1 + 2

- :sprint x -- print only of already evaluated
