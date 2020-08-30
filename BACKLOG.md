

# Board Game Application Backlog

## System Improvements

- Add dictionaries for other languages to the system. Include user's preferred
  language for words to be played in the settings page.

- Some two-letter words in the dictionary don't mean anything to me.
  Clean up two-letter words, and perhaps add in common abbreviations.

  Get list of 2-letter words.  `egrep '^..$' moby-english.txt`

  Provide a way for the user to see which two-letter words are allowed.
  Maybe a button or a hover action.

- Show all cross words found in the UI so user can ascertain their meanings.

- Clicking on buttons in iPad browsers at times just shows the tooltip.
  And a second click is needed to actually press the button. Check again
  and improve user experience if possible.

- Identify an initially empty word list by an initial item 'words played',
  which is replaced by the first played word.

- Multi-player games. The control of the game would be changed to the
  server-side. Server tells each player it is their turn and obtains 
  the play. Maybe use an actor as the representative of each player 
  in the server.

- Replaying games - going backwards and forwards in games.

- Benchmarking the game. Benchmarking code exists for the earlier version
  of eagerwords (the boardgame project). Copy and upgrade as appropriate.

## User Suggestions

- Keyboard shortcut for undo.

- A 'vowels' option: if checked the tray is guaranteed to have at least 
  one vowel.

- Ability to suggest words to add to the dictionary. Automatically check againt
  a well-known online dictionary.

- Make it more colorful. Also better color scheme to make letters more readable.
  Background color?

- Timer to tell how much time elapsed from previous play, how much time is left.
  Maybe progress bar - https://react-bootstrap.github.io/components/progress/.

## Known Issues

- Graceful message when server is down. Currently the user gets no message.
  But the console gets the undecipherable message (bug): "Cannot read property
  kill of undefined."

- Back and forward button behavior not regulated so far by the application. Back
  button from a modal should just close the modal. Back or forward button fro
  elsewhere should have an intuitive interpretation, or, should tell use to
  please use the application's navigation's navigation controls. But there has
  to a simple way for the user to get out of the content of eagerwords and go
  back to what they were doing before.

## Technical Debt

- Remove the game cache from the server code. Each api call needing a
  game would first retrieve it from the database. For good measure, 
  use optimistic concurreny control using versions.

- Server-side validations of data. Much is expected to have be valid as
  its only client is the eagerwords UI.

  Example, pointValues is currently not validated in the start game API call.

- Blue-green deployment.

  Use docker compose to seamlessly upgrade the application.
  
  https://docs.docker.com/compose/gettingstarted/#step-4-build-and-run-your-app-with-compose
  https://docs.docker.com/compose/compose-file/#environment

- See TODO and FUTURE comments in the code base.

- On initial startup - check that at least the english dictionary exists in the 
  specified directory. If not abort.

- Clean up tests.

- Performance of large databases. Indexing. 

- Use JSHint for Javascript code.

- CICD. Integrate with a CI frameword on github. 

- More testing of checks for correctness of crosswords created by play.

## Development Nice-to-Haves

- Use typed JS code where possible. See for example, React DND Knight example:
  https://github.com/react-dnd/react-dnd/tree/master/packages/documentation/examples-decorators/src/00-chessboard

- Understand debugging of Javascript with source maps in the browser.

- Asynchronous sending of emails.

## Testing

- Test preferred input device thoroughly on various platforms. Touch needs 
  testing.

- Use a different database for tests in general as well as for integration
  tests. Use random user id for integration tests, so they do not have to clean
  up after themselves. Just remove the test database manually every so often.

- React UI unit testing where it makes sense. Found it hard to simulate drag and
  drop circa 2018. Perhapse it has become easier.

  Either complete the mock UI api (call it stub) or get rid of it. Need to
  decide if keeping it is worth the effort for UI unit tests.

## Dicionary Errors

- Button to notify developers about dictionary mistakes - English word rejected
  or non-English word accepted.

## Perfomance

- Indexing in general. 

  Example: Index games in the database by userId. 

  http://scala-slick.org/doc/3.0.0/schemas.html#index-13
      
  def idx = index("idx\_a", (k1, k2), unique = true)

## Recovery

After a crash the running games will be marked as running in the database.
Ideally recovery from a crash should mark them as suspended.

Test that no matter when the crash occurred, it can be resumed. It could be
after a commit of a user play but before the corresponding machine play, etc.

## Migration

- Proper migration - use Liquibase or lighter migration framework.

## Deployment to AWS CloudFront

- Add ALB in front of server machine.

- Further scripting for production deployment.

## IT

- IT instructions - checking logs, restarting server, etc.

