

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

- Should be able to drop a letter on on a board square containing an
  as-yet-uncommitted letter. In this case, the existing as-yet-uncommitted
  letter would be moved back to tray.

- Provide a keyboard shortcut to move a single played but uncommitted tile
  back to the tray.

- Provide a 'vowels' option: if checked the tray is guaranteed to have at least 
  one vowel.

## Known Issues

- Security. CORS. Generally cross origin requests are disallowed. 

  But in doing web development, we use the webpack server at localhost:3000 for
  the Javascript and access the Servant server. We are able to do the latter,
  but not in a verifiably secure manner yet.  

  Adding "http://localhost:3000" to 'Access-Control-Allow-Origin' in the server
  has not worked for me.

  https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

  https://www.html5rocks.com/en/tutorials/cors/

- Graceful message when server is down. Currently the user gets no message.
  But the console gets the undecipherable message (bug): "Cannot read property
  kill of undefined."

## Technical Debt

- Write low-level tests for persistence of games. Including duplicate
  key checks.

- The game cache should go under the persistence layer. An LRU cache would be
  simplest. The current cache is used as a list of live games. Just keep a list
  of live game ids only so that abandoned games can be detected and harvested.

    https://twitter.github.io/util/docs/com/twitter/util/LruMap.html

- Make sure adequate tests exist and pass for the service layer with 
  slick json persistence.

- Validate a piece placed on the board - must be upper case alpha.

- Blue-green deployment for the application.

  Use docker compose to seamlessly upgrade the application.
  
  https://docs.docker.com/compose/gettingstarted/#step-4-build-and-run-your-app-with-compose
  https://docs.docker.com/compose/compose-file/#environment

- Validate all API input arguments. In particular, pointValues is currently 
  not validated in the start game API call.

- See also TODO and FUTURE comments in the code base.

- On initial startup - check that at least the english dictionary exists in the 
  specified directory. If not abort.

- Benchmark think time configuration is currently the maximum. It should be
  the average. Fix PlayPersona. Also provide a generic default implementation
  for getThinkTime in abstract persona.

- Use database transactions. 

- Clean up database migration code.

- Clean up tests.

- Standardize errors sent to the client so we get the same API behavior from all
  servers.

- Test for cache full.

- Performance of large databases. Indexing. 

- Use JSHint for Javascript code.

- CICD. Integrate with TravisCI on github. Free for open source.

- More testing of checks for correctness of crosswords created by play.

## Development Nice-to-Haves

- Use typed JS code where possible. See for example, React DND Knight example:
  https://github.com/react-dnd/react-dnd/tree/master/packages/documentation/examples-decorators/src/00-chessboard

- Use NIX for reproducible builds.

- Check that you can debug minimized UI code with source maps in chrome.

- Put the persistence layer in a microservice so we won't have to port it to 
  the specific database access facilties of different languages. Learning the 
  specific and sometimes arcane bahaviors of all different database packages 
  for different languages is not the best use of our time.

  Perhaps use GraphQL.

## Testing

- Test scala against postgres.

- Test preferred input device thoroughly on various platforms.
  For now just using mouse.

- Use a different database for tests in general as well as for 
  integration tests. Use random user id for integration tests,
  so they do noyt have to clean up after themselves. Just remove
  the test database manually every so often.

  /path/to/bin/<project-name> -Dconfig.resource=production.conf

  You can supposedly just package different config files with the app in the conf directory.
   
  -- sbt '; set javaOptions += "-Dconfig.file=conf/integration.conf"; run'
  -- sbt '; set javaOptions += "-Dconfig.file=conf/integration.conf"; test'
  -- sbt '; set javaOptions += "-Dconfig.file=conf/integration.conf"; testOnly Integration\*'

- React UI unit testing where it makes sense. Found it hard to simulate drag and
  drop circa 2018. Perhapse it is easier in 2020.

  Either complete the mock UI api (call it stub) caller or get rid of it. 
  Need to decide if keeping it is worth the effort for UI unit tests.

- Player email and user id have to be unique. Test with Okta.

## Dicionary Errors

- Button to notify developers about dictionary mistakes - English word rejected
  or non-English word accepted.

## Perfomance

- Index games in the database by userId.

  http://scala-slick.org/doc/3.0.0/schemas.html#index-13
      
  def idx = index("idx_a", (k1, k2), unique = true)

## Authentication 

- To the extent possible customize the signin and the signup pages used by Okta.

## Recovery

After a crash the running games will be marked as running in the database.
Ideally recovery from a crash should mark them as suspended.

Test that no matter when the crash occurred, it can be resumed. It could be
after a commit of a user play but before the corresponding machine play, etc.

## Migration

- Proper migration - use Liquibase or lighter migration framework.

## Deployment to AWS CloudFront

- See cloudfront notes. Referesh memory by taking another look at AWS. Read
  the docs.

  You need to build in the server URL at deployment time. Should have a template
  for that that gets used at deploy time - and a parameter to the deploy script. 
  Rmove from the source code.

  But it will still be in the UI code base and exposed to snooping. 
  Not sure how to protecte that.

- Deploy needs to change env from dev to prod in config.js.

## Server Deployment

Try to minimize the footprint of the scala server. Use smaller docker Linux image?

Try the docker image on different systems. Try a1xlarge - 8G - 4 vCPU - 1000 for
3 years.

T3 optimized for bursty applications. That is us. You get credit up to 24 hours
for time not used.  t3.large 8G 36 cpu credits/hour - 2 cpu units So I guess
that means if you are idle for an hour - you get to use 18 minutes at full
bore. Seems reasonable for us. A bit cheaper.

## IT

- IT instructions - checking logs, restarting server, etc.

## Reviewer Comments
    
- Make it more colorful. Also better color scheme to make letters more readable.
  Background color?

- Timer to tell how much time elapsed from previous play, how much time is left.
  Use progress bar - https://react-bootstrap.github.io/components/progress/.

