
## CICD

CI - github actions is one possibility.

For Scala - https://diamantidis.github.io/2020/05/17/ci-with-github-actions-for-scala-project.

For React and integration tests through javascrip api??

## API Client

Package the common code in a client library, and use it in a java client. The
benchmark can then be done in terms of the scala client library. The client API
calls should really be derived from an API specification (Swagger, etc.
something that is easy and commonly used).

(The precursor project to eagerwords (boardgame) included a hand-written Java
client (in its java-client directory) for expedience.)

## Minimum Hardware Requirements

The application runs well for occasional use on an EC2 m3.large machine. 
It is useful to try to pare it down and see if it is runnable of a small 
machine with only 4G of memory.

## Production Application Best Practices

App logs in docker should go to the data area so you can see them easily from the
outside.

Truncate nohup.out on production machine.

Logging of the error details for requests that are rejected by the play application,
externally to the application code.

Security audit. Passing play secret to the docker container securely.

## Factor Word Frequency in Scores

Use word frequencies to scale the score for a word. Perhaps use a logarithmic
scale of word frequencies, scale it from 1 to 3, call it 'rarity'. The score is
multiplied by the rarity factor.

http://norvig.com/ngrams/count\_1w.txt. Unfortunately this is not a clean set.
I'd say about 50% are non-words. So just use it for frequencies - not as a
dictionary. If a word in not in the list, consider it very rare.

https://www.wordfrequency.info/purchase.asp $250.00 for 100K words
https://www.wordfrequency.info/files/entries.pdf. This is a clean list. But 100K
is too few. The least frequent words are still relatively common.

## Misc 

- The port number for docker has to be specified redundantly in a couple of
  docker scripts. Make it dry if possible.

- Controller tests considered flaky because of timeouts. For now we are not 
  distinguishing flaky tests.

- Intellij scala compile gets stack overflow even with 4096m stack!
  Related to the circe json library, it seems.

- Use continuous builds in development: sbt ~build, etc.
