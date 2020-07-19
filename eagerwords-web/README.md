
# EagerWords React UI

## Development

### Integration Tests

To get some confidence that the UI code and the server are in sync. First
congigure your environment in file ../branch.sh and run it to set up your
environment:

```
  . branch.sh

```

Then start the game server and the UI development server:

```
cd ../scala-server && run.sh
```

And run the integration tests:

```
run-integration_tests.sh
```

The tests should all pass. Note it will take some time for the scala server to
get going.

### Running the UI

With the scala server running, start the UI development server:

```
npm start
```

And bring up game UI at localhost:3000.


