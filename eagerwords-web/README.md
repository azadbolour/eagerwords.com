
# EagerWords React UI

## Development

### Integration Tests

To get some confidence that the UI code and the server are in sync. First
configure your environment in file ../branch.sh and run it to set up your
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

### The React Environment

The React application is created by using create-react-app, which allows 
the application to be provided with environment variables on startup.
EagerWords uses a few environment variables. A template for the needed
environment variables is provided in the file dotenv.template. For development, 
copy this file to .env.development.local and set its variables to 
appropriate values. For testing, copy to .env.test.local and edit. 
For production, the file .env is used to set the environment variables.
Please refer to the template file dotenv.template for a description of
the accepted environment variables.

### Running the UI

With the scala server running, start the UI development server:

```
npm start
```

And bring up game UI at localhost:3000.





