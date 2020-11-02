
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

### Code Structure

Under src/lib there are a number of pseudo-packages. Each one is supposed
to represent a separate npm package. Howeever, due to issues with builds and
tests in making them independent libraries, that effort was abandoned.

The structure of the code, however remains the same with an index.js in 
each pseudo-package that defines public exports from that package.

When we get around to understanding exactly how libraries are supposed
to be split off from react code and include their own tests, it will be 
easy to refactor the code to use libraries. Currently, imports from 
pseudo-packages looke like this:

import {HttpUtil} from "lib/js-util/index";

These imports use global paths (see jsconfig.json) for how to set up the 
root of global paths.

With real npm library packages they would look simple:

import {HttpUtil} from "js-util";

Thus moving to use a real library will only require a global replace of 
the absolute paths to just use the library name.

