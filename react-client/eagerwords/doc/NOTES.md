
- Location of global installs for npm:

    npm config get prefix

  To change:

    npm config edit

- Info about npm packages:

    npm view package-name 
      gives information about latest version and dependencies

    npm show react-dnd@* version
      gets all the versions

    npm show react-dnd version
      gets latest version

- To upgrade a node package remove it from package.json dependencies and:

  npm install --save-dev package-name

- Useful links for touch vs mouse:

    https://github.com/react-dnd/react-dnd/issues/743 - windows 10 firefox issue
    https://developer.mozilla.org/en-US/docs/Web/HTTP/Browser_detection_using_the_user_agent
    https://github.com/react-dnd/react-dnd/issues/298 - discussion
    https://github.com/jcoreio/react-dnd-multi-backend - has the code for it

- Some useful react-dnd links:

    https://github.com/yahoo/react-dnd-touch-backend
    https://www.npmjs.com/package/react-dnd-touch-backend
    https://github.com/yahoo/react-dnd-touch-backend/blob/master/examples/js/DraggableItem.jsx

    https://medium.com/@adamrackis/react-dnd-intro-for-the-redux-developer-d447c2c1577b
    https://github.com/react-dnd/react-dnd/blob/master/examples/02%20Drag%20Around/Custom%20Drag%20Layer/BoxDragPreview.js
    https://github.com/react-dnd/react-dnd/blob/312269a244a19ae8f25b6ec9a3847c90c1ae9cdf/docs/00%20Quick%20Start/Tutorial.md

    https://react-dnd.github.io/react-dnd/docs-drag-source-connector.html

    https://reactjs.org/docs/optimizing-performance.html

- On MAC got the following error after a clean npm install. 

  xcode-select: error: tool 'xcodebuild' requires Xcode, but active developer directory 
  '/Library/Developer/CommandLineTools' is a command line tools instance

  Reinstalled developer command line tools and the error did not go away.
  Could not see solution. Decided to ignore for now.

- Always use arrow functions when passing a component method to an inner render 
  handler. Otherwise, this will be undefined when the "method" executes.

- Use pre tags for spaces and just add spaces. Do not use &nbsp; it gets turned
  into circumflex A in unpredictable ways when using srevant and react.

- jest - test.only and test.skip.

- jest - default timeout is 5 seconds. For integration tests against the Scala
  Play server, the first interaction with the server causes the main server
  initialization steps which can take upwards of 30 seconds. To be safe, we set the 
  the option --testTimeout=60000 for the test script on package.json.

- Use let instead of var. Use const where possible.

- http://racingtadpole.com/blog/test-react-with-jest/

- https://react-dnd.github.io/react-dnd/docs-testing.html

- http://www.2ality.com/2014/09/es6-modules-final.html

- https://reactjsnews.com/testing-drag-and-drop-components-in-react-js

- Modules imported with require are mocked automatically in jest.

- Jest provides an it.only() function to run a single test. 

- This depends on caller of function - review your own notes.
  Read the javascript the best parts book again. Also the secrets.

- Multiple thens can have the same catch. The implementation of
  when then goes wrong calling the reject function remains a mystery.

  The implementation of how tests wait for promises to be 
  fullfilled remains a mystery.

- Good writeup on js modules and import:

    http://www.2ality.com/2014/09/es6-modules-final.html
    You shoulid just use that.

- Rather than module.exports use export default something.
  You can still export other stuff by prefixing - like public.

- https://facebook.github.io/jest/docs/tutorial-async.html

- Promises, futures, and monads in JS.

  https://hackernoon.com/from-callback-to-future-functor-monad-6c86d9c16cb5#.5ff3cjepw
  https://blog.jcoglan.com/2011/03/11/promises-are-the-monad-of-asynchronous-programming/

  Is this really the api?

      resolve(response.json) - actually should just get an object not json
      reject(response.status, response.json()) - again it should just be an error object
        or response.json.meta.error

- Cannot have 2 html5 backends at the same time. Put the context at the highest level.
  The lower levels will inherit.

  https://github.com/react-dnd/react-dnd/issues/186

- https://github.com/facebook/jest/tree/master/examples/react
  https://facebook.github.io/jest/docs/tutorial-react.html
  https://facebook.github.io/jest/docs/getting-started.html
  https://facebook.github.io/jest/docs/api.html
  https://github.com/facebook/jest/blob/master/examples/snapshot/__tests__/Link.react-test.js
  https://facebook.github.io/jest/docs/expect.html

- https://developers.google.com/web/fundamentals/getting-started/primers/promises

  this is a pretty thorough explanation

- fetch https://davidwalsh.name/fetch 

- https://davidwalsh.name/fetch

- javascript promise notes

  http://www.datchley.name/es6-promises/

  var p = new Promise(function(resolve, reject)

  The resolve callback is called when the value becomes available.
  The reject callback is called if the value can't be provided.

  var p = Promise.resolve(42);

  Q: Is there a relationship between resolve and then, reject and catch?
  Who provides the resolve and how is the "then" called when resolve is called?
  Who provides the reject ditto.

  catch is a shorthand for then(null, rejectHandler)

  A return value from a then is automatically wrapped in a promise.
  That is why then's can be chained.

  Can use just one catch at the end of a promise chain.

  Looks like you can have multiple chains attached to a single
  promise.

  One thing that is confusing is that the promise api confounds
  promises with values. Automatic wrapping and unwrapping 
  in promises reduces code bulk, but obsures semantics.

  Here is an example from http://www.datchley.name/promise-patterns-anti-patterns/:

  firstThingAsync()  
  .then(function(result1) {
    return Promise.all([result1, secondThingAsync(result1)]); 
  })
  .then(function(results) {
    // do something with results array: results[0], results[1]
  })
  .catch(function(err){ /* ... */ });

  Is result1 a promise or a value inside a promise? It is both
  depending on context. In the context of Promise.all it is a promise.

  But now the semantics get complicated because we have to specify 
  the places where wrapping and unwrapping take place to fully understand
  what is going on.

  Interesting chaining example from same blog:

  // Promise returning functions to execute
  function doFirstThing(){ return Promise.resolve(1); }  
  function doSecondThing(res){ return Promise.resolve(res + 1); }  
  function doThirdThing(res){ return Promise.resolve(res + 2); }  
  function lastThing(res){ console.log("result:", res); }
  
  var fnlist = [ doFirstThing, doSecondThing, doThirdThing, lastThing];
  
  // Execute a list of Promise return functions in series
  function pseries(list) {  
    var p = Promise.resolve();
    return list.reduce(function(pacc, fn) {
      return pacc = pacc.then(fn);
    }, p);
  }
  
  pseries(fnlist);  

- Problem trying to solve - make two async calls in succession.
  Each can be rejected or can return a !OK status but not rejected.

  https://stackoverflow.com/questions/33445415/javascript-promises-reject-vs-throw

  http://2ality.com/2016/03/promise-rejections-vs-exceptions.html

- Comments is JSX - use curly braces with javascript block comments.

            {/*
            <div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div>
            <div style={paddingStyle}>
              <button onClick={this.machinePlay} style={buttonStyle()}>Machine Play</button>
            </div>
            */}

- If you need something to change in the UI without explicit re-rendering,
  you need to change the state of the component, and have the UI element's
  look depend on the state. For example, you can define a state variable 
  for the component that the cursor is currently on top of, and show a 
  a tool tip for that component conditionally. Conditional rendering 
  uses the following idiom:

  `{someCondition && <div>render me</div>}`

  The condition may use state variables.

- Seems like the recommendation is to use functional components
  (components defined by a function) for stateless components that 
  do not need lifecycle functions, and use classes inheriting
  from component if you need state or lifecycle support.
  
  https://medium.com/@dan_abramov/how-to-use-classes-and-sleep-at-night-9af8de78ccb4

  You might try to change to stateless functional components where
  possible however.

- DragPreview is a component - it should basically be a copy of the component 
  being dragged with some indication that it is being dragged as shown here
  
  https://github.com/react-dnd/react-dnd/blob/master/examples/02%20Drag%20Around/Custom%20Drag%20Layer/BoxDragPreview.js

  dragPreview is a function that returns a function that can be used to create
  the preview component.

  It works like the dragSource function and can be injected in the same manner.

## Observations by AlleyCat at various points in the code.

- GameHandler.js: 'consider using uerror instead of error': with `uerror` the
  user gets a message.

- gameSelector.js: before our refactor (pre-reselect), `mkGameHandler` and
  `mkUserHandler` were called several times while the board was prepared. In the
  current code, although selectors are memoised, they still *may* be called
  several times. This is for sure a performance hit. Also, we must be sure they
  are pure functions, or else it will lead to bugs.

- In the components: if we want to use async in front of React lifecycle methods
  we need to wrap the whole function body in `try/catch`. This is because this
  function is called by React itself and they don't know how to deal with our
  rejected promises, resulting in 'Unhandled promise rejection error'.

  The model we use in lifecycle methods is to have the library function
  (`checkAuthentication`, the `UserHandler` functions, etc.) catch the error,
  log it / show the bubble, and rethrow it, to keep the code on the calling end
  light. All we need to do on error in the component code is return. Also, this
  allows us to wrap the entire body in `try/catch`, instead of each individual
  await statement, because we don't care any more why it's rejecting.

- The call `this.props.onLogout()` in e.g. GamesComponent both clears the local
  auth state and logs us out remotely at Okta.

- At the end of PlayComponent, several functions access `evt.gameId`, but the
  argument is just `gameId`.

- PlayComponent: `onExit` gets an unneeded argument. We've marked this with
  `void`, which can help keep strict linters happy or just show that we're aware
  it's not being used.

- helpers.js: (Probably a moot point now, but still): `setState` might end up
  being called after the component has already unmounted, since this function is
  asynchronous. This leads to warnings in the dev console about a memory leak.

- AuthReducer: Remember that a user might be able to tamper with these values
  without all too much trouble from the browser and therefore not to depend on
  them too heavily. 

- Suggestion: store string sizes (small, normal, large) on the server instead of
  pixels. This will make the client code simpler, as we are currently doing
  several forward & reverse conversions.

- Places where a render method returns null because some condition hasn't been
  satisfied yet are good places to show a loading indicator, e.g. a spinner.

- GamesComponent line 341, also PlayComponent line 511: The reason for putting
  the rest into a separate component Main is to avoid weird artifacts with
  elements with a high z-index appearing over the modal.

## General tips from AlleyCat.

- Note that there are caveats associated with updating the store from inside a
  component lifecycle method: it could cause an infinite loop.

- You can return an object from an arrow function using parentheses:

    it.setState((state) => ({ ...state, games }))

  (See GamesComponent line 199).

- `mapStateToProps` and `mapDispatchToProps` probably don't need to be exported.
  (An exception would be if they are called by unit testing code).

- Deconstructing objects and arrays can be a nice idiom, e.g.

    let { props, } = this;
    let { gamePlayCanCommit, gamePlayCanEnd, ..., } = props;
    ...

- FYI, when using the non-standard 'class properties' syntax, methods are
  automatically bound and don't need a separate `.bind(this)`. (See Eula.js line
  59).
