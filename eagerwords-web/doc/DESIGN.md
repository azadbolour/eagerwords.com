
# Design of the Game UI

The design is inspired by but does not necessarily follow the flux pattern.

The data needed by a component to be able to render itself is modeled
a separate module. That module acts as what flux calls a store. 
It is named with the base name of the concept being modeled: for example,
Game. The corresponding component is then named by suffixing with 'Component',
e.g., GameComponent.

User interface events lead to events being created that are listened to 
by GameEventHandler. React's dispatcher is used to publish
and subscribe to events.

GameEventHandler has a Game field that keeps track of the current state of the
game. Upon receiving an event, the game event handler updates its Game field 
appropriately, and then in turn emits a change event. The change event is 
listened to by the game compoenent, which then renders the new state of
the game.

A change that affects two different components is best handled by 
the closest common ancestor of those components, I think. In other 
words, that ancestor responds to the change event and then updates 
the two descendants. In this approach there will only be one 
change event emitted by a state: the one emitted by the state 
of the common ancestor. 

The other approach is to have each of the several affected component
states recognize that event and respond to it independently. The problem there 
is that then each of the states will emit an independent change event.
Then how should a view that depends on the change as a whole deal with
these several events. Unclear.

One problem is that the entire page seems to need to be redrawn 
(at least as far as the application is concerned) in the React model.
React will optimize redraws, of course, to just redraw what is needed.
I don't really know how to redraw a lower level component. That means
that any single change will lead to a redraw of the highest anecestor
of all the effected components, and the notion of getting several 
change emits from a single event becomes problematic.

I guess one can have independent responses in each affected component,
as long as the highest level component gets that complex event and 
just emits a change and that change in the only one picked up by the 
page as a whole. That might be the cleanest approach.

But still we have the problem that we need the state changes to occur
before the game is redrawn. And to control the ordering considerably
complicates things. You have to use dispatcher.waitFor the other callbacks,
which is kind of ugly.

We don't really need to do that for our application. So let's
keep things simple and not have lower-level component states
respond independently to the move event.

### How to Add a New Action

To add a new user interface action to the UI:

- Add a game action type to GameActionTypes.js for the new action.

- Add an action dispatch method for the new action to the list of action
  dispatchers in GameActions.js. The dispatch method simply uses the game
  dispatcher to notify interested components of the action event.

  The dispatch method may take parameters describing the action, and it passes
  the parameters in the object describing the action's event.

- Add button to GameComponent to trigger the action and provide an onClick
  method for it that calls the function's action dispatcher.

- Add a handler for the action's event in GameEventHandler.js. This 
  is where the internal functions of the action are implemented.
  
  In this implementation, the state of the world is represented 
  by two internal state variables: _game_ designating the current
  state of the game (see Game.js), and _status_, a string variable
  that tracks the completion state of the last action.
  
  The implementation does some computation that may include API
  calls, changes the state of these variables based on the
  result of the computation, and then notifies interested 
  listeners of the new state of the world using the function
  _emitChange_.

- In the initial implementation the only listener for changes
  to the state of the game is the main page of the application
  (index.js) which re-renders the game component using its new state.

### Asynchronous API Calls

An event handler in the state object may need to make API calls
to a server. These calls are normally done asynchronously. We can choose
not to emit the change of state event from the handler until the 
async callback happens. That is the simplest approach. 

In a more complicated case, we may want to make some UI changes immediately,
while the async call is being processed in the background, and then
make another UI change based on the result of the API call. In this case,
we can just emit two different change events one synchronously, 
and one in the async callback.

The main issue with async calls is that the user has to be prevented 
from doing some user interactions in cases when the next user interaction
depends on the result of the async API call. 
Also when the API call comes back, it could overwrite 
the results of the concurrent user interaction. So the easiest approach
is to give the user the appearance of being free to do things, but 
actually do nothing in response to an actual interaction by the user
before the async callback is called. I guess one can just have an
async-in-progress flag and drop all events on the floor when it is on.

Actually you need 3 different events - before request, 
request succeeded, and request failed.

This is a good document about async requests - though it is about redux
which I have chosen for simplicity not to use right now.

http://redux.js.org/docs/advanced/AsyncActions.html

## Error Processing

Two types of errors may be returned by HTTP calls: errors generated by 
server side developer code, and errors generated by the server framework used 
to communicate with clients through HTTP. 

Errors generated by developer code can be transmitted to the client through the 
body of the response. A good strategy is to convert the exception that 
causes the error into JSON having the name of the exception, and its instance
variables as JSON properties. That way the front end has complete freedom 
in constructing the error message presented to the user. These errors are 
transmitted as _unprocessable_ (status 422) in the response.

While this is the strategy followed by the server, the server also includes
the exception message as defined in the server as the _message_ property
of the transmitted JSON in the body of the response. Currently, for expedience,
the client simply uses that message to notify the user of the user errors.
But this is an interim solution. When the application is internationalized, the 
messages need to be constructed in the locale's language by using the specific 
details of the exception transmitted in the response JSON. 

Errors generated by frameworks are beyond the control of the the developer and
likely cannot be standardized over different server-side HTTP frameworks and
programming languages. In this case, the body sometimes include HTML that
embodies the default representation of the error in the browser. Examples of 
these errors are status 400 (bad request - e.g., validation errors), and status
404 (not found - bad path).

The UI code uses the _status_ of these errors and produces its own messages.

In summary, if the HTTP response is not OK, then for 422 errors, the 
body is currently assumed to be a JSON data structure containing a message. 
And that is the default message presented to the user. For non-422 statuses,
the UI produces the message.

The client API layer of the UI simply returns the HTTP response obtained from
the server. The service layer of the UI converts HTTP responses to Result data
structures. In our application, a Result data sructure is a javascript
incarnation of the Scala Try data structure or the Rust Result data structure.
It has two fields: ok, and data. The ok field is true iff the call completed
successfully. The data field represents the successful return value in case of
success, or a standard representation of the error in case of failure. The
standard representation of errors includes the fields tag, message, and 
classifier. The tag is the server error identifier. The message is the server 
error message by default, or a more intention-revealing message created in 
the UI code. The classifier categorizes errors as recoverable, unrecoverable,
loggedOut, and warning. It is used for generic error processing in UI elements.

When a UI component calls a service-level function and obtains an error, the
errors is reflected in the state of the component, and generic error processing
is triggered based on the type of error. For recoverable errors, the
presentation remains the same, except that the error message is rendered in 
the component's message area. For unrecoverable errors, we use the simplest
strategy: just send the user back to the landing page.

ComponentProcessingStateManager provides a generic framework for dealing with
errors in components. See comments there for details of how errors are 
detected and presented generically in components.

## Passwordless Authentication Notes

See also passwordless.notes - though that document may not be competely current.

Please see source code and comments file ComponentProcessingStateManager.

## API

TBD.

## Notes

Watch out for circular dependencies in requires. You get weird errors.

In order to separate private from public fields of objects, we use the module
pattern. A factory method defines private variables and then returns an object
with public fields. The object has closed on the private variables and has
access to them. But they are inaccessible to its clients.

To avoid flashing in the UI, do not set the state of a component in two
different calls one of which is async.

