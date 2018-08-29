# Interfaces

This section holds the JavaScript interfaces for various Halogen components. These provide easy interop for teams who wish to use these Halogen components within JavaScript applications.

### Example

```js
import { textField } from 'bower_components/purescript-halogen-ocelot/output/Interfaces/TextField/index.js';

// Mount the component at a given HTML element
const component = textField(getElement());

// Use the `subscribe` function to process outputs from the component.
// You can match on particular outputs by name with _.type and can retrieve
// values with _.value
const subscription = component.subscribe((out) => {
  switch (out.type) {
    case "textChanged":
      console.log("Something changed: ", out.value);
      break;
  }
});

// If the component supports _requests_ that return a value,
// you can use them like this:
component.getText().then((value) => {
  console.log("Fetched the current value: ", value);
});

// You can use component _queries_ to trigger behaviors in
// the component like this:
component.setText("Hello, I'm Qian!").then(() => {
  console.log("The value was set.");
});

// Now, the value ought to have changed to the previous message
// and we can retrieve it.
component.getText().then((value) => {
  console.log("Fetched the current value: ", value);
});
```
