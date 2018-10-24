import { mountSingleTypeahead } from '../../../output/Ocelot.Interface.Typeahead/index.js';

// Create a div to hold the Halogen component
function mkElement(id) {
  let element = document.createElement('div');
  element.setAttribute("id", id);
  element.style.width = "32em";

  // Ensure that you are using ocelot-scoped to avoid any potential
  // CSS conflicts
  element.setAttribute("class", "ocelot-scoped");

  document.body.appendChild(element);
  return element;
}

// Create a valid input
const items = [ { name: "Thomas" }, { name: "Chris" }, { name: "Qian" }, { name: "Jeff" } ]
const input =
  { items: [] // empty to start
  , debounceTime: 300
  , placeholder: "Type to search..."
  , key: "name"
  , keepOpen: false
  }

// Mount the component at the given element
const component = mountSingleTypeahead(mkElement("single-ta"), input);

// Use the `subscribe` function to process outputs from the component.
// You can match on particular outputs by name with _.type and can retrieve
// values with _.value
const subscription = component.subscribe((out) => {
  switch (out.type) {
    case "searched":
      console.log("The user performed a search: ", out.value);
      break;

    case "selected":
      console.log("The user selected an item: ", out.value);
      break;

    case "selectionChanged":
      console.log("The selections have changed: ", out.value);
      break;

    case "emit":
      console.log("Something was emitted, which I'll likely ignore: ", out.value);
      break;
  }
});

// Example: imperatively set status
component.setLoading().then(() => {
  console.log("Now in loading status...");
  setTimeout(() => {
    // Example: imperatively set selections
    component.setSelected([{ name: "Thomas" }]).then(() => {
      console.log("New selections set");
    });

    // Example: imperatively get selections
    component.getSelected().then((selections) => {
      console.log("These are the selections: ", selections);
    });

    // Example: imperatively set status
    component.setItems(items).then(() => {
      console.log("Now in loaded status...");
    });
  }, 3000);
});
