import { mountSingleTypeahead } from '../../../output/Ocelot.Interface.Typeahead/index.js';
import { mountDropdownTypeahead } from '../../../output/Ocelot.Interface.Typeahead/index.js';
import { mountDropdown } from '../../../output/Ocelot.Interface.Dropdown/index.js';

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
const items = [ { name: "Thomas" }, { name: "Chris" }, { name: "Qian", email: "qian@cn.com", icon: "https://www.citizennet.com/hubfs/images/website/icons/icn-facebook-glyph.svg" }, { name: "Jeff" } ]
const input =
  { items: [] // empty to start
  , debounceTime: 300
  , placeholder: "Type to search..."
  , key: "name"
  , keepOpen: false
  , insertable: true
  , imageSource: "icon"
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
      console.log("Items set to: ", items);
    });
  }, 3000);
});


// Example: Dropdown Typeahead
// This component supports passing the HTML to be rendered as the toggling element
// The labelHTML key must be a function (String -> String), where the output string
// should be valid HTML. (Usually it will include the input string, which will be the
// currently selected element)

const dropdownTAInput =
  { items,
    defaultLabel: "All The Things",
    resetLabel: "Use All The Things",
    key: "name",
    labelHTML: (item) => `
      <span class="text-black text-3xl font-thin cursor-pointer whitespace-no-wrap">
        ${item}
        <span class="icon-collapse text-2xl"></span>
      </span>`
  }

const dropdownTAComponent = mountDropdownTypeahead(mkElement("dropdown-ta"), dropdownTAInput);

// It supports all of the same operations as the standard typeaheads

const dropdownTAsubscription = dropdownTAComponent.subscribe((out) => {
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

dropdownTAComponent.setLoading().then(() => {
  console.log("Now in loading status...");
  setTimeout(() => {
    // Example: imperatively set selections
    dropdownTAComponent.setSelected([{ name: "Thomas" }]).then(() => {
      console.log("New selections set");
    });

    // Example: imperatively get selections
    dropdownTAComponent.getSelected().then((selections) => {
      console.log("These are the selections: ", selections);
    });

    // Example: imperatively set status
    dropdownTAComponent.setItems(items).then(() => {
      console.log("Items set to: ", items);
    });
  }, 3000);
});
