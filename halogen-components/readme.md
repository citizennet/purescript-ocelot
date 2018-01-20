# CitizenNet PureScript UI

This repository centralizes Halogen components used in various parts of the Wildcat project. These components can be imported and lightly modified to make various screens.

## Goals

The primary goal is to centralize Halogen components. This allows us to keep our components' CSS and definitions all in one place. It means that have a single place we can update a component and see that change reflected everywhere it is used, ideally including its bundled CSS. It also means we only have to update dependencies and versions in a single place.

In various screens, we're only expected to mount these components in child slots rather than build them again each time they're needed.

## Caveats

- CSS is not yet bundled in with components. This work is pending @thomashoneyman and @whoadave looking in to mimicking the CSS modules approach in PureScript.
- It's possible this overly reduces our flexibility
