# Ocelot [![CircleCI](https://circleci.com/gh/citizennet/purescript-ocelot.svg?style=badge)](https://circleci.com/gh/citizennet/purescript-ocelot)

Ocelot is an opinionated component library for Halogen-based PureScript applications. Components generally obey a few guidelines:

- Components bundle with scoped CSS and can be embedded pre-styled even in applications with existing CSS
- "Blocks" are simple HTML/CSS chunks for things like buttons and form controls; "Components" are Halogen components that encapsulate behaviors and states
- Blocks and components should be built, styled, and rigorously tested in this sandbox and imported into apps like Wildcat

## Philosophy

This repository holds the blocks and components we use in CitizenNet applications. These atoms can be built, styled, and rigorously tested without being forced into the context of the application. That allows for faster fixes and iteration in a sandbox environment so we can isolate errors, and more importantly, it allows easy access for business and design teams to verify our components without having to wait for a staging release or build anything locally.

This separation of concerns carries the same benefits as functional programming generally and fits well with using Tailwind (functional css) and PureScript. The Wildcat repository focuses on building a robust, usable application for our end users, whereas the CN UI repository focuses on providing self-contained, well-tested design atoms that can be dropped into any Wildcat screen and just as easily be updated or replaced.

There are four relevant parts to this project:

- `src`: contains the pre-built components and UI styles we can drop into Wildcat screens
- `test`: contains snapshot and logic tests for our components
- `css`: contains our modified version of Tailwind, built and minified with dead code elimination
- `ui-guide`: contains a small PureScript site demonstrating our components and styling so we can visually test implementations
- `dist`: contains the built CSS to import into Wildcat, and an index.html file usable for local testing


## Use

### Preview & Testing
One of the major goals of this project is to provide a minimal environment to test the UI components we build and ensure the behaviors and styling are correct without coupling them to a particular screen in the app. Once the individual units are built and tested, we know we can safely import and use them in the app.

- Components can be viewed and tested by anyone at the [documentation site](https://citizennet.github.io/purescript-ocelot).

### Developing
Both the PureScript and CSS folders have convenience scripts that will watch and rebuild files on save.

### Publishing
One of the primary aims of this project is to provide a way for non-PureScript developers to view and test our components. That ability is provided by the generated [documentation site](https://citizennet.github.io/purescript-ocelot/). This site is automatically built by Circle CI on the `gh-pages` branch.

Our CSS, however, is not automatically built because it's on our master branch so we can bower install into Wildcat. We have to generate it on new builds to master.

To do that, ensure that any commit to the master branch is preceded by a call to `npm run build-all`:

```sh
yarn run build-all
git commit -m "..."
git push origin master
```

If you haven't changed the CSS, then feel free to use just `yarn build-ui`.

### Releasing

To create a new release:

1. Adjust the version in `package.json`.
    Some of our dependents rely on this version being correct.
1. Submit the changes to `master` via a PR.
1. Create a tag for the version.
    ```sh
    $ git tag $NEW_VERSION
    ```
1. Push `master` and the tag to the repo.
    ```sh
    $ git push --tags origin master
    ```
1. Create a release on [GitHub][releases] for the tag.
    The description should document the changes in this new version.

[releases]: https://github.com/citizennet/purescript-ocelot/releases
