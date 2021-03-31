# Ocelot [![CircleCI](https://circleci.com/gh/citizennet/purescript-ocelot.svg?style=badge)](https://circleci.com/gh/citizennet/purescript-ocelot)

Ocelot is an opinionated component library for Halogen-based PureScript applications.

## Philosophy

This repository holds the blocks and components we use in CitizenNet applications. These atoms can be built, styled, and rigorously tested without being forced into the context of the application. That allows for faster fixes and iteration in a sandbox environment so we can isolate errors, and more importantly, it allows easy access for business and design teams to verify our components without having to wait for a staging release or build anything locally.

This separation of concerns carries the same benefits as functional programming generally and fits well with using Tailwind (functional CSS) and PureScript. This UI repository focuses on providing self-contained, well-tested design atoms that can be dropped into any Halogen-based application and just as easily be updated or replaced. This allows the consuming application codebases to focus on providing a robust, usable application for users.

There are four relevant parts to this project:

- `src`: contains the pre-built components and UI styles we can drop into application screens
- `test`: contains snapshot and logic tests for our components
- `css`: contains our modified version of Tailwind
- `ui-guide`: contains a small PureScript site demonstrating our components and styling so we can visually test implementations
- `dist`: contains the built CSS to import into Wildcat, and an index.html file usable for local testing


## Use

### Preview & Testing
One of the major goals of this project is to provide a minimal environment to test the UI components we build and ensure the behaviors and styling are correct without coupling them to a particular screen in the app. Once the individual units are built and tested, we know we can safely import and use them in the app.

- Components can be viewed and tested by anyone at the [documentation site](https://citizennet.github.io/purescript-ocelot).

### Developing
All you need to do to get started is run:

```sh
make
```

This will install all dependencies and build the project. After making changes you can run the same command to build those changes. To see a list of the potentially useful commands, run:

```sh
make help
```

### Publishing
One of the primary aims of this project is to provide a way for non-PureScript developers to view and test our components. That ability is provided by the generated [documentation site](https://citizennet.github.io/purescript-ocelot/). This site is automatically built by Circle CI on the `gh-pages` branch.

Our built CSS is version controlled, so that consuming applications can use it with minimal changes to their build system. Running `make` before committing will not only validate that you're checking in code that compiles, but that the CSS is up to date. However, if you forget to build before checking in your code, there is a Circle CI step that will validate that the CSS is up to date for you.

### Releasing

To create a new release:

1. Adjust the version in `package.json`.
    Some of our dependents rely on this version being correct.
1. Submit the changes to `main` via a PR.
1. Create a tag for the version.
    ```sh
    $ git tag $NEW_VERSION
    ```
1. Push `main` and the tag to the repo.
    ```sh
    $ git push --tags origin main
    ```
1. Create a release on [GitHub][releases] for the tag.
    The description should document the changes in this new version.

[releases]: https://github.com/citizennet/purescript-ocelot/releases
