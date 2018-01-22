# Purescript UI

This repository provides building blocks usable to create Wildcat screens. This includes common styles based on Tachyons, like primary buttons or tables, and pre-built components like dropdowns and typeaheads.

This separation of concerns carries many benefits. The Wildcat repository can focus entirely on building the application for our users, while this repository can focus on robust, self-contained and testable units that can be leveraged by Wildcat.

There are three relevant parts:

- Pre-built Halogen components that can be dropped in place pre-styled
- Pre-made styles for HTML elements like tables and buttons, built from Tachyons classes and usable as functions on CSS
- A style guide that serves as a sandbox to configure and test components and ui components to prove they work as expected and to preview changes
