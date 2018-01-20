# CSS

This module holds three things:
- An NPM install of Tachyons, kept in sync with the official repository
- Our set of overrides and augmentations of that CSS to provide a CitizenNet theme
- A PureScript definition of common ui components based on the theme

## Motivation

Tachyons is a way to write CSS by combining atomic properties. Rather than define a button, you combine relevant CSS classes to describe the shape you want, like a border-radius, color, and font. This provides a great foundation for creating ui components from primitives that can be combined without side effects.

In practice, we still want to define ui components and use those directly in Wildcat rather than always combine primitives from Tachyons. Otherwise, we'd lose the ability to make a change once and see it reflected across our ui; instead, we'd have to find every place we've used these styles and re-write them.

In PureScript, we can accomplish this by defining our ui components as groups of Tachyons classes. Then, we store them in a record that can be imported into any screen and used to create common elements like buttons or tables. This record is also used to generate our style guide, which gives us a sandbox with which to preview the effects of our changes. This also gives us the chance to define functions on CSS which can do things like create a table given some width and row count.

## In use

Tachyons is imported from NPM. Our overrides are the only CSS that is writable. Our PureScript record leverages our CSS combined with the base Tachyons layer. The Wildcat application should always import this style record and use those styles or helpers rather than define new ones in place.
