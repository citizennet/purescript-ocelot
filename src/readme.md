# Components vs. Blocks

We have two kinds of 'components' in PureScript: smart components, which manage state and have potentially complex behaviors, and dumb components, which are simply a collection of HTML with some values passed in for customization. Usually, smart components will consist of HTML with various event listeners and behaviors attached.

In the same vein as [styled-components](https://www.styled-components.com/), we can couple our CSS with our components or our simple blocks of HTML. The primary difference:

- **Blocks** are simple HTML with styles attached, like "button" or "table".
- **Parts** are HTML and functions specialized to an abstract Halogen component with constraints, which can be used inside `render` or `eval` functions.
- **Components** are Halogen components with styles and behavior, often relying on blocks.
