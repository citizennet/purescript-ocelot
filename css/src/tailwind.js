// This file modifies the base Tailwind configuration. We can freely re-use the same names or add new options to existing variables.

var config = require('tailwindcss/defaultConfig')()

// Additional padding and margin sizes
config.padding = Object.assign(config.padding, {
  '10': '2.5rem',
  '12': '3rem',
  '16': '4rem',
  '20': '5rem',
  '80': '20rem',
})

config.margin = Object.assign(config.margin, {
  '10': '2.5rem',
  '12': '3rem',
  '16': '4rem',
  '20': '5rem',
  '80': '20rem',
})

config.negativeMargin = config.margin


// Export the new configuration
module.exports = config
