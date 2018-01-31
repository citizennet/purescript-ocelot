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

config.colors = Object.assign(config.colors, {
  'black': '#333',
  'grey-darkest': '#666',
  'grey-darker': '#999',
  'grey-dark': 'rgba(0,0,0,0.35)',
  'grey': '#BFBFBF',
  'grey-light': 'rgba(128,128,128,0.2)',
  'grey-lighter': '#E6E6E6',
  'grey-lightest': '#F5F5F5',
  'white': '#FFF',
  'teal': '#0BB6D9',
  'teal-dark': '#00A3CC'
})

// Export the new configuration
module.exports = config
