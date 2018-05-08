// This file modifies the base Tailwind configuration. We can freely re-use the same names or add new options to existing variables.

let config = require('tailwindcss/defaultConfig')()

const spacing = {
  '0': '0',
  '5': '1.25rem',
  '7': '1.75rem',
  '9': '2.25rem',
  '10': '2.5rem',
  '12': '3rem',
  '16': '4rem',
  '20': '5rem',
  '40': '10rem',
  '80': '20rem',
}

config.padding = Object.assign(config.padding, spacing)

config.margin = Object.assign(config.margin, spacing)

config.negativeMargin = config.margin

config.height = Object.assign(config.height, spacing)

config.width = Object.assign(config.width, spacing)

config.maxWidth = Object.assign(config.maxWidth, spacing, {
  '6xl': '110rem',
})

config.minWidth = Object.assign(config.minWidth, spacing)

config.maxHeight = Object.assign(config.maxHeight, spacing)

config.minHeight = Object.assign(config.minHeight, spacing)

config.zIndex = Object.assign(config.zIndex, {
  '60': '60'
})

config.colors = Object.assign(config.colors, {
  'black-10': '#00081A',
  'black-20': '#242A33',
  'black-modal-a90': 'rgba(0,9,26,0.9);',
  'grey-50': 'rgb(102,113,128)',
  'grey-50-a20': 'rgba(102,113,128,0.2)',
  'grey-50-a30': 'rgba(102,113,128,0.3)',
  'grey-70': 'rgb(143,158,179)',
  'grey-70-a30': 'rgba(143,158,179,0.3)',
  'grey-70-a40': 'rgba(143,158,179,0.4)',
  'grey-90': '#E1E3E6',
  'grey-95': '#F0F1F2',
  'blue-75': '#F0F1F2',
  'blue-82': '#00ABD1',
  'blue-88': '#00BBE0',
  'blue-100': '#E6F0FF',
  'fb-blue': '#3B5998',
  'tw-blue': '#00ACED',
  'ig-brown': '#675144',
  'red': '#FF5471',
  'yellow': '#FFC859',
})

config.borderColors = Object.assign(
  { 'default': config.colors['grey-light'] },
  config.colors
)

config.borderWidths = Object.assign(config.borderWidths, {
  '3': '3px'
})

config.modules = Object.assign(config.modules, {
  'borderWidths': ['responsive', 'hover'],
  'visibility': ['responsive', 'hover', 'focus', 'group-hover'],
  'borderColors': ['responsive', 'hover', 'focus', 'group-hover'],
  'textColors': ['responsive', 'hover', 'group-hover'],
  'backgroundColors': ['responsive', 'hover', 'focus', 'active'],
})

// Export the new configuration
module.exports = config
