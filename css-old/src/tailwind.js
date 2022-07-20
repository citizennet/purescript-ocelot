// This file modifies the base Tailwind configuration. We can freely re-use the same names or add new options to existing variables.

let config = require('tailwindcss/defaultConfig')()

const spacing = {
  '0': '0',
  '5': '1.25rem',
  '7': '1.75rem',
  '9': '2.25rem',
  '10': '2.5rem',
  '12': '3rem',
  '14': '3.5rem',
  '16': '4rem',
  '20': '5rem',
  '30': '7.5rem',
  '40': '10rem',
  '50': '12.5rem',
  '60': '15rem',
  '80': '20rem',
  '90': '22.5rem',
  '120': '30rem',
  '160': '40rem',
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
  'black-10': '#00091A',
  'black-20': '#242A33',
  'black-modal-a90': 'rgba(0,9,26,0.9);',
  'grey-50': '#5C6573',
  'grey-50-a20': 'rgba(102,113,128,0.2)',
  'grey-50-a30': 'rgba(102,113,128,0.3)',
  'grey-50-a80': 'rgba(102,113,128,0.8)',
  'grey-50-a90': 'rgba(102,113,128,0.9)',
  'grey-70': 'rgb(143,158,179)',
  'grey-70-a30': 'rgba(143,158,179,0.3)',
  'grey-70-a40': 'rgba(143,158,179,0.4)',
  'grey-80': '#C2C6CC',
  'grey-90': '#E1E3E6',
  'grey-95': '#F0F1F2',
  'grey-97': '#F7F7F7',
  'blue-65': '#008AA6',
  'blue-75': '#009FBF',
  'blue-82': '#00ABD1',
  'blue-88': '#00BBE0',
  'facebook-blue': '#3B5998',
  'fb-blue': '#3B5998',
  'google-blue': '#4285F4',
  'ig-brown': '#675144',
  'instagram-purple': '#C13584',
  'linkedin-blue': '#0E76A8',
  'pinterest-red': '#C8232C',
  'reddit-orange': '#FF4500',
  'snapchat-yellow': '#FFFC00',
  'taboola-blue': '#00659F',
  'tiktok-green': '#69C9D0',
  'tw-blue': '#00ACEE',
  'twitter-blue': '#00ACEE',
  'youtube-red': '#C4302B',
  'red': '#FF5471',
  'yellow': '#FFC859',
  'green': '#66C7AF',
  'steel-75': '#A8B2BF',
  'steel-85': '#BFCAD9',
  'steel-100': '#E6F0FF'
});

config.borderColors = Object.assign(
  { 'default': config.colors['grey-80'] },
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
  'backgroundColors': ['responsive', 'hover', 'focus', 'active', 'group-hover'],
})

// Export the new configuration
module.exports = config
