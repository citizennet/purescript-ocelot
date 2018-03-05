module.exports = env => {
  if (env === 'localized') {
    return [
      require('import-postcss'),
      require('tailwindcss')('./src/tailwind.js'),
      require('postcss-prefix-selector')({
        prefix: '.ocelot-localized',
        transform(prefix, selector, prefixedSelector) {
          if (selector === 'html') {
            return prefix + ', ' + prefix + ' *';
          }
          if (selector === 'body') {
            return 'body' + prefix;
          }
          return prefixedSelector;
        }
      }),
    ]
  }
  return [
    require('import-postcss'),
    require('tailwindcss')('./src/tailwind.js'),
  ]
}
