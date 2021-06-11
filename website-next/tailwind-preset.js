module.exports = {
  theme: {
    fontFamily: {
      headline: ['Marfa-Bold', 'serif'],
      menu: ['Marfa-Medium', 'serif'],
      body: ['Inter', 'serif'],
      button: ['Marfa-Medium', 'serif'],
    },
    extend: {
      flexGrow: {
        2: '2',
        3: '3',
      },
      zIndex: {
        60: '60',
        70: '70',
        80: '80',
        90: '90',
        100: '100',
      },
      colors: {
        lemon: {
          light: 'lemon',
          DEFAULT: 'pink',
          dark: 'orange',
        },
      },
    },
  },
  plugins: [require('@tailwindcss/typography'), require('@tailwindcss/aspect-ratio')],
}
