import type { Config } from 'tailwindcss'

export default {
  content: ['./app/**/*.{js,jsx,ts,tsx}'],
  theme: {
    fontFamily: {
      headline: ['RecklessNeue-Book', 'serif'],
      menu: ['RecklessNeue-Book', 'serif'],
      body: ['Moderat-Regular', 'sans-serif'],
      button: ['RecklessNeue-Book', 'serif'],
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
} satisfies Config
