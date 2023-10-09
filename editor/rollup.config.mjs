import { resolve, join } from 'path'
import html from '@rollup/plugin-html' // Assuming a similar plugin exists
import nodeResolve from '@rollup/plugin-node-resolve'
import external from 'rollup-plugin-peer-deps-external' // To handle external dependencies
import replace from '@rollup/plugin-replace' // To handle environment variables
import commonjs from '@rollup/plugin-commonjs' // To handle CommonJS modules
import { terser } from 'rollup-plugin-terser' // For minification
import esbuild from 'rollup-plugin-esbuild'
import json from '@rollup/plugin-json'
import css from 'rollup-plugin-import-css'

export default {
  input: 'src/templates/editor-entry-point.tsx', // Adjust the input file accordingly
  output: {
    dir: 'lib',
    format: 'es',
    sourcemap: true,
  },
  plugins: [
    css(),
    json(),
    esbuild({
      include: /\.tsx?$/, // Only transpile TypeScript files
      tsconfig: 'tsconfig.json', // specify your tsconfig path here
      minify: process.env.NODE_ENV === 'production',
      target: 'esnext',
      exclude: /node_modules/, // default
    }),
    external(), // Handle external dependencies
    html(),
    commonjs(),
    nodeResolve({
      preferBuiltins: false, // do not prefer Node.js builtins over node_modules
      extensions: ['.mjs', '.js', '.json', '.node', '.cjs'],
    }),
    replace({
      preventAssignment: true,
      'process.env.NODE_ENV': JSON.stringify(process.env.APP_ENV),
      'process.env.REACT_APP_ENVIRONMENT_CONFIG': JSON.stringify(
        process.env.REACT_APP_ENVIRONMENT_CONFIG,
      ),
      // ... other environment variables
    }),
    process.env.NODE_ENV !== 'development' && terser(), // Minify in production
  ],
  external: ['domtoimage'], // Assuming domtoimage is an external dependency
  watch: {
    clearScreen: false,
  },
}
