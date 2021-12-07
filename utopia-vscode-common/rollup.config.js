import { defineConfig } from 'rollup'
import dts from 'rollup-plugin-dts'
import typescript from '@rollup/plugin-typescript'
import resolve from '@rollup/plugin-node-resolve'
import pkg from './package.json'

const extensions = ['.js', '.jsx', '.ts', '.tsx']

const rollupConfig = defineConfig({
  input: 'src/index.ts',
  output: [
    {
      file: pkg.module,
      format: 'esm',
      sourcemap: 'inline',
    },
    { file: 'dist/index.cjs.js', format: 'cjs' },
  ],
  plugins: [
    resolve(),
    typescript(),
    // babel({
    //   exclude: './node_modules/**',
    //   extensions,
    // }),
  ],
})

const dtsConfig = defineConfig({
  input: 'src/index.ts',
  output: { file: 'dist/index.d.ts' },
  plugins: [dts()],
})

export default [rollupConfig, dtsConfig]
