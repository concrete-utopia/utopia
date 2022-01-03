import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
// import polyfill from 'rollup-plugin-polyfill-node'

// https://vitejs.dev/config/
export default defineConfig({
  // plugins: [react(), polyfill()],
  plugins: [react()],
  root: './src/vite',
  publicDir: '../../resources/editor',
  server: {
    port: 8088,
    host: '0.0.0.0',
    fs: {
      allow: [
        '../..',
        '../../../website-next/node_modules/',
        '../../../website-next/components/common/',
        '../../../utopia-api/',
        '../../../utopia-vscode-common/',
      ],
    },
  },
  base: '/editor/',
  compilerOptions: {
    types: ['vite/client'],
  },
  define: {
    global: '{}',
    module: '{}',
    exports: '{}',
    'process.env.NODE_ENV': `"${process.env.APP_ENV}"`,
    'process.env.REACT_APP_ENVIRONMENT_CONFIG': `"${process.env.REACT_APP_ENVIRONMENT_CONFIG}"`,
    'process.env.REACT_APP_AUTH0_REDIRECT_URI': `"${process.env.REACT_APP_AUTH0_REDIRECT_URI}"`,
    'process.env.REACT_APP_AUTH0_CLIENT_ID': `"${process.env.REACT_APP_AUTH0_CLIENT_ID}"`,
    'process.env.REACT_APP_AUTH0_ENDPOINT': `"${process.env.REACT_APP_AUTH0_ENDPOINT}"`,
    'process.env.GOOGLE_WEB_FONTS_KEY': "''",
    'process.env.REACT_APP_COMMIT_HASH': `"${process.env.REACT_APP_COMMIT_HASH}"`,
  },
})
