import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
// import { viteCommonjs } from '@originjs/vite-plugin-commonjs'
import { esbuildCommonjs } from '@originjs/vite-plugin-commonjs'
import polyfill from 'rollup-plugin-polyfill-node'

// https://vitejs.dev/config/
export default defineConfig({
  // plugins: [react(), viteCommonjs()],
  plugins: [react(), polyfill()],
  root: './vite',
  optimizeDeps: {
    esbuildOptions: {
      // plugins: [esbuildCommonjs(['utopia-vscode-common'])],
    },
  },
  server: {
    port: 3005,
  },
  // esbuild: {
  //   jsxFactory: `jsx`,
  //   jsxInject: `import { jsx } from '@emotion/react'`,
  // },
  define: {
    'process.env.NODE_ENV': "'development'",
    'process.env.REACT_APP_ENVIRONMENT_CONFIG': "'development'",
    'process.env.REACT_APP_AUTH0_REDIRECT_URI': undefined,
    'process.env.REACT_APP_AUTH0_CLIENT_ID': undefined,
    'process.env.REACT_APP_AUTH0_ENDPOINT': undefined,
    'process.env.REACT_APP_COMMIT_HASH': undefined,
    'process.env.GOOGLE_WEB_FONTS_KEY': "''",
    'process.cwd': 'function(){return ""}',
  },
})
