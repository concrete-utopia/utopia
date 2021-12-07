import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
// import { viteCommonjs } from '@originjs/vite-plugin-commonjs'
import { esbuildCommonjs } from '@originjs/vite-plugin-commonjs'

// https://vitejs.dev/config/
export default defineConfig({
  // plugins: [react(), viteCommonjs()],
  plugins: [react()],
  root: './vite',
  optimizeDeps: {
    esbuildOptions: {
      plugins: [esbuildCommonjs(['utopia-vscode-common'])],
    },
  },
  server: {
    port: 3005,
  },
  // esbuild: {
  //   jsxFactory: `jsx`,
  //   jsxInject: `import { jsx } from '@emotion/react'`,
  // },
})
