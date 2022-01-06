import { resolve } from 'path'
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  root: './src/',
  base: '/editor/',
  define: {
    global: '{}',
    module: '{}',
    exports: '{}',
  },
  build: {
    target: 'esnext',
    rollupOptions: {
      // https://rollupjs.org/guide/en/#big-list-of-options
      input: {
        index: resolve(__dirname, 'src/main.tsx'),
      },
    },
  },
})
