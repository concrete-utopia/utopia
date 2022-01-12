import { resolve } from 'path'
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import { injectHtml } from 'vite-plugin-html'
import { visualizer } from 'rollup-plugin-visualizer'
import worker from 'rollup-plugin-workers'
import { nodeResolve } from '@rollup/plugin-node-resolve'

export default defineConfig(({ mode }) => {
  function createManualChunks(id, { getModuleInfo }) {
    if (id.includes('node_modules')) {
      const mod = getModuleInfo(id)
      if (id.includes('@babel/standalone')) {
        return 'babel'
      }
      if (id.includes('eslint')) {
        // console.log('getModuleInfo(id)', id, mod?.importers)
        return 'eslint'
      }
      return 'vendor'
      // } else if (id.includes('uuiui')) {
      //   return 'uuiui'
    }
  }

  const isDevEnv = mode === 'development'
  return {
    plugins: [
      isDevEnv && react(),
      injectHtml({ data: { UTOPIA_SHA: process.env.REACT_APP_COMMIT_HASH } }),
    ],
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
    define: {
      'process.env.NODE_ENV': `"${process.env.APP_ENV}"`,
      'process.env.REACT_APP_ENVIRONMENT_CONFIG': `"${process.env.REACT_APP_ENVIRONMENT_CONFIG}"`,
      'process.env.REACT_APP_AUTH0_REDIRECT_URI': `"${process.env.REACT_APP_AUTH0_REDIRECT_URI}"`,
      'process.env.REACT_APP_AUTH0_CLIENT_ID': `"${process.env.REACT_APP_AUTH0_CLIENT_ID}"`,
      'process.env.REACT_APP_AUTH0_ENDPOINT': `"${process.env.REACT_APP_AUTH0_ENDPOINT}"`,
      'process.env.GOOGLE_WEB_FONTS_KEY': `"${process.env.GOOGLE_WEB_FONTS_KEY}"`,
      'process.env.REACT_APP_COMMIT_HASH': `"${process.env.REACT_APP_COMMIT_HASH}"`,
    },
    optimizeDeps: {
      // This is a workaround for an apparent issue in the vite react plugin, which leads to
      // "react/jsx-runtime" potentially being an undiscoverable dependency, meaning it won't be
      // optimised, leading to a runtime error that "module is not defined"
      // https://github.com/vitejs/vite/issues/6215
      include: ['react/jsx-runtime'],
    },
    build: {
      target: 'esnext',
      outDir: resolve(__dirname, 'lib'),
      emptyOutDir: true,
      commonjsOptions: {},
      sourcemap: true,
      compact: true,
      minify: 'terser',
      rollupOptions: {
        plugins: [
          nodeResolve({ moduleDirectories: module.paths }),
          worker(),
          // visualizer(),
          // replace({ 'process.env.NODE_ENV': 'production' }),
        ],
        input: {
          index: resolve(__dirname, 'src/vite/index.html'),
          'vscode-outer': resolve(__dirname, 'src/vite/vscode-outer/index.html'),
        },
        // output: {
        //   manualChunks: createManualChunks,
        // },
      },
    },
  }
})
