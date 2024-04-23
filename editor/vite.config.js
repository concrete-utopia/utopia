import { resolve, join } from 'path'
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import { injectHtml } from 'vite-plugin-html'
import { nodeResolve } from '@rollup/plugin-node-resolve'

export default defineConfig(({ mode }) => {
  const isDevEnv = mode === 'development'
  return {
    plugins: [
      isDevEnv && react(),
      injectHtml({
        data: {
          VITE: true,
          UTOPIA_SHA: process.env.REACT_APP_COMMIT_HASH,
          UTOPIA_DOMAIN: isDevEnv ? 'http://localhost:8000' : '',
          VSCODE_DOMAIN: '${window.location.origin}',
        },
      }),
    ],
    root: './src/templates',
    publicDir: '../../resources/editor',
    server: {
      port: 8088,
      host: 'localhost',
      fs: {
        allow: [
          '../..',
          '../../../website-next/node_modules/',
          '../../../website-next/components/common/',
          '../../../utopia-api/',
          '../../../utopia-vscode-common/',
        ],
      },
      hmr: {
        protocol: 'ws',
        host: 'localhost',
        port: 8088,
      },
    },
    base: '/editor/',
    resolve: {
      alias: {
        'worker-imports': join(__dirname, 'src/core/workers/vite-worker-import-utils.ts'),
      },
    },
    define: {
      'process.env.NODE_ENV': `"${process.env.APP_ENV}"`,
      'process.env.REACT_APP_ENVIRONMENT_CONFIG': `"${process.env.REACT_APP_ENVIRONMENT_CONFIG}"`,
      'process.env.REACT_APP_BROWSER_TEST_DEBUG': `"${process.env.REACT_APP_BROWSER_TEST_DEBUG}"`,
      'process.env.REACT_APP_AUTH0_CLIENT_ID': `"${process.env.REACT_APP_AUTH0_CLIENT_ID}"`,
      'process.env.REACT_APP_AUTH0_ENDPOINT': `"${process.env.REACT_APP_AUTH0_ENDPOINT}"`,
      'process.env.GOOGLE_WEB_FONTS_KEY': `"${process.env.GOOGLE_WEB_FONTS_KEY}"`,
      'process.env.REACT_APP_COMMIT_HASH': `"${process.env.REACT_APP_COMMIT_HASH}"`,
      'process.env.HMR': mode === 'development' ? 'true' : 'false',
      'process.env.UTOPIA_DOMAIN': isDevEnv ? '"http://localhost:8000"' : '""',
      'process.env.UTOPIA_SHA': `"${process.env.REACT_APP_COMMIT_HASH}"`,
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
      commonjsOptions: {},
      sourcemap: true,
      compact: true,
      minify: 'terser',
      rollupOptions: {
        plugins: [
          nodeResolve({
            moduleDirectories: module.paths,
          }),
        ],
        // input: {
        //   index: resolve(__dirname, 'src/vite/index.html'),
        //   'vscode-inner': resolve(__dirname, 'src/vite/vscode-inner/index.html'),
        //   'vscode-outer': resolve(__dirname, 'src/vite/vscode-outer/index.html'),
        // },
      },
    },
  }
})
