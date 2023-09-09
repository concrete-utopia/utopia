import { esbuildPlugin } from '@web/dev-server-esbuild'
import { puppeteerLauncher } from '@web/test-runner-puppeteer'
import vite from 'vite-web-test-runner-plugin'

export default {
  nodeResolve: true,
  debug: true,
  'root-dir': './resources',
  coverage: true,
  manual: true,
  browsers: [
    puppeteerLauncher({
      launchOptions: {
        headless: false,
        devtools: true,
      },
    }),
  ],

  testFramework: {
    config: {
      ui: 'bdd',
      timeout: '100000',
    },
  },

  // testRunnerHtml: (testFramework) => `
  //   <html>
  //     <head>
  //       <script type="module">
  //         // Note: globals expected by @testing-library/react
  //         window.global = window;
  //         window.process = { env: {} };

  //         // Note: adapted from https://github.com/vitejs/vite/issues/1984#issuecomment-778289660
  //         // Note: without this you'll run into https://github.com/vitejs/vite-plugin-react/pull/11#discussion_r430879201
  //         window.__vite_plugin_react_preamble_installed__ = true;
  //       </script>
  //       <script type="module" src="${testFramework}"></script>
  //     </head>
  //   </html>
  // `,
  plugins: [
    esbuildPlugin({ ts: true, tsx: true }),
    // vite(),
  ],
}
