import { esbuildPlugin } from '@web/dev-server-esbuild'
import { puppeteerLauncher } from '@web/test-runner-puppeteer'
import vite from 'vite-web-test-runner-plugin'

export default {
  rnodeResolve: true,
  debug: true,
  manual: true,
  browsers: [
    puppeteerLauncher({
      launchOptions: {
        headless: false,
        devtools: true,
      },
    }),
  ],
  plugins: [vite()],
}
