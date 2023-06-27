import { chromeLauncher } from '@web/test-runner-chrome'
import { esbuildPlugin } from '@web/dev-server-esbuild'

export default {
  nodeResolve: true,
  plugins: [
    esbuildPlugin({
      tsx: true,
      ts: true,
      jsxFactory: 'React.createElement',
      jsxFragment: 'Fragment',
    }),
  ],
  browsers: [
    chromeLauncher({
      launchOptions: {
        headless: false,
        devtools: true,
      },
    }),
  ],
}
