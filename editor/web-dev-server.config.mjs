import { esbuildPlugin } from '@web/dev-server-esbuild'

export default {
  nodeResolve: true,
  plugins: [
    esbuildPlugin({ tsx: true, jsxFactory: 'React.createElement', jsxFragment: 'Fragment' }),
  ],
}
