import type { ServerBuild } from '@remix-run/server-runtime'

export declare const assets: ServerBuild['assets']
export declare const entry: ServerBuild['entry']
export declare const routes: ServerBuild['routes']
export declare const future: ServerBuild['future']
export declare const publicPath: ServerBuild['publicPath']
export declare const assetsBuildDirectory: ServerBuild['assetsBuildDirectory']

// We don't really need these, but otherwise the import in server.js will break
