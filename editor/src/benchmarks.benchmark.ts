import { benchmarkBuildTree } from './core/shared/element-path-tree.benchmark'
import { benchmarkElementPathFunction } from './core/shared/element-path.benchmark'

await benchmarkBuildTree()
await benchmarkElementPathFunction()
