import { benchmarkBuildTree } from './core/shared/element-path-tree.benchmark'
import { benchmarkElementPathFunction } from './core/shared/element-path.benchmark'
import { benchmarkPropertyPathFunction } from './core/shared/property-path.benchmark'

await benchmarkBuildTree()
await benchmarkElementPathFunction()
await benchmarkPropertyPathFunction()
