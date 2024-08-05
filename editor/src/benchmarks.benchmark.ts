import { benchmarkBuildTree } from './core/shared/element-path-tree.benchmark'
import { benchmarkElementPathFunction } from './core/shared/element-path.benchmark'
import { benchmarkAttributes } from './core/shared/jsx-attributes.benchmark'
import { benchmarkOptics } from './core/shared/optics.benchmark'
import { benchmarkPropertyPathFunction } from './core/shared/property-path.benchmark'

/*
await benchmarkBuildTree()
await benchmarkElementPathFunction()
await benchmarkPropertyPathFunction()
await benchmarkAttributes()
*/
await benchmarkOptics()
