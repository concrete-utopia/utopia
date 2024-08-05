import { benchmarkBuildTree } from './core/shared/element-path-tree.benchmark'
import { benchmarkElementPathFunction } from './core/shared/element-path.benchmark'
import { benchmarkAttributes } from './core/shared/jsx-attributes.benchmark'
import { benchmarkOptics } from './core/shared/optics.benchmark'
import { benchmarkPropertyPathFunction } from './core/shared/property-path.benchmark'
import { benchmarkGetUniqueUids } from './core/model/get-unique-ids.benchmark'

// await benchmarkBuildTree()
// await benchmarkElementPathFunction()
// await benchmarkPropertyPathFunction()
// await benchmarkAttributes()
// await benchmarkOptics()
await benchmarkGetUniqueUids()
