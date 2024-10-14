import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { GridGapControlComponent } from './grid-gap-control-component'
import { GridGapControlComponent2 } from './grid-gap-control-component-2'

export const GridGapControl = controlForStrategyMemoized(GridGapControlComponent2)
