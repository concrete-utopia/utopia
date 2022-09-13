import { absoluteDuplicateStrategy } from './absolute-duplicate-strategy'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { absoluteResizeBoundingBoxStrategy } from './absolute-resize-bounding-box-strategy'
import { getApplicableControls, isResizableStrategy } from './canvas-strategies'
import { CanvasStrategy, CanvasStrategyId, ControlWithKey } from './canvas-strategy-types'
import { dragToInsertStrategy } from './drag-to-insert-strategy'
import { escapeHatchStrategy } from './escape-hatch-strategy'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { flexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import {
  flowReorderAutoConversionStrategy,
  flowReorderNoConversionStrategy,
  flowReorderSameTypeOnlyStrategy,
} from './flow-reorder-strategy'
import { keyboardAbsoluteMoveStrategy } from './keyboard-absolute-move-strategy'
import { keyboardAbsoluteResizeStrategy } from './keyboard-absolute-resize-strategy'

const isResizableStrategyAndResults: Array<[CanvasStrategy, boolean]> = [
  [absoluteMoveStrategy, false],
  [absoluteReparentStrategy, false],
  [absoluteDuplicateStrategy, false],
  [keyboardAbsoluteMoveStrategy, false],
  [keyboardAbsoluteResizeStrategy, true],
  [absoluteResizeBoundingBoxStrategy, true],
  [flexReorderStrategy, false],
  [flexReparentToAbsoluteStrategy, false],
  [flexReparentToFlexStrategy, false],
  [escapeHatchStrategy, false],
  [absoluteReparentToFlexStrategy, false],
  [dragToInsertStrategy, false],
  [flowReorderAutoConversionStrategy, false],
  [flowReorderNoConversionStrategy, false],
  [flowReorderSameTypeOnlyStrategy, false],
]

const isResizableExpectedResults: Array<[boolean, CanvasStrategyId, CanvasStrategy]> =
  isResizableStrategyAndResults.map((entry) => {
    return [entry[1], entry[0].id, entry[0]]
  })

describe('isResizableStrategy', () => {
  it.each(isResizableExpectedResults)(
    'returns %s for the strategy %s',
    (expectedResult, strategyId, strategy) => {
      const actualResult = isResizableStrategy(strategy)
      expect(actualResult).toEqual(expectedResult)
    },
  )
})

const getApplicableControlsExpectedResults: Array<
  [CanvasStrategy, CanvasStrategyId | null, Array<string>]
> = [
  [absoluteMoveStrategy, null, []],
  [
    absoluteResizeBoundingBoxStrategy,
    null,
    ['absolute-resize-control', 'zero-size-resize-control'],
  ],
  [absoluteResizeBoundingBoxStrategy, absoluteMoveStrategy.id, []],
  [flowReorderAutoConversionStrategy, null, []],
  [
    flowReorderAutoConversionStrategy,
    flowReorderAutoConversionStrategy.id,
    ['parent-outlines-control', 'parent-bounds-control', 'flow-reorder-drag-outline'],
  ],
  [flowReorderAutoConversionStrategy, absoluteMoveStrategy.id, []],
]

const getApplicableControlsExpectedResultsForTest: Array<
  [Array<string>, CanvasStrategyId, CanvasStrategyId | null, CanvasStrategy]
> = getApplicableControlsExpectedResults.map((entry) => {
  return [entry[2], entry[0].id, entry[1], entry[0]]
})

describe('getApplicableControls', () => {
  it.each(getApplicableControlsExpectedResultsForTest)(
    'return %s against strategy %s with the current strategy %s',
    (expectedResult, strategyId, currentStrategyId, strategy) => {
      const actualResult = getApplicableControls(currentStrategyId, strategy).map((c) => c.key)
      expect(actualResult).toEqual(expectedResult)
    },
  )
})
