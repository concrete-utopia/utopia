import type { ElementPath } from '../../core/shared/project-file-types'
import { PinType } from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import type { LocalRectangle, Vector } from '../../core/shared/math-utils'
import type { OriginalFrame } from '../editor/store/editor-state'
import type { ConstrainedDragAxis } from './guideline'

export const ControlFontSize = 11

// The segments of the circle in sixteenths(!) on the right hand side of the circle.
// BB: I neutered it so it only does horizontal or vertical, sorry!
const circleSteps: Array<ConstrainedDragAxis> = ['y', 'y', 'x', 'x', 'x', 'x', 'y', 'y']

export function determineConstrainedDragAxis(drag: Vector<any>): ConstrainedDragAxis {
  const angleFromVertical = Utils.angleOfPointFromVertical(drag)
  // Handle the angle being past the 180 degree mark.
  const angleToUse = angleFromVertical > Math.PI ? angleFromVertical - Math.PI : angleFromVertical
  const indexOfSteps = Math.floor((angleToUse / Math.PI) * 8)
  const result = circleSteps[indexOfSteps === 8 ? 7 : indexOfSteps]
  return result
}

export function togglePinType(pinType: PinType): PinType {
  return pinType === PinType.Absolute ? PinType.Relative : PinType.Absolute
}

export function getOriginalFrame(
  originalFrames: Array<OriginalFrame>,
  target: ElementPath,
): LocalRectangle {
  throw new Error(`This code path should be dead - getOriginalFrame`)
}

const FrameCanvasControls = {
  determineConstrainedDragAxis: determineConstrainedDragAxis,
}

export default FrameCanvasControls
