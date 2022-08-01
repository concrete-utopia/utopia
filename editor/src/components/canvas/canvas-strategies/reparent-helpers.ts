import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { InteractionCanvasState, StrategyApplicationResult } from './canvas-strategy-types'

export function isGeneratedElement(
  canvasState: InteractionCanvasState,
  target: ElementPath,
): boolean {
  return MetadataUtils.anyUnknownOrGeneratedElements(
    canvasState.projectContents,
    {},
    canvasState.openFile ?? null,
    [target],
  )
}

export function isAllowedToReparent(
  canvasState: InteractionCanvasState,
  target: ElementPath,
): boolean {
  return !isGeneratedElement(canvasState, target)
}

export function ifAllowedToReparent(
  canvasState: InteractionCanvasState,
  targets: Array<ElementPath>,
  ifAllowed: () => StrategyApplicationResult,
): StrategyApplicationResult {
  const allowed = targets.every((target) => isAllowedToReparent(canvasState, target))
  if (allowed) {
    return ifAllowed()
  } else {
    return {
      commands: [setCursorCommand('transient', CSSCursor.ReparentNotPermitted)],
      customState: null,
    }
  }
}
