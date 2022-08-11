import { foldEither } from '../../../core/shared/either'
import { elementReferencesElsewhere } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { InteractionCanvasState, StrategyApplicationResult } from './canvas-strategy-types'
import { StrategyState } from './interaction-state'
import * as EP from '../../../core/shared/element-path'

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
  strategyState: StrategyState,
  target: ElementPath,
): boolean {
  if (isGeneratedElement(canvasState, target)) {
    return false
  } else {
    const metadata = MetadataUtils.findElementByElementPath(strategyState.startingMetadata, target)
    if (metadata == null) {
      return false
    } else {
      return foldEither(
        (_) => true,
        (elementFromMetadata) => !elementReferencesElsewhere(elementFromMetadata),
        metadata.element,
      )
    }
  }
}

export function ifAllowedToReparent(
  canvasState: InteractionCanvasState,
  strategyState: StrategyState,
  targets: Array<ElementPath>,
  ifAllowed: () => StrategyApplicationResult,
): StrategyApplicationResult {
  const allowed = targets.every((target) => isAllowedToReparent(canvasState, strategyState, target))
  if (allowed) {
    return ifAllowed()
  } else {
    return {
      commands: [setCursorCommand('mid-interaction', CSSCursor.ReparentNotPermitted)],
      customState: null,
    }
  }
}
