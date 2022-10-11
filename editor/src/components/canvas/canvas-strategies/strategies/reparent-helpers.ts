import { foldEither } from '../../../../core/shared/either'
import {
  ElementInstanceMetadataMap,
  elementReferencesElsewhere,
} from '../../../../core/shared/element-template'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { CSSCursor } from '../../canvas-types'
import { setCursorCommand } from '../../commands/set-cursor-command'
import {
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { ProjectContentTreeRoot } from '../../../assets'

export function isGeneratedElement(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
  target: ElementPath,
): boolean {
  return MetadataUtils.anyUnknownOrGeneratedElements(projectContents, {}, openFile ?? null, [
    target,
  ])
}

export function isAllowedToReparent(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null | undefined,
  startingMetadata: ElementInstanceMetadataMap,
  target: ElementPath,
): boolean {
  if (isGeneratedElement(projectContents, openFile, target)) {
    return false
  } else {
    const metadata = MetadataUtils.findElementByElementPath(startingMetadata, target)
    if (metadata == null) {
      return false
    } else {
      return foldEither(
        (_) => true,
        (elementFromMetadata) => {
          return (
            !elementReferencesElsewhere(elementFromMetadata) &&
            MetadataUtils.targetHonoursPropsPosition(projectContents, openFile, target)
          )
        },
        metadata.element,
      )
    }
  }
}

export function ifAllowedToReparent(
  canvasState: InteractionCanvasState,
  startingMetadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
  ifAllowed: () => StrategyApplicationResult,
): StrategyApplicationResult {
  const allowed = targets.every((target) => {
    return isAllowedToReparent(
      canvasState.projectContents,
      canvasState.openFile,
      startingMetadata,
      target,
    )
  })
  if (allowed) {
    return ifAllowed()
  } else {
    return strategyApplicationResult(
      [setCursorCommand('mid-interaction', CSSCursor.NotPermitted)],
      {},
      'failure',
    )
  }
}
