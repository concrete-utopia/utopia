import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import {
  CanvasRectangle,
  boundingRectangle,
  boundingRectangleArray,
  canvasVector,
  magnitude,
  nullIfInfinity,
  offsetRect,
  vectorDifference,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { cssNumber } from '../../inspector/common/css-utils'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { CanvasFrameAndTarget } from '../canvas-types'
import { adjustCssLengthProperty } from './adjust-css-length-command'
import {
  BaseCommand,
  CanvasCommand,
  CommandFunction,
  WhenToRun,
  foldAndApplyCommandsSimple,
  getPatchForComponentChange,
  runCanvasCommand,
} from './commands'
import { CommandFunctionResult } from './commands'
import { setCssLengthProperty } from './set-css-length-command'
import { wildcardPatch } from './wildcard-patch-command'

export interface PushIntendedBounds extends BaseCommand {
  type: 'PUSH_INTENDED_BOUNDS'
  value: Array<CanvasFrameAndTarget>
}

export function pushIntendedBounds(value: Array<CanvasFrameAndTarget>): PushIntendedBounds {
  return {
    type: 'PUSH_INTENDED_BOUNDS',
    whenToRun: 'always',
    value: value,
  }
}

export const runPushIntendedBounds = (
  editor: EditorState,
  command: PushIntendedBounds,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  const resizeAncestorsPatch = getResizeAncestorsPatches(editor, command)

  const intendedBoundsPatch =
    commandLifecycle === 'mid-interaction' ? pushCommandStatePatch(command) : []

  return {
    editorStatePatches: [resizeAncestorsPatch, ...intendedBoundsPatch],
    commandDescription: `Set Intended Bounds for ${command.value
      .map((c) => EP.toString(c.target))
      .join(', ')}`,
  }
}

function pushCommandStatePatch(command: PushIntendedBounds): Array<EditorStatePatch> {
  return [
    {
      canvas: {
        controls: {
          strategyIntendedBounds: { $push: command.value },
        },
      },
    },
  ]
}

function getResizeAncestorsPatches(
  editor: EditorState,
  command: PushIntendedBounds,
): EditorStatePatch {
  const targets = command.value

  // we are going to mutate this as we iterate over targets
  let updatedGlobalFrames: { [path: string]: CanvasRectangle } = {}

  function getGlobalFrame(path: ElementPath): CanvasRectangle {
    return forceNotNull(
      `Invariant: found null globalFrame for ${EP.toString(path)}`,
      updatedGlobalFrames[EP.toString(path)] ??
        MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)?.globalFrame,
    )
  }

  targets.forEach((frameAndTarget) => {
    const parentPath = EP.parentPath(frameAndTarget.target)
    const parentMetadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, parentPath)
    const parentIsGroup = MetadataUtils.isGroupAgainstImports(parentMetadata)

    if (!parentIsGroup || parentPath == null || parentMetadata == null) {
      // bail out
      return
    }

    // I assume that affectedAncestors are ordered bottom-up
    // the ancestor's globalFrame shall be the union of the current global frame and the target's frame
    const childrenExceptTheTarget = MetadataUtils.getChildrenPathsUnordered(
      editor.jsxMetadata,
      parentPath,
    ).filter((c) => !EP.pathsEqual(c, frameAndTarget.target))
    const childrenGlobalFrames = childrenExceptTheTarget.map(getGlobalFrame)
    const newGlobalFrame = boundingRectangleArray([...childrenGlobalFrames, frameAndTarget.frame])
    if (newGlobalFrame != null) {
      updatedGlobalFrames[EP.toString(parentPath)] = newGlobalFrame
    }
  })

  // we REALLY need a lens for this!
  let commandsToRun: Array<CanvasCommand> = []

  // okay so now we have a bunch of new globalFrames. what do we do with them?
  Object.keys(updatedGlobalFrames).forEach((pathStr) => {
    const elementToUpdate = EP.fromString(pathStr)
    const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, elementToUpdate)

    // TODO rewrite it as happy-path-to-the-left
    if (metadata != null) {
      const currentGlobalFrame = nullIfInfinity(metadata.globalFrame)
      const updatedGlobalFrame = updatedGlobalFrames[pathStr]

      if (currentGlobalFrame != null) {
        commandsToRun.push(
          ...setElementTopLeftWidthHeight(metadata, currentGlobalFrame, updatedGlobalFrame),
          wildcardPatch('always', {
            jsxMetadata: { [pathStr]: { globalFrame: { $set: updatedGlobalFrame } } },
          }),
        )

        // TODO we also need to offset all children for top and left changes
        const offsetChangeForChildren = vectorDifference(currentGlobalFrame, updatedGlobalFrame)
        if (magnitude(offsetChangeForChildren) != 0) {
          const children = MetadataUtils.getChildrenPathsUnordered(
            editor.jsxMetadata,
            elementToUpdate,
          )
          children.forEach((childPath) => {
            const childMetadata = MetadataUtils.findElementByElementPath(
              editor.jsxMetadata,
              childPath,
            )
            if (childMetadata != null) {
              // unshift children now that their parent's top left moved
              const currentChildGlobalFrame = getGlobalFrame(childPath)
              const currentChildGlobalFrameOffset = offsetRect(
                currentChildGlobalFrame,
                offsetChangeForChildren,
              )

              if (currentChildGlobalFrame != null) {
                commandsToRun.push(
                  ...setElementTopLeftWidthHeight(
                    childMetadata,
                    currentChildGlobalFrameOffset,
                    currentChildGlobalFrame,
                  ),
                )
              }
            }
          })
        }
      }
    }
  })

  const updatedEditor = foldAndApplyCommandsSimple(editor, commandsToRun)
  return { projectContents: { $set: updatedEditor.projectContents } }
}

function setElementTopLeftWidthHeight(
  instance: ElementInstanceMetadata,
  currentGlobalFrame: CanvasRectangle,
  updatedGlobalFrame: CanvasRectangle,
): Array<CanvasCommand> {
  return [
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'top'),
      updatedGlobalFrame.y - currentGlobalFrame.y,
      instance.specialSizeMeasurements.coordinateSystemBounds?.height,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'left'),
      updatedGlobalFrame.x - currentGlobalFrame.x,
      instance.specialSizeMeasurements.coordinateSystemBounds?.width,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'right'),
      // prettier-ignore
      (updatedGlobalFrame.x + updatedGlobalFrame.width) - (currentGlobalFrame.x + currentGlobalFrame.width),
      instance.specialSizeMeasurements.coordinateSystemBounds?.width,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'bottom'),
      // prettier-ignore
      (updatedGlobalFrame.y + updatedGlobalFrame.height) - (currentGlobalFrame.y + currentGlobalFrame.height),
      instance.specialSizeMeasurements.coordinateSystemBounds?.height,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'width'),
      updatedGlobalFrame.width - currentGlobalFrame.width,
      instance.specialSizeMeasurements.coordinateSystemBounds?.width,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    adjustCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'height'),
      updatedGlobalFrame.height - currentGlobalFrame.height,
      instance.specialSizeMeasurements.coordinateSystemBounds?.height,
      instance.specialSizeMeasurements.parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
  ]
}
