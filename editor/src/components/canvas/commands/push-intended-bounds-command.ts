import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isNotNull } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPathTrees } from '../../../core/shared/element-path-tree'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import {
  CanvasRectangle,
  MaybeInfinityCanvasRectangle,
  boundingRectangleArray,
  isFiniteRectangle,
  isInfinityRectangle,
  magnitude,
  nullIfInfinity,
  offsetRect,
  rectangleDifference,
  vectorDifference,
} from '../../../core/shared/math-utils'
import { notNull } from '../../../core/shared/optics/optic-creators'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type {
  AllElementProps,
  EditorState,
  EditorStatePatch,
} from '../../editor/store/editor-state'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from '../canvas-strategies/strategies/fragment-like-helpers'
import { treatElementAsGroupLike } from '../canvas-strategies/strategies/group-helpers'
import { CanvasFrameAndTarget } from '../canvas-types'
import { adjustCssLengthProperty } from './adjust-css-length-command'
import {
  BaseCommand,
  CanvasCommand,
  CommandFunctionResult,
  foldAndApplyCommandsSimple,
} from './commands'
import { setCssLengthProperty, setValueKeepingOriginalUnit } from './set-css-length-command'
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
  const { statePatch: resizeAncestorsPatch, intendedBounds: extraIndendedBounds } =
    getResizeAncestorsPatches(editor, command)

  const intendedBoundsPatch =
    commandLifecycle === 'mid-interaction'
      ? pushCommandStatePatch([...command.value, ...extraIndendedBounds])
      : []

  return {
    editorStatePatches: [resizeAncestorsPatch, ...intendedBoundsPatch],
    commandDescription: `Set Intended Bounds for ${command.value
      .map((c) => EP.toString(c.target))
      .join(', ')}`,
  }
}

function pushCommandStatePatch(
  intendedBounds: Array<CanvasFrameAndTarget>,
): Array<EditorStatePatch> {
  return [
    {
      canvas: {
        controls: {
          strategyIntendedBounds: { $push: intendedBounds },
        },
      },
    },
  ]
}

function getResizeAncestorsPatches(
  editor: EditorState,
  command: PushIntendedBounds,
): { statePatch: EditorStatePatch; intendedBounds: Array<CanvasFrameAndTarget> } {
  const targets = command.value

  // we are going to mutate this as we iterate over targets
  let updatedGlobalFrames: { [path: string]: CanvasFrameAndTarget | undefined } = {}

  function getGlobalFrame(path: ElementPath): CanvasRectangle {
    return forceNotNull(
      `Invariant: found null globalFrame for ${EP.toString(path)}`,
      updatedGlobalFrames[EP.toString(path)]?.frame ??
        nullIfInfinity(
          MetadataUtils.findElementByElementPath(editor.jsxMetadata, path)?.globalFrame,
        ),
    )
  }

  targets.forEach((frameAndTarget) => {
    const parentPath = EP.parentPath(frameAndTarget.target)
    const parentIsGroup = treatElementAsGroupLike(editor.jsxMetadata, parentPath)

    if (!parentIsGroup || parentPath == null) {
      // bail out
      return
    }

    // I assume that affectedAncestors are ordered bottom-up
    // the ancestor's globalFrame shall be the union of the current global frame and the target's frame
    const childrenExceptTheTarget = MetadataUtils.getChildrenPathsOrdered(
      editor.jsxMetadata,
      editor.elementPathTree,
      parentPath,
    ).filter((c) => !EP.pathsEqual(c, frameAndTarget.target))
    const childrenGlobalFrames = childrenExceptTheTarget.map(getGlobalFrame)

    const newGlobalFrame = boundingRectangleArray([...childrenGlobalFrames, frameAndTarget.frame])
    if (newGlobalFrame != null) {
      updatedGlobalFrames[EP.toString(parentPath)] = {
        frame: newGlobalFrame,
        target: parentPath,
      }
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
      const updatedGlobalFrame = updatedGlobalFrames[pathStr]?.frame

      if (currentGlobalFrame != null && updatedGlobalFrame != null) {
        commandsToRun.push(
          ...setGroupPins(metadata, currentGlobalFrame, updatedGlobalFrame),
          wildcardPatch('always', {
            jsxMetadata: { [pathStr]: { globalFrame: { $set: updatedGlobalFrame } } },
          }),
        )

        // TODO we also need to offset all children for top and left changes
        const globalFrameDiff = rectangleDifference(currentGlobalFrame, updatedGlobalFrame)
        if (
          globalFrameDiff.x !== 0 ||
          globalFrameDiff.y !== 0 ||
          globalFrameDiff.width !== 0 ||
          globalFrameDiff.height !== 0
        ) {
          const children = MetadataUtils.getChildrenPathsOrdered(
            editor.jsxMetadata,
            editor.elementPathTree,
            elementToUpdate,
          )
          children.forEach((childPath) => {
            const childMetadata = MetadataUtils.findElementByElementPath(
              editor.jsxMetadata,
              childPath,
            )
            if (childMetadata == null) {
              return
            }

            commandsToRun.push(
              ...keepElementPutInParent(
                editor.jsxMetadata,
                editor.allElementProps,
                editor.elementPathTree,
                childPath,
                currentGlobalFrame,
                updatedGlobalFrame,
              ),
            )
          })
        }
      }
    }
  })

  const updatedEditor = foldAndApplyCommandsSimple(editor, commandsToRun)
  return {
    statePatch: { projectContents: { $set: updatedEditor.projectContents } },
    intendedBounds: Object.values(updatedGlobalFrames).filter(isNotNull),
  }
}

function setGroupPins(
  instance: ElementInstanceMetadata,
  currentGlobalFrame: CanvasRectangle,
  updatedGlobalFrame: CanvasRectangle,
): Array<CanvasCommand> {
  // TODO retarget Fragments
  const result = [
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
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'width'),
      setValueKeepingOriginalUnit(
        updatedGlobalFrame.width,
        instance.specialSizeMeasurements.coordinateSystemBounds?.width,
      ),
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'height'),
      setValueKeepingOriginalUnit(
        updatedGlobalFrame.height,
        instance.specialSizeMeasurements.coordinateSystemBounds?.height,
      ),
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
  ]
  return result
}

function keepElementPutInParent(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  targetMaybeFragent: ElementPath,
  currentGlobalFrame: CanvasRectangle,
  updatedGlobalFrame: CanvasRectangle,
): Array<CanvasCommand> {
  const targets = replaceFragmentLikePathsWithTheirChildrenRecursive(
    metadata,
    allElementProps,
    elementPathTrees,
    [targetMaybeFragent],
  )
  const result = targets.flatMap((target) => {
    const instance = forceNotNull(
      `could not find element ${EP.toString(target)}`,
      MetadataUtils.findElementByElementPath(metadata, target),
    )
    return [
      adjustCssLengthProperty(
        'always',
        target,
        PP.create('style', 'top'),
        currentGlobalFrame.y - updatedGlobalFrame.y,
        instance.specialSizeMeasurements.coordinateSystemBounds?.height,
        instance.specialSizeMeasurements.parentFlexDirection,
        'do-not-create-if-doesnt-exist',
      ),
      adjustCssLengthProperty(
        'always',
        instance.elementPath,
        PP.create('style', 'left'),
        currentGlobalFrame.x - updatedGlobalFrame.x,
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
    ]
  })
  return result
}
