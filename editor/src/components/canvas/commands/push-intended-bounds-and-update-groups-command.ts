import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import type { CanvasRectangle, LocalRectangle, Size } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  localRectangle,
  nullIfInfinity,
  rectangleDifference,
  roundRectangleToNearestWhole,
  sizeFromRectangle,
  transformFrameUsingBoundingBox,
} from '../../../core/shared/math-utils'
import { forceNotNull, isNotNull } from '../../../core/shared/optional-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type {
  AllElementProps,
  EditorState,
  EditorStatePatch,
} from '../../editor/store/editor-state'
import { trueUpElementChanged } from '../../editor/store/editor-state'
import type { FlexDirection } from '../../inspector/common/css-utils'
import { isFixedHugFillModeAppliedOnAnySide } from '../../inspector/inspector-common'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  replaceNonDomElementWithFirstDomAncestorPath,
} from '../canvas-strategies/strategies/fragment-like-helpers'
import { allowGroupTrueUp } from '../canvas-strategies/strategies/group-helpers'
import type { CanvasFrameAndTarget } from '../canvas-types'
import type { CreateIfNotExistant } from './adjust-css-length-command'
import { adjustCssLengthProperties, lengthPropertyToAdjust } from './adjust-css-length-command'
import type { BaseCommand, CanvasCommand, CommandFunctionResult } from './commands'
import { foldAndApplyCommandsSimple } from './commands'
import { setCssLengthProperty, setValueKeepingOriginalUnit } from './set-css-length-command'
import { wildcardPatch } from './wildcard-patch-command'

export interface PushIntendedBoundsAndUpdateGroups extends BaseCommand {
  type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_GROUPS'
  value: Array<CanvasFrameAndTarget>
  isStartingMetadata: 'starting-metadata' | 'live-metadata' // TODO rename to reflect that what this stores is whether the command is running as a queued true up or as a predictive change during a user interaction
  mode: PushIntendedBoundsAndUpdateGroupsMode
}

export type PushIntendedBoundsAndUpdateGroupsMode = 'move' | 'resize' | 'wrap'

export function pushIntendedBoundsAndUpdateGroups(
  value: Array<CanvasFrameAndTarget>,
  isStartingMetadata: 'starting-metadata' | 'live-metadata',
  mode: PushIntendedBoundsAndUpdateGroupsMode,
): PushIntendedBoundsAndUpdateGroups {
  return {
    type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_GROUPS',
    whenToRun: 'always',
    value: value,
    isStartingMetadata: isStartingMetadata,
    mode: mode,
  }
}

export const runPushIntendedBoundsAndUpdateGroups = (
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateGroups,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  const commandRanBecauseOfQueuedTrueUp = command.isStartingMetadata === 'live-metadata'

  const { updatedEditor: editorAfterResizingGroupChildren, resizedGroupChildren } =
    getUpdateResizedGroupChildrenCommands(editor, command)

  const {
    updatedEditor: editorAfterResizingAncestors,
    intendedBounds: resizeAncestorsIntendedBounds,
  } = getResizeAncestorGroupsCommands(
    editorAfterResizingGroupChildren,
    command,
    commandRanBecauseOfQueuedTrueUp
      ? 'do-not-create-if-doesnt-exist'
      : // TODO this can be removed in a future PR, but for now matching the previous behavior
        'create-if-not-existing',
  )

  // TODO this is the worst editor patch in history, this should be much more fine grained, only patching the elements that changed
  const editorPatch = { projectContents: { $set: editorAfterResizingAncestors.projectContents } }

  const intendedBoundsPatch =
    commandLifecycle === 'mid-interaction'
      ? pushIntendedBoundsPatch([...command.value, ...resizeAncestorsIntendedBounds])
      : []

  const queueFollowUpGroupTrueUpPatch: Array<EditorStatePatch> =
    commandLifecycle === 'end-interaction' && !commandRanBecauseOfQueuedTrueUp
      ? [
          {
            trueUpGroupsForElementAfterDomWalkerRuns: {
              $set: resizedGroupChildren.map((c) => trueUpElementChanged(c)),
            },
          },
        ]
      : []

  return {
    editorStatePatches: [editorPatch, ...intendedBoundsPatch, ...queueFollowUpGroupTrueUpPatch],
    commandDescription: `Set Intended Bounds for ${command.value
      .map((c) => EP.toString(c.target))
      .join(', ')}`,
  }
}

function pushIntendedBoundsPatch(
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

type LocalFrameAndTarget = {
  target: ElementPath
  parentSize: Size
  localFrame: LocalRectangle
}

/**
 * returns whether the given path can be used as a target for pushing intended bounds
 * and have its frame changed when in a given mode.
 */
function canPushIntendedBoundsForElement(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  currentMode: PushIntendedBoundsAndUpdateGroupsMode,
  validModes: PushIntendedBoundsAndUpdateGroupsMode[],
): boolean {
  return (
    validModes.includes(currentMode) || !isFixedHugFillModeAppliedOnAnySide(metadata, path, 'hug')
  )
}

function getUpdateResizedGroupChildrenCommands(
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateGroups,
): { updatedEditor: EditorState; resizedGroupChildren: Array<ElementPath> } {
  const targets: Array<{
    target: ElementPath
    size: Size
  }> = command.value.map((ft) => ({
    target: ft.target,
    size: sizeFromRectangle(ft.frame),
  }))

  // we are going to mutate this as we iterate over targets
  let updatedLocalFrames: { [path: string]: LocalFrameAndTarget | undefined } = {}

  for (const frameAndTarget of targets) {
    const targetIsGroup = allowGroupTrueUp(
      editor.projectContents,
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      frameAndTarget.target,
    )
    if (targetIsGroup) {
      const children = MetadataUtils.getChildrenPathsOrdered(
        editor.jsxMetadata,
        editor.elementPathTree,
        frameAndTarget.target,
      ).filter((path) => {
        return canPushIntendedBoundsForElement(path, editor.jsxMetadata, command.mode, [
          'resize',
          'wrap',
        ])
      })

      // the original size of the group before the interaction ran
      const originalSize: Size =
        command.isStartingMetadata === 'starting-metadata'
          ? // if we have the starting metadata, we can simply get the original measured bounds of the element and we know it's the originalSize
            sizeFromRectangle(
              MetadataUtils.getLocalFrameFromSpecialSizeMeasurements(
                frameAndTarget.target,
                editor.jsxMetadata,
              ),
            )
          : // if the metadata is fresh, the group is already resized. so we need to query the size of its children AABB
            //(which was not yet updated, since this function is updating the children sizes) to get the originalSize
            sizeFromRectangle(
              boundingRectangleArray(
                children.map((c) =>
                  nullIfInfinity(
                    MetadataUtils.findElementByElementPath(editor.jsxMetadata, c)?.globalFrame,
                  ),
                ),
              ),
            )

      const updatedSize: Size = frameAndTarget.size

      // if the target is a group and the reason for resizing is _NOT_ child-changed, then resize all the children to fit the new AABB
      const childrenWithFragmentsRetargeted = replaceFragmentLikePathsWithTheirChildrenRecursive(
        editor.jsxMetadata,
        editor.allElementProps,
        editor.elementPathTree,
        children,
      )
      childrenWithFragmentsRetargeted.forEach((child) => {
        const currentLocalFrame = MetadataUtils.getLocalFrameFromSpecialSizeMeasurements(
          child,
          editor.jsxMetadata,
        )
        if (currentLocalFrame == null) {
          // bail
          return
        }
        const resizedLocalFrame = roundRectangleToNearestWhole(
          transformFrameUsingBoundingBox(
            localRectangle({ ...updatedSize, x: 0, y: 0 }),
            localRectangle({ ...originalSize, x: 0, y: 0 }),
            currentLocalFrame,
          ),
        )
        updatedLocalFrames[EP.toString(child)] = {
          localFrame: resizedLocalFrame,
          parentSize: updatedSize,
          target: child,
        }
        targets.push({ target: child, size: sizeFromRectangle(resizedLocalFrame) })
      })
    }
  }

  let commandsToRun: Array<CanvasCommand> = []
  let updatedElements: Array<ElementPath> = []

  Object.entries(updatedLocalFrames).forEach(([pathStr, frameAndTarget]) => {
    const elementToUpdate = EP.fromString(pathStr)
    const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, elementToUpdate)

    if (frameAndTarget == null || metadata == null) {
      return
    }

    updatedElements.push(elementToUpdate)

    if (
      canPushIntendedBoundsForElement(elementToUpdate, editor.jsxMetadata, command.mode, ['resize'])
    ) {
      commandsToRun.push(
        ...setElementPins(
          elementToUpdate,
          frameAndTarget.localFrame,
          frameAndTarget.parentSize,
          metadata.specialSizeMeasurements.parentFlexDirection,
        ),
      )
    }
  })

  const updatedEditor = foldAndApplyCommandsSimple(editor, commandsToRun)
  return {
    updatedEditor: updatedEditor,
    resizedGroupChildren: updatedElements,
  }
}

function getResizeAncestorGroupsCommands(
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateGroups,
  addGroupSizeIfNonExistant: CreateIfNotExistant,
): { updatedEditor: EditorState; intendedBounds: Array<CanvasFrameAndTarget> } {
  const targets: Array<CanvasFrameAndTarget> = [...command.value]

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

  for (const frameAndTarget of targets) {
    const parentPath = replaceNonDomElementWithFirstDomAncestorPath(
      editor.jsxMetadata,
      editor.allElementProps,
      editor.elementPathTree,
      EP.parentPath(frameAndTarget.target),
    )
    const groupTrueUpPermitted = allowGroupTrueUp(
      editor.projectContents,
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      parentPath,
    )

    if (!groupTrueUpPermitted || parentPath == null) {
      // bail out
      continue
    }

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
      targets.push({ target: parentPath, frame: newGlobalFrame })
    }
  }

  let commandsToRun: Array<CanvasCommand> = []

  Object.entries(updatedGlobalFrames).forEach(([pathStr, frameAndTarget]) => {
    const elementToUpdate = EP.fromString(pathStr)
    const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, elementToUpdate)

    // TODO rewrite it as happy-path-to-the-left
    if (metadata != null) {
      const currentGlobalFrame = nullIfInfinity(metadata.globalFrame)
      const updatedGlobalFrame = frameAndTarget?.frame

      if (currentGlobalFrame != null && updatedGlobalFrame != null) {
        commandsToRun.push(
          ...setGroupPins(
            metadata,
            currentGlobalFrame,
            updatedGlobalFrame,
            addGroupSizeIfNonExistant,
          ),
          wildcardPatch('always', {
            jsxMetadata: { [pathStr]: { globalFrame: { $set: updatedGlobalFrame } } },
          }),
        )

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
    updatedEditor: updatedEditor,
    intendedBounds: Object.values(updatedGlobalFrames).filter(isNotNull),
  }
}

function setElementPins(
  target: ElementPath,
  localFrame: LocalRectangle,
  parentSize: Size,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  // TODO retarget Fragments
  const result = [
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'left'),
      setValueKeepingOriginalUnit(localFrame.x, parentSize.width),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'top'),
      setValueKeepingOriginalUnit(localFrame.y, parentSize.height),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'right'),
      setValueKeepingOriginalUnit(
        parentSize.width - (localFrame.x + localFrame.width),
        parentSize.width,
      ),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'bottom'),
      setValueKeepingOriginalUnit(
        parentSize.height - (localFrame.y + localFrame.height),
        parentSize.height,
      ),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'width'),
      setValueKeepingOriginalUnit(localFrame.width, parentSize.width),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'height'),
      setValueKeepingOriginalUnit(localFrame.height, parentSize.height),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
    ),
  ]
  return result
}

function setGroupPins(
  instance: ElementInstanceMetadata,
  currentGlobalFrame: CanvasRectangle,
  updatedGlobalFrame: CanvasRectangle,
  createSizeIfNonExistant: CreateIfNotExistant,
): Array<CanvasCommand> {
  // TODO retarget Fragments
  const result = [
    adjustCssLengthProperties(
      'always',
      instance.elementPath,
      instance.specialSizeMeasurements.parentFlexDirection,
      [
        lengthPropertyToAdjust(
          PP.create('style', 'top'),
          updatedGlobalFrame.y - currentGlobalFrame.y,
          instance.specialSizeMeasurements.coordinateSystemBounds?.height,
          'do-not-create-if-doesnt-exist',
        ),
        lengthPropertyToAdjust(
          PP.create('style', 'left'),
          updatedGlobalFrame.x - currentGlobalFrame.x,
          instance.specialSizeMeasurements.coordinateSystemBounds?.width,
          'do-not-create-if-doesnt-exist',
        ),
        lengthPropertyToAdjust(
          PP.create('style', 'right'),
          // prettier-ignore
          (currentGlobalFrame.x + currentGlobalFrame.width) - (updatedGlobalFrame.x + updatedGlobalFrame.width),
          instance.specialSizeMeasurements.coordinateSystemBounds?.width,
          'do-not-create-if-doesnt-exist',
        ),
        lengthPropertyToAdjust(
          PP.create('style', 'bottom'),
          // prettier-ignore
          (currentGlobalFrame.y + currentGlobalFrame.height) - (updatedGlobalFrame.y + updatedGlobalFrame.height),
          instance.specialSizeMeasurements.coordinateSystemBounds?.height,
          'do-not-create-if-doesnt-exist',
        ),
      ],
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
      createSizeIfNonExistant,
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
      createSizeIfNonExistant,
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
      adjustCssLengthProperties(
        'always',
        target,
        instance.specialSizeMeasurements.parentFlexDirection,
        [
          lengthPropertyToAdjust(
            PP.create('style', 'top'),
            currentGlobalFrame.y - updatedGlobalFrame.y,
            instance.specialSizeMeasurements.coordinateSystemBounds?.height,
            'do-not-create-if-doesnt-exist',
          ),
          lengthPropertyToAdjust(
            PP.create('style', 'left'),
            currentGlobalFrame.x - updatedGlobalFrame.x,
            instance.specialSizeMeasurements.coordinateSystemBounds?.width,
            'do-not-create-if-doesnt-exist',
          ),
          lengthPropertyToAdjust(
            PP.create('style', 'right'),
            // prettier-ignore
            (updatedGlobalFrame.x + updatedGlobalFrame.width) - (currentGlobalFrame.x + currentGlobalFrame.width),
            instance.specialSizeMeasurements.coordinateSystemBounds?.width,
            'do-not-create-if-doesnt-exist',
          ),
          lengthPropertyToAdjust(
            PP.create('style', 'bottom'),
            // prettier-ignore
            (updatedGlobalFrame.y + updatedGlobalFrame.height) - (currentGlobalFrame.y + currentGlobalFrame.height),
            instance.specialSizeMeasurements.coordinateSystemBounds?.height,
            'do-not-create-if-doesnt-exist',
          ),
        ],
      ),
    ]
  })
  return result
}
