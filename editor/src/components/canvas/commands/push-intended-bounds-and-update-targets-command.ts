import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXAttributes,
} from '../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  nullIfInfinity,
  rectangleDifference,
} from '../../../core/shared/math-utils'
import { forceNotNull, isNotNull } from '../../../core/shared/optional-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type {
  AllElementProps,
  EditorState,
  EditorStatePatch,
} from '../../editor/store/editor-state'
import { trueUpGroupElementChanged } from '../../editor/store/editor-state'
import { cssPixelLength, type FlexDirection } from '../../inspector/common/css-utils'
import {
  isFixedHugFillModeAppliedOnAnySide,
  isHugFromStyleAttribute,
  isHugFromStyleAttributeOrNull,
} from '../../inspector/inspector-common'
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
import { deleteProperties } from './delete-properties-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
  setValueKeepingOriginalUnit,
} from './set-css-length-command'
import type { FrameWithAllPoints } from './utils/group-resize-utils'
import {
  rectangleToSixFramePoints,
  roundSixPointFrameToNearestWhole,
  sixFramePointsToCanvasRectangle,
  transformConstrainedLocalFullFrameUsingBoundingBox,
} from './utils/group-resize-utils'
import { wildcardPatch } from './wildcard-patch-command'

export type PushIntendedBoundsTargetGroup = {
  type: 'PUSH_INTENDED_BOUNDS_GROUP'
} & CanvasFrameAndTarget

export function pushIntendedBoundsGroup(
  target: ElementPath,
  frame: CanvasRectangle,
): PushIntendedBoundsTargetGroup {
  return {
    type: 'PUSH_INTENDED_BOUNDS_GROUP',
    target: target,
    frame: frame,
  }
}

export type PushIntendedBoundsTargetEmptyElement = {
  type: 'PUSH_INTENDED_BOUNDS_EMPTY_ELEMENT'
} & CanvasFrameAndTarget

export function pushIntendedBoundsEmptyElement(
  target: ElementPath,
  frame: CanvasRectangle,
): PushIntendedBoundsTargetEmptyElement {
  return {
    type: 'PUSH_INTENDED_BOUNDS_EMPTY_ELEMENT',
    target: target,
    frame: frame,
  }
}

function isPushIntendedBoundsTargetEmptyElement(
  u: unknown,
): u is PushIntendedBoundsTargetEmptyElement {
  return (u as PushIntendedBoundsTargetEmptyElement).type === 'PUSH_INTENDED_BOUNDS_EMPTY_ELEMENT'
}

function isPushIntendedBoundsTargetGroup(u: unknown): u is PushIntendedBoundsTargetGroup {
  return (u as PushIntendedBoundsTargetGroup).type === 'PUSH_INTENDED_BOUNDS_GROUP'
}

export type PushIntendedBoundsTarget =
  | PushIntendedBoundsTargetGroup
  | PushIntendedBoundsTargetEmptyElement

export interface PushIntendedBoundsAndUpdateTargets extends BaseCommand {
  type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_TARGETS'
  value: Array<PushIntendedBoundsTarget>
  isStartingMetadata: 'starting-metadata' | 'live-metadata' // TODO rename to reflect that what this stores is whether the command is running as a queued true up or as a predictive change during a user interaction
}

export function pushIntendedBoundsAndUpdateTargets(
  value: Array<PushIntendedBoundsTarget>,
  isStartingMetadata: 'starting-metadata' | 'live-metadata',
): PushIntendedBoundsAndUpdateTargets {
  return {
    type: 'PUSH_INTENDED_BOUNDS_AND_UPDATE_TARGETS',
    whenToRun: 'always',
    value: value,
    isStartingMetadata: isStartingMetadata,
  }
}

export const runPushIntendedBoundsAndUpdateTargets = (
  editor: EditorState,
  command: PushIntendedBoundsAndUpdateTargets,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  const commandRanBecauseOfQueuedTrueUp = command.isStartingMetadata === 'live-metadata'

  let editorStatePatches: Array<EditorStatePatch> = []

  // Groups
  const groupTargets = command.value.filter(isPushIntendedBoundsTargetGroup)
  const { updatedEditor: editorAfterGroups, editorStatePatches: groupsEditorStatePatches } =
    runPushIntendedBoundsAndUpdateTargetsGroup(
      editor,
      groupTargets,
      commandRanBecauseOfQueuedTrueUp,
      commandLifecycle,
    )
  editorStatePatches.push(...groupsEditorStatePatches)

  // Empty elements
  const emptyTargets = command.value.filter(isPushIntendedBoundsTargetEmptyElement)
  const { updatedEditor: editorAfterEmptyElements } =
    runPushIntendedBoundsAndUpdateTargetsEmptyElement(editorAfterGroups, emptyTargets)

  // TODO this is the worst editor patch in history, this should be much more fine grained, only patching the elements that changed
  editorStatePatches.push({
    projectContents: {
      $set: editorAfterEmptyElements.projectContents,
    },
    toasts: {
      $set: editorAfterEmptyElements.toasts,
    },
  })

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Set Intended Bounds for ${command.value
      .map((c) => EP.toString(c.target))
      .join(', ')}`,
  }
}

function runPushIntendedBoundsAndUpdateTargetsGroup(
  editor: EditorState,
  targets: Array<PushIntendedBoundsTargetGroup>,
  commandRanBecauseOfQueuedTrueUp: boolean,
  commandLifecycle: InteractionLifecycle,
): {
  updatedEditor: EditorState
  editorStatePatches: Array<EditorStatePatch>
} {
  const { updatedEditor: editorAfterResizingGroupChildren, resizedGroupChildren } =
    getUpdateResizedGroupChildrenCommands(editor, targets)

  const {
    updatedEditor: editorAfterResizingAncestors,
    intendedBounds: resizeAncestorsIntendedBounds,
  } = getResizeAncestorGroupsCommands(
    editorAfterResizingGroupChildren,
    targets,
    commandRanBecauseOfQueuedTrueUp
      ? 'do-not-create-if-doesnt-exist'
      : // TODO this can be removed in a future PR, but for now matching the previous behavior
        'create-if-not-existing',
  )

  let editorStatePatches: Array<EditorStatePatch> = []
  if (commandLifecycle === 'mid-interaction') {
    editorStatePatches.push({
      canvas: {
        controls: {
          strategyIntendedBounds: {
            $push: [...targets, ...resizeAncestorsIntendedBounds],
          },
        },
      },
    })
  }
  if (commandLifecycle === 'end-interaction' && !commandRanBecauseOfQueuedTrueUp) {
    editorStatePatches.push({
      trueUpElementsAfterDomWalkerRuns: {
        $set: resizedGroupChildren.map(trueUpGroupElementChanged),
      },
    })
  }

  return {
    updatedEditor: editorAfterResizingAncestors,
    editorStatePatches: editorStatePatches,
  }
}

function runPushIntendedBoundsAndUpdateTargetsEmptyElement(
  editor: EditorState,
  targets: Array<PushIntendedBoundsTargetEmptyElement>,
): {
  updatedEditor: EditorState
} {
  let commands: Array<CanvasCommand> = []
  for (const v of targets) {
    const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, v.target)
    if (
      metadata == null ||
      // must be empty
      MetadataUtils.getChildrenUnordered(editor.jsxMetadata, v.target).length > 0 ||
      // must be hugging
      !isFixedHugFillModeAppliedOnAnySide(editor.jsxMetadata, v.target, 'hug')
    ) {
      continue
    }

    function setProperty(
      flexDirection: FlexDirection | null,
      prop: 'left' | 'top' | 'width' | 'height',
      value: number,
    ) {
      return setCssLengthProperty(
        'always',
        v.target,
        PP.create('style', prop),
        setExplicitCssValue(cssPixelLength(value)),
        flexDirection,
        'create-if-not-existing',
        'warn-about-replacement',
      )
    }
    commands.push(
      setProperty(metadata.specialSizeMeasurements.flexDirection, 'left', v.frame.x),
      setProperty(metadata.specialSizeMeasurements.flexDirection, 'top', v.frame.y),
      setProperty(metadata.specialSizeMeasurements.flexDirection, 'width', v.frame.width),
      setProperty(metadata.specialSizeMeasurements.flexDirection, 'height', v.frame.height),
      deleteProperties('always', v.target, [PP.create('style', 'right')]),
      deleteProperties('always', v.target, [PP.create('style', 'bottom')]),
    )
  }
  return {
    updatedEditor: foldAndApplyCommandsSimple(editor, commands),
  }
}

type LocalFrameAndTarget = {
  target: ElementPath
  allSixFramePoints: FrameWithAllPoints
}

function rectangleFromChildrenBounds(
  editor: EditorState,
  children: Array<ElementPath>,
): CanvasRectangle | null {
  return boundingRectangleArray(
    children.map((c) => {
      return nullIfInfinity(
        MetadataUtils.findElementByElementPath(editor.jsxMetadata, c)?.globalFrame,
      )
    }),
  )
}

function getUpdateResizedGroupChildrenCommands(
  editor: EditorState,
  groupTargets: Array<PushIntendedBoundsTargetGroup>,
): { updatedEditor: EditorState; resizedGroupChildren: Array<ElementPath> } {
  const targets: Array<PushIntendedBoundsTargetGroup> = [...groupTargets]

  // we are going to mutate this as we iterate over targets
  let updatedLocalFrames: { [path: string]: LocalFrameAndTarget | undefined } = {}

  for (const v of targets) {
    const targetIsGroup = allowGroupTrueUp(
      editor.projectContents,
      editor.jsxMetadata,
      editor.elementPathTree,
      editor.allElementProps,
      v.target,
    )
    if (targetIsGroup) {
      const children = MetadataUtils.getChildrenPathsOrdered(
        editor.jsxMetadata,
        editor.elementPathTree,
        v.target,
      )

      function frameFromMeasuredBounds(): CanvasRectangle | null {
        return MetadataUtils.getFrameOrZeroRectInCanvasCoords(v.target, editor.jsxMetadata)
      }

      // The original size of the children before the interaction ran.
      const childrenBounds: CanvasRectangle | null = rectangleFromChildrenBounds(editor, children)

      // The original size of the group before the interaction ran.
      const groupOriginalBounds = frameFromMeasuredBounds()

      if (childrenBounds != null && groupOriginalBounds != null) {
        const updatedGroupBounds = v.frame

        // if the target is a group and the reason for resizing is _NOT_ child-changed, then resize all the children to fit the new AABB
        const childrenWithFragmentsRetargeted = replaceFragmentLikePathsWithTheirChildrenRecursive(
          editor.jsxMetadata,
          editor.allElementProps,
          editor.elementPathTree,
          children,
        )
        for (const child of childrenWithFragmentsRetargeted) {
          const currentLocalFrame = MetadataUtils.getLocalFrameFromSpecialSizeMeasurements(
            child,
            editor.jsxMetadata,
          )
          if (currentLocalFrame == null) {
            // bail
            continue
          }

          let constraints: Set<keyof FrameWithAllPoints> = new Set(
            editor.allElementProps[EP.toString(child)]?.['data-constraints'] ?? [],
          )

          const elementMetadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, child)

          const jsxElement = MetadataUtils.getJSXElementFromMetadata(editor.jsxMetadata, child)
          if (jsxElement != null) {
            if (isHugFromStyleAttribute(jsxElement.props, 'width')) {
              constraints.add('width')
            }
            if (isHugFromStyleAttribute(jsxElement.props, 'height')) {
              constraints.add('height')
            }
          }

          const resizedLocalFramePoints = roundSixPointFrameToNearestWhole(
            transformConstrainedLocalFullFrameUsingBoundingBox(
              groupOriginalBounds,
              updatedGroupBounds,
              childrenBounds,
              rectangleToSixFramePoints(currentLocalFrame, childrenBounds),
              Array.from(constraints),
            ),
          )

          function getAdjustedResizedLocalFramePoints(): FrameWithAllPoints {
            if (elementMetadata != null && MetadataUtils.isTextFromMetadata(elementMetadata)) {
              if (elementMetadata.specialSizeMeasurements.textBounds != null) {
                return {
                  ...resizedLocalFramePoints,
                  width: Math.max(
                    resizedLocalFramePoints.width,
                    elementMetadata.specialSizeMeasurements.textBounds.width,
                  ),
                  height: Math.max(
                    resizedLocalFramePoints.height,
                    elementMetadata.specialSizeMeasurements.textBounds.height,
                  ),
                }
              }
            }
            return resizedLocalFramePoints
          }

          const adjustedResizedLocalFramePoints = getAdjustedResizedLocalFramePoints()

          updatedLocalFrames[EP.toString(child)] = {
            allSixFramePoints: adjustedResizedLocalFramePoints,
            target: child,
          }
          targets.push(
            pushIntendedBoundsGroup(
              child,
              sixFramePointsToCanvasRectangle(adjustedResizedLocalFramePoints),
            ),
          )
        }
      }
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

    commandsToRun.push(
      ...setElementPins(
        elementToUpdate,
        MetadataUtils.getJSXElementFromMetadata(editor.jsxMetadata, elementToUpdate)?.props ?? null,
        frameAndTarget.allSixFramePoints,
        metadata.specialSizeMeasurements.parentFlexDirection,
      ),
    )
  })

  const updatedEditor = foldAndApplyCommandsSimple(editor, commandsToRun)
  return {
    updatedEditor: updatedEditor,
    resizedGroupChildren: updatedElements,
  }
}

function getResizeAncestorGroupsCommands(
  editor: EditorState,
  targets: Array<PushIntendedBoundsTargetGroup>,
  addGroupSizeIfNonExistant: CreateIfNotExistant,
): { updatedEditor: EditorState; intendedBounds: Array<CanvasFrameAndTarget> } {
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

  for (const v of targets) {
    const parentPath = replaceNonDomElementWithFirstDomAncestorPath(
      editor.jsxMetadata,
      editor.allElementProps,
      editor.elementPathTree,
      EP.parentPath(v.target),
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
    ).filter((c) => !EP.pathsEqual(c, v.target))
    const childrenGlobalFrames = childrenExceptTheTarget.map(getGlobalFrame)

    const newGlobalFrame = boundingRectangleArray([...childrenGlobalFrames, v.frame])
    if (newGlobalFrame != null) {
      updatedGlobalFrames[EP.toString(parentPath)] = {
        frame: newGlobalFrame,
        target: parentPath,
      }
      targets.push(pushIntendedBoundsGroup(parentPath, newGlobalFrame))
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
  targetProps: JSXAttributes | null,
  framePoints: FrameWithAllPoints,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  // TODO retarget Fragments
  let result: Array<CanvasCommand> = [
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'left'),
      setExplicitCssValue(cssPixelLength(framePoints.left)),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
      'warn-about-replacement',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'top'),
      setExplicitCssValue(cssPixelLength(framePoints.top)),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
      'warn-about-replacement',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'right'),
      setExplicitCssValue(cssPixelLength(framePoints.right)),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
      'warn-about-replacement',
    ),
    setCssLengthProperty(
      'always',
      target,
      PP.create('style', 'bottom'),
      setExplicitCssValue(cssPixelLength(framePoints.bottom)),
      parentFlexDirection,
      'do-not-create-if-doesnt-exist',
      'warn-about-replacement',
    ),
  ]

  if (!isHugFromStyleAttributeOrNull(targetProps, 'width')) {
    result.push(
      setCssLengthProperty(
        'always',
        target,
        PP.create('style', 'width'),
        setExplicitCssValue(cssPixelLength(framePoints.width)),
        parentFlexDirection,
        'do-not-create-if-doesnt-exist',
        'warn-about-replacement',
      ),
    )
  }
  if (!isHugFromStyleAttributeOrNull(targetProps, 'height')) {
    result.push(
      setCssLengthProperty(
        'always',
        target,
        PP.create('style', 'height'),
        setExplicitCssValue(cssPixelLength(framePoints.height)),
        parentFlexDirection,
        'do-not-create-if-doesnt-exist',
        'warn-about-replacement',
      ),
    )
  }
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
