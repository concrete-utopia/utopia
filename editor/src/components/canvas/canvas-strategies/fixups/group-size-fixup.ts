import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  canvasRectangle,
  CanvasRectangle,
  isFiniteRectangle,
  magnitude,
  nullIfInfinity,
  offsetRect,
  rectanglesEqual,
  vectorDifference,
} from '../../../../core/shared/math-utils'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { cssNumber } from '../../../inspector/common/css-utils'
import { adjustCssLengthProperty } from '../../commands/adjust-css-length-command'
import { CanvasCommand, foldAndApplyCommandsSimple } from '../../commands/commands'
import { PushIntendedBounds } from '../../commands/push-intended-bounds-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { PostStrategyFixupStep } from '../canvas-strategies'
import {
  isSizedContainerWithAbsoluteChildren,
  listAllGroupLikeAffectedAncestorsForTarget,
} from '../strategies/group-like-helpers'

function setElementTopLeftWidthHeight(
  instance: ElementInstanceMetadata,
  aabb: CanvasRectangle,
): Array<CanvasCommand> {
  return [
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.y, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.x, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'width'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.width, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      instance.elementPath,
      PP.create('style', 'height'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.height, null) },
      instance.specialSizeMeasurements.parentFlexDirection,
    ),
  ]
}

export const groupSizingFixup: PostStrategyFixupStep = {
  name: 'Fix Group Sizes',
  fixup: (store) => {
    const metadata = store.jsxMetadata
    return Object.values(metadata)
      .filter((instance) =>
        isSizedContainerWithAbsoluteChildren(metadata, store.allElementProps, instance.elementPath),
      )
      .flatMap((instance) => {
        const children = MetadataUtils.getChildrenUnordered(metadata, instance.elementPath)
        const aabb = boundingRectangleArray(
          mapDropNulls(
            (e) =>
              e.globalFrame != null && isFiniteRectangle(e.globalFrame) ? e.globalFrame : null,
            children,
          ),
        )

        if (aabb == null) {
          return []
        }

        if (
          instance.globalFrame != null &&
          isFiniteRectangle(instance.globalFrame) &&
          rectanglesEqual(instance.globalFrame, aabb)
        ) {
          return []
        }

        return [
          ...setElementTopLeftWidthHeight(instance, aabb),
          ...children.flatMap((child) =>
            child.globalFrame != null && isFiniteRectangle(child.globalFrame)
              ? setElementTopLeftWidthHeight(
                  child,
                  canvasRectangle({
                    x: child.globalFrame.x - aabb.x,
                    y: child.globalFrame.y - aabb.y,
                    width: child.globalFrame.width,
                    height: child.globalFrame.height,
                  }),
                )
              : [],
          ),
          setElementsToRerenderCommand('rerender-all-elements'),
        ]
      })
  },
}

// some code which may be useful

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
    // find out which ancestors need resizing
    const affectedAncestors = listAllGroupLikeAffectedAncestorsForTarget(
      editor.jsxMetadata,
      frameAndTarget.target,
    )

    // I assume that affectedAncestors are ordered bottom-up
    affectedAncestors.forEach((ancestor) => {
      // the ancestor's globalFrame shall be the union of the current global frame and the target's frame
      const childrenExceptTheTarget = MetadataUtils.getChildrenPathsUnordered(
        editor.jsxMetadata,
        ancestor,
      ).filter((c) => !EP.pathsEqual(c, frameAndTarget.target))
      const childrenGlobalFrames = childrenExceptTheTarget.map(getGlobalFrame)
      const newGlobalFrame = boundingRectangleArray([...childrenGlobalFrames, frameAndTarget.frame])
      if (newGlobalFrame != null) {
        updatedGlobalFrames[EP.toString(ancestor)] = newGlobalFrame
      }
    })
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
          ...setElementTopLeftWidthHeight2(metadata, currentGlobalFrame, updatedGlobalFrame),
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
                  ...setElementTopLeftWidthHeight2(
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

function setElementTopLeftWidthHeight2(
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
