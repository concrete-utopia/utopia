import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { KeyCharacter } from '../../utils/keyboard'
import Utils from '../../utils/utils'
import type { CanvasPoint, CanvasRectangle } from '../../core/shared/math-utils'
import {
  rectangleIntersection,
  canvasRectangle,
  isInfinityRectangle,
} from '../../core/shared/math-utils'
import type { EditorAction } from '../editor/action-types'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  LockedElements,
} from '../editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import type { ElementPathTree, ElementPathTrees } from '../../core/shared/element-path-tree'
import { forEachElementPathTreeChild, getSubTree } from '../../core/shared/element-path-tree'
import { assertNever, fastForEach } from '../../core/shared/utils'
import { memoize } from '../../core/shared/memoize'
import { maybeToArray } from '../../core/shared/optional-utils'
import { getAllLockedElementPaths, unlockedParent } from '../../core/shared/element-locking'

export enum TargetSearchType {
  ParentsOfSelected = 'ParentsOfSelected',
  SiblingsOfSelected = 'SiblingsOfSelected',
  ChildrenOfSelected = 'ChildrenOfSelected',
  SelectedElements = 'SelectedElements',
  TopLevelElements = 'TopLevelElements',
  All = 'all',
}

type FrameWithPath = {
  path: ElementPath
  frame: CanvasRectangle
}

function getFramesInCanvasContextUncached(
  allElementProps: AllElementProps,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  useBoundingFrames: boolean,
): Array<FrameWithPath> {
  const projectTree = elementPathTree

  function recurseChildren(componentTree: ElementPathTree): {
    boundingRect: CanvasRectangle | null
    frames: Array<FrameWithPath>
  } {
    const component = MetadataUtils.findElementByElementPath(metadata, componentTree.path)
    if (component == null) {
      return {
        boundingRect: null,
        frames: [],
      }
    }
    const globalFrame = useBoundingFrames
      ? component.specialSizeMeasurements.globalFrameWithTextContent ?? component.globalFrame
      : component.globalFrame
    if (globalFrame != null && isInfinityRectangle(globalFrame)) {
      // TODO Will this work for the storyboard?
      return {
        boundingRect: null,
        frames: [],
      }
    }

    const overflows = MetadataUtils.overflows(allElementProps, componentTree.path)
    const includeClippedNext = useBoundingFrames && overflows

    let children: Array<ElementPathTree> = []
    let unfurledComponents: Array<ElementPathTree> = []
    forEachElementPathTreeChild(componentTree, (childTree) => {
      if (EP.isRootElementOfInstance(childTree.path)) {
        unfurledComponents.push(childTree)
      } else {
        children.push(childTree)
      }
    })

    const childFrames = children.map((child) => {
      const recurseResults = recurseChildren(child)
      const rectToBoundWith =
        includeClippedNext || globalFrame == null ? recurseResults.boundingRect : globalFrame
      return { boundingRect: rectToBoundWith, frames: recurseResults.frames }
    })
    const unfurledFrames = unfurledComponents.map((unfurledElement) => {
      const recurseResults = recurseChildren(unfurledElement)
      const rectToBoundWith =
        includeClippedNext || globalFrame == null ? recurseResults.boundingRect : globalFrame
      return { boundingRect: rectToBoundWith, frames: recurseResults.frames }
    })
    const allFrames = [...childFrames, ...unfurledFrames]
    const allChildrenBounds = Utils.boundingRectangleArray(Utils.pluck(allFrames, 'boundingRect'))
    if (allFrames.length > 0 && allChildrenBounds != null) {
      const allChildrenFrames = Utils.pluck(allFrames, 'frames').flat()
      const boundingRect =
        globalFrame == null
          ? allChildrenBounds
          : Utils.boundingRectangle(globalFrame, allChildrenBounds)
      const toAppend: FrameWithPath = { path: component.elementPath, frame: boundingRect }
      return {
        boundingRect: boundingRect,
        frames: [toAppend].concat(allChildrenFrames),
      }
    } else {
      const boundingRect = globalFrame
      const toAppend: FrameWithPath | null =
        boundingRect == null ? null : { path: component.elementPath, frame: boundingRect }
      return { boundingRect: boundingRect, frames: maybeToArray(toAppend) }
    }
  }

  const storyboardChildren = MetadataUtils.getAllStoryboardChildrenPathsOrdered(
    metadata,
    elementPathTree,
  )
  return storyboardChildren.flatMap((storyboardChild) => {
    const subTree = getSubTree(projectTree, storyboardChild)
    if (subTree == null) {
      return []
    } else {
      return recurseChildren(subTree).frames
    }
  })
}

// eslint-disable-next-line object-shorthand
const Canvas = {
  parentsAndSiblings: [
    TargetSearchType.SelectedElements,
    TargetSearchType.SiblingsOfSelected,
    TargetSearchType.ParentsOfSelected,
  ],
  parentsSiblingsAndChildren: [
    TargetSearchType.ChildrenOfSelected,
    TargetSearchType.SelectedElements,
    TargetSearchType.SiblingsOfSelected,
    TargetSearchType.ParentsOfSelected,
  ],
  getFramesInCanvasContext: memoize(getFramesInCanvasContextUncached, { maxSize: 2 }),
  jumpToParent(
    selectedViews: Array<ElementPath>,
    metadata: ElementInstanceMetadataMap,
    pathTrees: ElementPathTrees,
    lockedElements: LockedElements,
  ): ElementPath | 'CLEAR' | null {
    switch (selectedViews.length) {
      case 0:
        // Nothing is selected, so do nothing.
        return null
      case 1:
        // Only a single element is selected...
        const parentPath = unlockedParent(metadata, pathTrees, lockedElements, selectedViews[0])
        if (parentPath == null || EP.isEmptyPath(parentPath)) {
          // ...the selected element is a top level one, so deselect.
          return 'CLEAR'
        }
        // ...the selected element has a parent, so select that.
        return parentPath

      default:
        // Multiple elements are selected so select the topmost element amongst them.
        const newSelection: ElementPath | null = selectedViews.reduce(
          (working: ElementPath | null, selectedView) => {
            if (working == null) {
              return selectedView
            } else {
              if (EP.depth(selectedView) < EP.depth(working)) {
                return selectedView
              } else {
                return working
              }
            }
          },
          null,
        )
        return Utils.forceNotNull('Internal Error.', newSelection)
    }
  },
  jumpToSibling(
    selectedViews: Array<ElementPath>,
    components: ElementInstanceMetadataMap,
    pathTrees: ElementPathTrees,
    forwards: boolean,
  ): ElementPath | null {
    switch (selectedViews.length) {
      case 0:
        return null
      case 1:
        const singleSelectedElement = selectedViews[0]
        const siblings = MetadataUtils.getSiblingsOrdered(
          components,
          pathTrees,
          singleSelectedElement,
        )
        const pathsToStep = siblings.map((s) => s.elementPath)
        return Utils.stepInArray(
          EP.pathsEqual,
          forwards ? 1 : -1,
          pathsToStep,
          singleSelectedElement,
        )
      default:
        // Multiple elements are selected so select the topmost element amongst them.
        const newSelection: ElementPath | null = selectedViews.reduce(
          (working: ElementPath | null, selectedView) => {
            if (working == null) {
              return selectedView
            } else {
              if (EP.depth(selectedView) < EP.depth(working)) {
                return selectedView
              } else {
                return working
              }
            }
          },
          null,
        )
        return Utils.forceNotNull('Internal Error.', newSelection)
    }
  },
  getFirstChild(
    selectedViews: Array<ElementPath>,
    components: ElementInstanceMetadataMap,
    pathTrees: ElementPathTrees,
  ): ElementPath | null {
    if (selectedViews.length !== 1) {
      return null
    } else {
      const children = MetadataUtils.getImmediateChildrenOrdered(
        components,
        pathTrees,
        selectedViews[0],
      )
      return children.length > 0 ? children[0].elementPath : null
    }
  },
  targetFilter(
    selectedViews: Array<ElementPath>,
    searchTypes: Array<TargetSearchType>,
  ): Array<(path: ElementPath) => boolean> {
    return searchTypes.map((searchType) => {
      switch (searchType) {
        case TargetSearchType.ParentsOfSelected:
          return (path: ElementPath) => {
            for (const selectedView of selectedViews) {
              if (EP.isDescendantOfOrEqualTo(selectedView, path)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.ChildrenOfSelected:
          return (path: ElementPath) => {
            for (const selectedView of selectedViews) {
              if (EP.isChildOf(path, selectedView)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.SiblingsOfSelected:
          return (path: ElementPath) => {
            for (const selectedView of selectedViews) {
              if (EP.isSiblingOf(selectedView, path) && !EP.containsPath(path, selectedViews)) {
                return true
              }
            }
            return false
          }
        case TargetSearchType.TopLevelElements:
          return (path: ElementPath) => {
            // TODO Scene Implementation
            if (EP.depth(path) === 2) {
              return true
            }
            return false
          }
        case TargetSearchType.SelectedElements:
          return (path: ElementPath) => {
            if (EP.containsPath(path, selectedViews)) {
              return true
            }
            return false
          }
        case TargetSearchType.All:
          return (path: ElementPath) => {
            return true
          }
        default:
          assertNever(searchType)
      }
    })
  },
  getMousePositionCanvasArea(canvasPosition: CanvasPoint | null): CanvasRectangle | null {
    if (canvasPosition == null) {
      return null
    }
    return canvasRectangle({
      x: canvasPosition.x - 1,
      y: canvasPosition.y - 1,
      width: 1,
      height: 1,
    })
  },
  getAllTargetsUnderArea(
    componentMetadata: ElementInstanceMetadataMap,
    selectedViews: Array<ElementPath>,
    hiddenInstances: Array<ElementPath>,
    canvasArea: CanvasRectangle,
    searchTypes: Array<TargetSearchType>,
    useBoundingFrames: boolean,
    looseTargetingForZeroSizedElements: 'strict' | 'loose',
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ): Array<{ elementPath: ElementPath; canBeFilteredOut: boolean }> {
    const looseReparentThreshold = 5
    const targetFilters = Canvas.targetFilter(selectedViews, searchTypes)
    const framesWithPaths = Canvas.getFramesInCanvasContext(
      allElementProps,
      componentMetadata,
      elementPathTree,
      useBoundingFrames,
    )
    const filteredFrames = framesWithPaths.filter((frameWithPath) => {
      const shouldUseLooseTargeting =
        looseTargetingForZeroSizedElements === 'loose' &&
        (frameWithPath.frame.width <= 0 || frameWithPath.frame.height <= 0)

      if (
        hiddenInstances.some((hidden) => EP.isDescendantOfOrEqualTo(frameWithPath.path, hidden))
      ) {
        return false
      }
      return targetFilters.some((filter) => filter(frameWithPath.path)) && shouldUseLooseTargeting
        ? rectangleIntersection(
            canvasRectangle({
              x: frameWithPath.frame.x,
              y: frameWithPath.frame.y,
              width: frameWithPath.frame.width === 0 ? 1 : frameWithPath.frame.width,
              height: frameWithPath.frame.height === 0 ? 1 : frameWithPath.frame.height,
            }),
            canvasRectangle({
              x: canvasArea.x - looseReparentThreshold,
              y: canvasArea.y - looseReparentThreshold,
              width: canvasArea.width + 2 * looseReparentThreshold,
              height: canvasArea.height + 2 * looseReparentThreshold,
            }),
          ) != null
        : rectangleIntersection(frameWithPath.frame, canvasArea)
    })
    filteredFrames.reverse()

    const targets = filteredFrames.map((filteredFrame) => {
      const zeroSized = filteredFrame.frame.width === 0 || filteredFrame.frame.height === 0
      return {
        elementPath: filteredFrame.path,
        canBeFilteredOut: !zeroSized,
      }
    })
    return targets
  },
  getNextTarget(
    componentMetadata: ElementInstanceMetadataMap,
    elementPathTree: ElementPathTrees,
    lockedElements: LockedElements,
    current: Array<ElementPath>,
    targetStack: Array<ElementPath>,
  ): ElementPath | null {
    const allLockedElementPaths = getAllLockedElementPaths(
      componentMetadata,
      elementPathTree,
      lockedElements,
    )
    const filteredTargetStack = targetStack.filter((stackEntry) => {
      return !allLockedElementPaths.some((lockedPath) => EP.pathsEqual(lockedPath, stackEntry))
    })
    if (filteredTargetStack.length > 0) {
      if (current.length <= 1) {
        const currentIndex =
          current.length === 0
            ? -1
            : filteredTargetStack.findIndex((target) => EP.pathsEqual(target, current[0]))
        const endOrNotFound = currentIndex === -1 || currentIndex === filteredTargetStack.length - 1
        if (endOrNotFound) {
          return filteredTargetStack[0]
        } else {
          return filteredTargetStack[currentIndex + 1]
        }
      }
    }
    return null
  },
  handleKeyUp(key: KeyCharacter, editor: EditorState, derived: DerivedState): Array<EditorAction> {
    switch (key) {
      default:
        return []
    }
  },
}

export default Canvas
