import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { KeyCharacter } from '../../utils/keyboard'
import Utils from '../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  rectangleIntersection,
  canvasRectangle,
  isInfinityRectangle,
} from '../../core/shared/math-utils'
import { EditorAction } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { AllElementProps, DerivedState, EditorState } from '../editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import { buildTree, ElementPathTree, getSubTree } from '../../core/shared/element-path-tree'
import { objectValues } from '../../core/shared/object-utils'
import { fastForEach } from '../../core/shared/utils'
import { memoize } from '../../core/shared/memoize'
import { maybeToArray } from '../../core/shared/optional-utils'
import { isFeatureEnabled } from '../../utils/feature-switches'

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
  useBoundingFrames: boolean,
): Array<FrameWithPath> {
  // Note: This will not necessarily be representative of the structured ordering in
  // the code that produced these elements.
  const projectTree = buildTree(objectValues(metadata).map((m) => m.elementPath))

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
    const globalFrame = component.globalFrame
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
    fastForEach(Object.values(componentTree.children), (childTree) => {
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

  const storyboardChildren = MetadataUtils.getAllStoryboardChildrenPathsUnordered(metadata)
  return storyboardChildren.flatMap((storyboardChild) => {
    const subTree = getSubTree(projectTree, storyboardChild)
    if (subTree == null) {
      return []
    } else {
      return recurseChildren(subTree).frames
    }
  })
}

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
  ): ElementPath | 'CLEAR' | null {
    switch (selectedViews.length) {
      case 0:
        // Nothing is selected, so do nothing.
        return null
      case 1:
        // Only a single element is selected...
        const parentPath = EP.parentPath(selectedViews[0])
        if (parentPath == null) {
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
    forwards: boolean,
  ): ElementPath | null {
    switch (selectedViews.length) {
      case 0:
        return null
      case 1:
        const singleSelectedElement = selectedViews[0]
        const siblings = MetadataUtils.getSiblingsUnordered(components, singleSelectedElement)
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
  ): ElementPath | null {
    if (selectedViews.length !== 1) {
      return null
    } else {
      const children = MetadataUtils.getImmediateChildrenUnordered(components, selectedViews[0])
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
          const _exhaustiveCheck: never = searchType
          throw new Error(`Unknown search type ${JSON.stringify(searchType)}`)
      }
    })
  },
  getAllTargetsAtPoint(
    componentMetadata: ElementInstanceMetadataMap,
    selectedViews: Array<ElementPath>,
    hiddenInstances: Array<ElementPath>,
    canvasPosition: CanvasPoint,
    searchTypes: Array<TargetSearchType>,
    useBoundingFrames: boolean,
    looseTargetingForZeroSizedElements: 'strict' | 'loose',
    allElementProps: AllElementProps,
  ): Array<{ elementPath: ElementPath; canBeFilteredOut: boolean }> {
    const looseReparentThreshold = 5
    const targetFilters = Canvas.targetFilter(selectedViews, searchTypes)
    const framesWithPaths = Canvas.getFramesInCanvasContext(
      allElementProps,
      componentMetadata,
      useBoundingFrames,
    )
    const filteredFrames = framesWithPaths.filter((frameWithPath) => {
      const shouldUseLooseTargeting =
        looseTargetingForZeroSizedElements === 'loose' &&
        (frameWithPath.frame.width <= 0 || frameWithPath.frame.height <= 0)

      return targetFilters.some((filter) => filter(frameWithPath.path)) &&
        !hiddenInstances.some((hidden) => EP.isDescendantOfOrEqualTo(frameWithPath.path, hidden)) &&
        shouldUseLooseTargeting
        ? rectangleIntersection(
            canvasRectangle({
              x: frameWithPath.frame.x,
              y: frameWithPath.frame.y,
              width: frameWithPath.frame.width === 0 ? 1 : frameWithPath.frame.width,
              height: frameWithPath.frame.height === 0 ? 1 : frameWithPath.frame.height,
            }),
            canvasRectangle({
              x: canvasPosition.x - looseReparentThreshold,
              y: canvasPosition.y - looseReparentThreshold,
              width: 2 * looseReparentThreshold,
              height: 2 * looseReparentThreshold,
            }),
          ) != null
        : Utils.rectContainsPoint(frameWithPath.frame, canvasPosition)
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
  getNextTarget(current: Array<ElementPath>, targetStack: Array<ElementPath>): ElementPath | null {
    if (current.length <= 1) {
      const currentIndex =
        current.length === 0
          ? -1
          : targetStack.findIndex((target) => EP.pathsEqual(target, current[0]))
      const endOrNotFound = currentIndex === -1 || currentIndex === targetStack.length - 1
      if (endOrNotFound) {
        return targetStack[0]
      } else {
        return targetStack[currentIndex + 1]
      }
    } else {
      return null
    }
  },
  handleKeyUp(key: KeyCharacter, editor: EditorState, derived: DerivedState): Array<EditorAction> {
    switch (key) {
      default:
        return []
    }
  },
}

export default Canvas
