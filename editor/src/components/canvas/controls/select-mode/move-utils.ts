import * as R from 'ramda'
import {
  findJSXElementAtPath,
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import {
  ComponentMetadata,
  ElementInstanceMetadata,
  isUtopiaJSXComponent,
  jsxAttributeValue,
} from '../../../../core/shared/element-template'
import {
  InstancePath,
  ParseSuccess,
  TemplatePath,
} from '../../../../core/shared/project-file-types'
import Utils from '../../../../utils/utils'
import { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { EditorAction, EditorDispatch } from '../../../editor/action-types'
import * as EditorActions from '../../../editor/actions/actions'
import { setCanvasFrames } from '../../../editor/actions/actions'
import {
  DerivedState,
  EditorState,
  getOpenFile,
  getOpenUIJSFile,
} from '../../../editor/store/editor-state'
import * as TP from '../../../../core/shared/template-path'
import {
  CanvasFrameAndTarget,
  flexAlignChange,
  FlexAlignChange,
  flexMoveChange,
  FlexMoveChange,
  flexResizeChange,
  moveTranslateChange,
  MoveTranslateChange,
  pinFrameChange,
  PinMoveChange,
  pinMoveChange,
  ReorderChange,
  reorderChange,
} from '../../canvas-types'
import Canvas, { TargetSearchType } from '../../canvas'
import { ConstrainedDragAxis, Guideline, Guidelines } from '../../guideline'
import { getSnapDelta } from '../guideline-helpers'
import { getNewIndex } from './yoga-utils'
import { flatMapArray } from '../../../../core/shared/array-utils'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { FlexAlignment } from 'utopia-api'
import { getUtopiaJSXComponentsFromSuccess } from '../../../../core/model/project-file-utils'
import { eitherToMaybe, isRight, right } from '../../../../core/shared/either'
import { createLayoutPropertyPath } from '../../../../core/layout/layout-helpers-new'

function determineConstrainedDragAxis(dragDelta: CanvasVector): 'x' | 'y' {
  if (Math.abs(dragDelta.x) > Math.abs(dragDelta.y)) {
    return 'x'
  } else {
    return 'y'
  }
}

export function extendSelectedViewsForInteraction(
  selectedViews: Array<TemplatePath>,
  componentMetadata: ComponentMetadata[],
): Array<TemplatePath> {
  return Utils.flatMapArray((view) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(view, componentMetadata)
    if (frame == null) {
      // What's the deal here? Why are we checking if the thing has a global frame but then not using it?
      return []
    } else {
      // autoSizing views are the new groups
      if (MetadataUtils.isAutoSizingViewFromComponents(componentMetadata, view)) {
        return MetadataUtils.getChildrenHandlingGroups(componentMetadata, view, false).map(
          (component) => component.templatePath,
        )
      } else {
        return [view]
      }
    }
  }, selectedViews)
}

export function determineElementsToOperateOnForDragging(
  selectedViews: Array<TemplatePath>,
  componentMetadata: ComponentMetadata[],
  isMoving: boolean,
  isAnchor: boolean,
): Array<TemplatePath> {
  if (isMoving) {
    // Moving.
    return extendSelectedViewsForInteraction(
      selectedViews.filter((view) =>
        R.none((otherView) => {
          return !TP.pathsEqual(view, otherView) && TP.isAncestorOf(view, otherView)
        }, selectedViews),
      ),
      componentMetadata,
    ).map((view) => {
      const parentPath = TP.parentPath(view)
      if (parentPath != null && TP.isScenePath(parentPath)) {
        const scene = MetadataUtils.findSceneByTemplatePath(componentMetadata, parentPath)
        if (MetadataUtils.isSceneTreatedAsGroup(scene)) {
          return parentPath
        } else {
          return view
        }
      } else {
        return view
      }
    })
  } else {
    // Resizing.
    return flatMapArray<TemplatePath, TemplatePath>((view) => {
      if (TP.isScenePath(view)) {
        const scene = MetadataUtils.findSceneByTemplatePath(componentMetadata, view)
        if (scene != null && MetadataUtils.isSceneTreatedAsGroup(scene)) {
          return scene.rootElements.map((e) => e.templatePath)
        } else {
          return [view]
        }
      } else {
        return [view]
      }
    }, extendSelectedViewsForInteraction(selectedViews, componentMetadata))
  }
}

export function dragComponent(
  componentsMetadata: ComponentMetadata[],
  selectedViews: Array<TemplatePath>,
  originalFrames: Array<CanvasFrameAndTarget>,
  moveGuidelines: Array<Guideline>,
  dragSelectionBoundingBox: CanvasRectangle | null,
  furthestDragDelta: CanvasVector | null,
  dragDelta: CanvasVector,
  enableSnapping: boolean,
  constrainDragAxis: boolean,
  scale: number,
  translateMode: boolean,
  dragStart: CanvasPoint,
  editor: EditorState,
  dispatch: EditorDispatch | null,
  derivedState: DerivedState | null,
): Array<PinMoveChange | FlexMoveChange | MoveTranslateChange | FlexAlignChange | ReorderChange> {
  const roundedDragDelta = Utils.roundPointTo(dragDelta, 0)
  // TODO: Probably makes more sense to pull this out.
  const viewsToOperateOn = determineElementsToOperateOnForDragging(
    selectedViews,
    componentsMetadata,
    true,
    false,
  )
  let dragChanges: Array<
    PinMoveChange | FlexMoveChange | FlexAlignChange | MoveTranslateChange | ReorderChange
  > = []
  Utils.fastForEach(viewsToOperateOn, (view) => {
    const parentPath = TP.parentPath(view)
    const isFlexContainer = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      view,
      componentsMetadata,
    )
    const originalFrame = originalFrames.find((frame) => TP.pathsEqual(frame.target, view))
    if (originalFrame == null) {
      // found a target with no original frame
      return
    }
    const isNotTranslateMode = isFeatureEnabled('Toolbar For Controls') ? !translateMode : true
    if (isFlexContainer && isNotTranslateMode) {
      if (originalFrame.frame != null) {
        const flexDirection = MetadataUtils.getYogaDirection(
          MetadataUtils.getParent(componentsMetadata, view),
        )
        const draggedFrame = Utils.offsetRect(originalFrame.frame, dragDelta)
        const newIndex = getNewIndex(
          componentsMetadata,
          view,
          parentPath,
          flexDirection,
          draggedFrame,
        )
        if (newIndex != null) {
          const currentIndex = MetadataUtils.getViewZIndexFromMetadata(componentsMetadata, view)
          const beforeOrAfter = currentIndex < newIndex ? 'after' : 'before'
          dragChanges.push(flexMoveChange(view, newIndex, beforeOrAfter))
        } else {
          if (parentPath != null && isFeatureEnabled('Flex Properties (Timer)')) {
            const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, componentsMetadata)
            let currentAlignment: FlexAlignment | null = null
            const openFile = getOpenUIJSFile(editor)
            if (TP.isInstancePath(view) && openFile != null && isRight(openFile.fileContents)) {
              const draggedJsxElements = derivedState?.canvas.transientState?.fileState?.topLevelElementsIncludingScenes?.filter(
                isUtopiaJSXComponent,
              )
              const jsxElements =
                draggedJsxElements != null
                  ? draggedJsxElements
                  : getUtopiaJSXComponentsFromSuccess(eitherToMaybe(openFile.fileContents)!)
              const element = findJSXElementAtPath(view, jsxElements, componentsMetadata)
              if (element != null) {
                currentAlignment = eitherToMaybe(
                  getSimpleAttributeAtPath(
                    right(element.props),
                    createLayoutPropertyPath('alignSelf'),
                  ),
                )
              }
            }
            let childElements: ElementInstanceMetadata[] | null = []
            if (TP.isInstancePath(parentPath)) {
              childElements =
                MetadataUtils.getElementByInstancePathMaybe(componentsMetadata, parentPath)
                  ?.children ?? null
            }
            if (parentFrame != null) {
              let newAlignment: FlexAlignment = FlexAlignment.Auto
              const draggedPoint = Utils.offsetPoint(dragStart, roundedDragDelta)
              switch (flexDirection) {
                case 'column': {
                  const draggedPointPlacedInParent =
                    (draggedPoint.x - parentFrame.x) / parentFrame.width
                  if (draggedPointPlacedInParent >= 0 && draggedPointPlacedInParent < 0.33) {
                    newAlignment = FlexAlignment.FlexStart
                  } else if (
                    draggedPointPlacedInParent >= 0.33 &&
                    draggedPointPlacedInParent < 0.66
                  ) {
                    newAlignment = FlexAlignment.Center
                  } else if (draggedPointPlacedInParent >= 0.66 && draggedPointPlacedInParent < 1) {
                    newAlignment = FlexAlignment.FlexEnd
                  }
                  break
                }
                case 'column-reverse': {
                  const draggedPointPlacedInParent =
                    (draggedPoint.x - parentFrame.x) / parentFrame.width
                  if (draggedPointPlacedInParent >= 0 && draggedPointPlacedInParent < 0.33) {
                    newAlignment = FlexAlignment.FlexEnd
                  } else if (
                    draggedPointPlacedInParent >= 0.33 &&
                    draggedPointPlacedInParent < 0.66
                  ) {
                    newAlignment = FlexAlignment.Center
                  } else if (draggedPointPlacedInParent >= 0.66 && draggedPointPlacedInParent < 1) {
                    newAlignment = FlexAlignment.FlexStart
                  }
                  break
                }
                case 'row': {
                  const draggedPointPlacedInParent =
                    (draggedPoint.y - parentFrame.y) / parentFrame.height
                  if (draggedPointPlacedInParent >= 0 && draggedPointPlacedInParent < 0.33) {
                    newAlignment = FlexAlignment.FlexStart
                  } else if (
                    draggedPointPlacedInParent >= 0.33 &&
                    draggedPointPlacedInParent < 0.66
                  ) {
                    newAlignment = FlexAlignment.Center
                  } else if (draggedPointPlacedInParent >= 0.66 && draggedPointPlacedInParent < 1) {
                    newAlignment = FlexAlignment.FlexEnd
                  }
                  break
                }
                case 'row-reverse': {
                  const draggedPointPlacedInParent =
                    (draggedPoint.y - parentFrame.y) / parentFrame.height
                  if (draggedPointPlacedInParent >= 0 && draggedPointPlacedInParent < 0.33) {
                    newAlignment = FlexAlignment.FlexEnd
                  } else if (
                    draggedPointPlacedInParent >= 0.33 &&
                    draggedPointPlacedInParent < 0.66
                  ) {
                    newAlignment = FlexAlignment.Center
                  } else if (draggedPointPlacedInParent >= 0.66 && draggedPointPlacedInParent < 1) {
                    newAlignment = FlexAlignment.FlexStart
                  }
                  break
                }
              }
              dragChanges.push(flexAlignChange(view, newAlignment))
              if (currentAlignment != newAlignment) {
                dragChanges.push(flexAlignChange(view, newAlignment))
                clearTimeout((window as any)['flexParentTimer'])
                clearTimeout((window as any)['flexParentHighlightTimer'])
                ;(window as any)['flexParentTimer'] = null
                ;(window as any)['flexParentHighlightTimer'] = null
                if (dispatch != null) dispatch([EditorActions.clearHighlightedViews()], 'canvas')
              } else {
                if ((window as any)['flexParentTimer'] == null) {
                  const flexParentTimer = setTimeout(() => {
                    if (
                      dispatch != null &&
                      parentPath != null &&
                      TP.isInstancePath(parentPath) &&
                      (window as any)['flexAlignmentDrag'] === newAlignment
                    ) {
                      let actions: EditorAction[] = [
                        EditorActions.setProp_UNSAFE(
                          parentPath,
                          createLayoutPropertyPath('alignItems'),
                          jsxAttributeValue(newAlignment),
                        ),
                        EditorActions.clearHighlightedViews(),
                      ]
                      Utils.fastForEach(childElements ?? [], (element) => {
                        actions.push(
                          EditorActions.unsetProperty(
                            element.templatePath,
                            createLayoutPropertyPath('alignSelf'),
                          ),
                        )
                      })
                      dispatch(actions, 'canvas')
                    } else if (dispatch != null) {
                      dispatch([EditorActions.clearHighlightedViews()], 'canvas')
                    }

                    clearTimeout((window as any)['flexParentHighlightTimer'])
                    ;(window as any)['flexParentTimer'] = null
                    ;(window as any)['flexParentHighlightTimer'] = null
                  }, 2000)

                  const flexParentHighlightTimer = setTimeout(() => {
                    if (dispatch != null && (window as any)['flexParentTimer'] != null) {
                      dispatch([EditorActions.setHighlightedView(parentPath)], 'canvas')
                    }
                  }, 1500)

                  ;(window as any)['flexParentTimer'] = flexParentTimer
                  ;(window as any)['flexParentHighlightTimer'] = flexParentHighlightTimer
                }
              }
              ;(window as any)['flexAlignmentDrag'] = newAlignment
            }
          }
        }
      }
    } else {
      // TODO determine if node graph affects the drag
      const element = MetadataUtils.getElementByTemplatePathMaybe(componentsMetadata, view)
      const isFlow =
        TP.isInstancePath(view) &&
        element?.specialSizeMeasurements.immediateParentProvidesLayout === false

      const constrainedDragAxis: ConstrainedDragAxis | null =
        constrainDragAxis && furthestDragDelta != null
          ? determineConstrainedDragAxis(furthestDragDelta)
          : null
      const snapDelta = enableSnapping
        ? getSnapDelta(
            moveGuidelines,
            constrainedDragAxis,
            Utils.offsetRect(
              Utils.defaultIfNull(Utils.zeroRectangle as CanvasRectangle, dragSelectionBoundingBox),
              roundedDragDelta,
            ),
            scale,
          )
        : (Utils.zeroPoint as CanvasPoint)

      const dragDeltaToApply = Guidelines.applyDirectionConstraint(
        constrainedDragAxis,
        Utils.offsetPoint(roundedDragDelta, snapDelta),
      )
      if (originalFrame.frame != null) {
        if (translateMode) {
          dragChanges.push(moveTranslateChange(view, dragDeltaToApply))
        } else if (isFlow && isFeatureEnabled('Flow Resize')) {
          const cursorPoint = Utils.offsetPoint(dragStart, dragDelta)
          const targetsUnderCursor = Canvas.getAllTargetsAtPoint(
            editor,
            cursorPoint,
            [TargetSearchType.SiblingsOfSelected],
            false,
            'strict',
          )
          const flowTarget = targetsUnderCursor.find(
            (target) =>
              MetadataUtils.getElementByTemplatePathMaybe(componentsMetadata, target)
                ?.specialSizeMeasurements.immediateParentProvidesLayout === false,
          )
          if (flowTarget != null) {
            const newIndex = MetadataUtils.getViewZIndexFromMetadata(componentsMetadata, flowTarget)
            const currentIndex = MetadataUtils.getViewZIndexFromMetadata(componentsMetadata, view)
            const beforeOrAfter = currentIndex < newIndex ? 'after' : 'before'
            dragChanges.push(reorderChange(view, newIndex, beforeOrAfter))
          }
        } else {
          dragChanges.push(pinMoveChange(view, dragDeltaToApply))
        }
      }
    }
  })
  return dragChanges
}

export function dragComponentForActions(
  componentsMetadata: ComponentMetadata[],
  selectedViews: Array<TemplatePath>,
  originalFrames: Array<CanvasFrameAndTarget>,
  moveGuidelines: Array<Guideline>,
  dragSelectionBoundingBox: CanvasRectangle,
  furthestDragDelta: CanvasVector | null,
  dragDelta: CanvasVector,
  enableSnapping: boolean,
  constrainDragAxis: boolean,
  scale: number,
  translateMode: boolean,
  start: CanvasPoint,
  editor: EditorState,
): Array<EditorAction> {
  const frameAndTargets = dragComponent(
    componentsMetadata,
    selectedViews,
    originalFrames,
    moveGuidelines,
    dragSelectionBoundingBox,
    furthestDragDelta,
    dragDelta,
    enableSnapping,
    constrainDragAxis,
    scale,
    translateMode,
    start,
    editor,
    null,
    null,
  )
  return [setCanvasFrames(frameAndTargets, false)]
}

export function adjustAllSelectedFrames(
  editor: EditorState,
  dispatch: EditorDispatch,
  keepChildrenAtPlace: boolean,
  isResizing: boolean,
  directionModifier: -1 | 1,
  direction: 'vertical' | 'horizontal',
  adjustment: 1 | 10,
): Array<EditorAction> {
  if (
    editor.selectedViews.length === 0 ||
    (editor.selectedViews.some((view) => {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        view,
        editor.jsxMetadataKILLME,
      )
    }) &&
      !isResizing)
  ) {
    // if any of the selected views have a Yoga parent, we bail out
    return []
  }
  const selectedFrames = editor.selectedViews.map((view) =>
    MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadataKILLME),
  )

  const boundingBox = Utils.boundingRectangleArray(selectedFrames)

  if (boundingBox == null) {
    // none of the selected elements are layoutable, return early
    return []
  }

  let actions: Array<EditorAction> = []
  if (isResizing) {
    let roundedAdjustment: number = 0
    let newBoundingBox: CanvasRectangle
    if (direction === 'vertical') {
      roundedAdjustment = getRoundedAdjustment(
        boundingBox.y + boundingBox.height,
        adjustment,
        directionModifier,
      )
      newBoundingBox = Utils.combineRectangles(boundingBox, {
        x: 0,
        y: 0,
        width: 0,
        height: roundedAdjustment,
      } as CanvasRectangle)
    } else {
      roundedAdjustment = getRoundedAdjustment(
        boundingBox.x + boundingBox.width,
        adjustment,
        directionModifier,
      )
      newBoundingBox = Utils.combineRectangles(boundingBox, {
        x: 0,
        y: 0,
        width: roundedAdjustment,
        height: 0,
      } as CanvasRectangle)
    }
    const newFrameAndTargets = Utils.stripNulls(
      editor.selectedViews.map((view) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadataKILLME)
        if (frame == null) {
          return null
        } else {
          const newFrame = Utils.transformFrameUsingBoundingBox(newBoundingBox, boundingBox, frame)
          const hasFlexParent = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
            view,
            editor.jsxMetadataKILLME,
          )
          if (hasFlexParent) {
            return flexResizeChange(view, 'FlexFlexBasis', adjustment * directionModifier)
          } else {
            return pinFrameChange(view, newFrame)
          }
        }
      }),
    )
    actions = [EditorActions.setCanvasFrames(newFrameAndTargets, keepChildrenAtPlace)]
  } else {
    const originalFrames: CanvasFrameAndTarget[] = Utils.stripNulls(
      editor.selectedViews.map((view) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(view, editor.jsxMetadataKILLME)
        if (frame == null) {
          return null
        }
        return {
          target: view,
          frame: frame,
        }
      }),
    )

    const delta = getRoundedDeltaForKeyboardShortcut(
      boundingBox,
      direction,
      directionModifier,
      adjustment,
    )
    EditorActions.hideAndShowSelectionControls(dispatch)
    actions = dragComponentForActions(
      editor.jsxMetadataKILLME,
      editor.selectedViews,
      originalFrames,
      [],
      boundingBox,
      delta,
      delta,
      false,
      false,
      editor.canvas.scale,
      false,
      Utils.zeroPoint as CanvasPoint,
      editor,
    )
  }

  actions.push(EditorActions.startCheckpointTimer())
  return actions
}

function getRoundedDeltaForKeyboardShortcut(
  boundingBox: CanvasRectangle,
  side: 'vertical' | 'horizontal',
  directionModifier: -1 | 1,
  adjustment: 1 | 10,
): CanvasPoint {
  if (side === 'vertical') {
    return {
      x: 0,
      y: getRoundedAdjustment(boundingBox.y, adjustment, directionModifier),
    } as CanvasPoint
  } else {
    return {
      x: getRoundedAdjustment(boundingBox.x, adjustment, directionModifier),
      y: 0,
    } as CanvasPoint
  }
}

function getRoundedAdjustment(n: number, adjustment: 10 | 1, directionModifier: -1 | 1): number {
  let roundDelta = Utils.roundTo(n, 0) - n

  if (roundDelta === 0) {
    // default case, no need to round
    return adjustment * directionModifier
  } else if (roundDelta * directionModifier < 0) {
    // when the rounding and frameshift happens in opposite direction
    return (adjustment - Math.abs(roundDelta)) * directionModifier
  } else {
    return (adjustment + Math.abs(roundDelta)) * directionModifier
  }
}
