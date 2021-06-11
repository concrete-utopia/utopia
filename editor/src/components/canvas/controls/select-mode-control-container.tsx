import * as React from 'react'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import { DuplicationState, RightMenuTab } from '../../editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import { CanvasPositions, MoveDragState, ResizeDragState } from '../canvas-types'
import { Guidelines, Guideline } from '../guideline'
import { ConstraintsControls } from './constraints-control'
import { DistanceGuideline } from './distance-guideline'
import { GuidelineControl } from './guideline-control'
import { collectParentAndSiblingGuidelines, getSnappedGuidelines } from './guideline-helpers'
import { ControlProps } from './new-canvas-controls'
import { ComponentLabelControl } from './component-area-control'
import { YogaControls } from './yoga-control'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { OutlineControls } from './outline-control'
import { RepositionableControl } from './repositionable-control'
import { areYogaChildren } from './select-mode/yoga-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { BoundingMarks } from './parent-bounding-marks'
import { getAllTargetsAtPoint } from '../dom-lookup'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isSceneAgainstImports, isSceneFromMetadata } from '../../../core/model/project-file-utils'
import { foldEither, isRight } from '../../../core/shared/either'

function getDistanceGuidelines(
  highlightedView: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

interface SelectModeControlContainerProps extends ControlProps {
  startDragStateAfterDragExceedsThreshold: (
    nativeEvent: MouseEvent,
    foundTarget: ElementPath,
  ) => void
  setSelectedViewsLocally: (newSelectedViews: Array<ElementPath>) => void
  keysPressed: KeysPressed
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  isDragging: boolean // set only when user already moves a cursor a little after a mousedown
  isResizing: boolean
  selectionEnabled: boolean
  draggingEnabled: boolean
  contextMenuEnabled: boolean
  maybeHighlightOnHover: (target: ElementPath) => void
  maybeClearHighlightsOnHoverEnd: () => void
  duplicationState: DuplicationState | null
  dragState: MoveDragState | ResizeDragState | null
}

interface SelectModeControlContainerState {
  moveGuidelines: Array<Guideline>
  lastHovered: ElementPath | null
}

// SelectModeControlContainer is a single React component containing all the individual
// SelectModeControl components (one for every utopia component).
export class SelectModeControlContainer extends React.Component<
  SelectModeControlContainerProps,
  SelectModeControlContainerState
> {
  // put local state that does not affect the render here instead of setState!
  hasReparented: boolean = false
  furthestDragDelta: CanvasVector | null = null
  dragDelta: CanvasVector = Utils.zeroPoint as CanvasVector
  previousDragPosition: CanvasPositions | null = null

  currentlyReparenting: boolean = false

  constructor(props: SelectModeControlContainerProps) {
    super(props)
    this.state = {
      moveGuidelines: [],
      lastHovered: null,
    }
  }

  static getDerivedStateFromProps(
    props: SelectModeControlContainerProps,
    previousState: SelectModeControlContainerState,
  ): Partial<SelectModeControlContainerState> {
    const guidelines = collectParentAndSiblingGuidelines(
      props.componentMetadata,
      props.selectedViews,
    )
    return {
      moveGuidelines: keepDeepReferenceEqualityIfPossible(previousState.moveGuidelines, guidelines),
    }
  }

  selectComponent = (target: ElementPath, isMultiselect: boolean): Array<ElementPath> => {
    // TODO BALAZS Remove this and unify with select-mode-hooks
    if (this.props.selectedViews.some((view) => EP.pathsEqual(target, view))) {
      return this.props.selectedViews
    } else {
      let updatedSelection = [target]
      if (isMultiselect) {
        updatedSelection = EP.addPathIfMissing(target, this.props.selectedViews)
      }

      this.props.setSelectedViewsLocally(updatedSelection)

      /**
       * As of November 2020, we need two nested requestAnimationFrames here,
       * the first one is called almost immediately (before vsync),
       * the second one is properly called in the next frame
       */
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          let selectActions = [
            EditorActions.clearHighlightedViews(),
            EditorActions.selectComponents(updatedSelection, false),
            EditorActions.setRightMenuTab(RightMenuTab.Inspector),
          ]
          this.props.dispatch(selectActions, 'canvas')
        })
      })

      return updatedSelection
    }
  }

  onControlMouseDown = (
    _selectedViews: Array<ElementPath>,
    target: ElementPath,
    _start: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ): void => {
    if (this.props.draggingEnabled) {
      this.props.startDragStateAfterDragExceedsThreshold(originalEvent.nativeEvent, target)
    }
  }

  onContextMenu = (event: React.MouseEvent<HTMLDivElement>): void => {
    event.stopPropagation()
    event.preventDefault()
    if (this.props.contextMenuEnabled && this.props.selectedViews.length > 0) {
      this.props.dispatch(
        [EditorActions.showContextMenu('context-menu-canvas', event.nativeEvent)],
        'canvas',
      )
    }
  }

  getTargetViews(): Array<ElementPath> {
    return this.props.duplicationState == null
      ? this.props.selectedViews
      : this.props.duplicationState.duplicateRoots.map((r) => r.currentTP)
  }

  // Highlights the parent of the actual selection. It does not highlight anything if not all the
  // components in the selection have the same parent.
  highlightSelectionParent(): EditorAction[] {
    if (this.props.selectedViews.length > 0) {
      const firstParent = EP.parentPath(this.props.selectedViews[0])
      if (firstParent != null) {
        const allSelectedViewsHasSameParent = this.props.selectedViews.every((et) =>
          EP.pathsEqual(EP.parentPath(et), firstParent),
        )
        if (allSelectedViewsHasSameParent) {
          return [EditorActions.setHighlightedView(firstParent)]
        }
      }
    }
    return []
  }

  resetState = (): void => {
    this.furthestDragDelta = null
    this.dragDelta = Utils.zeroPoint as CanvasVector
    this.previousDragPosition = null
    this.currentlyReparenting = false
  }

  isHighlighted = (path: ElementPath): boolean => {
    return this.props.highlightedViews.some((highlighted) => EP.pathsEqual(path, highlighted))
  }

  getClippedArea = (target: ElementPath): CanvasRectangle | null => {
    const targetFrame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)

    return EP.getAncestorsForLastPart(target).reduce(
      (frameIntersect: CanvasRectangle | null, current: ElementPath) => {
        const currentInstance = MetadataUtils.findElementByElementPath(
          this.props.componentMetadata,
          current,
        )
        if (currentInstance == null) {
          return frameIntersect
        } else {
          if (MetadataUtils.overflows(currentInstance)) {
            return frameIntersect
          } else {
            const currentFrame = currentInstance.globalFrame
            if (frameIntersect != null && currentFrame != null) {
              return Utils.rectangleIntersection(frameIntersect, currentFrame)
            } else {
              return null
            }
          }
        }
      },
      targetFrame,
    )
  }

  renderLabel = (target: ElementPath, hoverEnabled: boolean): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentLabelControl
        key={`${EP.toComponentId(target)}-label`}
        testID={`label-control-${EP.toComponentId(target)}`}
        mouseEnabled={true}
        componentMetadata={this.props.componentMetadata}
        target={target}
        frame={frame}
        scale={this.props.scale}
        highlighted={this.isHighlighted(target)}
        selectedComponents={this.props.selectedViews}
        dispatch={this.props.dispatch}
        canvasOffset={this.props.canvasOffset}
        hoverEffectEnabled={hoverEnabled}
        doubleClickToSelect={false}
        selectComponent={this.selectComponent}
        onMouseDown={this.onControlMouseDown}
        onHover={this.onHover}
        onHoverEnd={this.onHoverEnd}
        keysPressed={this.props.keysPressed}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        selectedViews={this.props.selectedViews}
        showAdditionalControls={this.props.showAdditionalControls}
      />
    )
  }

  clearSelection = (): void => {
    if (this.props.selectionEnabled) {
      this.props.dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }

  getSelectedBoundingBox = (views: ElementPath[]): CanvasRectangle | null => {
    let boundingRectangles: Array<CanvasRectangle> = []
    Utils.fastForEach(views, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
      if (frame != null) {
        boundingRectangles.push(frame)
      }
    })
    return Utils.boundingRectangleArray(boundingRectangles)
  }

  getMoveGuidelines = (): Array<JSX.Element> => {
    if (
      this.props.selectedViews.length > 0 &&
      this.props.isDragging &&
      this.props.draggingEnabled &&
      !areYogaChildren(this.props.componentMetadata, this.props.selectedViews) &&
      !this.props.keysPressed['cmd']
    ) {
      const draggedElements =
        this.props.dragState?.type === 'MOVE_DRAG_STATE' ? this.props.dragState.draggedElements : []
      const boundingBox = this.getSelectedBoundingBox(draggedElements)

      if (boundingBox == null) {
        return []
      }

      const closestGuideLines = getSnappedGuidelines(
        this.state.moveGuidelines,
        null,
        boundingBox,
        this.props.scale,
      )
      return closestGuideLines.map((g, i) => {
        return (
          <GuidelineControl
            key={
              (g.guideline.type === 'XAxisGuideline' ? 'x' + g.guideline.x : 'y' + g.guideline.y) +
              i
            }
            guidelineWithSnapping={g}
            targetFrame={boundingBox}
            canvasOffset={this.props.canvasOffset}
            scale={this.props.scale}
          />
        )
      })
    } else {
      return []
    }
  }

  onHover = (target: ElementPath): void => {
    if (this.inSelection(target)) {
      this.props.maybeClearHighlightsOnHoverEnd()
    } else {
      this.props.maybeHighlightOnHover(target)
    }
    this.setState({
      lastHovered: target,
    })
  }

  onHoverEnd = (_target: ElementPath): void => {
    if (this.props.selectionEnabled && !this.props.isDragging) {
      this.props.dispatch([EditorActions.clearHighlightedViews()], 'canvas')
    }
    this.setState({
      lastHovered: null,
    })
  }

  getDistanceGuidelines = (): Array<JSX.Element> => {
    if (
      this.props.selectedViews.length > 0 &&
      !this.props.isDragging &&
      this.props.keysPressed['alt']
    ) {
      let boundingBoxes: (CanvasRectangle | null)[] = []
      if (EP.areAllElementsInSameInstance(this.props.selectedViews)) {
        boundingBoxes = [this.getSelectedBoundingBox(this.props.selectedViews)]
      } else {
        boundingBoxes = this.props.selectedViews.map((view) => this.getSelectedBoundingBox([view]))
      }

      if (boundingBoxes.length < 1) {
        return []
      }
      let hoveredSelectedItem: ElementPath | null = null
      Utils.fastForEach(this.props.selectedViews, (selectedView) => {
        if (EP.pathsEqual(selectedView, this.state.lastHovered)) {
          if (
            hoveredSelectedItem == null ||
            EP.depth(hoveredSelectedItem) < EP.depth(selectedView)
          ) {
            hoveredSelectedItem = selectedView
          }
        }
      })

      let guideLineElements: Array<JSX.Element> = []
      Utils.fastForEach(boundingBoxes, (boundingBox, index) => {
        if (boundingBox != null) {
          let distanceGuidelines: Array<Guideline> = []
          if (hoveredSelectedItem == null) {
            distanceGuidelines = Utils.flatMapArray((highlightedView) => {
              const highlightedViewIsSelected = this.props.selectedViews.some((selectedView) =>
                EP.pathsEqual(selectedView, highlightedView),
              )
              if (highlightedViewIsSelected) {
                return []
              } else {
                if (EP.isFromSameInstanceAs(highlightedView, this.props.selectedViews[index])) {
                  return getDistanceGuidelines(highlightedView, this.props.componentMetadata)
                } else {
                  return []
                }
              }
            }, this.props.highlightedViews)
          } else {
            const parentPath = EP.parentPath(hoveredSelectedItem)
            if (parentPath != null) {
              if (EP.isFromSameInstanceAs(parentPath, this.props.selectedViews[index])) {
                distanceGuidelines = getDistanceGuidelines(parentPath, this.props.componentMetadata)
              }
            }
          }
          guideLineElements.push(
            <DistanceGuideline
              key={`${EP.toComponentId(this.props.selectedViews[index])}-distance-guidelines`}
              canvasOffset={this.props.canvasOffset}
              scale={this.props.scale}
              guidelines={distanceGuidelines}
              selectedViews={this.props.selectedViews}
              highlightedViews={this.props.highlightedViews}
              boundingBox={boundingBox}
            />,
          )
        }
      })

      return guideLineElements
    } else {
      return []
    }
  }

  getBoundingMarks = (): Array<JSX.Element> => {
    let boundingMarks: Array<JSX.Element> = []
    if (this.props.isDragging || this.props.keysPressed['alt']) {
      const targets = this.props.selectedViews
      if (targets.length > 0) {
        const targetInstance = MetadataUtils.findElementByElementPath(
          this.props.componentMetadata,
          targets[0],
        )
        if (targetInstance != null) {
          const { specialSizeMeasurements } = targetInstance

          const immediateParentBounds = specialSizeMeasurements.immediateParentBounds
          if (
            !specialSizeMeasurements.immediateParentProvidesLayout &&
            immediateParentBounds != null
          ) {
            boundingMarks.push(
              <BoundingMarks
                key={`${EP.toComponentId(targets[0])}-bounding-marks-immediate-parent`}
                canvasOffset={this.props.canvasOffset}
                scale={this.props.scale}
                rect={immediateParentBounds}
                boundsType='immediateParent'
              />,
            )
          }

          const coordinateSystemBounds = MetadataUtils.isPositionAbsolute(targetInstance)
            ? specialSizeMeasurements.coordinateSystemBounds
            : specialSizeMeasurements.immediateParentBounds
          if (coordinateSystemBounds != null) {
            boundingMarks.push(
              <BoundingMarks
                key={`${EP.toComponentId(targets[0])}-bounding-marks-coordinate-system`}
                canvasOffset={this.props.canvasOffset}
                scale={this.props.scale}
                rect={coordinateSystemBounds}
                boundsType='coordinateSystem'
              />,
            )
          }
        }
      }
    }
    return boundingMarks
  }

  inSelection(tp: ElementPath): boolean {
    return this.props.selectedViews.some((et) => EP.pathsEqual(et, tp))
  }

  canResizeElements(): boolean {
    return this.props.draggingEnabled
  }

  render(): JSX.Element {
    const cmdPressed = this.props.keysPressed['cmd'] || false
    const allElementsDirectlySelectable = cmdPressed && !this.props.isDragging
    const storyboardChildren = MetadataUtils.getAllStoryboardChildren(this.props.componentMetadata)
    const roots = mapDropNulls((child) => {
      return foldEither(
        () => null, // TODO do we still need to explicitly return null if child.element is Left?
        () => {
          if (isSceneFromMetadata(child)) {
            return child.elementPath
          } else {
            return null
          }
        },
        child.element,
      )
    }, storyboardChildren)
    let labelDirectlySelectable = this.props.highlightsEnabled

    // TODO future span element should be included here
    let repositionOnly = false
    if (this.props.selectedViews.length === 1) {
      const path = this.props.selectedViews[0]
      const element = MetadataUtils.findElementByElementPath(this.props.componentMetadata, path)
      repositionOnly = element != null && MetadataUtils.isAutoSizingText(element)
    }

    return (
      <div
        data-testid='select-mode-control-container-root'
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: 0,
          top: 0,
          right: 0,
          bottom: 0,
        }}
        onContextMenu={this.onContextMenu}
      >
        {roots.map((root) => {
          return (
            <React.Fragment key={`${EP.toComponentId(root)}}-root-controls`}>
              {this.renderLabel(root, allElementsDirectlySelectable || labelDirectlySelectable)}
            </React.Fragment>
          )
        })}
        {this.props.selectionEnabled ? (
          <>
            <OutlineControls {...this.props} />
            {this.canResizeElements() ? (
              repositionOnly ? (
                <RepositionableControl {...this.props} />
              ) : (
                <>
                  <ConstraintsControls {...this.props} />
                  <YogaControls
                    {...this.props}
                    dragState={
                      this.props.dragState != null &&
                      this.props.dragState.type === 'RESIZE_DRAG_STATE'
                        ? this.props.dragState
                        : null
                    }
                  />
                </>
              )
            ) : null}
          </>
        ) : null}
        {...this.getMoveGuidelines()}
        {this.getDistanceGuidelines()}
        {this.getBoundingMarks()}
      </div>
    )
  }
}
