import * as R from 'ramda'
import * as React from 'react'
import { KeysPressed } from '../../../utils/keyboard'
import Utils from '../../../utils/utils'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  rectanglesEqual,
} from '../../../core/shared/math-utils'
import { TemplatePath, ScenePath } from '../../../core/shared/project-file-types'
import { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/actions'
import { DuplicationState } from '../../editor/store/editor-state'
import * as TP from '../../../core/shared/template-path'
import {
  CanvasPositions,
  MoveDragState,
  ResizeDragState,
  moveDragState,
  ReparentTargetIndicatorPosition,
} from '../canvas-types'
import { Guidelines, Guideline } from '../guideline'
import { ConstraintsControls } from './constraints-control'
import { DistanceGuideline } from './distance-guideline'
import { GuidelineControl } from './guideline-control'
import { collectParentAndSiblingGuidelines, getSnappedGuidelines } from './guideline-helpers'
import { ControlProps, SelectModeState } from './new-canvas-controls'
import { ComponentAreaControl, ComponentLabelControl } from './component-area-control'
import { YogaControls } from './yoga-control'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { OutlineControls } from './outline-control'
import { RepositionableControl } from './repositionable-control'
import { LeftMenuTab } from '../../navigator/left-pane'
import CanvasActions from '../canvas-actions'
import { getOriginalCanvasFrames, createDuplicationNewUIDs } from '../canvas-utils'
import { areYogaChildren } from './select-mode/yoga-utils'
import { ComponentMetadata } from '../../../core/shared/element-template'
import { BoundingMarks } from './parent-bounding-marks'
import { RightMenuTab } from '../right-menu'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { ParentControls } from './parent-controls'
import { fastForEach } from '../../../core/shared/utils'
import { flatMapArray, uniqBy } from '../../../core/shared/array-utils'
import { ReorderInsertIndicator } from './reorder-insert-indicator'

export const SnappingThreshold = 5

function getDistanceGuidelines(
  highlightedView: TemplatePath,
  componentMetadata: ComponentMetadata[],
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

interface SelectModeControlContainerProps extends ControlProps {
  keysPressed: KeysPressed
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  isDragging: boolean // set only when user already moves a cursor a little after a mousedown
  isResizing: boolean
  selectionEnabled: boolean
  maybeHighlightOnHover: (target: TemplatePath) => void
  maybeClearHighlightsOnHoverEnd: () => void
  duplicationState: DuplicationState | null
  dragState: MoveDragState | ResizeDragState | null
  layoutInspectorSectionHovered: boolean
  selectModeState: SelectModeState
  setSelectModeState: (newState: SelectModeState) => void
  xrayMode: boolean
  selectedScene: ScenePath | null
  reparentTargetPositions: Array<ReparentTargetIndicatorPosition>
}

interface SelectModeControlContainerState {
  moveGuidelines: Array<Guideline>
  lastHovered: TemplatePath | null
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
  ) {
    const guidelines = collectParentAndSiblingGuidelines(
      props.componentMetadata,
      props.selectedViews,
    )
    return {
      moveGuidelines: keepDeepReferenceEqualityIfPossible(previousState.moveGuidelines, guidelines),
    }
  }

  selectComponent = (target: TemplatePath, isMultiselect: boolean): Array<TemplatePath> => {
    if (this.props.selectedViews.some((view) => TP.pathsEqual(target, view))) {
      return this.props.selectedViews
    } else {
      let updatedSelection = [target]
      if (isMultiselect) {
        updatedSelection = TP.addPathIfMissing(target, this.props.selectedViews)
      }
      let selectActions = [
        EditorActions.clearHighlightedViews(),
        EditorActions.selectComponents(updatedSelection, false),
        EditorActions.setLeftMenuTab(LeftMenuTab.UINavigate),
        EditorActions.setRightMenuTab(RightMenuTab.Inspector),
      ]
      this.props.dispatch(selectActions, 'canvas')
      return updatedSelection
    }
  }

  onControlMouseDown = (
    selectedViews: Array<TemplatePath>,
    target: TemplatePath,
    start: CanvasPoint,
    originalEvent: React.MouseEvent<HTMLDivElement>,
  ) => {
    if (
      // Only on left mouse down.
      originalEvent.buttons === 1 &&
      !MetadataUtils.anyUnknownOrGeneratedElements(
        this.props.rootComponents,
        this.props.componentMetadata,
        selectedViews,
      )
    ) {
      if (this.props.xrayMode) {
        return
      }
      const selection = TP.areAllElementsInSameScene(selectedViews) ? selectedViews : [target]
      const moveTargets = selection.filter(
        (view) =>
          TP.isScenePath(view) ||
          this.props.elementsThatRespectLayout.some((path) => TP.pathsEqual(path, view)),
      )

      if (isFeatureEnabled('Toolbar For Controls') && this.props.selectModeState === 'resize') {
        // early exit
        return
      }

      // setting original frames
      if (moveTargets.length > 0) {
        let originalFrames = getOriginalCanvasFrames(moveTargets, this.props.componentMetadata)
        originalFrames = originalFrames.filter((f) => f.frame != null)
        const selectionArea = Utils.boundingRectangleArray(
          selectedViews.map((view) => {
            return MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
          }),
        )

        const duplicate = originalEvent.altKey
        const duplicateNewUIDs = duplicate
          ? createDuplicationNewUIDs(
              this.props.selectedViews,
              this.props.componentMetadata,
              this.props.rootComponents,
            )
          : null

        this.props.dispatch([
          CanvasActions.createDragState(
            moveDragState(
              start,
              null,
              null,
              originalFrames,
              selectionArea,
              !originalEvent.metaKey,
              originalEvent.shiftKey,
              duplicate,
              isFeatureEnabled('Toolbar For Controls')
                ? this.props.selectModeState === 'reparentGlobal'
                : false,
              isFeatureEnabled('Toolbar For Controls')
                ? this.props.selectModeState === 'reparentMove'
                : originalEvent.metaKey,
              isFeatureEnabled('Toolbar For Controls')
                ? this.props.selectModeState === 'reparentLocal'
                : false,
              duplicateNewUIDs,
              start,
              this.props.componentMetadata,
              moveTargets,
              isFeatureEnabled('Toolbar For Controls')
                ? this.props.selectModeState === 'translate'
                : false,
            ),
          ),
        ])
      }
    }
  }

  onContextMenu = (event: React.MouseEvent<HTMLDivElement>) => {
    event.stopPropagation()
    event.preventDefault()
    this.props.dispatch(
      [EditorActions.showContextMenu('context-menu-canvas', event.nativeEvent)],
      'canvas',
    )
  }

  getTargetViews(): Array<TemplatePath> {
    return this.props.duplicationState == null
      ? this.props.selectedViews
      : this.props.duplicationState.duplicateRoots.map((r) => r.currentTP)
  }

  // Highlights the parent of the actual selection. It does not highlight anything if not all the
  // components in the selection have the same parent.
  highlightSelectionParent(): EditorAction[] {
    if (this.props.selectedViews.length > 0) {
      const firstParent = TP.parentPath(this.props.selectedViews[0])
      if (firstParent != null) {
        const allSelectedViewsHasSameParent = this.props.selectedViews.every((et) =>
          TP.pathsEqual(TP.parentPath(et), firstParent),
        )
        if (allSelectedViewsHasSameParent) {
          return [EditorActions.setHighlightedView(firstParent)]
        }
      }
    }
    return []
  }

  resetState = () => {
    this.furthestDragDelta = null
    this.dragDelta = Utils.zeroPoint as CanvasVector
    this.previousDragPosition = null
    this.currentlyReparenting = false
  }

  filterHiddenInstances = (paths: Array<TemplatePath>): Array<TemplatePath> => {
    return paths.filter((path) =>
      this.props.hiddenInstances.every((hidden) => !TP.pathsEqual(path, hidden)),
    )
  }

  isHighlighted = (path: TemplatePath) => {
    return this.props.highlightedViews.some((highlighted) => TP.pathsEqual(path, highlighted))
  }

  getSelectableViews(allElementsDirectlySelectable: boolean): TemplatePath[] {
    let candidateViews: Array<TemplatePath>

    if (this.props.xrayMode) {
      candidateViews = MetadataUtils.getAllPaths(this.props.componentMetadata)
    } else if (allElementsDirectlySelectable) {
      candidateViews = MetadataUtils.getAllPaths(this.props.componentMetadata)
    } else {
      const scenes = MetadataUtils.getAllScenePaths(this.props.componentMetadata)
      let rootElementsToFilter: TemplatePath[] = []
      let dynamicScenesWithFragmentRootViews: ScenePath[] = []
      Utils.fastForEach(scenes, (path) => {
        const scene = MetadataUtils.findSceneByTemplatePath(this.props.componentMetadata, path)
        const rootElements = scene?.rootElements
        if (
          MetadataUtils.isSceneTreatedAsGroup(scene) &&
          rootElements != null &&
          rootElements.length > 1
        ) {
          rootElementsToFilter = [
            ...rootElementsToFilter,
            ...rootElements.map((element) => element.templatePath),
          ]
          dynamicScenesWithFragmentRootViews.push(path)
        }
      })
      const allRoots = MetadataUtils.getAllCanvasRootPaths(this.props.componentMetadata).filter(
        (rootPath) => {
          return !rootElementsToFilter.some((path) => TP.pathsEqual(rootPath, path))
        },
      )
      let siblings: Array<TemplatePath> = []
      Utils.fastForEach(this.props.selectedViews, (view) => {
        Utils.fastForEach(TP.allPaths(view), (ancestor) => {
          const ancestorChildren = MetadataUtils.getImmediateChildren(
            this.props.componentMetadata,
            ancestor,
          )

          siblings.push(...ancestorChildren.map((child) => child.templatePath))
        })
      })

      const selectableViews = [...dynamicScenesWithFragmentRootViews, ...allRoots, ...siblings]
      const uniqueSelectableViews = R.uniqWith<TemplatePath>(TP.pathsEqual, selectableViews)

      const selectableViewsFiltered = uniqueSelectableViews.filter((view) => {
        // I kept the group-like behavior here that the user can't single-click select the parent group, even though it is a view now
        const isGroup = MetadataUtils.isAutoSizingViewFromComponents(
          this.props.componentMetadata,
          view,
        )
        const isAncestorOfSelected = this.props.selectedViews.some((selectedView) =>
          TP.isAncestorOf(selectedView, view, false),
        )
        if (isGroup && isAncestorOfSelected) {
          return false
        } else {
          return true
        }
      })
      candidateViews = selectableViewsFiltered
    }

    return this.filterHiddenInstances(candidateViews)
  }

  layoutInfoLabel = (label: string, color: string, elementsUseThisLayout: number): JSX.Element => {
    return (
      <div
        style={{
          padding: '0px 8px',
          display: 'flex',
          alignItems: 'center',
          borderRadius: 6,
        }}
      >
        <div
          style={{
            padding: 2,
            display: 'flex',
            alignItems: 'center',
            borderRadius: 4,
            backgroundColor: elementsUseThisLayout > 0 ? '#e6e6e6' : 'inherit',
          }}
        >
          <div
            style={{
              display: 'inline-block',
              width: 6,
              height: 6,
              margin: 5,
              borderRadius: 6,
              backgroundColor: elementsUseThisLayout > 0 ? color : '#c2c2c2',
            }}
          />
          <span
            style={{
              padding: 2,
              fontWeight: elementsUseThisLayout > 0 ? 600 : 500,
            }}
          >
            {label}
          </span>
          {elementsUseThisLayout > 1 ? (
            <span
              style={{
                paddingRight: 2,
              }}
            >
              {elementsUseThisLayout}
            </span>
          ) : null}
        </div>
      </div>
    )
  }

  layoutInfo = (): JSX.Element | null => {
    if (this.props.selectedViews.length === 0) {
      return null
    }
    const elementLayouts = this.props.selectedViews.map((view) => {
      if (TP.isScenePath(view)) {
        return 'none'
      } else {
        const element = MetadataUtils.getElementByInstancePathMaybe(
          this.props.componentMetadata,
          view,
        )
        if (element?.specialSizeMeasurements.immediateParentProvidesLayout === false) {
          return 'flow'
        } else {
          return element?.specialSizeMeasurements.parentLayoutSystem
        }
      }
    })

    const flow = elementLayouts.filter((layout) => layout === 'flow' || layout === 'none').length
    const block = elementLayouts.filter((layout) => layout === 'nonfixed').length
    const flex = elementLayouts.filter((layout) => layout === 'flex').length
    const grid = elementLayouts.filter((layout) => layout === 'grid').length
    return (
      <div
        style={{
          position: 'absolute',
          bottom: 10,
          height: 25,
          marginLeft: 25,
          marginRight: 25,
          minWidth: 330,
          width: 'calc(100% - 50px)',
          backgroundColor: '#f9f9f9',
          borderRadius: 8,
          display: 'flex',
        }}
      >
        {this.layoutInfoLabel('Flow', 'hotpink', flow)}
        {this.layoutInfoLabel('Block', '#fa006c', block)}
        {this.layoutInfoLabel('Flex', '#32c5ff', flex)}
        {this.layoutInfoLabel('Grid', '#18c39e', grid)}
      </div>
    )
  }

  getOverlappingLabels = (targets: TemplatePath[]): any[] => {
    const allFrames = Utils.stripNulls(
      targets.map((target) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
        if (frame == null) {
          return null
        }
        return {
          frame: frame,
          target: target,
        }
      }),
    )
    let overlappingElements: { frame: CanvasRectangle; targets: TemplatePath[] }[] = []
    fastForEach(allFrames, (targetAndFrame) => {
      return allFrames.some((allFramesFrame) => {
        if (
          TP.isAncestorOf(allFramesFrame.target, targetAndFrame.target, false) &&
          rectanglesEqual(targetAndFrame.frame, allFramesFrame.frame)
        ) {
          const alreadyAdded = overlappingElements.findIndex((overlapData) =>
            rectanglesEqual(overlapData.frame, allFramesFrame.frame),
          )
          if (alreadyAdded > -1) {
            overlappingElements[alreadyAdded] = {
              frame: targetAndFrame.frame,
              targets: uniqBy(
                [
                  ...overlappingElements[alreadyAdded].targets,
                  targetAndFrame.target,
                  allFramesFrame.target,
                ],
                TP.pathsEqual,
              ),
            }
          } else {
            overlappingElements.push({
              frame: targetAndFrame.frame,
              targets: [targetAndFrame.target, allFramesFrame.target],
            })
          }
        }
      })
    })

    return flatMapArray((overlapData) => {
      const frame = overlapData.frame
      return overlapData.targets.map((target, index) => {
        const isSelected = this.props.selectedViews.some((view) => TP.pathsEqual(target, view))
        return (
          <div
            key={TP.toString(target)}
            style={{
              position: 'absolute',
              left: frame.x + this.props.canvasOffset.x + 30 * index,
              top: frame.y + this.props.canvasOffset.y - 12,
              backgroundColor: isSelected ? 'rgb(255,105,180,0.9)' : 'rgb(255,105,180,0.2)',
              border: '1px solid hotpink',
              paddingLeft: 2,
              fontSize: 8,
              width: 30,
              borderRadius: 2,
            }}
            onMouseDown={() => {
              this.props.dispatch([EditorActions.selectComponents([target], false)], 'canvas')
            }}
            onMouseOver={() => this.onHover(target)}
            onMouseLeave={() => this.onHoverEnd(target)}
          >
            {index + 1}
          </div>
        )
      })
    }, overlappingElements)
  }

  getClippedArea = (target: TemplatePath): CanvasRectangle | null => {
    const targetFrame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)

    return TP.getAncestors(target).reduce(
      (frameIntersect: CanvasRectangle | null, current: TemplatePath) => {
        if (TP.isScenePath(current)) {
          // TODO Scene Implementation - should scenes act like they clip?
          return frameIntersect
        }

        const currentInstance = MetadataUtils.getElementByInstancePathMaybe(
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

  renderControl = (
    target: TemplatePath,
    index: number,
    isChild: boolean,
    cmdIsPressed: boolean,
  ): JSX.Element | null => {
    const frame = this.getClippedArea(target)
    const siblingIsSelected =
      this.props.selectedViews.some((view) =>
        TP.pathsEqual(TP.parentPath(view), TP.parentPath(target)),
      ) && cmdIsPressed
    const parentIsSelectedAndFlex =
      this.props.selectedViews.some((view) => {
        return (
          TP.pathsEqual(TP.parentPath(target), view) &&
          TP.isInstancePath(view) &&
          MetadataUtils.isFlexLayoutedContainer(
            MetadataUtils.getElementByInstancePathMaybe(this.props.componentMetadata, view),
          )
        )
      }) && !cmdIsPressed
    const showSiblingIndex =
      isFeatureEnabled('Flex Sibling Numbers') && (siblingIsSelected || parentIsSelectedAndFlex)

    const siblingIndex = showSiblingIndex
      ? MetadataUtils.getViewZIndexFromMetadata(this.props.componentMetadata, target) + 1
      : null
    if (frame != null) {
      return (
        <ComponentAreaControl
          key={`${TP.toComponentId(target)}-${index}-control`}
          testID={`component-area-control-${TP.toComponentId(target)}-${index}`}
          componentMetadata={this.props.componentMetadata}
          target={target}
          frame={frame}
          scale={this.props.scale}
          highlighted={this.isHighlighted(target)}
          selectedComponents={this.props.selectedViews}
          dispatch={this.props.dispatch}
          canvasOffset={this.props.canvasOffset}
          hoverEffectEnabled={!isChild}
          doubleClickToSelect={isChild}
          selectComponent={this.selectComponent}
          onMouseDown={this.onControlMouseDown}
          onHover={this.onHover}
          onHoverEnd={this.onHoverEnd}
          keysPressed={this.props.keysPressed}
          windowToCanvasPosition={this.props.windowToCanvasPosition}
          selectedViews={this.props.selectedViews}
          imports={this.props.imports}
          showAdditionalControls={this.props.showAdditionalControls}
          siblingIndex={siblingIndex}
          xrayMode={this.props.xrayMode}
        />
      )
    } else {
      return null
    }
  }

  renderLabel = (target: TemplatePath, hoverEnabled: boolean): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentLabelControl
        key={`${TP.toComponentId(target)}-label`}
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
        imports={this.props.imports}
        showAdditionalControls={this.props.showAdditionalControls}
        xrayMode={this.props.xrayMode}
      />
    )
  }

  clearSelection = () => {
    if (this.props.selectionEnabled) {
      this.props.dispatch([EditorActions.clearSelection()], 'canvas')
    }
  }

  renderNonSelectableControl = (target: TemplatePath): JSX.Element | null => {
    const frame = MetadataUtils.getFrameInCanvasCoords(target, this.props.componentMetadata)
    if (frame == null) {
      return null
    }
    return (
      <ComponentAreaControl
        key={`${TP.toComponentId(target)}-non-selectable`}
        componentMetadata={this.props.componentMetadata}
        target={target}
        frame={frame}
        scale={this.props.scale}
        highlighted={this.isHighlighted(target)}
        selectedComponents={this.props.selectedViews}
        dispatch={this.props.dispatch}
        canvasOffset={this.props.canvasOffset}
        hoverEffectEnabled={true}
        doubleClickToSelect={false}
        onMouseDown={this.clearSelection}
        onHover={Utils.NO_OP}
        onHoverEnd={Utils.NO_OP}
        keysPressed={this.props.keysPressed}
        windowToCanvasPosition={this.props.windowToCanvasPosition}
        selectedViews={this.props.selectedViews}
        imports={this.props.imports}
        showAdditionalControls={this.props.showAdditionalControls}
        xrayMode={this.props.xrayMode}
      />
    )
  }

  getSelectedBoundingBox = (views: TemplatePath[]) => {
    let boundingRectangles: Array<CanvasRectangle> = []
    Utils.fastForEach(views, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, this.props.componentMetadata)
      if (frame != null) {
        boundingRectangles.push(frame)
      }
    })
    return Utils.boundingRectangleArray(boundingRectangles)
  }

  getMoveGuidelines = () => {
    if (
      this.props.selectedViews.length > 0 &&
      this.props.isDragging &&
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

  onHover = (target: TemplatePath) => {
    if (this.inSelection(target)) {
      this.props.maybeClearHighlightsOnHoverEnd()
    } else {
      this.props.maybeHighlightOnHover(target)
    }
    this.setState({
      lastHovered: target,
    })
  }

  onHoverEnd = (target: TemplatePath) => {
    if (this.props.selectionEnabled && !this.props.isDragging) {
      this.props.dispatch([EditorActions.clearHighlightedViews()], 'canvas')
    }
    this.setState({
      lastHovered: null,
    })
  }

  getDistanceGuidelines = () => {
    if (
      this.props.selectedViews.length > 0 &&
      !this.props.isDragging &&
      this.props.keysPressed['alt']
    ) {
      let boundingBoxes: (CanvasRectangle | null)[] = []
      if (TP.areAllElementsInSameScene(this.props.selectedViews)) {
        boundingBoxes = [this.getSelectedBoundingBox(this.props.selectedViews)]
      } else {
        boundingBoxes = this.props.selectedViews.map((view) => this.getSelectedBoundingBox([view]))
      }

      if (boundingBoxes.length < 1) {
        return []
      }
      let hoveredSelectedItem: TemplatePath | null = null
      Utils.fastForEach(this.props.selectedViews, (selectedView) => {
        if (TP.pathsEqual(selectedView, this.state.lastHovered)) {
          if (
            hoveredSelectedItem == null ||
            TP.depth(hoveredSelectedItem) < TP.depth(selectedView)
          ) {
            hoveredSelectedItem = selectedView
          }
        }
      })

      let guideLineElements: any[] = []
      Utils.fastForEach(boundingBoxes, (boundingBox, index) => {
        if (boundingBox != null) {
          let distanceGuidelines: Array<Guideline> = []
          if (hoveredSelectedItem == null) {
            distanceGuidelines = Utils.flatMapArray((highlightedView) => {
              const highlightedViewIsSelected = this.props.selectedViews.some((selectedView) =>
                TP.pathsEqual(selectedView, highlightedView),
              )
              if (highlightedViewIsSelected) {
                return []
              } else {
                if (TP.isFromSameSceneAs(highlightedView, this.props.selectedViews[index])) {
                  return getDistanceGuidelines(highlightedView, this.props.componentMetadata)
                } else {
                  return []
                }
              }
            }, this.props.highlightedViews)
          } else {
            const parentPath = TP.parentPath(hoveredSelectedItem)
            if (parentPath != null) {
              if (TP.isFromSameSceneAs(parentPath, this.props.selectedViews[index])) {
                distanceGuidelines = getDistanceGuidelines(parentPath, this.props.componentMetadata)
              }
            }
          }
          guideLineElements.push(
            <DistanceGuideline
              key={`${TP.toComponentId(this.props.selectedViews[index])}-distance-guidelines`}
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
      const targets = TP.filterScenes(this.props.selectedViews)
      if (targets.length > 0) {
        const targetInstance = MetadataUtils.getElementByInstancePathMaybe(
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
                key={`${TP.toComponentId(targets[0])}-bounding-marks-immediate-parent`}
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
                key={`${TP.toComponentId(targets[0])}-bounding-marks-coordinate-system`}
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

  inSelection(tp: TemplatePath) {
    return this.props.selectedViews.some((et) => TP.pathsEqual(et, tp))
  }

  canResizeElements(): boolean {
    if (isFeatureEnabled('Toolbar For Controls') && this.props.selectModeState !== 'resize') {
      return false
    }

    if (this.props.xrayMode) {
      return false
    }
    return this.props.selectedViews.every((target) => {
      if (TP.isScenePath(target)) {
        const scene = MetadataUtils.findSceneByTemplatePath(this.props.componentMetadata, target)
        let rootHasStyleProp = false
        if (scene != null) {
          rootHasStyleProp = scene.rootElements.some((rootElement) => {
            return this.props.elementsThatRespectLayout.some((path) => {
              return TP.pathsEqual(path, rootElement.templatePath)
            })
          })
        }
        return MetadataUtils.isSceneTreatedAsGroup(scene) || rootHasStyleProp
      } else {
        return this.props.elementsThatRespectLayout.some((path) => TP.pathsEqual(path, target))
      }
    })
  }

  render() {
    const cmdPressed = this.props.keysPressed['cmd'] || false
    const allElementsDirectlySelectable = cmdPressed && !this.props.isDragging
    const roots = MetadataUtils.getAllScenePaths(this.props.componentMetadata)
    let labelDirectlySelectable = true
    let draggableViews = this.getSelectableViews(allElementsDirectlySelectable)
    if (!this.props.highlightsEnabled) {
      draggableViews = []
      labelDirectlySelectable = false
    }

    // TODO future span element should be included here
    let repositionOnly = false
    if (this.props.selectedViews.length === 1 && !TP.isScenePath(this.props.selectedViews[0])) {
      const path = this.props.selectedViews[0]
      const element = MetadataUtils.getElementByInstancePathMaybe(
        this.props.componentMetadata,
        path,
      )
      repositionOnly =
        element != null && MetadataUtils.isAutoSizingText(this.props.imports, element)
    }

    const selectedElementsLayoutInfo = isFeatureEnabled('Layout Info Box')
      ? this.layoutInfo()
      : null

    const overlappingLabels =
      allElementsDirectlySelectable && isFeatureEnabled('Wrapper Element Controls')
        ? this.getOverlappingLabels(draggableViews)
        : null
    if (this.props.xrayMode) {
      return (
        <div
          style={{
            pointerEvents: 'initial',
          }}
        >
          {draggableViews.map((draggableView, index) => {
            return this.renderControl(draggableView, index, false, false)
          })}
          <OutlineControls {...this.props} />
        </div>
      )
    }

    return (
      <div
        style={{
          pointerEvents: 'initial',
        }}
        onContextMenu={this.onContextMenu}
      >
        {selectedElementsLayoutInfo}
        {roots.map((root) => {
          return (
            <React.Fragment key={`${TP.toComponentId(root)}}-root-controls`}>
              {this.renderLabel(root, allElementsDirectlySelectable || labelDirectlySelectable)}
              {this.renderNonSelectableControl(root)}
            </React.Fragment>
          )
        })}
        {this.props.selectionEnabled
          ? draggableViews.map((draggableView, index) => {
              if (
                !allElementsDirectlySelectable &&
                this.props.selectedViews.some((view) =>
                  TP.pathsEqual(TP.parentPath(draggableView), view),
                )
              ) {
                // only double clickable to select and drag
                return this.renderControl(
                  draggableView,
                  index,
                  true,
                  cmdPressed || this.props.layoutInspectorSectionHovered,
                )
              } else {
                // directly draggable
                return this.renderControl(
                  draggableView,
                  index,
                  false,
                  cmdPressed || this.props.layoutInspectorSectionHovered,
                )
              }
            })
          : null}
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
        {overlappingLabels}
        {...this.getMoveGuidelines()}
        {this.getDistanceGuidelines()}
        {this.getBoundingMarks()}
        {this.props.selectionEnabled && <ParentControls {...this.props} />}
        {this.props.reparentTargetPositions.map((reparentTarget) => {
          return (
            <ReorderInsertIndicator
              key={`${TP.toString(reparentTarget.parent!)}-${reparentTarget.drawAtChildIndex}`}
              target={reparentTarget.parent}
              showAtChildIndex={reparentTarget.drawAtChildIndex}
              beforeOrAfter={reparentTarget.beforeOrAfter}
            />
          )
        })}
      </div>
    )
  }
}
