/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { colorTheme } from 'uuiui'
import { EditorDispatch } from '../../editor/action-types'
import { setCanvasAnimationsEnabled } from '../../editor/actions/actions'
import { ControlFontSize } from '../canvas-controls-frame'
import {
  CSSCursor,
  ResizeDragState,
  resizeDragState,
  CanvasPositions,
  EdgePosition,
  EnabledDirection,
  DirectionVertical,
  DirectionHorizontal,
  DirectionAll,
  DragState,
} from '../canvas-types'
import { ResizeStatus, useTargetSelector } from './new-canvas-controls'
import { TemplatePath } from '../../../core/shared/project-file-types'
import CanvasActions from '../canvas-actions'
import { OriginalCanvasAndLocalFrame } from '../../editor/store/editor-state'
import { ComponentMetadata, ElementInstanceMetadata } from '../../../core/shared/element-template'
import { calculateExtraSizeForZeroSizedElement } from './outline-utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { betterReactMemo } from '../../../uuiui-deps'
import { SizeBoxLabel } from './size-box-label'
import { PropertyTargetSelector } from './property-target-selector'
import {
  LayoutFlexElementProp,
  LayoutTargetableProp,
} from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { KeysPressed } from '../../../utils/keyboard'
import * as TP from '../../../core/shared/template-path'
import { FlexGrowControl, FlexShrinkControl } from './flex-shrink-grow-controls'

interface ResizeControlProps extends ResizeRectangleProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EnabledDirection
  selectedViews: Array<TemplatePath>
  dragState: ResizeDragState | null
}

function useStartDrag(
  activeTargetProperty: LayoutTargetableProp,
  props: {
    windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
    elementAspectRatioLocked: boolean
    dispatch: EditorDispatch
    getOriginalFrames: () => Array<OriginalCanvasAndLocalFrame>
    selectedViews: Array<TemplatePath>
    measureSize: CanvasRectangle // this is the size we want to adjust when the user drags
    position: EdgePosition
    enabledDirection: EnabledDirection
    metadata: Array<ComponentMetadata>
    onResizeStart: (originalSize: CanvasRectangle, draggedPoint: EdgePosition) => void
  },
) {
  const dispatch = props.dispatch
  React.useEffect(() => {
    return function cleanup() {
      dispatch([setCanvasAnimationsEnabled(true)], 'canvas')
    }
  }, [dispatch])

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      if (event.buttons === 1) {
        const centerBasedResize = event.altKey
        const keepAspectRatio = event.shiftKey || props.elementAspectRatioLocked
        const enableSnapping = !event.metaKey
        const canvasPositions = props.windowToCanvasPosition(event.nativeEvent)
        const start: CanvasPoint = canvasPositions.canvasPositionRaw
        const originalFrames = props.getOriginalFrames()
        const isMultiSelect = props.selectedViews.length !== 1
        const newDragState = resizeDragState(
          start,
          null,
          enableSnapping,
          centerBasedResize,
          keepAspectRatio,
          props.measureSize,
          originalFrames,
          props.position,
          props.enabledDirection,
          props.metadata,
          props.selectedViews,
          isMultiSelect,
          activeTargetProperty,
        )

        props.dispatch(
          [CanvasActions.createDragState(newDragState), setCanvasAnimationsEnabled(false)],
          'canvas',
        )
        props.onResizeStart(props.measureSize, props.position)
      }
    },
    [props, activeTargetProperty],
  )

  return onMouseDown
}

interface ResizeEdgeProps extends Omit<ResizeControlProps, 'dragState' | 'labels'> {
  targetComponentMetadata: ElementInstanceMetadata | null
  dispatch: EditorDispatch
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  labels: {
    vertical: string
    horizontal: string
  }
  dragState: DragState | null
  keysPressed: KeysPressed
}

interface ResizeEdgeState {
  showLabel: boolean
}

const ResizeEdge: React.FunctionComponent<ResizeEdgeProps> = (props) => {
  const options =
    props.direction === 'horizontal'
      ? (['Height', 'minHeight', 'maxHeight'] as const)
      : (['Width', 'minWidth', 'maxWidth'] as const)

  const [showLabel, setShowLabel] = React.useState(false)
  const [targets, targetIndex] = useTargetSelector(options, props.keysPressed)

  const onMouseDown = useStartDrag(targets[targetIndex], props)

  if (props.resizeStatus != 'enabled') {
    return null
  }
  const beforeOrAfter = props.position.y === 0.5 ? props.position.x : props.position.y
  const edge = beforeOrAfter === 0 ? 'before' : 'after'
  const baseLeft =
    props.canvasOffset.x + props.visualSize.x + props.position.x * props.visualSize.width
  const baseTop =
    props.canvasOffset.y + props.visualSize.y + props.position.y * props.visualSize.height

  const lineSize = 10 / props.scale
  const width = props.direction === 'horizontal' ? props.visualSize.width : lineSize
  const height = props.direction === 'vertical' ? props.visualSize.height : lineSize
  const left =
    baseLeft + (props.direction === 'horizontal' ? -props.visualSize.width / 2 : -lineSize / 2)
  const top =
    baseTop + (props.direction === 'vertical' ? -props.visualSize.height / 2 : -lineSize / 2)

  const isEdgeDragged =
    props.dragState != null &&
    props.dragState.type === 'RESIZE_DRAG_STATE' &&
    props.dragState.start != null &&
    props.dragState.edgePosition.x === props.position.x &&
    props.dragState.edgePosition.y === props.position.y &&
    props.targetComponentMetadata != null &&
    props.dragState.draggedElements.some((element) =>
      TP.pathsEqual(element, props.targetComponentMetadata!.templatePath),
    )

  return (
    <div onMouseDown={onMouseDown}>
      <div
        onMouseOver={() => setShowLabel(true)}
        onMouseOut={() => setShowLabel(false)}
        style={{
          position: 'absolute',
          left: left,
          top: top,
          width: width,
          height: height,
          boxSizing: 'border-box',
          backgroundColor: 'transparent',
          cursor: props.resizeStatus === 'enabled' ? props.cursor : undefined,
        }}
      />
      {(showLabel || isEdgeDragged) && (
        <PropertyTargetSelector
          top={
            top +
            (props.direction === 'horizontal'
              ? edge === 'before' && props.direction === 'horizontal'
                ? -25
                : 10
              : -10)
          }
          left={
            left +
            (props.direction === 'vertical'
              ? edge === 'before' && props.direction === 'vertical'
                ? -25
                : 10
              : -10)
          }
          options={targets}
          targetIndex={targetIndex}
          targetComponentMetadata={props.targetComponentMetadata}
          keysPressed={props.keysPressed}
        />
      )}
    </div>
  )
}

interface ResizeLinesProps extends Omit<ResizeControlProps, 'dragState'> {
  targetComponentMetadata: ElementInstanceMetadata | null
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  dragState: DragState | null
  color?: string
  labels: {
    vertical: 'Width' | 'FlexFlexBasis' | 'FlexCrossBasis'
    horizontal: 'Height' | 'FlexFlexBasis' | 'FlexCrossBasis'
  }
  keysPressed: KeysPressed
}

const LineOffset = 6
const ResizeLines = (props: ResizeLinesProps) => {
  const options =
    props.direction === 'vertical'
      ? ([props.labels.vertical, 'minWidth', 'maxWidth'] as const)
      : ([props.labels.horizontal, 'minHeight', 'maxHeight'] as const)

  const [targets, targetIndex] = useTargetSelector(options, props.keysPressed)

  const onMouseDown = useStartDrag(targets[targetIndex], props)

  const [showLabel, setShowLabel] = React.useState(false)
  const [showFlexGrowLabel, setShowFlexGrowLabel] = React.useState(false)
  const [showFlexShrinkLabel, setShowFlexShrinkLabel] = React.useState(false)
  const reference = React.createRef<HTMLDivElement>()
  const LineSVGComponent =
    props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal

  const beforeOrAfter = props.position.y === 0.5 ? props.position.x : props.position.y
  const edge = beforeOrAfter === 0 ? 'before' : 'after'

  const isEdgeDragged =
    props.dragState != null &&
    props.dragState.type === 'RESIZE_DRAG_STATE' &&
    props.dragState.start != null &&
    props.dragState.edgePosition.x === props.position.x &&
    props.dragState.edgePosition.y === props.position.y &&
    props.targetComponentMetadata != null &&
    props.dragState.draggedElements.some((element) =>
      TP.pathsEqual(element, props.targetComponentMetadata!.templatePath),
    )

  const left = props.canvasOffset.x + props.visualSize.x + props.position.x * props.visualSize.width
  const top = props.canvasOffset.y + props.visualSize.y + props.position.y * props.visualSize.height

  const catchmentSize = 12 / props.scale

  const showFlexGrowControl = () => setShowFlexGrowLabel(true)
  const hideFlexGrowControl = () => setShowFlexGrowLabel(false)

  const mouseCatcher =
    props.resizeStatus !== 'enabled' ? null : (
      <div
        ref={reference}
        onMouseEnter={() => {
          setShowLabel(true)
        }}
        onMouseLeave={() => {
          setShowLabel(false)
        }}
        style={{
          position: 'absolute',
          width: catchmentSize,
          height: catchmentSize,
          top: top - catchmentSize / 2,
          left: left - catchmentSize / 2,
          backgroundColor: 'transparent',
          cursor: props.cursor,
        }}
      />
    )

  return (
    <div onMouseDown={onMouseDown}>
      <FlexShrinkControl
        direction={props.direction === 'vertical' ? 'row' : 'column'}
        targetComponentMetadata={props.targetComponentMetadata}
        top={top}
        left={left}
      />
      <LineSVGComponent
        scale={props.scale}
        centerX={left}
        centerY={top}
        edge={edge}
        color={props.color}
      />
      <FlexGrowControl
        direction={props.direction === 'vertical' ? 'row' : 'column'}
        targetComponentMetadata={props.targetComponentMetadata}
        top={top}
        left={left}
        onMouseOver={showFlexGrowControl}
        onMouseOut={hideFlexGrowControl}
      />
      {showFlexGrowLabel && (
        <PropertyTargetSelector
          top={
            top +
            (props.direction === 'horizontal'
              ? edge === 'before' && props.direction === 'horizontal'
                ? -25
                : 10
              : -10)
          }
          left={
            left +
            (props.direction === 'vertical'
              ? edge === 'before' && props.direction === 'vertical'
                ? -25
                : 10
              : -10)
          }
          options={['flexGrow']}
          targetIndex={0}
          targetComponentMetadata={props.targetComponentMetadata}
          keysPressed={props.keysPressed}
        />
      )}
      {(showLabel || isEdgeDragged) && (
        <PropertyTargetSelector
          top={
            top +
            (props.direction === 'horizontal'
              ? edge === 'before' && props.direction === 'horizontal'
                ? -25
                : 10
              : -10)
          }
          left={
            left +
            (props.direction === 'vertical'
              ? edge === 'before' && props.direction === 'vertical'
                ? -25
                : 10
              : -10)
          }
          options={targets}
          targetIndex={targetIndex}
          targetComponentMetadata={props.targetComponentMetadata}
          keysPressed={props.keysPressed}
        />
      )}
      {mouseCatcher}
    </div>
  )
}

interface DimensionableControlProps {
  centerX: number
  centerY: number
  edge: 'before' | 'after'
  color?: string
  scale: number
}

const ControlSideShort = 3
const ControlSideLong = 8

const DimensionableControlVertical = (props: DimensionableControlProps) => {
  const controlLength = 15
  const controlWidth = ControlSideShort
  const scaledControlLength = controlLength / props.scale
  const scaledControlOffsetTop = -(scaledControlLength / 2)

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'absolute',
        backgroundColor: '#d4f3ff',
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px ${colorTheme.controlledBlue.value}, 0px ${
          1 / props.scale
        }px ${3 / props.scale}px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX - 1,
        top: props.centerY + scaledControlOffsetTop,
      }}
    />
  )
}

const DimensionableControlHorizontal = (props: DimensionableControlProps) => {
  const controlLength = ControlSideShort
  const controlWidth = 15
  const scaledControlWidth = controlWidth / props.scale
  const scaledControlOffsetLeft = -(scaledControlWidth / 2)

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'absolute',
        backgroundColor: '#d4f3ff',
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px ${colorTheme.controlledBlue.value}, 0px ${
          1 / props.scale
        }px ${3 / props.scale}px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX + scaledControlOffsetLeft,
        top: props.centerY - 1,
      }}
    />
  )
}

interface ResizePointProps extends ResizeControlProps {
  dispatch: EditorDispatch
  cursor: CSSCursor
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
  position: EdgePosition
  resizeStatus: ResizeStatus
  extraStyle?: React.CSSProperties
  testID: string
}

const ResizePoint: React.FunctionComponent<ResizePointProps> = (props) => {
  const onMouseDown = useStartDrag('Width', props)

  const left = props.canvasOffset.x + props.visualSize.x + props.position.x * props.visualSize.width
  const top = props.canvasOffset.y + props.visualSize.y + props.position.y * props.visualSize.height

  const size = 6 / props.scale
  const catchmentSize = 12 / props.scale

  const mouseCatcher =
    props.resizeStatus !== 'enabled' ? null : (
      <div
        style={{
          position: 'absolute',
          width: catchmentSize,
          height: catchmentSize,
          top: top - catchmentSize / 2,
          left: left - catchmentSize / 2,
          backgroundColor: 'transparent',
          cursor: props.cursor,
        }}
        data-testid={`${props.testID}-${props.position.x}-${props.position.y}`}
      />
    )
  return (
    <div onMouseDown={onMouseDown}>
      <div
        style={{
          position: 'absolute',
          width: size,
          height: size,
          top: top - size / 2,
          left: left - size / 2,
          borderWidth: 1 / props.scale,
          boxSizing: 'border-box',
          backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
          borderRadius: '10%',
          borderStyle: 'none',
          borderColor: 'transparent',
          boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
            1 / props.scale
          }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${
            1 / props.scale
          }px ${2 / props.scale}px ${1 / props.scale}px `,
          ...props.extraStyle,
        }}
      />
      {mouseCatcher}
    </div>
  )
}

interface DoubleClickExtenderProps {
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  visualSize: CanvasRectangle
  scale: number
}

interface ResizeRectangleProps {
  targetComponentMetadata: ElementInstanceMetadata | null
  dispatch: EditorDispatch
  scale: number
  canvasOffset: CanvasPoint
  measureSize: CanvasRectangle // this is the size we want to adjust when the user drags
  visualSize: CanvasRectangle // this is the canvas size of the selection (might not be the same as measureSize in case of Yoga)
  resizeStatus: ResizeStatus
  selectedViews: Array<TemplatePath>
  elementAspectRatioLocked: boolean
  imageMultiplier: number | null
  sideResizer: boolean
  color?: string
  dragState: ResizeDragState | null
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  getOriginalFrames: () => Array<OriginalCanvasAndLocalFrame>
  metadata: Array<ComponentMetadata>
  onResizeStart: (originalSize: CanvasRectangle, draggedPoint: EdgePosition) => void
  testID: string
  labels: {
    vertical: 'Width' | 'FlexFlexBasis' | 'FlexCrossBasis'
    horizontal: 'Height' | 'FlexFlexBasis' | 'FlexCrossBasis'
  }
  keysPressed: KeysPressed
}

export class ResizeRectangle extends React.Component<ResizeRectangleProps> {
  render() {
    const controlProps = this.props

    const { showingInvisibleElement, dimension } = calculateExtraSizeForZeroSizedElement(
      this.props.measureSize,
    )

    if (showingInvisibleElement && isFeatureEnabled('Invisible Element Controls')) {
      // is it a one dimensional element?
      const verticalResizeControls =
        dimension === 'vertical' ? (
          <React.Fragment>
            <ResizePoint
              {...controlProps}
              enabledDirection={DirectionVertical}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 0 }}
              extraStyle={{
                opacity: 0,
              }}
            />
            <ResizePoint
              {...controlProps}
              enabledDirection={DirectionVertical}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 1 }}
              extraStyle={{
                opacity: 0,
              }}
            />
          </React.Fragment>
        ) : null

      const horizontalResizeControls =
        dimension === 'horizontal' ? (
          <React.Fragment>
            <ResizePoint
              {...controlProps}
              enabledDirection={DirectionHorizontal}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 0, y: 0.5 }}
              extraStyle={{
                opacity: 0,
              }}
            />
            <ResizePoint
              {...controlProps}
              enabledDirection={DirectionHorizontal}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 1, y: 0.5 }}
              extraStyle={{
                opacity: 0,
              }}
            />
          </React.Fragment>
        ) : null

      // TODO  double click sets an arbitrary size in the missing dimensions
      return [verticalResizeControls, horizontalResizeControls]
    } else if (
      this.props.resizeStatus === 'enabled' ||
      this.props.resizeStatus === 'noninteractive'
    ) {
      const pointControls = this.props.sideResizer ? null : (
        <React.Fragment>
          <ResizeEdge
            {...controlProps}
            enabledDirection={DirectionVertical}
            cursor={CSSCursor.ResizeNS}
            direction='horizontal'
            position={{ x: 0.5, y: 0 }}
          />
          <ResizeEdge
            {...controlProps}
            enabledDirection={DirectionVertical}
            cursor={CSSCursor.ResizeNS}
            direction='horizontal'
            position={{ x: 0.5, y: 1 }}
          />
          <ResizeEdge
            {...controlProps}
            enabledDirection={DirectionHorizontal}
            cursor={CSSCursor.ResizeEW}
            direction='vertical'
            position={{ x: 0, y: 0.5 }}
          />
          <ResizeEdge
            {...controlProps}
            enabledDirection={DirectionHorizontal}
            cursor={CSSCursor.ResizeEW}
            direction='vertical'
            position={{ x: 1, y: 0.5 }}
          />
          <ResizePoint
            {...controlProps}
            enabledDirection={DirectionAll}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 0, y: 0 }}
          />
          <ResizePoint
            {...controlProps}
            enabledDirection={DirectionAll}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 1, y: 0 }}
          />
          <ResizePoint
            {...controlProps}
            enabledDirection={DirectionAll}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 0, y: 1 }}
          />
          <ResizePoint
            {...controlProps}
            enabledDirection={DirectionAll}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 1, y: 1 }}
          />
        </React.Fragment>
      )

      const resizeLines = !this.props.sideResizer ? null : (
        <React.Fragment>
          <ResizeLines
            {...controlProps}
            enabledDirection={DirectionVertical}
            cursor={CSSCursor.ResizeNS}
            direction='horizontal'
            position={{ x: 0.5, y: 1 }}
          />
          <ResizeLines
            {...controlProps}
            enabledDirection={DirectionHorizontal}
            cursor={CSSCursor.ResizeEW}
            direction='vertical'
            position={{ x: 1, y: 0.5 }}
          />
        </React.Fragment>
      )

      return (
        <React.Fragment>
          {resizeLines}
          {pointControls}
        </React.Fragment>
      )
    } else {
      return null
    }
  }
}
