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

interface ResizeControlProps extends ResizeRectangleProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EnabledDirection
  selectedViews: Array<TemplatePath>
  dragState: ResizeDragState | null
}

const ResizeControl: React.FunctionComponent<ResizeControlProps> = (props) => {
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
          props.activeTargetProperty,
        )

        props.dispatch(
          [CanvasActions.createDragState(newDragState), setCanvasAnimationsEnabled(false)],
          'canvas',
        )
        props.onResizeStart(props.measureSize, props.position)
      }
    },
    [props],
  )

  return (
    <React.Fragment>
      {props.resizeStatus === 'enabled' ? (
        <div onMouseDown={onMouseDown}>{props.children}</div>
      ) : (
        props.children
      )}
    </React.Fragment>
  )
}

interface ResizeEdgeProps {
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
    props.dragState.edgePosition.y === props.position.y

  return (
    <React.Fragment>
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
    </React.Fragment>
  )
}

interface ResizeLinesProps {
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

  const [showLabel, setShowLabel] = React.useState(false)
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
    props.dragState.edgePosition.y === props.position.y

  const left = props.canvasOffset.x + props.visualSize.x + props.position.x * props.visualSize.width
  const top = props.canvasOffset.y + props.visualSize.y + props.position.y * props.visualSize.height

  const catchmentSize = 12 / props.scale

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
    <React.Fragment>
      <LineSVGComponent
        scale={props.scale}
        centerX={left}
        centerY={top}
        edge={edge}
        color={props.color}
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
      {mouseCatcher}
    </React.Fragment>
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

interface ResizePointProps {
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

class ResizePoint extends React.Component<ResizePointProps> {
  reference = React.createRef<HTMLDivElement>()

  render() {
    const left =
      this.props.canvasOffset.x +
      this.props.visualSize.x +
      this.props.position.x * this.props.visualSize.width
    const top =
      this.props.canvasOffset.y +
      this.props.visualSize.y +
      this.props.position.y * this.props.visualSize.height

    const size = 6 / this.props.scale
    const catchmentSize = 12 / this.props.scale

    const mouseCatcher =
      this.props.resizeStatus !== 'enabled' ? null : (
        <div
          ref={this.reference}
          style={{
            position: 'absolute',
            width: catchmentSize,
            height: catchmentSize,
            top: top - catchmentSize / 2,
            left: left - catchmentSize / 2,
            backgroundColor: 'transparent',
            cursor: this.props.cursor,
          }}
          data-testid={`${this.props.testID}-${this.props.position.x}-${this.props.position.y}`}
        />
      )
    return (
      <React.Fragment>
        <div
          style={{
            position: 'absolute',
            width: size,
            height: size,
            top: top - size / 2,
            left: left - size / 2,
            borderWidth: 1 / this.props.scale,
            boxSizing: 'border-box',
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
              1 / this.props.scale
            }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${
              1 / this.props.scale
            }px ${2 / this.props.scale}px ${1 / this.props.scale}px `,
            ...this.props.extraStyle,
          }}
        />
        {mouseCatcher}
      </React.Fragment>
    )
  }
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
  activeTargetProperty: LayoutTargetableProp
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
            <ResizeControl
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 0 }}
              enabledDirection={DirectionVertical}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeNS}
                position={{ x: 0.5, y: 0 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
            <ResizeControl
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              position={{ x: 0.5, y: 1 }}
              enabledDirection={DirectionVertical}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeNS}
                position={{ x: 0.5, y: 1 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
          </React.Fragment>
        ) : null

      const horizontalResizeControls =
        dimension === 'horizontal' ? (
          <React.Fragment>
            <ResizeControl
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 0, y: 0.5 }}
              enabledDirection={DirectionHorizontal}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeEW}
                position={{ x: 0, y: 0.5 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
            <ResizeControl
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              position={{ x: 1, y: 0.5 }}
              enabledDirection={DirectionHorizontal}
            >
              <ResizePoint
                {...controlProps}
                cursor={CSSCursor.ResizeEW}
                position={{ x: 1, y: 0.5 }}
                extraStyle={{
                  opacity: 0,
                }}
              />
            </ResizeControl>
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
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 0 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 1 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 0, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 0, y: 0.5 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 1, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeEdge
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 1, y: 0.5 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 0, y: 0 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNWSE}
              position={{ x: 0, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 1, y: 0 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNESW}
              position={{ x: 1, y: 0 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNESW}
            position={{ x: 0, y: 1 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNESW}
              position={{ x: 0, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNWSE}
            position={{ x: 1, y: 1 }}
            enabledDirection={DirectionAll}
          >
            <ResizePoint
              {...controlProps}
              cursor={CSSCursor.ResizeNWSE}
              position={{ x: 1, y: 1 }}
            />
          </ResizeControl>
        </React.Fragment>
      )

      const resizeLines = !this.props.sideResizer ? null : (
        <React.Fragment>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeNS}
            position={{ x: 0.5, y: 1 }}
            enabledDirection={DirectionVertical}
          >
            <ResizeLines
              {...controlProps}
              cursor={CSSCursor.ResizeNS}
              direction='horizontal'
              position={{ x: 0.5, y: 1 }}
            />
          </ResizeControl>
          <ResizeControl
            {...controlProps}
            cursor={CSSCursor.ResizeEW}
            position={{ x: 1, y: 0.5 }}
            enabledDirection={DirectionHorizontal}
          >
            <ResizeLines
              {...controlProps}
              cursor={CSSCursor.ResizeEW}
              direction='vertical'
              position={{ x: 1, y: 0.5 }}
            />
          </ResizeControl>
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
