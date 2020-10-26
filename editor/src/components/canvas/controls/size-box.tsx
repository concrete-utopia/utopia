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
import { ResizeStatus } from './new-canvas-controls'
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

interface ResizeControlProps extends ResizeRectangleProps {
  cursor: CSSCursor
  position: EdgePosition
  enabledDirection: EnabledDirection
  selectedViews: Array<TemplatePath>
  dragState: ResizeDragState | null
}

class ResizeControl extends React.Component<ResizeControlProps> {
  reference = React.createRef<HTMLDivElement>()
  constructor(props: ResizeControlProps) {
    super(props)
  }

  componentWillUnmount() {
    this.props.dispatch([setCanvasAnimationsEnabled(true)], 'canvas')
  }

  onMouseDown = (event: React.MouseEvent<HTMLDivElement>) => {
    if (event.buttons === 1) {
      const centerBasedResize = event.altKey
      const keepAspectRatio = event.shiftKey || this.props.elementAspectRatioLocked
      const enableSnapping = !event.metaKey
      const canvasPositions = this.props.windowToCanvasPosition(event.nativeEvent)
      const start: CanvasPoint = canvasPositions.canvasPositionRaw
      const originalFrames = this.props.getOriginalFrames()
      const isMultiSelect = this.props.selectedViews.length !== 1
      const targetProp = isFeatureEnabled('Element Resize Menu')
        ? this.props.propertyTargetOptions[this.props.propertyTargetSelectedIndex]
        : this.props.propertyTargetOptions[0]
      const newDragState = resizeDragState(
        start,
        null,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
        this.props.measureSize,
        originalFrames,
        this.props.position,
        this.props.enabledDirection,
        this.props.metadata,
        this.props.selectedViews,
        isMultiSelect,
        targetProp,
      )

      this.props.dispatch(
        [CanvasActions.createDragState(newDragState), setCanvasAnimationsEnabled(false)],
        'canvas',
      )
      this.props.onResizeStart(this.props.measureSize, this.props.position)
    }
  }

  render() {
    const currentSize = this.props.visualSize
    const top =
      this.props.canvasOffset.y +
      this.props.visualSize.y +
      this.props.position.y * this.props.visualSize.height -
      this.props.position.y / this.props.scale
    const left =
      this.props.canvasOffset.x +
      this.props.visualSize.x +
      this.props.position.x * this.props.visualSize.width -
      this.props.position.x / this.props.scale

    const labelLeft = left + 20 / this.props.scale
    const labelTop = top + 20 / this.props.scale
    const shouldShowSizeLabel =
      this.props.dragState?.edgePosition.x === this.props.position.x &&
      this.props.dragState?.edgePosition.y === this.props.position.y
    return (
      <React.Fragment>
        {this.props.resizeStatus === 'enabled' ? (
          <div onMouseDown={this.onMouseDown}>{this.props.children}</div>
        ) : (
          this.props.children
        )}
        {!isFeatureEnabled('Element Resize Menu') && (
          <SizeBoxLabel
            visible={shouldShowSizeLabel}
            left={labelLeft}
            top={labelTop}
            scale={this.props.scale}
            size={currentSize}
            imageMultiplier={this.props.imageMultiplier}
            dragState={this.props.dragState}
          />
        )}
      </React.Fragment>
    )
  }
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
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
  setTargetOptionsArray: (newArray: Array<LayoutTargetableProp>) => void
}

interface ResizeEdgeState {
  showLabel: boolean
}

class ResizeEdge extends React.Component<ResizeEdgeProps, ResizeEdgeState> {
  constructor(props: ResizeEdgeProps) {
    super(props)
    this.state = {
      showLabel: false,
    }
  }
  reference = React.createRef<HTMLDivElement>()

  render() {
    if (this.props.resizeStatus != 'enabled') {
      return null
    }
    const beforeOrAfter =
      this.props.position.y === 0.5 ? this.props.position.x : this.props.position.y
    const edge = beforeOrAfter === 0 ? 'before' : 'after'
    const baseLeft =
      this.props.canvasOffset.x +
      this.props.visualSize.x +
      this.props.position.x * this.props.visualSize.width
    const baseTop =
      this.props.canvasOffset.y +
      this.props.visualSize.y +
      this.props.position.y * this.props.visualSize.height

    const lineSize = 10 / this.props.scale
    const width = this.props.direction === 'horizontal' ? this.props.visualSize.width : lineSize
    const height = this.props.direction === 'vertical' ? this.props.visualSize.height : lineSize
    const left =
      baseLeft +
      (this.props.direction === 'horizontal' ? -this.props.visualSize.width / 2 : -lineSize / 2)
    const top =
      baseTop +
      (this.props.direction === 'vertical' ? -this.props.visualSize.height / 2 : -lineSize / 2)

    const isEdgeDragged =
      this.props.dragState != null &&
      this.props.dragState.type === 'RESIZE_DRAG_STATE' &&
      this.props.dragState.start != null &&
      this.props.dragState.edgePosition.x === this.props.position.x &&
      this.props.dragState.edgePosition.y === this.props.position.y

    const options: LayoutTargetableProp[] =
      this.props.direction === 'horizontal'
        ? [
            'Height',
            edge === 'before' ? 'paddingTop' : 'paddingBottom',
            edge === 'before' ? 'marginTop' : 'marginBottom',
            'minHeight',
            'maxHeight',
          ]
        : [
            'Width',
            edge === 'before' ? 'paddingLeft' : 'paddingRight',
            edge === 'before' ? 'marginLeft' : 'marginRight',
            'minWidth',
            'maxWidth',
          ]

    return (
      <React.Fragment>
        <div
          ref={this.reference}
          onMouseOver={() => this.setState({ showLabel: true })}
          onMouseOut={() => this.setState({ showLabel: false })}
          style={{
            position: 'absolute',
            left: left,
            top: top,
            width: width,
            height: height,
            boxSizing: 'border-box',
            backgroundColor: 'transparent',
            cursor: this.props.resizeStatus === 'enabled' ? this.props.cursor : undefined,
          }}
        />
        {(this.state.showLabel || isEdgeDragged) && (
          <PropertyTargetSelector
            top={
              top +
              (this.props.direction === 'horizontal'
                ? edge === 'before' && this.props.direction === 'horizontal'
                  ? -105
                  : 10
                : -10)
            }
            left={
              left +
              (this.props.direction === 'vertical'
                ? edge === 'before' && this.props.direction === 'vertical'
                  ? -95
                  : 10
                : -10)
            }
            options={options}
            selected={this.props.propertyTargetSelectedIndex}
            setOptionsCallback={this.props.setTargetOptionsArray}
            targetComponentMetadata={this.props.targetComponentMetadata}
          />
        )}
      </React.Fragment>
    )
  }
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
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
  setTargetOptionsArray: (newArray: Array<LayoutTargetableProp>) => void
}

const LineOffset = 6
const ResizeLines = (props: ResizeLinesProps) => {
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
          options={
            props.direction === 'vertical'
              ? [props.labels.vertical, 'minWidth', 'maxWidth']
              : [props.labels.horizontal, 'minHeight', 'maxHeight']
          }
          setOptionsCallback={props.setTargetOptionsArray}
          selected={props.propertyTargetSelectedIndex}
          targetComponentMetadata={props.targetComponentMetadata}
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

  const style: React.CSSProperties = isFeatureEnabled('Element Resize Menu')
    ? {
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
      }
    : {
        position: 'absolute',
        backgroundColor: 'white',
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px hsla(0,0%,0%,.7), 0px ${1 / props.scale}px ${
          3 / props.scale
        }px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX + (props.edge === 'before' ? -(controlWidth + 2) : 2) / props.scale,
        top: props.centerY + scaledControlOffsetTop,
      }

  return <div className='label-dimensionableControlVertical' style={style} />
}

const DimensionableControlHorizontal = (props: DimensionableControlProps) => {
  const controlLength = ControlSideShort
  const controlWidth = 15
  const scaledControlWidth = controlWidth / props.scale
  const scaledControlOffsetLeft = -(scaledControlWidth / 2)

  const style: React.CSSProperties = isFeatureEnabled('Element Resize Menu')
    ? {
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
      }
    : {
        position: 'absolute',
        backgroundColor: 'white',
        borderRadius: `${5 / props.scale}px`,
        // These just about work. I can clean them up afterwards
        boxShadow: `0px 0px 0px ${0.3 / props.scale}px hsla(0,0%,0%,.7), 0px ${1 / props.scale}px ${
          3 / props.scale
        }px rgba(140,140,140,.9)`,
        height: controlLength / props.scale,
        width: controlWidth / props.scale,
        left: props.centerX + scaledControlOffsetLeft,
        top: props.centerY + (props.edge === 'before' ? -(controlLength + 2) : 2) / props.scale,
      }

  return <div className='label-dimensionableControlVertical' style={style} />
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
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
  setTargetOptionsArray: (newArray: Array<LayoutTargetableProp>) => void
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

      let resizeLines = null
      if (this.props.sideResizer) {
        if (isFeatureEnabled('Element Resize Menu') || isFeatureEnabled('Flow Resize')) {
          resizeLines = (
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
        } else {
          resizeLines = (
            <React.Fragment>
              <ResizeControl
                {...controlProps}
                cursor={CSSCursor.ResizeNS}
                position={{ x: 0.5, y: 0 }}
                enabledDirection={DirectionVertical}
              >
                <ResizeLines
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
                position={{ x: 0, y: 0.5 }}
                enabledDirection={DirectionHorizontal}
              >
                <ResizeLines
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
                <ResizeLines
                  {...controlProps}
                  cursor={CSSCursor.ResizeEW}
                  direction='vertical'
                  position={{ x: 1, y: 0.5 }}
                />
              </ResizeControl>
            </React.Fragment>
          )
        }
      }

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
