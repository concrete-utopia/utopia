import React from 'react'
import RU from '../../../utils/react-utils'
import Utils from '../../../utils/utils'
import type { CanvasPoint } from '../../../core/shared/math-utils'
import type {
  ControlComponentProps,
  DivControl,
  SvgControl,
  SvgFragmentControl,
} from '../canvas-types'

export class SvgControlComponent extends React.Component<ControlComponentProps<SvgControl>> {
  render() {
    const offset = getComponentOffset(
      this.props.canvasOffset,
      this.props.control.followCanvas,
      this.props.control.offset,
    )
    const svgChildren = this.props.control.controls.map((control, index): React.ReactFragment => {
      if ((control.type as any) === 'svgControl' || (control.type as any) === 'divControl') {
        throw new Error(
          `please wrap ${control.controlid} HigherOrderSvgControlComponent in HigherOrderDivControlComponents to avoid CSS sadness!`,
        )
      } else {
        return controlToSvg(control, index)
      }
    })
    return (
      <svg
        id={this.props.control.controlid}
        style={{
          position: 'absolute',
          transform: `translate3d(${offset.x}px, ${offset.y}px, 0)`,
        }}
        width={1} // holy shit 0 makes the svg not appear, 1 allows overflow
        height={1}
      >
        <defs>
          <filter
            x='-200%'
            y='-200%'
            width='400%'
            height='400%'
            filterUnits='objectBoundingBox'
            id='corner-dropshadow'
          >
            <feDropShadow dx='0' dy='0' stdDeviation='1' floodColor='#000' floodOpacity='.25' />
          </filter>
        </defs>
        <g>{svgChildren}</g>
      </svg>
    )
  }
}

export class DivControlComponent extends React.Component<ControlComponentProps<DivControl>> {
  render() {
    const offset = getComponentOffset(
      this.props.canvasOffset,
      this.props.control.followCanvas,
      this.props.control.offset,
    )
    const children = this.props.control.controls.map((control, index): React.ReactFragment => {
      if (control.type === 'svgControl') {
        // okay something is very fishy here, I needed this IF here for typescript type narrowing... but why?!
        // both branches of the if statement are 100% similar
        return (
          <control.component
            key={control.controlid}
            control={control}
            canvasOffset={this.props.canvasOffset}
            index={index}
          />
        ) as any // TODO delete this file
      } else {
        // okay something is very fishy here, I needed this IF here for typescript type narrowing... but why?!
        // both branches of the if statement are 100% similar
        return (
          <control.component
            key={control.controlid}
            control={control}
            canvasOffset={this.props.canvasOffset}
            index={index}
          />
        ) as any // TODO delete this file
      }
    })
    return (
      <div
        id={this.props.control.controlid}
        style={{
          position: 'absolute',
          transform: `translate3d(${offset.x}px, ${offset.y}px, 0)`,
        }}
      >
        {children}
      </div>
    )
  }
}

const getComponentOffset = (
  canvasOffset: CanvasPoint,
  followCanvas: boolean | 'x' | 'y',
  controlOffset: CanvasPoint = { x: 0, y: 0 } as CanvasPoint,
) => {
  const filteredCanvasOffset = filterCanvasOffset(canvasOffset, followCanvas)
  const offset = Utils.offsetPoint(controlOffset, filteredCanvasOffset)
  return offset
}

const filterCanvasOffset = (offset: CanvasPoint, followCanvas: boolean | 'x' | 'y') => {
  if (followCanvas === 'x') {
    return {
      x: offset.x,
      y: 0,
    } as CanvasPoint
  } else if (followCanvas === 'y') {
    return {
      x: 0,
      y: offset.y,
    } as CanvasPoint
  } else if (followCanvas) {
    return offset
  } else {
    return { x: 0, y: 0 } as CanvasPoint
  }
}

const controlToSvg = (control: SvgFragmentControl, index: number) => {
  var children: Array<any> = []
  if (control.type === 'text') {
    children.push(control.props.text)
  }

  let elements: Array<React.ReactElement<any>>
  if (control.fill != null && control.fill.type === 'verticalrectangles') {
    const patternId = `control-fill-pattern-${control.controlid}`
    const patternPathId = `control-fill-path-${control.controlid}`
    const patternDefsId = `control-fill-defs-${control.controlid}`
    elements = [
      RU.create(
        'defs',
        { key: patternDefsId },
        RU.create(
          'pattern',
          {
            key: patternId,
            id: patternId,
            x: 0,
            y: 0,
            width: 12,
            height: 16,
            patternUnits: 'userSpaceOnUse',
          },
          RU.create('path', {
            key: patternPathId,
            id: patternPathId,
            fill: 'white',
            d: 'M4,0.990777969 C4,0.443586406 4.44386482,0 5,0 C5.55228475,0 6,0.45097518 6,0.990777969 L6,5.00922203 C6,5.55641359 5.55613518,6 5,6 C4.44771525,6 4,5.54902482 4,5.00922203 L4,0.990777969 Z M10,8.99077797 C10,8.44358641 10.4438648,8 11,8 C11.5522847,8 12,8.45097518 12,8.99077797 L12,13.009222 C12,13.5564136 11.5561352,14 11,14 C10.4477153,14 10,13.5490248 10,13.009222 L10,8.99077797 Z',
          }),
        ),
      ),
      RU.create(
        control.type,
        {
          ...control.props,
          id: `control-${control.controlid}`,
          fill: `url(#${patternId})`,
          key: `control-${control.controlid}-${index}`,
        } as any,
        children,
      ),
    ]
  } else {
    elements = [
      RU.create(
        control.type,
        {
          ...control.props,
          id: `control-${control.controlid}`,
          key: `control-${control.controlid}-${index}`,
        } as any,
        children,
      ),
    ]
  }

  return elements
}
