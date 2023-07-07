import type { KeyCharacter, KeysPressed } from '../../utils/keyboard'
import type { CanvasPoint } from '../../core/shared/math-utils'
import type { EditorAction } from '../editor/action-types'
import type {
  CircleControlProps,
  EllipseControlProps,
  RectControlProps,
  SvgFragmentControl,
} from './canvas-types'

const CanvasControl = {
  emptyControlEventHandlers: {
    onMouseDown: function (clickPoint: CanvasPoint): Array<EditorAction> {
      return []
    },
    onDoubleClick: function (point: CanvasPoint): Array<EditorAction> {
      return []
    },
    onKeyDown: function (key: KeyCharacter, keysPressed: KeysPressed): Array<EditorAction> {
      return []
    },
  },
  isAnchorControlType: function (controlType: string): boolean {
    return controlType === 'circle' || controlType === 'rect'
  },
  anchorControlCenterPoint: function (control: SvgFragmentControl): CanvasPoint {
    switch (control.type) {
      case 'circle':
        return { x: control.props.cx, y: control.props.cy } as CanvasPoint
      case 'rect':
        return {
          x: control.props.x + control.props.width / 2,
          y: control.props.y + control.props.height / 2,
        } as CanvasPoint
      default:
        throw `Invalid control type ${control.type} used as anchor`
    }
  },
  dontScale: function (props: any, scale: number): any {
    return props
  },
  scaleCircleAnchor: function <T extends CircleControlProps>(props: T, scale: number): T {
    const r = props.r / scale
    return {
      ...props,
      cx: props.cx,
      cy: props.cy,
      r: r,
      strokeWidth: props.strokeWidth / scale,
    } as T
  },
  scaleEllipseAnchor: function <T extends EllipseControlProps>(props: T, scale: number): T {
    return {
      ...props,
      cx: props.cx,
      cy: props.cy,
      rx: props.rx / scale,
      ry: props.ry / scale,
      strokeWidth: props.strokeWidth / scale,
    } as T
  },
  scaleRectAnchor: function <T extends RectControlProps>(props: T, scale: number): T {
    const width = props.width / scale
    const height = props.height / scale
    const xOffset = (props.width - width) / 2
    const yOffset = (props.height - height) / 2
    return {
      ...props,
      x: props.x + xOffset,
      y: props.y + yOffset,
      width: width,
      height: height,
      strokeWidth: props.strokeWidth / scale,
    } as T
  },
  scaleLine: function (
    x: number,
    y: number,
  ): (props: { transform?: string }, scale: number) => any {
    return (props: { transform?: string }, scale: number): any => {
      return {
        ...props,
        transform: `${props.transform ?? ''} translate(${x}, ${y}) scale(${1 / scale})`,
      }
    }
  },
  scaleStrokeOnly: function <P extends { strokeWidth: number; strokeDasharray?: string }>(
    props: P,
    scale: number,
  ): P {
    const dashArray = props.strokeDasharray ?? ''
    const scaledStrokeDasharray = dashArray
      .split(' ')
      .map((value: string) => {
        return parseInt(value) / scale
      })
      .join(' ')

    return {
      ...props,
      strokeWidth: props.strokeWidth / scale,
      strokeDasharray: scaledStrokeDasharray,
    } as P
  },
  scaleViaTransform: function (props: { transform?: string }, scale: number): any {
    return {
      ...props,
      transform: `${props.transform ?? ''} scale(${1 / scale})`,
    }
  },
}

export default CanvasControl
