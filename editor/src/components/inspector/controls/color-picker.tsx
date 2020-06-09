import React = require('react')
import * as Chroma from 'chroma-js'
import { colorTheme, SimpleNumberInput, SimplePercentInput, StringInput, Icn } from 'uuiui'
import { didWeHandleMouseMoveForThisFrame, mouseMoveHandled } from '../../mouse-move'
import {
  CSSColor,
  cssColorToChromaColorOrDefault,
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isHex,
  isHSL,
  isKeyword,
  parseAlphaFromCSSColor,
} from '../common/css-utils'
import { inspectorEdgePadding } from '../sections/style-section/background-subsection/background-picker'
import { ControlStyleDefaults } from '../common/control-status'
import { InspectorModal } from '../widgets/inspector-modal'
import { checkerboardBackground } from '../common/inspector-utils'

export interface ColorPickerProps extends ColorPickerInnerProps {
  closePopup: () => void
  portalTarget?: HTMLElement
}

export const colorPickerWidth = 290
export const MetadataEditorModalPreviewHeight = 150

export const ColorPicker: React.FunctionComponent<ColorPickerProps> = ({
  closePopup,
  portalTarget,
  ...props
}) => {
  return (
    <InspectorModal
      offsetX={props.offsetX - colorPickerWidth}
      offsetY={props.offsetY}
      closePopup={closePopup}
    >
      <div
        id={props.id}
        className='colorPicker-wrapper'
        style={{
          width: colorPickerWidth,
          borderRadius: 4,
          position: 'absolute',
          overflow: 'hidden',
          backgroundColor: colorTheme.inspectorBackground.value,
          boxShadow: `0 3px 6px ${colorTheme.canvasBackground.shade(140).o(40).value}`,
          zIndex: 2,
          marginBottom: 32,
        }}
      >
        <ColorPickerInner {...props} />
      </div>
    </InspectorModal>
  )
}

export interface ColorPickerInnerProps {
  value: CSSColor
  onSubmitValue: (value: CSSColor) => void
  onTransientSubmitValue: (value: CSSColor) => void
  offsetX: number
  offsetY: number
  id: string
}

function toPassedInColorType(color: Chroma.Color, passedInColor: CSSColor): CSSColor {
  if (isKeyword(passedInColor)) {
    const name = color.name()
    if (name.startsWith('#')) {
      return {
        type: 'Hex',
        hex: name.toUpperCase(),
      }
    } else {
      return {
        type: 'Keyword',
        keyword: name,
      }
    }
  } else if (isHex(passedInColor)) {
    return {
      type: 'Hex',
      hex: color.hex().toUpperCase(),
    }
  } else if (isHSL(passedInColor)) {
    const [h, s, l, a] = (color.hsl() as any) as [number, number, number, number] // again, the types are out of date
    return {
      type: 'HSL',
      h: h,
      s: s,
      l: l,
      a: a,
      percentageAlpha: passedInColor.percentageAlpha,
    }
  } else {
    const [r, g, b, a] = color.rgba()
    return {
      type: 'RGB',
      r: r,
      g: g,
      b: b,
      a: a,
      percentageAlpha: passedInColor.percentageAlpha,
      percentagesUsed: passedInColor.percentagesUsed,
    }
  }
}

export type ColorPickerInnerState = {
  hsvH: number
  hsvS: number
  hsvV: number
  hex: string
  alpha: number
  controlPosition2DX: number
  controlPosition2DY: number
  controlPosition1D: number
  controlPositionAlpha: number
}

export class ColorPickerInner extends React.Component<
  ColorPickerInnerProps,
  ColorPickerInnerState
> {
  private Ref2D = React.createRef<HTMLDivElement>()
  private RefAlpha = React.createRef<HTMLDivElement>()
  private RefFirstControl = React.createRef<HTMLInputElement>()
  constructor(props: ColorPickerInnerProps) {
    super(props)
    // Color values in different spaces need to be stored in state due to them not mapping 1:1 across color spaces. E.g. HSV(0, 0%, 100%) == HSV(180, 0%, 100%). A fully controlled component would be nice, but alas.
    this.state = {
      ...ColorPickerInner.calculateCurrentColor(this.props.value),
      controlPosition2DX: 0,
      controlPosition2DY: 0,
      controlPosition1D: 0,
      controlPositionAlpha: 0,
    }
  }

  componentDidMount() {
    if (this.RefFirstControl.current != null) {
      this.RefFirstControl.current.focus()
    }
  }

  componentWillUnmount() {
    document.removeEventListener('mousemove', this.handleMouseMove2D)
    document.removeEventListener('mouseup', this.handleMouseUp2D)
    document.removeEventListener('mousemove', this.handleMouseMove1D)
    document.removeEventListener('mouseup', this.handleMouseUp1D)
    document.removeEventListener('mousemove', this.handleMouseMoveAlpha)
    document.removeEventListener('mouseup', this.handleMouseUpAlpha)
  }

  static getDerivedStateFromProps(newProps: ColorPickerInnerProps) {
    return ColorPickerInner.calculateCurrentColor(newProps.value)
  }

  componentDidUpdate(prevProps: ColorPickerInnerProps) {
    if (this.RefFirstControl.current != null && prevProps.id !== this.props.id) {
      this.RefFirstControl.current.focus()
    }
  }

  handleHue = (hue: number) => {
    const moduloed = hue === 360 ? 360 : hue % 360
    return isNaN(hue) ? this.state.hsvH : Math.round(moduloed)
  }

  setNewHSV = (h: number, s: number, v: number, transient: boolean) => {
    const newChromaValue = Chroma({ h, s, v } as any).alpha(this.state.alpha)
    const newHex = newChromaValue.hex().toUpperCase()
    const newState = {
      hsvH: h,
      hsvS: s,
      hsvV: v,
      hex: newHex,
    }
    this.setState(newState)
    this.setCurrentColor(newChromaValue, transient)
  }

  setNewHex = (newHex: ColorPickerInnerState['hex']) => {
    try {
      const newChromaValue = Chroma.hex(newHex)
      const newHSV = newChromaValue.hsv()
      const newState = {
        hsvH: this.handleHue(newHSV[0]),
        hsvS: Number(newHSV[1].toFixed(2)),
        hsvV: Number(newHSV[2].toFixed(2)),
        hex: newChromaValue.hex().toUpperCase(),
        alpha: newChromaValue.alpha(),
      }
      this.setState(newState)
      this.setCurrentColor(newChromaValue, false)
    } catch (e) {
      console.warn(e)
    }
  }

  setNewAlpha = (newAlpha: ColorPickerInnerState['alpha'], transient: boolean) => {
    const newChromaValue = Chroma(this.state.hex).alpha(newAlpha)
    const newState = {
      alpha: newAlpha,
      hex: newChromaValue.hex().toUpperCase(),
    }
    this.setState(newState)
    this.setCurrentColor(newChromaValue, transient)
  }

  setCurrentColor = (chromaColor: Chroma.Color, transient: boolean) => {
    const newValue = toPassedInColorType(chromaColor, this.props.value)
    if (transient) {
      this.props.onTransientSubmitValue(newValue)
    } else {
      this.props.onSubmitValue(newValue)
    }
  }

  calculate2DPosition = (
    clientX: number,
    clientY: number,
  ): {
    clampedCoordinateValue: { x: number; y: number }
    dimension: { width: number; height: number }
  } => {
    if (this.Ref2D.current != null) {
      const dimension = {
        width: this.Ref2D.current.offsetWidth,
        height: this.Ref2D.current.offsetHeight,
      }
      const clampedCoordinateValue = {
        x: Math.max(0, Math.min(dimension.width, clientX - this.state.controlPosition2DX)),
        y: Math.max(0, Math.min(dimension.height, clientY - this.state.controlPosition2DY)),
      }
      return { clampedCoordinateValue, dimension }
    } else {
      return {
        clampedCoordinateValue: {
          x: 0,
          y: 0,
        },
        dimension: {
          width: 0,
          height: 0,
        },
      }
    }
  }

  calculateSV = (clientX: number, clientY: number): { s: number; v: number } => {
    const { clampedCoordinateValue, dimension } = this.calculate2DPosition(clientX, clientY)
    return {
      s: Number((clampedCoordinateValue.x / dimension.width).toFixed(2)),
      v: Number((1 - clampedCoordinateValue.y / dimension.height).toFixed(2)),
    }
  }

  setNewColor2D = (clientX: number, clientY: number, transient: boolean) => {
    if (this.Ref2D.current != null) {
      const { s, v } = this.calculateSV(clientX, clientY)
      this.setNewHSV(this.state.hsvH, s, v, transient)
    }
  }

  handleMouseDown2D = (e: React.MouseEvent<HTMLDivElement>) => {
    const { clientX, clientY } = e
    const { offsetX, offsetY } = e.nativeEvent
    this.setState(
      {
        controlPosition2DX: clientX - offsetX,
        controlPosition2DY: clientY - offsetY,
      },
      () => this.setNewColor2D(clientX, clientY, true),
    )
    document.addEventListener('mousemove', this.handleMouseMove2D)
    document.addEventListener('mouseup', this.handleMouseUp2D)
    e.stopPropagation()
  }

  handleMouseMove2D = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setNewColor2D(e.clientX, e.clientY, true)
    }
  }

  handleMouseUp2D = (e: MouseEvent) => {
    this.setNewColor2D(e.clientX, e.clientY, false)
    document.removeEventListener('mousemove', this.handleMouseMove2D)
    document.removeEventListener('mouseup', this.handleMouseUp2D)
    e.stopPropagation()
  }

  calculate1DPosition = (clientX: number): { x: number; width: number } => {
    if (this.RefAlpha.current != null) {
      const width = this.RefAlpha.current.offsetWidth
      const x = Math.max(0, Math.min(width, clientX - this.state.controlPosition1D))
      return { width, x }
    } else {
      return {
        x: 0,
        width: 0,
      }
    }
  }

  calculateHsvHue = (clientX: number): number => {
    const { x, width } = this.calculate1DPosition(clientX)
    return Math.round(360 * (x / width))
  }

  setNewColor1D = (clientX: number, transient: boolean) => {
    if (this.Ref2D.current != null) {
      const newHue = this.calculateHsvHue(clientX)
      this.setNewHSV(newHue, this.state.hsvS, this.state.hsvV, transient)
    }
  }

  handleMouseDown1D = (e: React.MouseEvent<HTMLDivElement>) => {
    const clientX = e.clientX
    const offsetX = e.nativeEvent.offsetX
    this.setState(
      {
        controlPosition1D: clientX - offsetX,
      },
      () => this.setNewColor1D(clientX, true),
    )
    document.addEventListener('mousemove', this.handleMouseMove1D)
    document.addEventListener('mouseup', this.handleMouseUp1D)
    e.stopPropagation()
  }

  handleMouseMove1D = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setNewColor1D(e.clientX, true)
    }
  }

  handleMouseUp1D = (e: MouseEvent) => {
    this.setNewColor1D(e.clientX, false)
    document.removeEventListener('mousemove', this.handleMouseMove1D)
    document.removeEventListener('mouseup', this.handleMouseUp1D)
    e.stopPropagation()
  }

  calculateAlphaPosition = (clientX: number): { x: number; width: number } => {
    if (this.RefAlpha.current != null) {
      const width = this.RefAlpha.current.offsetWidth
      const x = Math.max(0, Math.min(width, clientX - this.state.controlPositionAlpha))
      return { width, x }
    } else {
      return {
        x: 0,
        width: 0,
      }
    }
  }

  calculateAlpha = (clientX: number): number => {
    const { x, width } = this.calculateAlphaPosition(clientX)
    return Number((x / width).toFixed(2))
  }

  setNewColorAlpha = (clientX: number, transient: boolean) => {
    if (this.RefAlpha.current != null) {
      const newAlpha = this.calculateAlpha(clientX)
      this.setNewAlpha(newAlpha, transient)
    }
  }

  handleMouseDownAlpha = (e: React.MouseEvent<HTMLDivElement>) => {
    const clientX = e.clientX
    const offsetX = e.nativeEvent.offsetX
    this.setState(
      {
        controlPositionAlpha: clientX - offsetX,
      },
      () => this.setNewColorAlpha(clientX, true),
    )
    document.addEventListener('mousemove', this.handleMouseMoveAlpha)
    document.addEventListener('mouseup', this.handleMouseUpAlpha)
    e.stopPropagation()
  }

  handleMouseMoveAlpha = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setNewColorAlpha(e.clientX, true)
    }
  }

  handleMouseUpAlpha = (e: MouseEvent) => {
    this.setNewColorAlpha(e.clientX, false)
    document.removeEventListener('mousemove', this.handleMouseMoveAlpha)
    document.removeEventListener('mouseup', this.handleMouseUpAlpha)
  }

  onSubmitValueHue = (value: number | EmptyInputValue) => {
    const newHue = this.handleHue(fallbackOnEmptyInputValueToCSSEmptyValue(0, value))
    this.setNewHSV(newHue, this.state.hsvS, this.state.hsvV, false)
  }
  onTransientSubmitValueHue = (value: number | EmptyInputValue) => {
    const newHue = this.handleHue(fallbackOnEmptyInputValueToCSSEmptyValue(0, value))
    this.setNewHSV(newHue, this.state.hsvS, this.state.hsvV, true)
  }

  onSubmitValueSaturation = (value: number | EmptyInputValue) => {
    const newSaturation = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSV(this.state.hsvH, newSaturation, this.state.hsvV, false)
  }
  onTransientSubmitValueSaturation = (value: number | EmptyInputValue) => {
    const newSaturation = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSV(this.state.hsvH, newSaturation, this.state.hsvV, true)
  }

  onSubmitValueValue = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSV(this.state.hsvH, this.state.hsvS, newValue, false)
  }
  onTransientSubmitValueValue = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSV(this.state.hsvH, this.state.hsvS, newValue, true)
  }

  onSubmitValueAlpha = (value: number | EmptyInputValue) => {
    this.setNewAlpha(Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2)), false)
  }
  onTransientSubmitValueAlpha = (value: number | EmptyInputValue) => {
    this.setNewAlpha(Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2)), true)
  }

  onChangeValueHex = (e: React.ChangeEvent<HTMLInputElement>) =>
    this.setState({ hex: e.target.value })

  onBlurValueHex = (_: React.FocusEvent<HTMLInputElement>) => {
    this.setNewHex(this.state.hex)
  }

  static calculateCurrentColor = (color: CSSColor) => {
    const chroma = cssColorToChromaColorOrDefault(color)
    const hsv = chroma.hsv()
    const h = isNaN(hsv[0]) ? 0 : Math.round(hsv[0])
    const s = Number(hsv[1].toFixed(2))
    const v = Number(hsv[2].toFixed(2))
    const hex = chroma.hex().toUpperCase()
    return {
      hsvH: h,
      hsvS: s,
      hsvV: v,
      hex,
      alpha: parseAlphaFromCSSColor(color),
    }
  }

  render() {
    const h = this.state.hsvH
    const s = this.state.hsvS
    const v = this.state.hsvV
    const hue = Chroma(h, 1, 1, 'hsv').css()
    const color = Chroma({ h, s, v } as any)
    const cssWithAlpha = color.alpha(this.state.alpha).css()
    const cssWith1Alpha = color.alpha(1).css()
    const cssWith0Alpha = color.alpha(0).css()

    const hsvHue = `
      linear-gradient(to right,
        red 0%,
        yellow 16.66%,
        lime 33.33%,
        aqua 50%,
        blue 66.66%,
        fuchsia 83.33%,
        red 100%
      )`

    return (
      <div>
        <div
          className='colorPicker-2d'
          onMouseDown={this.handleMouseDown2D}
          ref={this.Ref2D}
          style={{
            height: MetadataEditorModalPreviewHeight,
            width: '100%',
            backgroundColor: hue,
            backgroundImage: `
                  linear-gradient(to top, #000, rgba(0, 0, 0, 0)),
                  linear-gradient(to right, #fff, rgba(255, 255, 255, 0))`,
            paddingBottom: 4,
            position: 'relative',
            display: 'flex',
            flexWrap: 'wrap',
          }}
        >
          <div
            className='colorPicker-2d-indicator'
            style={{
              width: 8,
              height: 8,
              backgroundColor: cssWith1Alpha,
              borderRadius: '50%',
              boxShadow:
                'inset 0 0 1px rgba(0, 0, 0, 0.24), 0 0 0 2px white, 0 0 2px 2px rgba(0, 0, 0, 0.24)',
              position: 'absolute',
              margin: -4,
              left: `${s * 100}%`,
              top: `${(1 - v) * 100}%`,
              pointerEvents: 'none',
            }}
          />
        </div>
        <div
          style={{
            padding: inspectorEdgePadding,
          }}
        >
          <div
            className='colorPicker-1d'
            onMouseDown={this.handleMouseDown1D}
            style={{
              height: 20,
              width: '100%',
              borderRadius: 4,
              boxShadow: `inset 0 0 0 1px ${ControlStyleDefaults.SetBorderColor})`,
              backgroundImage: hsvHue,
              padding: '0 4px',
            }}
          >
            <div style={{ position: 'relative' }}>
              <div
                className='colorPicker-1d-indicator'
                style={{
                  width: 8,
                  height: 20,
                  backgroundColor: hue,
                  borderRadius: 1,
                  boxShadow: `inset 0 0 1px rgba(0, 0, 0, 0.24), 0px 0px 0px 2px white, 0px 0px 2px 2px rgba(0, 0, 0, 0.239216)`,
                  position: 'absolute',
                  margin: '0 -4px',
                  left: `${(this.handleHue(h) / 360) * 100}%`,
                  top: 0,
                  pointerEvents: 'none',
                }}
              />
            </div>
          </div>
          <div
            className='colorPicker-alpha'
            ref={this.RefAlpha}
            onMouseDown={this.handleMouseDownAlpha}
            style={{
              height: 20,
              width: '100%',
              backgroundColor: 'white',
              borderRadius: 4,
              boxShadow: `inset 0 0 0 1px ${ControlStyleDefaults.SetBorderColor})`,
              backgroundImage: `
                    linear-gradient(to right, ${cssWith0Alpha} 0%, ${cssWith1Alpha} 100%),
                    ${checkerboardBackground.backgroundImage}`,
              backgroundSize: `100% 100%, ${checkerboardBackground.backgroundSize}`,
              backgroundPosition: `0 0, ${checkerboardBackground.backgroundPosition}`,
              marginTop: 10,
              padding: '0 4px',
            }}
          >
            <div style={{ position: 'relative' }}>
              <div
                className='colorPicker-alpha-indicator'
                style={{
                  width: 8,
                  height: 20,
                  backgroundImage: `
                        linear-gradient(${cssWithAlpha}, ${cssWithAlpha}),
                        ${checkerboardBackground.backgroundImage},
                        linear-gradient(white, white)`,
                  backgroundSize: `100% 100%, ${checkerboardBackground.backgroundSize}, 100% 100%`,
                  backgroundPosition: `0 0, ${checkerboardBackground.backgroundPosition}, 0 0`,
                  borderRadius: 1,
                  boxShadow: `inset 0 0 1px rgba(0, 0, 0, 0.24), 0px 0px 0px 2px white, 0px 0px 2px 2px rgba(0, 0, 0, 0.239216)`,
                  position: 'absolute',
                  margin: '0 -4px',
                  left: `${this.state.alpha * 100}%`,
                  top: 0,
                  pointerEvents: 'none',
                }}
              />
            </div>
          </div>
        </div>
        <div
          className='colorPicker-controls'
          style={{
            display: 'grid',
            gridTemplateColumns: '80px repeat(4, 1fr)',
            columnGap: 4,
            padding: '0 8px 8px',
          }}
        >
          <StringInput
            ref={this.RefFirstControl}
            key={this.props.id}
            value={this.state.hex}
            onChange={this.onChangeValueHex}
            onBlur={this.onBlurValueHex}
            id='colorPicker-controls-hex'
            style={{
              gridColumn: 'span 1',
            }}
            labelBelow='Hex'
          />
          <SimpleNumberInput
            value={this.state.hsvH}
            id='colorPicker-controls-hue'
            onSubmitValue={this.onSubmitValueHue}
            onTransientSubmitValue={this.onTransientSubmitValueHue}
            minimum={0}
            maximum={360}
            labelBelow='H'
            labelInner={
              <Icn
                category='layout/systems'
                type='transform-rotate'
                color='gray'
                width={10}
                height={10}
              />
            }
          />
          <SimplePercentInput
            value={this.state.hsvS}
            id='colorPicker-controls-saturation'
            onSubmitValue={this.onSubmitValueSaturation}
            onTransientSubmitValue={this.onTransientSubmitValueSaturation}
            style={{ gridColumn: 'span 1' }}
            minimum={0}
            maximum={1}
            stepSize={0.01}
            labelBelow='S'
          />
          <SimplePercentInput
            value={this.state.hsvV}
            id='colorPicker-controls-value'
            onSubmitValue={this.onSubmitValueValue}
            onTransientSubmitValue={this.onTransientSubmitValueValue}
            style={{ gridColumn: 'span 1' }}
            minimum={0}
            maximum={1}
            stepSize={0.01}
            labelBelow='V'
          />
          <SimplePercentInput
            value={this.state.alpha}
            id='colorPicker-controls-alpha'
            onSubmitValue={this.onSubmitValueAlpha}
            onTransientSubmitValue={this.onTransientSubmitValueAlpha}
            style={{ gridColumn: 'span 1' }}
            minimum={0}
            maximum={1}
            stepSize={0.01}
            labelBelow='A'
          />
        </div>
      </div>
    )
  }
}
