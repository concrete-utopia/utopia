import Chroma from 'chroma-js'
import React from 'react'
import type { WindowPoint } from '../../../core/shared/math-utils'
import { clamp, roundTo } from '../../../core/shared/math-utils'
import { getControlStyles } from '../common/control-styles'
import type { CSSColor, EmptyInputValue } from '../common/css-utils'
import {
  cssColor,
  cssColorToChromaColorOrDefault,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isHex,
  isHSL,
  isKeyword,
} from '../common/css-utils'
import { inspectorEdgePadding } from '../sections/style-section/background-subsection/background-picker'
import { InspectorModal } from '../widgets/inspector-modal'
import { StringControl } from './string-control'
//TODO: switch to functional component and make use of 'useColorTheme':
import {
  colorTheme,
  FlexRow,
  Icn,
  Icons,
  SimpleNumberInput,
  SimplePercentInput,
  SquareButton,
  UtopiaStyles,
} from '../../../uuiui'
import { pickColorWithEyeDropper } from '../../canvas/canvas-utils'
import { ColorPickerSwatches } from './color-picker-swatches'
import { didWeHandleMouseMoveForThisFrame, mouseMoveHandled } from '../../../components/mouse-move'
import { clearColorPickerElement, setColorPickerElement } from './color-picker-utils'

const checkerboardBackground = UtopiaStyles.backgrounds.checkerboardBackground

export interface ColorPickerProps extends ColorPickerInnerProps {
  closePopup: () => void
  portalTarget?: HTMLElement
  disabled: boolean
}

export const colorPickerWidth = 290
export const MetadataEditorModalPreviewHeight = 150

export const GradientStopSize = 24
export const GradientStopCaratSize = 5
export const StopsPadding = GradientStopSize / 2 + inspectorEdgePadding
export const GradientPickerWidth = colorPickerWidth - StopsPadding * 2

export const ColorPicker: React.FunctionComponent<React.PropsWithChildren<ColorPickerProps>> = ({
  closePopup,
  portalTarget,
  ...props
}) => {
  const onSubmitValue = props.onSubmitValue
  const onClickEyeDropper = React.useCallback(() => {
    closePopup()
    void pickColorWithEyeDropper()
      .then(({ sRGBHex }) => {
        onSubmitValue(cssColor(sRGBHex))
      })
      .catch((e) => console.error(e))
  }, [closePopup, onSubmitValue])
  return (
    <InspectorModal
      offsetX={props.offsetX - colorPickerWidth}
      offsetY={props.offsetY}
      closePopup={closePopup}
      outsideClickIgnoreClass={`ignore-react-onclickoutside-${props.id}`}
    >
      <div
        id={props.id}
        className='colorPicker-wrapper'
        style={{
          width: colorPickerWidth,
          position: 'absolute',
          overflow: 'hidden',
          zIndex: 2,
          marginBottom: 32,
          ...UtopiaStyles.popup,
        }}
      >
        <FlexRow
          style={{ justifyContent: 'flex-end', gap: 2, height: 32, padding: '8px 8px 14px 8px' }}
        >
          <SquareButton highlight onClick={onClickEyeDropper}>
            <Icons.SmallPipette />
          </SquareButton>
          <SquareButton highlight onClick={closePopup}>
            <Icons.SmallCross />
          </SquareButton>
        </FlexRow>
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
  testId: string
  disabled: boolean
}

function toPassedInColorType(
  color: Chroma.Color,
  passedInColor: CSSColor,
  stateNormalisedHuePosition: number,
): CSSColor {
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
      hex: color.hex('auto').toUpperCase(),
    }
  } else if (isHSL(passedInColor)) {
    const [h, s, l, a] = color.hsl() as any as [number, number, number, number] // again, the types are out of date
    return {
      type: 'HSL',
      h: getSafeHue(h, stateNormalisedHuePosition),
      s: roundTo(s * 100, 0),
      l: roundTo(l * 100, 0),
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

function deriveStateFromNewColor(
  color: Chroma.Color,
  stateNormalisedHuePosition: number,
): ColorPickerInnerState {
  const [h, s, v] = color.hsv()
  return {
    normalisedHuePosition: getSafeHue(h, stateNormalisedHuePosition) / 360,
    normalisedSaturationPosition: s,
    normalisedValuePosition: v,
    normalisedAlphaPosition: color.alpha(),
    dirty: false,
    isScrubbing: false,
    swatchColor: color.hex(),
  }
}

/**
 * All shades of grey don't have an HSV hue component, so Chroma returns NaN,
 * this safeguards against that.
 */
function getSafeHue(hue: number, stateNormalisedValue: number) {
  return Math.round(isNaN(hue) ? stateNormalisedValue * 360 : hue)
}

/**
 * State values for the HSVa sliders, normalised to 0.0—1.0.
 */
interface ColorPickerPositions {
  normalisedHuePosition: number
  normalisedSaturationPosition: number
  normalisedValuePosition: number
  normalisedAlphaPosition: number
}

export interface ColorPickerInnerState extends ColorPickerPositions {
  dirty: boolean
  isScrubbing: boolean
  swatchColor: string
}

export class ColorPickerInner extends React.Component<
  ColorPickerInnerProps,
  ColorPickerInnerState
> {
  private RefFirstControl = React.createRef<HTMLInputElement>()

  private fullWidth = colorPickerWidth
  private fullHeight = MetadataEditorModalPreviewHeight
  private paddedWidth = this.fullWidth - inspectorEdgePadding * 2

  private SVControlRef = React.createRef<HTMLDivElement>()
  private SVControlIndicatorRef = React.createRef<HTMLDivElement>()
  private SVOrigin: WindowPoint = { x: 0, y: 0 } as WindowPoint
  private HueControlRef = React.createRef<HTMLDivElement>()
  private HueControlIndicatorRef = React.createRef<HTMLDivElement>()
  private HueOriginLeft: number = 0
  private AlphaControlRef = React.createRef<HTMLDivElement>()
  private AlphaControlIndicatorRef = React.createRef<HTMLDivElement>()
  private AlphaOriginLeft: number = 0

  constructor(props: ColorPickerInnerProps) {
    super(props)
    // Color values in different spaces need to be stored in state due to them not mapping 1:1 across color spaces. E.g. HSV(0, 0%, 100%) == HSV(180, 0%, 100%). A fully controlled component would be nice, but alas.
    const calculatedState = deriveStateFromNewColor(
      cssColorToChromaColorOrDefault(this.props.value),
      0,
    )
    this.state = {
      ...calculatedState,
      dirty: false,
      isScrubbing: false,
      swatchColor: this.hexFromHue(
        calculatedState.normalisedHuePosition,
        calculatedState.normalisedSaturationPosition,
        calculatedState.normalisedValuePosition,
        calculatedState.normalisedAlphaPosition,
      ),
    }
  }

  componentDidMount() {
    setTimeout(() => {
      // wrapping in a setTimeout so we don't dispatch from inside React lifecycle
      if (this.RefFirstControl.current != null) {
        this.RefFirstControl.current.focus()
      }
      this.setIndicators()
    }, 0)
  }

  componentWillUnmount() {
    clearColorPickerElement()
    document.removeEventListener('mousemove', this.onSVMouseMove)
    document.removeEventListener('mouseup', this.onSVMouseUp)
    document.removeEventListener('mousemove', this.onHueSliderMouseMove)
    document.removeEventListener('mouseup', this.onHueSliderMouseUp)
    document.removeEventListener('mousemove', this.onAlphaSliderMouseMove)
    document.removeEventListener('mouseup', this.onAlphaSliderMouseUp)
  }

  static getHexaColorFromControlPositionState(state: ColorPickerInnerState): string {
    const h = state.normalisedHuePosition
    const s = state.normalisedSaturationPosition
    const v = state.normalisedValuePosition
    const a = state.normalisedAlphaPosition
    return Chroma(h * 360, s, v, 'hsv')
      .alpha(a)
      .hex('auto')
  }

  static getDerivedStateFromProps(
    props: ColorPickerInnerProps,
    state: ColorPickerInnerState,
  ): Partial<ColorPickerInnerState> | null {
    const chroma = cssColorToChromaColorOrDefault(props.value)
    if (state.dirty) {
      return {
        dirty: false,
        swatchColor: state.swatchColor,
      }
    } else {
      const controlStateHexa = ColorPickerInner.getHexaColorFromControlPositionState(state)
      const newPropsHexa = chroma.hex('auto')
      if (controlStateHexa === newPropsHexa) {
        return null
      } else {
        const newCalculatedState = deriveStateFromNewColor(chroma, state.normalisedHuePosition)
        return newCalculatedState
      }
    }
  }

  componentDidUpdate(prevProps: ColorPickerInnerProps, prevState: ColorPickerInnerState) {
    if (this.RefFirstControl.current != null && prevProps.id !== this.props.id) {
      this.RefFirstControl.current.focus()
    }
    if (
      this.state.normalisedSaturationPosition !== prevState.normalisedSaturationPosition ||
      this.state.normalisedValuePosition !== prevState.normalisedValuePosition ||
      this.state.normalisedHuePosition !== prevState.normalisedHuePosition ||
      this.state.normalisedAlphaPosition !== prevState.normalisedAlphaPosition
    ) {
      this.setIndicators()
    }
  }

  setNewHSVa = (
    h: number = this.state.normalisedHuePosition * 360,
    s: number = this.state.normalisedSaturationPosition,
    v: number = this.state.normalisedValuePosition,
    a: number = this.state.normalisedAlphaPosition,
    transient: boolean,
  ) => {
    const newChromaValue = Chroma(h, s, v, 'hsv').alpha(a)
    this.setState({
      normalisedHuePosition: h / 360,
      normalisedSaturationPosition: s,
      normalisedValuePosition: v,
      normalisedAlphaPosition: a,
      dirty: true,
    })
    this.submitNewColor(newChromaValue, transient)
  }

  setNewHex = (newHex: string) => {
    try {
      const newChromaValue = Chroma.hex(newHex)
      const newHSV = newChromaValue.hsv()
      this.setState((state) => ({
        normalisedHuePosition: getSafeHue(newHSV[0], state.normalisedHuePosition) / 360,
        normalisedSaturationPosition: newHSV[1],
        normalisedValuePosition: newHSV[2],
        hexa: newChromaValue.hex('auto').toUpperCase(),
        normalisedAlphaPosition: newChromaValue.alpha(),
        dirty: true,
        isScrubbing: state.isScrubbing,
        swatchColor: newChromaValue.hex('auto').toUpperCase(),
      }))
      this.submitNewColor(newChromaValue, false)
    } catch (e) {
      console.warn(e)
    }
  }

  submitNewColor = (chromaColor: Chroma.Color, transient: boolean) => {
    const newValue = toPassedInColorType(
      chromaColor,
      this.props.value,
      this.state.normalisedHuePosition,
    )
    if (transient) {
      this.props.onTransientSubmitValue(newValue)
    } else {
      this.props.onSubmitValue(newValue)
    }
  }

  // Saturation and Value (SV) slider functions
  setSVFromClientPosition = (clientX: number, clientY: number, transient: boolean) => {
    const x = clamp(0, this.fullWidth, clientX - this.SVOrigin.x)
    const y = clamp(0, this.fullHeight, clientY - this.SVOrigin.y)

    const newS = x / this.fullWidth
    const newV = 1 - y / this.fullHeight

    this.setNewHSVa(undefined, newS, newV, undefined, transient)
  }

  onMouseDownSV = (e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
    if (this.props.disabled) {
      return
    }
    this.setState({
      isScrubbing: true,
    })
    if (this.SVControlRef.current != null) {
      const origin = this.SVControlRef.current.getBoundingClientRect()
      this.SVOrigin = { x: origin.left, y: origin.top } as WindowPoint

      this.setSVFromClientPosition(e.clientX, e.clientY, true)

      document.addEventListener('mousemove', this.onSVMouseMove)
      document.addEventListener('mouseup', this.onSVMouseUp)
    }
  }

  onSVMouseMove = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setSVFromClientPosition(e.clientX, e.clientY, true)
    }
  }

  hexFromHue = (h: number, s: number, v: number, a: number): string => {
    return Chroma(h * 360, s, v, 'hsv')
      .alpha(a)
      .hex('auto')
      .toUpperCase()
  }

  hexFromState = (): string => {
    return this.hexFromHue(
      this.state.normalisedHuePosition,
      this.state.normalisedSaturationPosition,
      this.state.normalisedValuePosition,
      this.state.normalisedAlphaPosition,
    )
  }

  updateSwatchColor = (): void => {
    this.setState({ swatchColor: this.hexFromState() })
  }

  onSVMouseUp = (e: MouseEvent) => {
    this.setSVFromClientPosition(e.clientX, e.clientY, false)
    document.removeEventListener('mousemove', this.onSVMouseMove)
    document.removeEventListener('mouseup', this.onSVMouseUp)
    this.setState({
      isScrubbing: false,
      swatchColor: this.hexFromState(),
    })
    e.stopPropagation()
  }

  // Hue slider functions
  setHueFromClientX = (clientX: number, transient: boolean) => {
    const x = clamp(0, this.paddedWidth, clientX - this.HueOriginLeft)
    const newHue = Math.round(360 * (x / this.paddedWidth))
    this.setNewHSVa(newHue, undefined, undefined, undefined, transient)
  }

  onHueSliderMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
    if (this.props.disabled) {
      return
    }

    if (this.HueControlRef.current != null) {
      this.HueOriginLeft = this.HueControlRef.current.getBoundingClientRect().left
      const clientX = e.clientX
      this.setHueFromClientX(clientX, true)
      this.setState({
        isScrubbing: true,
      })
      document.addEventListener('mousemove', this.onHueSliderMouseMove)
      document.addEventListener('mouseup', this.onHueSliderMouseUp)
    }
  }

  onHueSliderMouseMove = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setHueFromClientX(e.clientX, true)
    }
  }

  onHueSliderMouseUp = (e: MouseEvent) => {
    this.setHueFromClientX(e.clientX, false)
    document.removeEventListener('mousemove', this.onHueSliderMouseMove)
    document.removeEventListener('mouseup', this.onHueSliderMouseUp)
    this.setState({
      isScrubbing: false,
      swatchColor: this.hexFromState(),
    })
    e.stopPropagation()
  }

  // Alpha slider functions
  setAlphaFromClientX = (clientX: number, transient: boolean) => {
    const x = clamp(0, this.paddedWidth, clientX - this.AlphaOriginLeft)
    const newAlpha = Number((x / this.paddedWidth).toFixed(2))
    this.setNewHSVa(undefined, undefined, undefined, newAlpha, transient)
  }

  onAlphaSliderMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
    if (this.props.disabled) {
      return
    }

    if (this.AlphaControlRef.current != null) {
      this.setState({
        isScrubbing: true,
      })
      this.AlphaOriginLeft = this.AlphaControlRef.current.getBoundingClientRect().left
      const clientX = e.clientX
      this.setAlphaFromClientX(clientX, true)
      document.addEventListener('mousemove', this.onAlphaSliderMouseMove)
      document.addEventListener('mouseup', this.onAlphaSliderMouseUp)
    }
  }

  onAlphaSliderMouseMove = (e: MouseEvent) => {
    e.stopPropagation()
    if (!didWeHandleMouseMoveForThisFrame) {
      mouseMoveHandled()
      this.setAlphaFromClientX(e.clientX, true)
    }
  }

  onAlphaSliderMouseUp = (e: MouseEvent) => {
    this.setAlphaFromClientX(e.clientX, false)
    document.removeEventListener('mousemove', this.onAlphaSliderMouseMove)
    document.removeEventListener('mouseup', this.onAlphaSliderMouseUp)
    this.setState({
      isScrubbing: false,
      swatchColor: this.hexFromState(),
    })
  }

  // SubmitValue functions
  onSubmitValueHue = (value: number | EmptyInputValue) => {
    const newHue = getSafeHue(
      fallbackOnEmptyInputValueToCSSEmptyValue(0, value),
      this.state.normalisedHuePosition,
    )
    this.setNewHSVa(newHue, undefined, undefined, undefined, false)
    this.updateSwatchColor()
  }
  onTransientSubmitValueHue = (value: number | EmptyInputValue) => {
    const newHue = getSafeHue(
      fallbackOnEmptyInputValueToCSSEmptyValue(0, value),
      this.state.normalisedHuePosition,
    )
    this.setNewHSVa(newHue, undefined, undefined, undefined, true)
  }

  onSubmitValueSaturation = (value: number | EmptyInputValue) => {
    const newSaturation = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, newSaturation, undefined, undefined, false)
    this.updateSwatchColor()
  }
  onTransientSubmitValueSaturation = (value: number | EmptyInputValue) => {
    const newSaturation = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, newSaturation, undefined, undefined, true)
  }

  onSubmitHSVValueValue = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, undefined, newValue, undefined, false)
    this.updateSwatchColor()
  }
  onTransientSubmitHSVValueValue = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, undefined, newValue, undefined, true)
  }

  onSubmitValueAlpha = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, undefined, undefined, newValue, false)
    this.updateSwatchColor()
  }
  onTransientSubmitValueAlpha = (value: number | EmptyInputValue) => {
    const newValue = Number(fallbackOnEmptyInputValueToCSSEmptyValue(100, value).toFixed(2))
    this.setNewHSVa(undefined, undefined, undefined, newValue, true)
  }

  onSubmitValueHex = (newValue: string) => {
    this.setNewHex(newValue)
  }

  setIndicators = () => {
    if (this.SVControlIndicatorRef.current != null) {
      this.SVControlIndicatorRef.current.style.left = `${
        this.state.normalisedSaturationPosition * 100
      }%`
      this.SVControlIndicatorRef.current.style.top = `${
        (1 - this.state.normalisedValuePosition) * 100
      }%`
    }
    if (this.HueControlIndicatorRef.current != null) {
      this.HueControlIndicatorRef.current.style.left = `${this.state.normalisedHuePosition * 100}%`
    }
    if (this.AlphaControlIndicatorRef.current != null) {
      this.AlphaControlIndicatorRef.current.style.left = `${
        this.state.normalisedAlphaPosition * 100
      }%`
    }
  }

  render() {
    const h = this.state.normalisedHuePosition
    const s = this.state.normalisedSaturationPosition
    const v = this.state.normalisedValuePosition
    const hueColor = Chroma(h * 360, 1, 1, 'hsv').css()
    const chroma = Chroma(h * 360, s, v, 'hsv').alpha(this.state.normalisedAlphaPosition)
    const cssWithAlpha = chroma.css()
    const cssWith1Alpha = chroma.alpha(1).css()
    const cssWith0Alpha = chroma.alpha(0).css()
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
      <div
        style={{ position: 'relative' }}
        ref={(node) => {
          setColorPickerElement(node)
        }}
      >
        <div>
          <div
            className='colorPicker-saturation-and-value'
            onMouseDown={this.onMouseDownSV}
            ref={this.SVControlRef}
            style={{
              height: MetadataEditorModalPreviewHeight,
              width: '100%',
              backgroundColor: hueColor,
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
              className='colorPicker-saturation-and-value-indicator'
              ref={this.SVControlIndicatorRef}
              style={{
                width: 8,
                height: 8,
                backgroundColor: cssWith1Alpha,
                borderRadius: '50%',
                boxShadow:
                  'inset 0 0 1px rgba(0, 0, 0, 0.24), 0 0 0 2px white, 0 0 2px 2px rgba(0, 0, 0, 0.24)',
                position: 'absolute',
                margin: -4,
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
              className='colorPicker-hue'
              ref={this.HueControlRef}
              onMouseDown={this.onHueSliderMouseDown}
              style={{
                height: 20,
                width: '100%',
                borderRadius: 4,
                boxShadow: `0 0 0 1px ${colorTheme.neutralBorder.value} inset`,
                backgroundImage: hsvHue,
                padding: '0 4px',
              }}
            >
              <div style={{ position: 'relative' }}>
                <div
                  className='colorPicker-hue-indicator'
                  ref={this.HueControlIndicatorRef}
                  style={{
                    width: 8,
                    height: 20,
                    backgroundColor: hueColor,
                    borderRadius: 1,
                    boxShadow: `inset 0 0 1px rgba(0, 0, 0, 0.24), 0px 0px 0px 2px white, 0px 0px 2px 2px rgba(0, 0, 0, 0.239216)`,
                    position: 'absolute',
                    margin: '0 -4px',
                    top: 0,
                    pointerEvents: 'none',
                  }}
                />
              </div>
            </div>
            <div
              className='colorPicker-alpha'
              ref={this.AlphaControlRef}
              onMouseDown={this.onAlphaSliderMouseDown}
              style={{
                height: 20,
                width: '100%',
                backgroundColor: 'white',
                borderRadius: 4,
                boxShadow: `0 0 0 1px ${colorTheme.neutralBorder.value} inset`,
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
                  ref={this.AlphaControlIndicatorRef}
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
            <StringControl
              ref={this.RefFirstControl}
              key={this.props.id}
              value={chroma.hex('auto').toUpperCase()}
              onSubmitValue={this.onSubmitValueHex}
              controlStatus='simple'
              controlStyles={getControlStyles('simple')}
              id='colorPicker-controls-hex'
              testId={`${this.props.testId}-colorPicker-controls-hex`}
              style={{
                gridColumn: 'span 1',
              }}
              DEPRECATED_controlOptions={{ DEPRECATED_labelBelow: 'Hex' }}
            />
            <SimpleNumberInput
              value={this.state.normalisedHuePosition * 360}
              id='colorPicker-controls-hue'
              testId={`${this.props.testId}-colorPicker-controls-hue`}
              onSubmitValue={this.onSubmitValueHue}
              onTransientSubmitValue={this.onTransientSubmitValueHue}
              onForcedSubmitValue={this.onSubmitValueHue}
              minimum={0}
              maximum={360}
              innerLabel='H'
              defaultUnitToHide={null}
            />
            <SimplePercentInput
              value={Number(this.state.normalisedSaturationPosition.toFixed(2))}
              id='colorPicker-controls-saturation'
              testId={`${this.props.testId}-colorPicker-controls-saturation`}
              onSubmitValue={this.onSubmitValueSaturation}
              onTransientSubmitValue={this.onTransientSubmitValueSaturation}
              onForcedSubmitValue={this.onSubmitValueSaturation}
              style={{ gridColumn: 'span 1' }}
              minimum={0}
              maximum={1}
              stepSize={0.01}
              innerLabel='S'
              defaultUnitToHide={null}
            />
            <SimplePercentInput
              value={Number(this.state.normalisedValuePosition.toFixed(2))}
              id='colorPicker-controls-value'
              testId={`${this.props.testId}-colorPicker-controls-value`}
              onSubmitValue={this.onSubmitHSVValueValue}
              onTransientSubmitValue={this.onTransientSubmitHSVValueValue}
              onForcedSubmitValue={this.onSubmitHSVValueValue}
              style={{ gridColumn: 'span 1' }}
              minimum={0}
              maximum={1}
              stepSize={0.01}
              innerLabel='V'
              defaultUnitToHide={null}
            />
            <SimplePercentInput
              value={this.state.normalisedAlphaPosition}
              id='colorPicker-controls-alpha'
              testId={`${this.props.testId}-colorPicker-controls-alpha`}
              onSubmitValue={this.onSubmitValueAlpha}
              onTransientSubmitValue={this.onTransientSubmitValueAlpha}
              onForcedSubmitValue={this.onSubmitValueAlpha}
              style={{ gridColumn: 'span 1' }}
              minimum={0}
              maximum={1}
              stepSize={0.01}
              innerLabel='A'
              defaultUnitToHide={null}
            />
          </div>
          <ColorPickerSwatches
            onSelectColor={this.onSubmitValueHex}
            currentColor={this.state.swatchColor}
          />
        </div>
        {/* hover preventer: don't trigger other hover events while sliding / scrubbing */}
        {this.state.isScrubbing ? (
          <div
            style={{
              position: 'fixed',
              left: 0,
              top: 0,
              right: 0,
              bottom: 0,
              background: 'transparent',
            }}
          />
        ) : null}
      </div>
    )
  }
}
