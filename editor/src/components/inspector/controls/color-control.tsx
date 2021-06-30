import React = require('react')
import { isRight } from '../../../core/shared/either'
import { ColorPicker } from './color-picker'
import { CSSColor, parseColor, cssColorToChromaColorOrDefault } from '../common/css-utils'
import { StringControl } from './string-control'
import { ControlStatus, ControlStyles } from '../common/control-status'
import { useColorTheme, UtopiaTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import Utils from '../../../utils/utils'

export interface ColorControlProps {
  value: CSSColor
  onSubmitValue: (value: CSSColor) => void
  onTransientSubmitValue: (value: CSSColor) => void
  id: string
  testId: string
  key: string
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  openPopup?: (id: string) => void
  closePopup?: () => void
  controlClassName?: string
  style?: React.CSSProperties
  pickerOffset?: {
    x: number
    y: number
  }
  showString?: boolean
  onSubmitSolidStringValue?: (value: string) => void
}

export function updateStringCSSColor(newValue: string, oldValue: CSSColor) {
  const parsed = parseColor(newValue)
  if (isRight(parsed)) {
    return parsed.value
  } else {
    return oldValue
  }
}

export const ColorControl = betterReactMemo('ColorControl', (props: ColorControlProps) => {
  const [popupOpen, setPopupOpen] = React.useState(false)
  const colorTheme = useColorTheme()

  const stringInput =
    props.showString && props.onSubmitSolidStringValue != null ? (
      <StringControl
        id={`string-${props.id}`}
        key={'color-string'}
        testId={'color-control-string-control'}
        value={cssColorToChromaColorOrDefault(props.value).hex('rgba').toUpperCase()}
        readOnly={props.controlStyles.interactive}
        onSubmitValue={props.onSubmitSolidStringValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        DEPRECATED_controlOptions={{
          labelBelow: 'hex',
        }}
        style={{
          marginLeft: 8,
        }}
      />
    ) : null

  let backgroundLayer: { backgroundImage?: string } = {}
  const [r, g, b, a] = cssColorToChromaColorOrDefault(props.value).rgba()
  const rgbString = `rgba(${r}, ${g}, ${b})`
  const rgbaString = `rgba(${r}, ${g}, ${b}, ${a})`
  backgroundLayer = {
    backgroundImage: `linear-gradient(to bottom right, transparent 65%, ${rgbString} 65%), linear-gradient(${rgbaString}, ${rgbaString})`,
  }
  const pickerOffset = props.pickerOffset != null ? props.pickerOffset : { x: 0, y: 0 }

  const closePopup = React.useCallback(() => setPopupOpen(false), [setPopupOpen])

  const picker = !popupOpen ? null : (
    <ColorPicker
      id={props.id}
      testId={`${props.testId}-color-picker`}
      offsetX={pickerOffset.x}
      offsetY={pickerOffset.y}
      closePopup={closePopup}
      value={props.value}
      onSubmitValue={props.onSubmitValue}
      onTransientSubmitValue={props.onTransientSubmitValue}
    />
  )

  return (
    <div
      key={props.id}
      id={`trigger-${props.id}`}
      className={` hexField ${Utils.pathOr('', ['controlClassName'], props)}`}
      style={props.style}
    >
      {picker}
      <div className={`widget-color-control relative`} key={`${props.id}-surround`}>
        <div
          key={`${props.id}-color`}
          className={'color-control'}
          style={{
            backgroundImage: `linear-gradient(to bottom left,   #e7e7e7 25%,      transparent 25%),
                              linear-gradient(to bottom left,   transparent 75%,  #e7e7e7 75%),
                              linear-gradient(to bottom right,  #e7e7e7 25%,      transparent 25%),
                              linear-gradient(to bottom right,  transparent 75%,  #e7e7e7 75%)`,
            backgroundColor: 'white',
            backgroundSize: '12px 12px',
            backgroundPosition: '-6px 0px, 0px -6px, 6px 6px, 0 0',
            borderRadius: UtopiaTheme.inputBorderRadius,
            height: 20,
            width: 24,
            flex: '0 0 28px',
            margin: 1,
          }}
          onMouseDown={(e) => {
            e.stopPropagation()
            setPopupOpen((value) => !value)
          }}
        >
          <div
            className='colorcontrol-colorfield'
            style={{
              boxShadow: `0 0 0 1px ${colorTheme.secondaryBorder.value}`,
              height: '100%',
              width: '100%',
              borderRadius: UtopiaTheme.inputBorderRadius,
              ...backgroundLayer,
            }}
          />
        </div>
        {stringInput}
      </div>
    </div>
  )
})

export const StringColorControl = betterReactMemo(
  'StringColorControl',
  (props: ColorControlProps) => {
    const color = props.value
    if (props.onSubmitSolidStringValue == null) {
      return null
    }

    return (
      <StringControl
        id={`string-${props.id}`}
        testId={`color-picker-string-control-${props.testId}`}
        key={'color-string'}
        style={props.style}
        value={cssColorToChromaColorOrDefault(color).hex('rgba').toUpperCase()}
        readOnly={props.controlStyles.interactive}
        onSubmitValue={props.onSubmitSolidStringValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        DEPRECATED_controlOptions={{
          labelBelow: 'hex',
        }}
      />
    )
  },
)
