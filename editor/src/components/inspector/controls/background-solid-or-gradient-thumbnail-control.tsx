import React from 'react'
import type { UseSubmitValueFactory } from '../common/property-path-hooks'
import { isRight } from '../../../core/shared/either'
import type { ControlStyles } from '../common/control-styles'
import type { ControlStatus } from '../common/control-status'
import type {
  CSSBackground,
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSColor,
  CSSGradientBackgroundLayer,
  CSSSolidBackgroundLayer,
} from '../common/css-utils'
import {
  cssColorToChromaColorOrDefault,
  isCSSGradientBackgroundLayer,
  isCSSSolidBackgroundLayer,
  parseColor,
  printConicGradientBackgroundLayer,
  printLinearGradientBackgroundLayer,
  printRadialGradientBackgroundLayer,
} from '../common/css-utils'
import { BackgroundPicker } from '../sections/style-section/background-subsection/background-picker'
import { StringControl } from './string-control'
import { useColorTheme, UtopiaTheme } from '../../../uuiui'
import Utils from '../../../utils/utils'

export interface BackgroundThumbnailControlProps {
  value: CSSBackgroundLayer
  id: string
  testId: string
  key: string
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  popupOpen: boolean
  setOpenPopup: React.Dispatch<React.SetStateAction<number | undefined>>
  controlClassName?: string
  style?: React.CSSProperties
  modalOffset?: {
    x: number
    y: number
  }
  showString?: boolean
  onSubmitSolidStringValue?: (value: string) => void
  backgroundIndex: number
  useSubmitValueFactory: UseSubmitValueFactory<CSSBackgroundLayers>
}

interface BackgroundSolidOrGradientThumbnailControlProps extends BackgroundThumbnailControlProps {
  value: CSSGradientBackgroundLayer | CSSSolidBackgroundLayer
}

export function updateStringCSSColor(newValue: string, oldValue: CSSColor) {
  const parsed = parseColor(newValue, 'hex-hash-optional')
  if (isRight(parsed)) {
    return parsed.value
  } else {
    return oldValue
  }
}

export function updateStringCSSBackgroundItem(
  newValue: string,
  oldValue: CSSBackground,
): CSSBackground {
  const parsed = parseColor(newValue, 'hex-hash-optional')
  if (isRight(parsed)) {
    return { type: 'solid', enabled: true, color: parsed.value }
  } else {
    return oldValue
  }
}

export const backgroundControlContainerStyle = {
  backgroundImage: `linear-gradient(to bottom left,   #e7e7e7 25%,      transparent 25%),
                  linear-gradient(to bottom left,   transparent 75%,  #e7e7e7 75%),
                  linear-gradient(to bottom right,  #e7e7e7 25%,      transparent 25%),
                  linear-gradient(to bottom right,  transparent 75%,  #e7e7e7 75%)`,
  backgroundColor: 'white',
  backgroundSize: '12px 12px',
  backgroundPosition: '-6px 0px, 0px -6px, 6px 6px, 0 0',
  borderRadius: UtopiaTheme.inputBorderRadius,
  height: 20,
  flex: '0 0 28px',
  margin: 1,
}

export const BackgroundSolidOrGradientThumbnailControl = React.memo(
  (props: BackgroundSolidOrGradientThumbnailControlProps) => {
    const colorTheme = useColorTheme()
    const setOpenPopup = props.setOpenPopup
    const closePopup = React.useCallback(() => setOpenPopup(undefined), [setOpenPopup])
    const backgroundArrayItem = (() => {
      switch (props.value.type) {
        case 'solid-background-layer': {
          const color = props.value.color
          const [r, g, b, a] = cssColorToChromaColorOrDefault(color).rgba()
          const rgbString = `rgba(${r}, ${g}, ${b})`
          const rgbaString = `rgba(${r}, ${g}, ${b}, ${a})`
          return {
            backgroundImage: `linear-gradient(to bottom right, transparent 65%, ${rgbString} 65%), linear-gradient(${rgbaString}, ${rgbaString})`,
          }
        }
        case 'linear-gradient-background-layer': {
          return {
            backgroundImage: printLinearGradientBackgroundLayer(props.value),
          }
        }
        case 'radial-gradient-background-layer': {
          return {
            backgroundImage: printRadialGradientBackgroundLayer(props.value),
          }
        }
        case 'conic-gradient-background-layer': {
          return {
            backgroundImage: printConicGradientBackgroundLayer(props.value),
          }
        }
        default:
          const _exhaustiveCheck: never = props.value
          throw new Error(`Unhandled background type: ${JSON.stringify(props.value)}`)
      }
    })()

    const pickerOffset = props.modalOffset != null ? props.modalOffset : { x: 0, y: 0 }

    const picker = !props.popupOpen ? null : (
      <BackgroundPicker
        id={props.id}
        testId={'background-solid-or-gradient-control-background-picker'}
        offsetX={pickerOffset.x}
        offsetY={pickerOffset.y}
        closePopup={closePopup}
        value={props.value}
        useSubmitValueFactory={props.useSubmitValueFactory}
        backgroundLayerIndex={props.backgroundIndex}
        controlStatus={props.controlStatus}
      />
    )

    const stringInput =
      props.showString &&
      isCSSSolidBackgroundLayer(props.value) &&
      props.onSubmitSolidStringValue != null ? (
        <StringControl
          id={`string-${props.id}`}
          key={'color-string'}
          testId={'background-solid-or-gradient-control-string-control'}
          value={cssColorToChromaColorOrDefault(props.value.color).hex().toUpperCase()}
          readOnly={props.controlStyles.interactive}
          onSubmitValue={props.onSubmitSolidStringValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
          DEPRECATED_controlOptions={{
            labelBelow: 'hex',
          }}
          style={{
            marginLeft: 6,
          }}
        />
      ) : null

    const backgroundIndex = props.backgroundIndex
    const onMouseDown: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
      (e) => {
        setOpenPopup((openPopup) => {
          if (openPopup != null) {
            return undefined
          } else {
            return backgroundIndex
          }
        })
      },
      [setOpenPopup, backgroundIndex],
    )

    return (
      <div
        key={props.id}
        id={`trigger-${props.id}`}
        className={` hexField ${Utils.pathOr(
          '',
          ['controlClassName'],
          props,
        )} ignore-react-onclickoutside-${props.id}`}
        style={{ ...props.style, width: 28 }}
      >
        {picker}
        <div className={`widget-color-control relative`} key={`${props.id}-surround`}>
          <div
            key={`${props.id}-color`}
            className={'color-control'}
            style={backgroundControlContainerStyle}
            onMouseDown={onMouseDown}
          >
            <div
              className='colorcontrol-colorfield'
              style={{
                boxShadow: `0 0 0 1px ${colorTheme.secondaryBorder.value}`,
                height: '100%',
                width: '100%',
                borderRadius: UtopiaTheme.inputBorderRadius,
                ...backgroundArrayItem,
              }}
            />
          </div>
          {stringInput}
        </div>
      </div>
    )
  },
)

export const StringBackgroundColorControl = React.memo(
  (props: BackgroundSolidOrGradientThumbnailControlProps) => {
    const value = props.value
    if (isCSSGradientBackgroundLayer(value) || props.onSubmitSolidStringValue == null) {
      return null
    }

    return (
      <StringControl
        id={`string-${props.id}`}
        key={'color-string'}
        testId={'background-color-control-string-control'}
        style={props.style}
        value={cssColorToChromaColorOrDefault(value.color).hex().toUpperCase()}
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
