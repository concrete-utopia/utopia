import React = require('react')
import * as R from 'ramda'
import { UtopiaTheme } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { UseSubmitValueFactory } from '../common/property-path-hooks'
import { isRight } from '../../../core/shared/either'
import { ControlStatus, ControlStyles } from '../common/control-status'
import {
  CSSBackground,
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSColor,
  cssColorToChromaColorOrDefault,
  CSSGradientBackgroundLayer,
  CSSSolidBackgroundLayer,
  isCSSGradientBackgroundLayer,
  isCSSSolidBackgroundLayer,
  parseColor,
  printConicGradientBackgroundLayer,
  printLinearGradientBackgroundLayer,
  printRadialGradientBackgroundLayer,
} from '../common/css-utils'
import { BackgroundPicker } from '../sections/style-section/background-subsection/background-picker'
import { StringControl } from './string-control'

export interface BackgroundThumbnailControlProps {
  value: CSSBackgroundLayer
  id: string
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
  const parsed = parseColor(newValue)
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
  const parsed = parseColor(newValue)
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

export const BackgroundSolidOrGradientThumbnailControl = betterReactMemo(
  'BackgroundControl',
  (props: BackgroundSolidOrGradientThumbnailControlProps) => {
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
          value={cssColorToChromaColorOrDefault(props.value.color).hex().toUpperCase()}
          readOnly={props.controlStyles.interactive}
          onSubmitValue={props.onSubmitSolidStringValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
          controlOptions={{
            labelBelow: 'hex',
          }}
          style={{
            marginLeft: 6,
          }}
        />
      ) : null

    const backgroundIndex = props.backgroundIndex
    const onMouseDown = React.useCallback(
      (e) => {
        e.stopPropagation()
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
        className={` hexField ${R.pathOr('', ['controlClassName'], props)}`}
        style={props.style}
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
                boxShadow: `0 0 0 1px ${props.controlStyles.borderColor}`,
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

export const StringBackgroundColorControl = betterReactMemo(
  'StringBackgroundColorControl',
  (props: BackgroundSolidOrGradientThumbnailControlProps) => {
    const value = props.value
    if (isCSSGradientBackgroundLayer(value) || props.onSubmitSolidStringValue == null) {
      return null
    }

    return (
      <StringControl
        id={`string-${props.id}`}
        key={'color-string'}
        style={props.style}
        value={cssColorToChromaColorOrDefault(value.color).hex().toUpperCase()}
        readOnly={props.controlStyles.interactive}
        onSubmitValue={props.onSubmitSolidStringValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        controlOptions={{
          labelBelow: 'hex',
        }}
      />
    )
  },
)
