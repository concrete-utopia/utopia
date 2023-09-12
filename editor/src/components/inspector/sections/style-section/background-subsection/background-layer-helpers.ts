import type React from 'react'
import type { ContextMenuItem } from '../../../../context-menu-items'
import type { SelectOption } from '../../../controls/select-control'
import type {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSBackgroundLayerType,
  CSSNumber,
  EmptyInputValue,
} from '../../../common/css-utils'
import {
  cssColorToChromaColorOrDefault,
  cssDefault,
  defaultConicGradientBackgroundLayer,
  defaultCSSRadialOrConicGradientCenter,
  defaultGradientStops,
  defaultLinearGradientBackgroundLayer,
  defaultRadialGradientBackgroundLayer,
  defaultSolidBackgroundLayer,
  emptyURLFunctionBackgroundLayer,
  isEmptyInputValue,
  isCSSBackgroundLayerWithBGSize,
} from '../../../common/css-utils'
import type { UseSubmitValueFactory } from '../../../common/property-path-hooks'
import type { ControlStyles } from '../../../common/control-styles'
import type { ControlStatus } from '../../../common/control-status'

export interface BackgroundLayerProps {
  value: CSSBackgroundLayer
  index: number
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  useSubmitTransformedValuesFactory: UseSubmitTransformedValuesFactory
  popupOpen: boolean
  setOpenPopup: React.Dispatch<React.SetStateAction<number | undefined>>
  unsetContextMenuItem: Array<ContextMenuItem<null>>
}

export function getIndexedUpdateEnabled(index: number) {
  return function indexedUpdateEnabled(
    enabled: boolean,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newCSSBackgroundLayers = [...oldValue]
    const workingValue = newCSSBackgroundLayers[index]
    workingValue.enabled = enabled
    if (isCSSBackgroundLayerWithBGSize(workingValue)) {
      workingValue.bgSize.enabled = enabled
    }
    newCSSBackgroundLayers[index] = workingValue
    return newCSSBackgroundLayers
  }
}

export interface CSSBackgroundLayerTypeSelectOption extends SelectOption {
  value: CSSBackgroundLayerType
}

export const solidSelectOption: CSSBackgroundLayerTypeSelectOption = {
  value: 'solid-background-layer',
  label: 'Solid',
}

export const linearGradientSelectOption: CSSBackgroundLayerTypeSelectOption = {
  value: 'linear-gradient-background-layer',
  label: 'Linear',
}

export const radialGradientSelectOption: CSSBackgroundLayerTypeSelectOption = {
  value: 'radial-gradient-background-layer',
  label: 'Radial',
}

export const conicGradientSelectOption: CSSBackgroundLayerTypeSelectOption = {
  value: 'conic-gradient-background-layer',
  label: 'Conic',
}

export const imageSelectOption: CSSBackgroundLayerTypeSelectOption = {
  value: 'url-function-background-layer',
  label: 'Image',
}

export const backgroundLayerTypeSelectOptions: Array<CSSBackgroundLayerTypeSelectOption> = [
  solidSelectOption,
  linearGradientSelectOption,
  radialGradientSelectOption,
  conicGradientSelectOption,
  imageSelectOption,
]

export function getIndexedOnCSSBackgroundLayerTypeSelectSubmitValue(backgroundLayerIndex: number) {
  return function onCSSBackgroundLayerTypeSelectSubmitValue(
    newValue: CSSBackgroundLayerTypeSelectOption,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newBackgroundLayerType = newValue.value
    let newBackgroundLayers = [...oldValue]
    const oldBackgroundLayer = oldValue[backgroundLayerIndex]
    if (oldBackgroundLayer != null) {
      if (newBackgroundLayerType === oldBackgroundLayer.type) {
        return newBackgroundLayers
      }
      /*
    Outer switch checks old background type, inner switches decides how to use data from the old
    background type in each possible new background type
    */
      switch (oldBackgroundLayer.type) {
        case 'unknown-array-item': {
          switch (newBackgroundLayerType) {
            case 'solid-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = { ...defaultSolidBackgroundLayer }
              return newBackgroundLayers
            }
            case 'linear-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultLinearGradientBackgroundLayer,
              }
              return newBackgroundLayers
            }
            case 'conic-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = { ...defaultConicGradientBackgroundLayer }
              return newBackgroundLayers
            }
            case 'radial-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultRadialGradientBackgroundLayer,
              }
              return newBackgroundLayers
            }
            case 'url-function-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = { ...emptyURLFunctionBackgroundLayer }
              return newBackgroundLayers
            }
            default: {
              const _exhaustiveCheck: never = newBackgroundLayerType
              throw new Error(`Unhandled new background layer type ${newBackgroundLayerType}`)
            }
          }
        }
        case 'solid-background-layer': {
          const newStops = [...defaultGradientStops]
          newStops[0].color = oldBackgroundLayer.color
          const chroma = cssColorToChromaColorOrDefault(oldBackgroundLayer.color)
          const luminance = chroma.luminance()
          const color1 =
            luminance < 1 ? chroma.luminance(luminance + 0.1) : chroma.luminance(luminance - 0.1)
          newStops[1].color = { type: 'Hex', hex: color1.hex() }
          switch (newBackgroundLayerType) {
            case 'solid-background-layer': {
              return oldValue
            }
            case 'linear-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultLinearGradientBackgroundLayer,
                stops: newStops,
              }
              return newBackgroundLayers
            }
            case 'conic-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultConicGradientBackgroundLayer,
                stops: newStops,
              }
              return newBackgroundLayers
            }
            case 'radial-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultRadialGradientBackgroundLayer,
                stops: newStops,
              }
              return newBackgroundLayers
            }
            case 'url-function-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...emptyURLFunctionBackgroundLayer,
              }
              return newBackgroundLayers
            }
            default: {
              const _exhaustiveCheck: never = newBackgroundLayerType
              throw new Error(`Unhandled new background layer type ${newBackgroundLayerType}`)
            }
          }
        }
        case 'linear-gradient-background-layer':
        case 'radial-gradient-background-layer':
        case 'conic-gradient-background-layer': {
          switch (newBackgroundLayerType) {
            case 'solid-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                type: 'solid-background-layer',
                enabled: true,
                color: oldBackgroundLayer.stops[0].color,
              }
              return newBackgroundLayers
            }
            case 'linear-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultLinearGradientBackgroundLayer,
                stops: [...oldBackgroundLayer.stops],
                bgSize: oldBackgroundLayer.bgSize,
              }
              return newBackgroundLayers
            }
            case 'radial-gradient-background-layer': {
              let newRadialGradient = { ...defaultRadialGradientBackgroundLayer }
              newRadialGradient.stops = [...oldBackgroundLayer.stops]
              newRadialGradient.bgSize = oldBackgroundLayer.bgSize
              if (oldBackgroundLayer.type === 'conic-gradient-background-layer') {
                newRadialGradient.center = { ...oldBackgroundLayer.center }
              }
              newBackgroundLayers[backgroundLayerIndex] = newRadialGradient
              return newBackgroundLayers
            }
            case 'conic-gradient-background-layer': {
              let newConicGradient = { ...defaultConicGradientBackgroundLayer }
              if (oldBackgroundLayer.type === 'radial-gradient-background-layer') {
                newConicGradient.center = { ...oldBackgroundLayer.center }
              }
              if (oldBackgroundLayer.type === 'linear-gradient-background-layer') {
                newConicGradient.fromAngle = { ...oldBackgroundLayer.angle }
              }
              newConicGradient.bgSize = oldBackgroundLayer.bgSize
              newConicGradient.stops = [...oldBackgroundLayer.stops]
              newBackgroundLayers[backgroundLayerIndex] = newConicGradient
              return newBackgroundLayers
            }
            case 'url-function-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...emptyURLFunctionBackgroundLayer,
                bgSize: oldBackgroundLayer.bgSize,
              }
              return newBackgroundLayers
            }
            default: {
              const _exhaustiveCheck: never = newBackgroundLayerType
              throw new Error(`Unhandled new background layer type ${newBackgroundLayerType}`)
            }
          }
        }
        case 'url-function-background-layer': {
          switch (newBackgroundLayerType) {
            case 'solid-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultSolidBackgroundLayer,
              }
              return newBackgroundLayers
            }
            case 'linear-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultLinearGradientBackgroundLayer,
                bgSize: oldBackgroundLayer.bgSize,
              }
              return newBackgroundLayers
            }
            case 'conic-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultConicGradientBackgroundLayer,
                bgSize: oldBackgroundLayer.bgSize,
              }
              return newBackgroundLayers
            }
            case 'radial-gradient-background-layer': {
              newBackgroundLayers[backgroundLayerIndex] = {
                ...defaultRadialGradientBackgroundLayer,
                bgSize: oldBackgroundLayer.bgSize,
              }
              return newBackgroundLayers
            }
            case 'url-function-background-layer': {
              return oldValue
            }
            default: {
              const _exhaustiveCheck: never = newBackgroundLayerType
              throw new Error(`Unhandled new background layer type ${newBackgroundLayerType}`)
            }
          }
        }
        default: {
          const _exhaustiveCheck: never = oldBackgroundLayer
          throw new Error(
            `Unhandled old background layer type ${JSON.stringify(oldBackgroundLayer)}`,
          )
        }
      }
    }
    throw new Error(`No background layer exists at index ${backgroundLayerIndex}`)
  }
}

export function getIndexedUpdateRadialOrConicGradientCenterX(index: number) {
  return function updateRadialOrConicGradientCenterX(
    newX: CSSNumber | EmptyInputValue,
    cssBackgroundImages: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newCssBackgroundImages = [...cssBackgroundImages]
    const workingBackgroundImage = newCssBackgroundImages[index]
    if (
      workingBackgroundImage.type === 'radial-gradient-background-layer' ||
      workingBackgroundImage.type === 'conic-gradient-background-layer'
    ) {
      if (isEmptyInputValue(newX)) {
        if (workingBackgroundImage.center.y.default) {
          workingBackgroundImage.center = { ...defaultCSSRadialOrConicGradientCenter }
        } else {
          workingBackgroundImage.center.x = { ...defaultCSSRadialOrConicGradientCenter.x }
        }
      } else {
        workingBackgroundImage.center.x = cssDefault(newX, false)
      }
      newCssBackgroundImages[index] = workingBackgroundImage
      return newCssBackgroundImages
    } else {
      return newCssBackgroundImages
    }
  }
}

export function getIndexedUpdateRadialOrConicGradientCenterY(index: number) {
  return function updateRadialOrConicGradientCenterY(
    newY: CSSNumber | EmptyInputValue,
    cssBackgroundImages: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const newCssBackgroundImages = [...cssBackgroundImages]
    const workingBackgroundImage = newCssBackgroundImages[index]
    if (
      workingBackgroundImage.type === 'radial-gradient-background-layer' ||
      workingBackgroundImage.type === 'conic-gradient-background-layer'
    ) {
      if (isEmptyInputValue(newY)) {
        if (workingBackgroundImage.center.x.default) {
          workingBackgroundImage.center = { ...defaultCSSRadialOrConicGradientCenter }
        } else {
          workingBackgroundImage.center.y = { ...defaultCSSRadialOrConicGradientCenter.y }
        }
      } else {
        workingBackgroundImage.center.y = cssDefault(newY, false)
      }

      newCssBackgroundImages[index] = workingBackgroundImage
      return newCssBackgroundImages
    } else {
      return newCssBackgroundImages
    }
  }
}

export type UseSubmitTransformedValuesFactory = UseSubmitValueFactory<CSSBackgroundLayers>
