import * as deepEqual from 'fast-deep-equal'
import { colorTheme, UtopiaTheme } from '../../../uuiui/styles/theme'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isPartOfJSXAttributeValue,
} from '../../../core/shared/element-template'
import { GetModifiableAttributeResult } from '../../../core/shared/jsx-attributes'
import { isLeft, isRight, Either } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { ParsedPropertiesKeys } from './css-utils'
import { MultiselectAtProps, MultiselectAtStringProps } from './property-path-hooks'
import { hsl } from 'chroma-js'

export interface ControlStyles {
  fontStyle: string
  fontWeight: number
  mainColor: string
  secondaryColor: string
  tertiaryColor: string
  borderColor: string
  backgroundColor: string
  segmentSelectorColor: string
  set: boolean
  interactive: boolean
  mixed: boolean
  unknown: boolean
  trackColor: string
  railColor: string
  showContent: boolean
  unsettable: boolean
}

const theme = {
  primary: colorTheme.primary.value,
  primary10pct: colorTheme.primary.shade(10).value, //'#E9F3FF',
  text: colorTheme.neutralForeground.value,
  textEmphasized: colorTheme.emphasizedForeground.value,
  subdued: colorTheme.subduedForeground.value,
  verySubdued: colorTheme.subduedForeground.shade(30).value,
  textInverted: 'white',
  selectedItemBg: 'hsl(0,0%,90%)',
  inputBg: 'hsl(0,0%,96%)',
  inputTextSubdued: 'hsl(0,0%,70%)',
  sliderRail: 'hsl(0,0%,80%)',
  // buttonBg: 'hsl(0,0%,95%)',
  // buttonBgEmphasized: 'hsl(0,0%,93%)',
  // buttonBorder: 'hsl(0,0%,90%)',
  // inputBgEmphasized: 'hsl(0,0%,93%)',
  // inputBorder: 'hsl(0,0%,85%)',
  // inputFocusedBg: 'hsl(0,0%,100%)',
}

export type ControlStatus =
  | 'off' // nothing is selected on the canvas
  | 'simple' // this single-selected element's property is set in code literally
  | 'simple-unknown-css' // this single-selected element's property is defined by a string or number that is not able to be successfully parsed
  | 'unset' // this single-selected element's property is not set in code literally, nor controlled
  | 'disabled' // this single-selected element's property is disabled due to some other style property or metadata (e.g. a hex string input for a border that is disabled)
  | 'unoverwritable' // this single-selected element's property is part of a (grand)parent prop that is 'controlled', and we can't edit it directly without destroying the parent prop
  | 'controlled' // this single-selected element's property is defined by unparseable arbitrary js (e.g. `15 + 15`, `isDark ? 'black' : white`)
  | 'detected-fromcss' // this single-selected element's property is detected from measurement, and we know it comes from a CSS Stylesheet (might be via Emotion)
  | 'detected' // this single-selected element's property is detected from measurement
  | 'trivial-default' // this single-selected element's property is detected from measurement and we think its not important to show it to the user
  | 'multiselect-identical-simple' // all elements in this multi-selection have this property 'simple', with identical values
  | 'multiselect-simple-unknown-css' // at least one element in the multiselection is 'unknown-css', and the rest are 'simple' or 'unset'
  | 'multiselect-identical-unset' // all elements in this multi-selection have this property either 'simple' or 'unset', with identical values
  | 'multiselect-mixed-simple-or-unset' // all elements in the multi-selection are 'simple' or 'unset', with at least one non-identical value
  | 'multiselect-controlled' // at least one element in the multiselection is 'controlled', and the rest are 'simple', 'unset', or 'unknown-css'
  | 'multiselect-detected-fromcss' // at least one element in the multiselection is 'detected-fromcss', and the rest are 'simple', 'unset', 'unknown-css', or 'controlled'
  | 'multiselect-detected' // at least one element in the multiselection is 'detected', and the rest are 'simple', 'unset', 'unknown-css', or 'controlled'
  | 'multiselect-unoverwritable' // at least one element in the multi-selection is 'unoverwritable'
  | 'multiselect-disabled' // at least one element in the multi-selection is 'disabled'
  | 'multiselect-trivial-default' // this single-selected element's property is detected from measurement and we think its not important to show it to the user

const AllControlStatuses: Array<ControlStatus> = [
  'off',
  'simple',
  'simple-unknown-css',
  'unset',
  'disabled',
  'unoverwritable',
  'controlled',
  'detected-fromcss',
  'detected',
  'trivial-default',
  'multiselect-identical-simple',
  'multiselect-simple-unknown-css',
  'multiselect-identical-unset',
  'multiselect-mixed-simple-or-unset',
  'multiselect-controlled',
  'multiselect-detected-fromcss',
  'multiselect-detected',
  'multiselect-unoverwritable',
  'multiselect-disabled',
  'multiselect-trivial-default',
]

export function isControlledStatus(controlStatus: ControlStatus): boolean {
  switch (controlStatus) {
    case 'controlled':
    case 'multiselect-controlled':
      return true
    case 'off':
    case 'simple':
    case 'simple-unknown-css':
    case 'unset':
    case 'disabled':
    case 'unoverwritable':
    case 'detected':
    case 'detected-fromcss':
    case 'multiselect-identical-simple':
    case 'multiselect-simple-unknown-css':
    case 'multiselect-identical-unset':
    case 'multiselect-mixed-simple-or-unset':
    case 'multiselect-unoverwritable':
    case 'multiselect-disabled':
    case 'multiselect-detected':
    case 'multiselect-detected-fromcss':
    case 'trivial-default':
    case 'multiselect-trivial-default':
      return false
    default:
      const _exhaustiveCheck: never = controlStatus
      throw new Error(`Unhandled status ${JSON.stringify(controlStatus)}`)
  }
}

const controlStylesByStatus: { [key: string]: ControlStyles } = Utils.mapArrayToDictionary(
  AllControlStatuses,
  (status: ControlStatus) => status,
  (status: ControlStatus): ControlStyles => {
    let fontStyle = 'normal'
    let fontWeight = 400
    let mainColor: string = colorTheme.neutralForeground.value
    let secondaryColor: string = colorTheme.subduedForeground.value
    let tertiaryColor: string = colorTheme.verySubduedForeground.value
    let borderColor: string = 'transparent'
    let backgroundColor: string = colorTheme.inputBackground.value
    let segmentSelectorColor: string = colorTheme.subtleBackground.value
    let trackColor = colorTheme.secondaryForeground.value
    let railColor = colorTheme.verySubduedForeground.value
    let set = true
    let interactive = true
    let mixed = false
    let unknown = false
    let showContent = true
    let unsettable = true

    switch (status) {
      case 'simple':
      case 'multiselect-identical-simple':
        break
      case 'detected-fromcss':
      case 'multiselect-detected-fromcss':
        mainColor = UtopiaTheme.color.css.value
        secondaryColor = UtopiaTheme.color.css.value
        trackColor = UtopiaTheme.color.css.value
        break
      case 'multiselect-mixed-simple-or-unset':
        set = false
        mixed = true
        mainColor = theme.inputTextSubdued
        trackColor = theme.inputTextSubdued
        break
      case 'simple-unknown-css':
      case 'multiselect-simple-unknown-css':
        unknown = true
        interactive = true
        mainColor = theme.subdued
        secondaryColor = theme.subdued
        backgroundColor = 'transparent'
        break
      case 'unoverwritable':
      case 'multiselect-unoverwritable':
        interactive = false
        mainColor = theme.subdued
        secondaryColor = theme.subdued
        trackColor = theme.subdued
        railColor = theme.subdued
        unsettable = false
        break
      case 'unset':
      case 'multiselect-identical-unset':
        set = false
        mainColor = theme.inputTextSubdued
        secondaryColor = theme.inputTextSubdued
        trackColor = theme.inputTextSubdued
        unsettable = false
        break
      case 'off':
        set = false
        interactive = false
        mainColor = theme.inputTextSubdued
        secondaryColor = theme.subdued
        trackColor = theme.subdued
        showContent = false
        unsettable = false
        break
      case 'disabled':
      case 'multiselect-disabled':
        interactive = false
        mainColor = theme.subdued
        secondaryColor = theme.subdued
        backgroundColor = 'transparent'
        segmentSelectorColor = 'transparent'

        trackColor = 'transparent'
        showContent = true
        unsettable = false
        break
      case 'controlled':
      case 'multiselect-controlled':
        interactive = true
        mainColor = theme.primary
        secondaryColor = theme.primary
        showContent = true
        break
      case 'detected':
      case 'multiselect-detected':
        interactive = true
        mainColor = theme.inputTextSubdued
        secondaryColor = theme.inputTextSubdued
        trackColor = theme.inputTextSubdued
        break
      case 'trivial-default':
      case 'multiselect-trivial-default':
        interactive = true
        mainColor = theme.subdued
        secondaryColor = theme.subdued
        showContent = false
        break
      default:
        break
    }

    return {
      fontStyle,
      fontWeight,
      mainColor,
      secondaryColor,
      tertiaryColor,
      borderColor,
      backgroundColor,
      segmentSelectorColor,
      set,
      interactive,
      mixed,
      trackColor,
      railColor,
      showContent,
      unknown,
      unsettable,
    }
  },
)

export function getControlStyles(controlStatus: ControlStatus): ControlStyles {
  return controlStylesByStatus[controlStatus]
}

interface SinglePropertyStatus {
  controlled: boolean
  set: boolean
  overwritable: boolean
  detected: boolean
  fromCssStyleSheet: boolean
  trivialDefault: boolean
}

export interface PropertyStatus extends SinglePropertyStatus {
  selectionLength: number
  identical: boolean
}

// TODO: this needs tests!!!
export function getControlStatusFromPropertyStatus(status: PropertyStatus): ControlStatus {
  if (status.selectionLength === 0) {
    return 'off'
  } else if (status.selectionLength === 1) {
    if (status.set) {
      if (status.controlled) {
        if (status.overwritable) {
          return 'controlled'
        } else {
          return 'unoverwritable'
        }
      } else {
        return 'simple'
      }
    } else {
      if (status.detected) {
        if (status.fromCssStyleSheet) {
          return 'detected-fromcss'
        } else if (status.trivialDefault) {
          return 'trivial-default'
        } else {
          return 'detected'
        }
      } else {
        return 'unset'
      }
    }
  } else {
    if (status.set) {
      if (status.controlled) {
        if (status.overwritable) {
          return 'multiselect-controlled'
        } else {
          return 'multiselect-unoverwritable'
        }
      } else {
        if (status.identical) {
          return 'multiselect-identical-simple'
        } else {
          return 'multiselect-mixed-simple-or-unset'
        }
      }
    } else {
      if (status.identical) {
        return 'multiselect-identical-unset'
      } else {
        if (status.detected) {
          if (status.fromCssStyleSheet) {
            return 'multiselect-detected-fromcss'
          } else if (status.trivialDefault) {
            return 'multiselect-trivial-default'
          } else {
            return 'multiselect-detected'
          }
        } else {
          return 'multiselect-controlled'
        }
      }
    }
  }
}

function isSet(
  modifiableAttributeResult: GetModifiableAttributeResult,
  spiedValue: unknown,
): boolean {
  if (isLeft(modifiableAttributeResult)) {
    return true
  } else {
    if (isJSXAttributeNotFound(modifiableAttributeResult.value)) {
      return spiedValue != null
    } else if (
      isJSXAttributeValue(modifiableAttributeResult.value) ||
      isPartOfJSXAttributeValue(modifiableAttributeResult.value)
    ) {
      return modifiableAttributeResult.value.value !== undefined
    } else {
      return true
    }
  }
}

function isControlled(
  modifiableAttributeResult: GetModifiableAttributeResult,
  spiedValue: unknown,
): boolean {
  if (isLeft(modifiableAttributeResult)) {
    return true
  } else {
    if (isJSXAttributeNotFound(modifiableAttributeResult.value)) {
      return spiedValue != null
    } else if (
      isJSXAttributeValue(modifiableAttributeResult.value) ||
      isPartOfJSXAttributeValue(modifiableAttributeResult.value)
    ) {
      return false
    } else {
      return true
    }
  }
}

function isOverwritable(modifiableAttributeResult: GetModifiableAttributeResult): boolean {
  return isRight(modifiableAttributeResult)
}

function calculatePropertyStatusPerProperty(
  modifiableAttributeResult: GetModifiableAttributeResult,
  realValue: unknown,
): SinglePropertyStatus {
  return {
    set: isSet(modifiableAttributeResult, realValue),
    controlled: isControlled(modifiableAttributeResult, realValue),
    overwritable: isOverwritable(modifiableAttributeResult),
    detected: false,
    fromCssStyleSheet: false,
    trivialDefault: false,
  }
}

// TODO MEMOIZE ME!
export function calculatePropertyStatusForSelection(
  modifiableAttributeResult: ReadonlyArray<GetModifiableAttributeResult>,
  spiedValues: ReadonlyArray<unknown>,
): PropertyStatus {
  const selectionLength = modifiableAttributeResult.length
  const firstResult = modifiableAttributeResult[0]
  if (firstResult == null) {
    return {
      controlled: false,
      set: false,
      overwritable: false,
      selectionLength,
      identical: true,
      detected: false,
      fromCssStyleSheet: false,
      trivialDefault: false,
    }
  }
  if (selectionLength === 1) {
    return {
      ...calculatePropertyStatusPerProperty(firstResult, spiedValues[0]),
      selectionLength,
      identical: true,
    }
  } else {
    let identical = true
    let controlled = false
    let set = false
    let overwritable = true
    const firstValue = firstResult.value
    modifiableAttributeResult.forEach((attribute, index) => {
      if (identical && isRight(attribute)) {
        // if any property is not identical, set to false
        identical = deepEqual(firstValue, attribute.value) // deepEqual here, for very large multiselection this might be bad
      }
      // if any property is controlled, set to true
      controlled = controlled || isControlled(attribute, spiedValues[index])
      // if any property is set, set to true
      set = set || isSet(attribute, spiedValues[index])
      // if any property is not overwritable, set to false
      overwritable = overwritable && isOverwritable(attribute)
    })
    return {
      identical,
      controlled,
      set,
      overwritable,
      selectionLength,
      detected: false,
      fromCssStyleSheet: false,
      trivialDefault: false,
    }
  }
}

export function calculateMultiPropertyStatusForSelection<
  PropertiesToControl extends ParsedPropertiesKeys
>(
  multiselectAtProps: MultiselectAtProps<PropertiesToControl>,
  realValues: {
    [key in PropertiesToControl]: {
      spiedValues: ReadonlyArray<unknown>
    }
  },
): PropertyStatus {
  const propertyStatuses: Array<PropertyStatus> = []
  const keys = Object.keys(multiselectAtProps)
  const selectionLength =
    keys.length > 0 ? multiselectAtProps[keys[0] as PropertiesToControl].length : 0
  keys.forEach((key) => {
    const attribute = multiselectAtProps[key as PropertiesToControl]
    const values = realValues[key as PropertiesToControl].spiedValues
    propertyStatuses.push(calculatePropertyStatusForSelection(attribute, values))
  })

  let identical = true
  let controlled = false
  let set = false
  let overwritable = true
  let detected = false
  let fromCssStyleSheet = false
  let trivialDefault = true
  propertyStatuses.forEach((propertyStatus) => {
    // if any property is not identical, set to false
    identical = identical && propertyStatus.identical
    // if any property is controlled, set to true
    controlled = controlled || propertyStatus.controlled
    // if any property is set, set to true
    set = set || propertyStatus.set
    // if any property is not overwritable, set to false
    overwritable = overwritable && propertyStatus.overwritable
    // if any property is detected, set to true
    detected = detected || propertyStatus.detected
    // if any property comes from a css stylesheet, set to true
    fromCssStyleSheet = fromCssStyleSheet || propertyStatus.fromCssStyleSheet
    // if all properties are trivial, set to true
    trivialDefault = trivialDefault && propertyStatus.trivialDefault
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
    detected,
    fromCssStyleSheet,
    trivialDefault,
  }
}

// FIXME: copy pasted for component prop section
export function calculateMultiStringPropertyStatusForSelection(
  multiselectAtProps: MultiselectAtStringProps,
  realValues: {
    [key in string]: {
      spiedValues: ReadonlyArray<unknown>
    }
  },
): PropertyStatus {
  const propertyStatuses: Array<PropertyStatus> = []
  const keys = Object.keys(multiselectAtProps)
  const selectionLength = keys.length > 0 ? multiselectAtProps[keys[0]].length : 0
  keys.forEach((key) => {
    const attribute = multiselectAtProps[key]
    const values = realValues[key].spiedValues
    propertyStatuses.push(calculatePropertyStatusForSelection(attribute, values))
  })

  let identical = true
  let controlled = false
  let set = false
  let overwritable = true
  let detected = false
  let fromCssStyleSheet = false
  let trivialDefault = true
  propertyStatuses.forEach((propertyStatus) => {
    // if any property is not identical, set to false
    identical = identical && propertyStatus.identical
    // if any property is controlled, set to true
    controlled = controlled || propertyStatus.controlled
    // if any property is set, set to true
    set = set || propertyStatus.set
    // if any property is not overwritable, set to false
    overwritable = overwritable && propertyStatus.overwritable
    // if any property is detected, set to true
    detected = detected || propertyStatus.detected
    // if any property comes from a css stylesheet, set to true
    fromCssStyleSheet = fromCssStyleSheet || propertyStatus.fromCssStyleSheet
    // if all properties are trivial, set to true
    trivialDefault = trivialDefault && propertyStatus.trivialDefault
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
    detected,
    fromCssStyleSheet,
    trivialDefault,
  }
}

export function isNotUnsetOrDefault(controlStatus: ControlStatus): boolean {
  return controlStatus !== 'unset' && controlStatus !== 'trivial-default' && controlStatus !== 'off'
}
