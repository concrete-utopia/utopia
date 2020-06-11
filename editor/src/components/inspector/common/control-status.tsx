import * as deepEqual from 'fast-deep-equal'
import { UtopiaUtils } from 'utopia-api'
import { colorTheme } from 'uuiui'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isPartOfJSXAttributeValue,
  JSXAttribute,
  JSXAttributeNotFound,
  PartOfJSXAttributeValue,
} from '../../../core/shared/element-template'
import { GetModifiableAttributeResult } from '../../../core/shared/jsx-attributes'
import { isLeft, isRight } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { ParsedPropertiesKeys } from './css-utils'
import { MultiselectAtProps, MultiselectAtStringProps } from './property-path-hooks'

export interface ControlStyles {
  fontStyle: string
  mainColor: string
  secondaryColor: string
  borderColor: string
  backgroundColor: string
  backgroundOnColor: string
  backgroundOffColor: string
  set: boolean
  interactive: boolean
  mixed: boolean
  unknown: boolean
  trackColor: string
  railColor: string
  showContent: boolean
  unsettable: boolean
}

// note: these colors need to be kept true with the inspector.css vars
export const ControlStyleDefaults = {
  SetMainColor: colorTheme.inspectorSetMainColor.value,
  SetSecondaryColor: colorTheme.inspectorSetSecondaryColor.value,
  SetBorderColor: colorTheme.inspectorSetBorderColor.value,
  SetBackgroundColor: colorTheme.inspectorSetBackgroundColor.value,
  SetBackgroundOnColor: colorTheme.inspectorSetBackgroundOnColor.value,
  SetBackgroundOffColor: colorTheme.inspectorSetBackgroundOffColor.value,
  UnsetMainColor: colorTheme.inspectorUnsetMainColor.value,
  UnsetSecondaryColor: colorTheme.inspectorUnsetSecondaryColor.value,
  UnsetBorderColor: colorTheme.inspectorUnsetBorderColor.value,
  UnsetBackgroundColor: colorTheme.inspectorUnsetBackgroundColor.value,
  UnsetBackgroundOnColor: colorTheme.inspectorUnsetBackgroundOnColor.value,
  UnsetBackgroundOffColor: colorTheme.inspectorUnsetBackgroundOffColor.value,
  UnsetFontStyle: 'normal',
  DisabledMainColor: colorTheme.inspectorDisabledMainColor.value,
  DisabledSecondaryColor: colorTheme.inspectorDisabledSecondaryColor.value,
  DisabledBackgroundColor: colorTheme.inspectorDisabledBackgroundColor.value,
  DisabledBackgroundOnColor: colorTheme.inspectorDisabledBackgroundOnColor.value,
  DisabledBackgroundOffColor: colorTheme.inspectorDisabledBackgroundOffColor.value,
  DisabledBorderColor: colorTheme.inspectorDisabledBorderColor.value,
  DisabledFontStyle: 'italic',
  UneditableMainColor: colorTheme.inspectorUneditableMainColor.value,
  UneditableSecondaryColor: colorTheme.inspectorUneditableSecondaryColor.value,
  UneditableBackgroundColor: colorTheme.inspectorUneditableBackgroundColor.value,
  UneditableBorderColor: colorTheme.inspectorUneditableBorderColor.value,
  ControlledMainColor: colorTheme.inspectorControlledMainColor.value,
  ControlledBorderColor: colorTheme.inspectorControlledBorderColor.value,
  ControlledBackgroundColor: colorTheme.inspectorControlledBackgroundColor.value,
  ControlledBackgroundOnColor: colorTheme.inspectorControlledBackgroundOnColor.value,
  ControlledBackgroundOffColor: colorTheme.inspectorControlledBackgroundOffColor.value,
  ControlledNodegraphMainColor: colorTheme.inspectorControlledMainColor.value,
  ControlledNodegraphBorderColor: colorTheme.inspectorControlledBorderColor.value,
  ControlledNodegraphBackgroundColor: colorTheme.inspectorControlledBackgroundColor.value,
  ControlledNodegraphBackgroundOnColor: colorTheme.inspectorControlledBackgroundOnColor.value,
  ControlledNodegraphBackgroundOffColor: colorTheme.inspectorControlledBackgroundOffColor.value,
  OffMainColor: colorTheme.inspectorOffMainColor.value,
  OffSecondaryColor: colorTheme.inspectorOffSecondaryColor.value,
  OffBackgroundColor: colorTheme.inspectorOffBackgroundColor.value,
  OffBackgroundOnColor: colorTheme.inspectorOffBackgroundOnColor.value,
  OffBackgroundOffColor: colorTheme.inspectorOffBackgroundOffColor.value,
  OffBorderColor: colorTheme.inspectorOffBorderColor.value,
}

export type ControlStatus =
  | 'off' // nothing is selected on the canvas
  | 'simple' // this single-selected element's property is set in code literally
  | 'simple-unknown-css' // this single-selected element's property is defined by a string or number that is not able to be successfully parsed
  | 'unset' // this single-selected element's property is not set in code literally, nor controlled
  | 'disabled' // this single-selected element's property is disabled due to some other style property or metadata (e.g. a hex string input for a border that is disabled)
  | 'unoverwritable' // this single-selected element's property is part of a (grand)parent prop that is 'controlled', and we can't edit it directly without destroying the parent prop
  | 'controlled' // this single-selected element's property is defined by unparseable arbitrary js (e.g. `15 + 15`, `isDark ? 'black' : white`)
  | 'multiselect-identical-simple' // all elements in this multi-selection have this property 'simple', with identical values
  | 'multiselect-simple-unknown-css' // at least one element in the multiselection is 'unknown-css', and the rest are 'simple' or 'unset'
  | 'multiselect-identical-unset' // all elements in this multi-selection have this property either 'simple' or 'unset', with identical values
  | 'multiselect-mixed-simple-or-unset' // all elements in the multi-selection are 'simple' or 'unset', with at least one non-identical value
  | 'multiselect-controlled' // at least one element in the multiselection is 'controlled', and the rest are 'simple', 'unset', or 'unknown-css'
  | 'multiselect-unoverwritable' // at least one element in the multi-selection is 'unoverwritable'
  | 'multiselect-disabled' // at least one element in the multi-selection is 'disabled' or 'set-disabled'

const AllControlStatuses: Array<ControlStatus> = [
  'off',
  'simple',
  'simple-unknown-css',
  'unset',
  'disabled',
  'unoverwritable',
  'controlled',
  'multiselect-identical-simple',
  'multiselect-simple-unknown-css',
  'multiselect-identical-unset',
  'multiselect-mixed-simple-or-unset',
  'multiselect-controlled',
  'multiselect-unoverwritable',
  'multiselect-disabled',
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
    case 'multiselect-identical-simple':
    case 'multiselect-simple-unknown-css':
    case 'multiselect-identical-unset':
    case 'multiselect-mixed-simple-or-unset':
    case 'multiselect-unoverwritable':
    case 'multiselect-disabled':
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
    let mainColor: string = ControlStyleDefaults.SetMainColor
    let secondaryColor: string = ControlStyleDefaults.SetSecondaryColor
    let borderColor: string = ControlStyleDefaults.SetBorderColor
    let backgroundColor: string = ControlStyleDefaults.SetBackgroundColor
    let backgroundOnColor: string = ControlStyleDefaults.SetBackgroundOnColor
    let backgroundOffColor: string = ControlStyleDefaults.SetBackgroundOffColor
    let set = true
    let interactive = true
    let mixed = false
    let unknown = false
    let trackColor = ControlStyleDefaults.SetMainColor
    let railColor = ControlStyleDefaults.SetSecondaryColor
    let showContent = true
    let unsettable = true

    switch (status) {
      case 'simple':
      case 'multiselect-identical-simple':
        break
      case 'multiselect-mixed-simple-or-unset':
        set = false
        mixed = true
        backgroundColor = ControlStyleDefaults.UnsetBackgroundColor
        backgroundOnColor = ControlStyleDefaults.UnsetBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.UnsetBackgroundOffColor
        borderColor = ControlStyleDefaults.UnsetBorderColor
        mainColor = ControlStyleDefaults.UnsetMainColor
        trackColor = ControlStyleDefaults.UnsetMainColor
        railColor = ControlStyleDefaults.UnsetSecondaryColor
        break
      case 'simple-unknown-css':
      case 'multiselect-simple-unknown-css':
        unknown = true
        borderColor = ControlStyleDefaults.DisabledBorderColor
        interactive = true
        mainColor = ControlStyleDefaults.UneditableMainColor
        secondaryColor = ControlStyleDefaults.UneditableSecondaryColor
        backgroundColor = ControlStyleDefaults.DisabledBackgroundColor
        backgroundOnColor = ControlStyleDefaults.DisabledBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.DisabledBackgroundOffColor
        trackColor = ControlStyleDefaults.UneditableMainColor
        railColor = ControlStyleDefaults.UneditableSecondaryColor
        fontStyle = ControlStyleDefaults.DisabledFontStyle
        break
      case 'unoverwritable':
      case 'multiselect-unoverwritable':
        borderColor = ControlStyleDefaults.DisabledBorderColor
        interactive = false
        mainColor = ControlStyleDefaults.UneditableMainColor
        secondaryColor = ControlStyleDefaults.UneditableSecondaryColor
        backgroundColor = ControlStyleDefaults.DisabledBackgroundColor
        backgroundOnColor = ControlStyleDefaults.DisabledBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.DisabledBackgroundOffColor
        trackColor = ControlStyleDefaults.UneditableMainColor
        railColor = ControlStyleDefaults.UneditableSecondaryColor
        fontStyle = ControlStyleDefaults.DisabledFontStyle
        unsettable = false
        break
      case 'unset':
      case 'multiselect-identical-unset':
        set = false
        backgroundColor = ControlStyleDefaults.UnsetBackgroundColor
        backgroundOnColor = ControlStyleDefaults.UnsetBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.UnsetBackgroundOffColor
        borderColor = ControlStyleDefaults.UnsetBorderColor
        mainColor = ControlStyleDefaults.UnsetMainColor
        trackColor = ControlStyleDefaults.UnsetMainColor
        railColor = ControlStyleDefaults.UnsetSecondaryColor
        unsettable = false
        break
      case 'off':
        set = false
        borderColor = ControlStyleDefaults.OffBorderColor
        interactive = false
        mainColor = ControlStyleDefaults.OffMainColor
        secondaryColor = ControlStyleDefaults.OffSecondaryColor
        backgroundColor = ControlStyleDefaults.OffBackgroundColor
        backgroundOnColor = ControlStyleDefaults.OffBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.OffBackgroundOffColor
        trackColor = ControlStyleDefaults.OffBorderColor
        railColor = ControlStyleDefaults.OffBorderColor
        showContent = false
        unsettable = false
        break
      case 'disabled':
      case 'multiselect-disabled':
        borderColor = ControlStyleDefaults.DisabledBorderColor
        interactive = false
        mainColor = ControlStyleDefaults.DisabledMainColor
        secondaryColor = ControlStyleDefaults.DisabledSecondaryColor
        backgroundColor = ControlStyleDefaults.DisabledBackgroundColor
        backgroundOnColor = ControlStyleDefaults.DisabledBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.DisabledBackgroundOffColor
        trackColor = ControlStyleDefaults.DisabledBorderColor
        fontStyle = ControlStyleDefaults.DisabledFontStyle
        railColor = ControlStyleDefaults.DisabledBorderColor
        showContent = true
        unsettable = false
        break
      case 'controlled':
      case 'multiselect-controlled':
        borderColor = ControlStyleDefaults.ControlledBorderColor
        interactive = true
        mainColor = ControlStyleDefaults.UneditableMainColor
        secondaryColor = ControlStyleDefaults.UneditableSecondaryColor
        backgroundColor = ControlStyleDefaults.ControlledBackgroundColor
        backgroundOnColor = ControlStyleDefaults.ControlledBackgroundOnColor
        backgroundOffColor = ControlStyleDefaults.ControlledBackgroundOffColor
        trackColor = ControlStyleDefaults.UneditableMainColor
        railColor = ControlStyleDefaults.UneditableSecondaryColor
        showContent = true
        break
      default:
        break
    }

    return {
      fontStyle,
      mainColor,
      secondaryColor,
      borderColor,
      backgroundColor,
      backgroundOnColor,
      backgroundOffColor,
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
      return 'unset'
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
        return 'multiselect-controlled'
      }
    }
  }
}

function isValidHelperFunction(
  jsxAttribute: JSXAttribute | PartOfJSXAttributeValue | JSXAttributeNotFound,
) {
  if (jsxAttribute.type === 'ATTRIBUTE_FUNCTION_CALL') {
    if (UtopiaUtils.hasOwnProperty(jsxAttribute.functionName)) {
      if (jsxAttribute.parameters.every(isJSXAttributeValue)) {
        return true
      }
    }
  }
  return false
}

function isSet(
  modifiableAttributeResult: GetModifiableAttributeResult,
  realValue: unknown,
): boolean {
  if (isLeft(modifiableAttributeResult)) {
    return true
  } else {
    if (isJSXAttributeNotFound(modifiableAttributeResult.value)) {
      return realValue != null
    } else if (
      isJSXAttributeValue(modifiableAttributeResult.value) ||
      isPartOfJSXAttributeValue(modifiableAttributeResult.value)
    ) {
      return modifiableAttributeResult.value.value !== undefined
    } else if (isValidHelperFunction(modifiableAttributeResult.value)) {
      return true
    } else {
      return true
    }
  }
}

function isControlled(
  modifiableAttributeResult: GetModifiableAttributeResult,
  realValue: unknown,
): boolean {
  if (isLeft(modifiableAttributeResult)) {
    return true
  } else {
    if (isJSXAttributeNotFound(modifiableAttributeResult.value)) {
      return realValue != null
    } else if (
      isJSXAttributeValue(modifiableAttributeResult.value) ||
      isPartOfJSXAttributeValue(modifiableAttributeResult.value) ||
      isValidHelperFunction(modifiableAttributeResult.value)
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
  }
}

// TODO MEMOIZE ME!
function calculatePropertyStatusForSelection(
  modifiableAttributeResult: ReadonlyArray<GetModifiableAttributeResult>,
  realValues: ReadonlyArray<unknown>,
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
    }
  }
  if (selectionLength === 1) {
    return {
      ...calculatePropertyStatusPerProperty(firstResult, realValues[0]),
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
      controlled = controlled || isControlled(attribute, realValues[index])
      // if any property is set, set to true
      set = set || isSet(attribute, realValues[index])
      // if any property is not overwritable, set to false
      overwritable = overwritable && isOverwritable(attribute)
    })
    return {
      identical,
      controlled,
      set,
      overwritable,
      selectionLength,
    }
  }
}

export function calculateMultiPropertyStatusForSelection<
  PropertiesToControl extends ParsedPropertiesKeys
>(
  multiselectAtProps: MultiselectAtProps<PropertiesToControl>,
  realValues: {
    [key in PropertiesToControl]: {
      realValues: ReadonlyArray<unknown>
    }
  },
): PropertyStatus {
  const propertyStatuses: Array<PropertyStatus> = []
  const keys = Object.keys(multiselectAtProps)
  const selectionLength =
    keys.length > 0 ? multiselectAtProps[keys[0] as PropertiesToControl].length : 0
  keys.forEach((key) => {
    const attribute = multiselectAtProps[key as PropertiesToControl]
    const values = realValues[key as PropertiesToControl].realValues
    propertyStatuses.push(calculatePropertyStatusForSelection(attribute, values))
  })

  let identical = true
  let controlled = false
  let set = false
  let overwritable = true
  propertyStatuses.forEach((propertyStatus) => {
    // if any property is not identical, set to false
    identical = identical && propertyStatus.identical
    // if any property is controlled, set to true
    controlled = controlled || propertyStatus.controlled
    // if any property is set, set to true
    set = set || propertyStatus.set
    // if any property is not overwritable, set to false
    overwritable = overwritable && propertyStatus.overwritable
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
  }
}

// FIXME: copy pasted for component prop section
export function calculateMultiStringPropertyStatusForSelection(
  multiselectAtProps: MultiselectAtStringProps,
  realValues: {
    [key in string]: {
      realValues: ReadonlyArray<unknown>
    }
  },
): PropertyStatus {
  const propertyStatuses: Array<PropertyStatus> = []
  const keys = Object.keys(multiselectAtProps)
  const selectionLength = keys.length > 0 ? multiselectAtProps[keys[0]].length : 0
  keys.forEach((key) => {
    const attribute = multiselectAtProps[key]
    const values = realValues[key].realValues
    propertyStatuses.push(calculatePropertyStatusForSelection(attribute, values))
  })

  let identical = true
  let controlled = false
  let set = false
  let overwritable = true
  propertyStatuses.forEach((propertyStatus) => {
    // if any property is not identical, set to false
    identical = identical && propertyStatus.identical
    // if any property is controlled, set to true
    controlled = controlled || propertyStatus.controlled
    // if any property is set, set to true
    set = set || propertyStatus.set
    // if any property is not overwritable, set to false
    overwritable = overwritable && propertyStatus.overwritable
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
  }
}
