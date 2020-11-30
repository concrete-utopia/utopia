import * as deepEqual from 'fast-deep-equal'
import { UtopiaUtils } from 'utopia-api'
import { colorTheme } from '../../../uuiui/styles/theme'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isPartOfJSXAttributeValue,
  JSXAttribute,
  JSXAttributeNotFound,
  PartOfJSXAttributeValue,
} from '../../../core/shared/element-template'
import {
  GetModifiableAttributeResult,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import { isLeft, isRight, Either } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { ParsedPropertiesKeys } from './css-utils'
import { MultiselectAtProps, MultiselectAtStringProps } from './property-path-hooks'
import { fastForEach } from '../../../core/shared/utils'

export interface ControlStyles {
  fontStyle: string
  fontWeight: number
  mainColor: string
  secondaryColor: string
  borderColor: string
  backgroundColor: string
  segmentSelectorColor: string
  segmentTrackColor: string
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
  SetSegmentSelectorColor: colorTheme.inspectorSetBackgroundColor.value,
  SetSegmentTrackColor: colorTheme.inspectorSetSegmentTrackColor.value,
  UnsetMainColor: colorTheme.inspectorUnsetMainColor.value,
  UnsetSecondaryColor: colorTheme.inspectorUnsetSecondaryColor.value,
  UnsetBorderColor: colorTheme.inspectorUnsetBorderColor.value,
  UnsetBorderHoverFocusColor: colorTheme.inspectorSetBorderColor.value,
  UnsetBackgroundColor: colorTheme.inspectorUnsetBackgroundColor.value,
  UnsetSegmentSelectorColor: colorTheme.inspectorUnsetSegmentSelectorColor.value,
  UnsetSegmentTrackColor: colorTheme.inspectorUnsetSegmentTrackColor.value,
  DisabledMainColor: colorTheme.inspectorDisabledMainColor.value,
  DisabledSecondaryColor: colorTheme.inspectorDisabledSecondaryColor.value,
  DisabledBackgroundColor: colorTheme.inspectorDisabledBackgroundColor.value,
  DisabledSegmentSelectorColor: colorTheme.inspectorDisabledSegmentSelectorColor.value,
  DisabledSegmentTrackColor: colorTheme.inspectorDisabledSegmentTrackColor.value,
  DisabledBorderColor: colorTheme.inspectorDisabledBorderColor.value,
  UneditableMainColor: colorTheme.inspectorUneditableMainColor.value,
  UneditableSecondaryColor: colorTheme.inspectorUneditableSecondaryColor.value,
  UneditableBackgroundColor: colorTheme.inspectorUneditableBackgroundColor.value,
  UneditableBorderColor: colorTheme.inspectorUneditableBorderColor.value,
  ControlledMainColor: colorTheme.inspectorControlledMainColor.value,
  ControlledSecondaryColor: colorTheme.inspectorControlledMainColor.value,
  ControlledBorderColor: colorTheme.inspectorControlledBorderColor.value,
  ControlledBackgroundColor: colorTheme.inspectorControlledBackgroundColor.value,
  ControlledSegmentSelectorColor: colorTheme.inspectorControlledSegmentSelectorColor.value,
  ControlledSegmentTrackColor: colorTheme.inspectorControlledSegmentTrackColor.value,
  DetectedMainColor: colorTheme.inspectorDetectedMainColor.value,
  DetectedSecondaryColor: colorTheme.inspectorDetectedMainColor.value,
  DetectedBorderColor: colorTheme.inspectorDetectedBorderColor.value,
  DetectedBackgroundColor: colorTheme.inspectorDetectedBackgroundColor.value,
  DetectedSegmentSelectorColor: colorTheme.inspectorDetectedSegmentSelectorColor.value,
  DetectedSegmentTrackColor: colorTheme.inspectorDetectedSegmentTrackColor.value,
  OffMainColor: colorTheme.inspectorOffMainColor.value,
  OffSecondaryColor: colorTheme.inspectorOffSecondaryColor.value,
  OffBackgroundColor: colorTheme.inspectorOffBackgroundColor.value,
  OffSegmentSelectorColor: colorTheme.inspectorOffSegmentSelectorColor.value,
  OffSegmentTrackColor: colorTheme.inspectorOffSegmentTrackColor.value,
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
  | 'detected' // this single-selected element's property is detected from measurement
  | 'multiselect-identical-simple' // all elements in this multi-selection have this property 'simple', with identical values
  | 'multiselect-simple-unknown-css' // at least one element in the multiselection is 'unknown-css', and the rest are 'simple' or 'unset'
  | 'multiselect-identical-unset' // all elements in this multi-selection have this property either 'simple' or 'unset', with identical values
  | 'multiselect-mixed-simple-or-unset' // all elements in the multi-selection are 'simple' or 'unset', with at least one non-identical value
  | 'multiselect-controlled' // at least one element in the multiselection is 'controlled', and the rest are 'simple', 'unset', or 'unknown-css'
  | 'multiselect-detected' // at least one element in the multiselection is 'detected', and the rest are 'simple', 'unset', 'unknown-css', or 'controlled'
  | 'multiselect-unoverwritable' // at least one element in the multi-selection is 'unoverwritable'
  | 'multiselect-disabled' // at least one element in the multi-selection is 'disabled'

const AllControlStatuses: Array<ControlStatus> = [
  'off',
  'simple',
  'simple-unknown-css',
  'unset',
  'disabled',
  'unoverwritable',
  'controlled',
  'detected',
  'multiselect-identical-simple',
  'multiselect-simple-unknown-css',
  'multiselect-identical-unset',
  'multiselect-mixed-simple-or-unset',
  'multiselect-controlled',
  'multiselect-detected',
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
    case 'detected':
    case 'multiselect-identical-simple':
    case 'multiselect-simple-unknown-css':
    case 'multiselect-identical-unset':
    case 'multiselect-mixed-simple-or-unset':
    case 'multiselect-unoverwritable':
    case 'multiselect-disabled':
    case 'multiselect-detected':
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
    let mainColor: string = ControlStyleDefaults.SetMainColor
    let secondaryColor: string = ControlStyleDefaults.SetSecondaryColor
    let borderColor: string = ControlStyleDefaults.SetBorderColor
    let backgroundColor: string = ControlStyleDefaults.SetBackgroundColor
    let segmentSelectorColor: string = ControlStyleDefaults.SetSegmentSelectorColor
    let segmentTrackColor: string = ControlStyleDefaults.SetSegmentTrackColor
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
        segmentSelectorColor = ControlStyleDefaults.UnsetSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.UnsetSegmentTrackColor
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
        segmentSelectorColor = ControlStyleDefaults.DisabledSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.DisabledSegmentTrackColor
        trackColor = ControlStyleDefaults.UneditableMainColor
        railColor = ControlStyleDefaults.UneditableSecondaryColor
        break
      case 'unoverwritable':
      case 'multiselect-unoverwritable':
        borderColor = ControlStyleDefaults.DisabledBorderColor
        interactive = false
        mainColor = ControlStyleDefaults.UneditableMainColor
        secondaryColor = ControlStyleDefaults.UneditableSecondaryColor
        backgroundColor = ControlStyleDefaults.DisabledBackgroundColor
        segmentSelectorColor = ControlStyleDefaults.DisabledSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.DisabledSegmentTrackColor
        trackColor = ControlStyleDefaults.UneditableMainColor
        railColor = ControlStyleDefaults.UneditableSecondaryColor
        unsettable = false
        break
      case 'unset':
      case 'multiselect-identical-unset':
        set = false
        backgroundColor = ControlStyleDefaults.UnsetBackgroundColor
        segmentSelectorColor = ControlStyleDefaults.UnsetSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.UnsetSegmentTrackColor
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
        segmentSelectorColor = ControlStyleDefaults.OffSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.OffSegmentTrackColor
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
        segmentSelectorColor = ControlStyleDefaults.DisabledSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.DisabledSegmentTrackColor
        trackColor = ControlStyleDefaults.DisabledBorderColor
        railColor = ControlStyleDefaults.DisabledBorderColor
        showContent = true
        unsettable = false
        break
      case 'controlled':
      case 'multiselect-controlled':
        fontWeight = 600
        borderColor = ControlStyleDefaults.ControlledBorderColor
        interactive = true
        mainColor = ControlStyleDefaults.ControlledMainColor
        secondaryColor = ControlStyleDefaults.ControlledSecondaryColor
        backgroundColor = ControlStyleDefaults.ControlledBackgroundColor
        segmentSelectorColor = ControlStyleDefaults.ControlledSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.ControlledSegmentTrackColor
        trackColor = ControlStyleDefaults.ControlledMainColor
        railColor = ControlStyleDefaults.ControlledSecondaryColor
        showContent = true
        break
      case 'detected':
      case 'multiselect-detected':
        fontWeight = 600
        borderColor = ControlStyleDefaults.DetectedBorderColor
        interactive = true
        mainColor = ControlStyleDefaults.DetectedMainColor
        secondaryColor = ControlStyleDefaults.DetectedSecondaryColor
        backgroundColor = ControlStyleDefaults.DetectedBackgroundColor
        segmentSelectorColor = ControlStyleDefaults.DetectedSegmentSelectorColor
        segmentTrackColor = ControlStyleDefaults.DetectedSegmentTrackColor
        trackColor = ControlStyleDefaults.DetectedMainColor
        railColor = ControlStyleDefaults.DetectedSecondaryColor
        showContent = true
        break
      default:
        break
    }

    return {
      fontStyle,
      fontWeight,
      mainColor,
      secondaryColor,
      borderColor,
      backgroundColor,
      segmentSelectorColor,
      segmentTrackColor,
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
        return 'detected'
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
          return 'multiselect-detected'
        } else {
          return 'multiselect-controlled'
        }
      }
    }
  }
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
  }
}

// TODO MEMOIZE ME!
export function calculatePropertyStatusForSelection(
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
      detected: false,
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
    let detected = false
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
      detected,
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
  let detected = false
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
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
    detected,
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
  let detected = false
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
  })

  return {
    identical,
    controlled,
    set,
    overwritable,
    selectionLength,
    detected,
  }
}
