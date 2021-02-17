import { last } from '../../../core/shared/array-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { getControlStatusFromPropertyStatus, PropertyStatus } from '../../../uuiui-deps'
import { ParsedProperties, ParsedPropertiesKeys } from './css-utils'
import {
  InspectorInfo,
  ParsedValues,
  PathMappingFn,
  useInspectorInfoNoDefaults,
} from './property-path-hooks'

function getShadowedLonghandShorthandValue<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys
>(
  longhand: LonghandKey,
  shorthand: ShorthandKey,
  longhandPropertyStatus: PropertyStatus,
  shorthandPropertyStatus: PropertyStatus,
  values: Partial<ParsedValues<LonghandKey | ShorthandKey>>,
  orderedPropKeys: (LonghandKey | ShorthandKey)[][], // multiselect
): { value: ParsedProperties[LonghandKey] | undefined; propertyStatus: PropertyStatus } {
  const allPropKeysEqual = orderedPropKeys.every((propKeys) => {
    return arrayEquals(propKeys, orderedPropKeys[0])
  })
  if (!allPropKeysEqual) {
    return {
      value: undefined,
      propertyStatus: longhandPropertyStatus,
    }
  }

  const propKeys = orderedPropKeys[0] ?? []
  const lastKey = last(propKeys)
  if (lastKey == null) {
    return {
      value: undefined,
      propertyStatus: longhandPropertyStatus,
    }
  } else {
    if (lastKey === longhand) {
      return {
        value: values[lastKey],
        propertyStatus: longhandPropertyStatus,
      }
    } else {
      // Important: we ASSUME that the transformed shorthand value is an object,
      // where the keys are the longhand keys and the values are the individual longhand values
      const shorthandValue = values[shorthand] as any
      if (longhand in shorthandValue) {
        return {
          value: shorthandValue?.[longhand] as ParsedProperties[LonghandKey] | undefined,
          propertyStatus: shorthandPropertyStatus,
        }
      } else {
        throw new Error(
          `We didn't find longhand key ${longhand} in resolved shorthand value for shorthand ${shorthand}`,
        )
      }
    }
  }
}

export function useInspectorInfoLonghandShorthand<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys
>(
  longhand: LonghandKey,
  shorthand: ShorthandKey,
  pathMappingFn: PathMappingFn<LonghandKey | ShorthandKey>,
): Omit<
  InspectorInfo<ParsedProperties[LonghandKey] | undefined, LonghandKey | ShorthandKey>,
  'useSubmitValueFactory'
> {
  let inspectorInfo = useInspectorInfoNoDefaults(
    [longhand, shorthand],
    (v) => v,
    () => {
      return null as any
    },
    pathMappingFn,
  )

  const longhandPropertyStatus = useInspectorInfoNoDefaults(
    [longhand],
    (v) => v,
    () => {
      return null as any
    },
    pathMappingFn,
  ).propertyStatus
  const shorthandPropertyStatus = useInspectorInfoNoDefaults(
    [shorthand],
    (v) => v,
    () => {
      return null as any
    },
    pathMappingFn,
  ).propertyStatus
  const { value, propertyStatus } = getShadowedLonghandShorthandValue(
    longhand,
    shorthand,
    longhandPropertyStatus,
    shorthandPropertyStatus,
    inspectorInfo.value,
    inspectorInfo.orderedPropKeys,
  )

  const onSubmitValue = (
    newTransformedValues: ParsedProperties[LonghandKey] | undefined,
    transient?: boolean | undefined,
    // eslint-disable-next-line @typescript-eslint/no-empty-function
  ) => {}

  const onTransientSubmitValue = (
    newTransformedValues: ParsedProperties[LonghandKey] | undefined,
  ) => onSubmitValue(newTransformedValues, true)

  // eslint-disable-next-line @typescript-eslint/no-empty-function
  const onUnsetValues = () => {}

  return {
    value: value,
    controlStatus: getControlStatusFromPropertyStatus(propertyStatus),
    propertyStatus: propertyStatus,
    controlStyles: inspectorInfo.controlStyles,
    orderedPropKeys: inspectorInfo.orderedPropKeys,
    onSubmitValue: onSubmitValue,
    onTransientSubmitValue: onTransientSubmitValue,
    onUnsetValues: onUnsetValues,
  }
}
