import { last } from '../../../core/shared/array-utils'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { create } from '../../../core/shared/property-path'
import { instancePath } from '../../../core/shared/template-path'
import { arrayEquals } from '../../../core/shared/utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import {
  getControlStatusFromPropertyStatus,
  getControlStyles,
  PropertyStatus,
} from '../../../uuiui-deps'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import { useEditorState } from '../../editor/store/store-hook'
import { ParsedProperties, ParsedPropertiesKeys } from './css-utils'
import {
  InspectorInfo,
  ParsedValues,
  PathMappingFn,
  useGetOrderedPropertyKeys,
  useInspectorInfo,
} from './property-path-hooks'

function getShadowedLonghandShorthandValue<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys
>(
  longhand: LonghandKey,
  shorthand: ShorthandKey,
  longhandPropertyStatus: PropertyStatus,
  shorthandPropertyStatus: PropertyStatus,
  longhandValue: ParsedValues<LonghandKey>,
  shorthandValue: ParsedValues<ShorthandKey>,
  orderedPropKeys: (LonghandKey | ShorthandKey)[][], // multiselect
): { value: ParsedProperties[LonghandKey] | undefined; propertyStatus: PropertyStatus } {
  const allPropKeysEqual = orderedPropKeys.every((propKeys) => {
    return arrayEquals(propKeys, orderedPropKeys[0])
  })
  if (!allPropKeysEqual) {
    return {
      value: undefined,
      propertyStatus: {
        ...longhandPropertyStatus,
        set: true,
        controlled: true,
        overwritable: false,
        identical: false,
      },
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
        value: longhandValue[longhand],
        propertyStatus: longhandPropertyStatus,
      }
    } else {
      // Important: we ASSUME that the transformed shorthand value is an object,
      // where the keys are the longhand keys and the values are the individual longhand values
      const propValue = shorthandValue[shorthand] as any
      if (longhand in propValue) {
        return {
          value: propValue?.[longhand] as ParsedProperties[LonghandKey] | undefined,
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
): Omit<InspectorInfo<ParsedProperties[LonghandKey] | undefined>, 'useSubmitValueFactory'> & {
  orderedPropKeys: Array<Array<LonghandKey | ShorthandKey>>
} {
  const dispatch = useEditorState(
    (store) => store.dispatch,
    'useInspectorInfoLonghandShorthand dispatch',
  )
  const orderedPropKeys = useGetOrderedPropertyKeys(pathMappingFn, [longhand, shorthand])
  const longhandInfo = useInspectorInfo(
    [longhand],
    (v) => v,
    () => {
      return null as any
    },
    pathMappingFn,
  )
  const shorthandInfo = useInspectorInfo(
    [shorthand],
    (v) => v,
    () => {
      return null as any
    },
    pathMappingFn,
  )
  const { value, propertyStatus } = getShadowedLonghandShorthandValue(
    longhand,
    shorthand,
    longhandInfo.propertyStatus,
    shorthandInfo.propertyStatus,
    longhandInfo.value,
    shorthandInfo.value,
    orderedPropKeys,
  )

  const onSubmitValue = (
    newTransformedValues: ParsedProperties[LonghandKey] | undefined,
    transient?: boolean | undefined,
    // eslint-disable-next-line @typescript-eslint/no-empty-function
  ) => {
    dispatch([
      setProp_UNSAFE(
        instancePath([], ['hello', 'eni']),
        create(['what', 'up']),
        jsxAttributeValue(newTransformedValues, emptyComments),
      ),
    ])
  }

  const onTransientSubmitValue = (
    newTransformedValues: ParsedProperties[LonghandKey] | undefined,
  ) => onSubmitValue(newTransformedValues, true)

  // eslint-disable-next-line @typescript-eslint/no-empty-function
  const onUnsetValues = () => {}

  const controlStatus = getControlStatusFromPropertyStatus(propertyStatus)
  return {
    value: value,
    propertyStatus: propertyStatus,
    controlStatus: controlStatus,
    controlStyles: getControlStyles(controlStatus),
    onSubmitValue: onSubmitValue,
    onTransientSubmitValue: onTransientSubmitValue,
    onUnsetValues: onUnsetValues,
    orderedPropKeys: orderedPropKeys,
  }
}
