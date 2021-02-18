import * as deepEqual from 'fast-deep-equal'
import { useContextSelector } from 'use-context-selector'
import { flatMapArray, last } from '../../../core/shared/array-utils'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { create } from '../../../core/shared/property-path'
import { instancePath, isInstancePath } from '../../../core/shared/template-path'
import { arrayEquals } from '../../../core/shared/utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  getControlStatusFromPropertyStatus,
  getControlStyles,
  PropertyStatus,
} from '../../../uuiui-deps'
import {
  setProp_UNSAFE,
  transientActions,
  unsetProperty,
} from '../../editor/actions/action-creators'
import { useEditorState } from '../../editor/store/store-hook'
import { ParsedProperties, ParsedPropertiesKeys, printCSSValue } from './css-utils'
import {
  InspectorInfo,
  InspectorPropsContext,
  ParsedValues,
  PathMappingFn,
  useGetOrderedPropertyKeys,
  useInspectorContext,
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
  const inspectorTargetPath = useKeepReferenceEqualityIfPossible(
    useContextSelector(InspectorPropsContext, (contextData) => contextData.targetPath, deepEqual),
  )
  const { selectedViewsRef } = useInspectorContext()
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
    const allPropKeysEqual = orderedPropKeys.every((propKeys) => {
      return arrayEquals(propKeys, orderedPropKeys[0])
    })
    if (!allPropKeysEqual || newTransformedValues == null) {
      // we do nothing for now. we cannot ensure that we can make a sensible update and surface it on the UI as well
      return
    }
    const propkeysToUse = orderedPropKeys[0]

    const doWeHaveToRemoveAShadowedLonghand =
      propkeysToUse.length === 2 && propkeysToUse[0] === longhand

    const dominantPropKey = last(propkeysToUse)
    if (dominantPropKey === shorthand && !shorthandInfo.propertyStatus.controlled) {
      // the shorthand key is the dominant, and it _can_ be updated
      // let's figure out the new value for the prop
      const currentValue = shorthandInfo.value
      const updatedValue = ({
        ...(currentValue[shorthand] as any),
        [longhand]: newTransformedValues, // VERY IMPORTANT here we assume that the longhand key is a valid key in the parsed shorthand value!!
      } as any) as ParsedProperties[ShorthandKey]
      const longhandPropertyPath = pathMappingFn(longhand, inspectorTargetPath)
      const shorthandPropertyPath = pathMappingFn(shorthand, inspectorTargetPath)
      const printedValue = printCSSValue(shorthand, updatedValue)
      const actionsToDispatch = flatMapArray((sv) => {
        if (isInstancePath(sv)) {
          return [
            ...(doWeHaveToRemoveAShadowedLonghand ? [unsetProperty(sv, longhandPropertyPath)] : []),
            setProp_UNSAFE(
              sv, // who is sv?
              shorthandPropertyPath,
              printedValue,
            ),
          ]
        } else {
          return []
        }
      }, selectedViewsRef.current)
      dispatch(transient ? [transientActions(actionsToDispatch)] : actionsToDispatch)
    } else {
      // we either have a dominant longhand key, or we need to append a new one
      const propertyPath = pathMappingFn(longhand, inspectorTargetPath)
      const printedValue = printCSSValue(longhand, newTransformedValues)
      const actionsToDispatch = flatMapArray((sv) => {
        if (isInstancePath(sv)) {
          return [
            ...(doWeHaveToRemoveAShadowedLonghand ? [unsetProperty(sv, propertyPath)] : []),
            setProp_UNSAFE(
              sv, // who is sv?
              propertyPath,
              printedValue,
            ),
          ]
        } else {
          return []
        }
      }, selectedViewsRef.current)
      dispatch(transient ? [transientActions(actionsToDispatch)] : actionsToDispatch)
    }
  }

  const onTransientSubmitValue = (
    newTransformedValues: ParsedProperties[LonghandKey] | undefined,
  ) => onSubmitValue(newTransformedValues, true)

  const onUnsetValues = () => {
    const longhandPropertyPath = pathMappingFn(longhand, inspectorTargetPath)
    const shorthandPropertyPath = pathMappingFn(shorthand, inspectorTargetPath)

    const actionsToDispatch = flatMapArray((selectedView) => {
      if (isInstancePath(selectedView)) {
        return [
          unsetProperty(selectedView, longhandPropertyPath),
          unsetProperty(selectedView, shorthandPropertyPath),
        ]
      } else {
        return []
      }
    }, selectedViewsRef.current)
    dispatch(actionsToDispatch)
  }

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
