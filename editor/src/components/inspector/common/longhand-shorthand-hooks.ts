import * as deepEqual from 'fast-deep-equal'
import { useContextSelector } from 'use-context-selector'
import { flatMapArray, last, mapArrayToDictionary } from '../../../core/shared/array-utils'
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
  longhandValue: ParsedProperties[LonghandKey],
  shorthandValueObject: ParsedProperties[ShorthandKey], // the shorthand value has to be an object where the keys are longhand property names and the types are same as the longhand values
  orderedPropKeys: (LonghandKey | ShorthandKey)[][], // multiselect
): { value: ParsedProperties[LonghandKey]; propertyStatus: PropertyStatus } {
  const allPropKeysEqual = orderedPropKeys.every((propKeys) => {
    return arrayEquals(propKeys, orderedPropKeys[0])
  })

  const propKeys = orderedPropKeys[0] ?? []
  const lastKey = last(propKeys)
  if (lastKey == null) {
    return {
      value: longhandValue,
      propertyStatus: longhandPropertyStatus,
    }
  } else {
    if (lastKey === longhand) {
      return {
        value: longhandValue,
        propertyStatus: allPropKeysEqual
          ? longhandPropertyStatus
          : {
              ...longhandPropertyStatus,
              set: true,
              controlled: true,
              overwritable: false,
              identical: false,
            },
      }
    } else {
      // Important: we assume that shorthandValue is an object
      // where the keys are the longhand keys and the values are the individual longhand values
      if (longhand in shorthandValueObject) {
        return {
          value: (shorthandValueObject as any)?.[longhand] as ParsedProperties[LonghandKey],
          propertyStatus: allPropKeysEqual
            ? shorthandPropertyStatus
            : {
                ...shorthandPropertyStatus,
                set: true,
                controlled: true,
                overwritable: false,
                identical: false,
              },
        }
      } else {
        throw new Error(
          `We didn't find longhand key ${longhand} in resolved shorthand value for shorthand ${shorthand}`,
        )
      }
    }
  }
}

export type InspectorInfoWithPropKeys<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys
> = Omit<InspectorInfo<ParsedProperties[LonghandKey]>, 'useSubmitValueFactory'> & {
  orderedPropKeys: Array<Array<LonghandKey | ShorthandKey>>
}

export function useInspectorInfoLonghandShorthand<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys
>(
  longhands: Array<LonghandKey>,
  shorthand: ShorthandKey,
  pathMappingFn: PathMappingFn<LonghandKey | ShorthandKey>,
): {
  [longhand in LonghandKey]: InspectorInfoWithPropKeys<LonghandKey, ShorthandKey>
} {
  const dispatch = useEditorState(
    (store) => store.dispatch,
    'useInspectorInfoLonghandShorthand dispatch',
  )
  const inspectorTargetPath = useKeepReferenceEqualityIfPossible(
    useContextSelector(InspectorPropsContext, (contextData) => contextData.targetPath, deepEqual),
  )
  const { selectedViewsRef } = useInspectorContext()

  const shorthandInfo = useInspectorInfo(
    [shorthand],
    (v) => v[shorthand],
    () => {
      throw new Error(`do not use useInspectorInfo's built-in onSubmitValue!`)
    },
    pathMappingFn,
  )

  const longhandResults = longhands.map((longhand) => {
    // we follow the Rules of Hooks because we know that the length of the longhands array is stable during the lifecycle of this hook
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const longhandInfo = useInspectorInfo(
      [longhand],
      (v) => v[longhand],
      () => {
        throw new Error(`do not use useInspectorInfo's built-in onSubmitValue!`)
      },
      pathMappingFn,
    )

    // we follow the Rules of Hooks because we know that the length of the longhands array is stable during the lifecycle of this hook
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const orderedPropKeys = useGetOrderedPropertyKeys(pathMappingFn, [longhand, shorthand])

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
      newTransformedValues: ParsedProperties[LonghandKey],
      transient?: boolean | undefined,
    ) => {
      const allPropKeysEqual = orderedPropKeys.every((propKeys) => {
        return arrayEquals(propKeys, orderedPropKeys[0])
      })
      if (!allPropKeysEqual) {
        // we do nothing for now. we cannot ensure that we can make a sensible update and surface it on the UI as well
        return
      }
      const propkeysToUse = orderedPropKeys[0]

      const doWeHaveToRemoveAShadowedLonghand =
        propkeysToUse.length === 2 && propkeysToUse[0] === longhand

      const dominantPropKey = last(propkeysToUse)
      if (
        dominantPropKey === shorthand &&
        !shorthandInfo.propertyStatus.controlled &&
        shorthandInfo.propertyStatus.overwritable
      ) {
        // the shorthand key is the dominant AND it can be updated
        // let's figure out the new value for the prop
        const currentValue = shorthandInfo.value
        const updatedValue = ({
          ...(currentValue as any),
          [longhand]: newTransformedValues, // VERY IMPORTANT here we assume that the longhand key is a valid key in the parsed shorthand value!!
        } as any) as ParsedProperties[ShorthandKey]
        const longhandPropertyPath = pathMappingFn(longhand, inspectorTargetPath)
        const shorthandPropertyPath = pathMappingFn(shorthand, inspectorTargetPath)
        const printedValue = printCSSValue(shorthand, updatedValue)
        const actionsToDispatch = flatMapArray((selectedView) => {
          if (isInstancePath(selectedView)) {
            return [
              ...(doWeHaveToRemoveAShadowedLonghand
                ? [unsetProperty(selectedView, longhandPropertyPath)]
                : []),
              setProp_UNSAFE(selectedView, shorthandPropertyPath, printedValue),
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
        const actionsToDispatch = flatMapArray((selectedView) => {
          if (isInstancePath(selectedView)) {
            return [
              ...(doWeHaveToRemoveAShadowedLonghand
                ? [unsetProperty(selectedView, propertyPath)]
                : []),
              setProp_UNSAFE(selectedView, propertyPath, printedValue),
            ]
          } else {
            return []
          }
        }, selectedViewsRef.current)
        dispatch(transient ? [transientActions(actionsToDispatch)] : actionsToDispatch)
      }
    }

    const onTransientSubmitValue = (newTransformedValues: ParsedProperties[LonghandKey]) =>
      onSubmitValue(newTransformedValues, true)

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
  })

  return mapArrayToDictionary(
    longhandResults,
    (_, index) => longhands[index],
    (result) => result,
  )
}
