import deepEqual from 'fast-deep-equal'
import { useContextSelector } from 'use-context-selector'
import { flatMapArray, last, mapArrayToDictionary } from '../../../core/shared/array-utils'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import { objectMap } from '../../../core/shared/object-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { arrayEqualsByReference, NO_OP } from '../../../core/shared/utils'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  setProp_UNSAFE,
  transientActions,
  unsetProperty,
} from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { useEditorState } from '../../editor/store/store-hook'
import type { PropertyStatus } from './control-status'
import { getControlStatusFromPropertyStatus } from './control-status'
import { getControlStyles } from './control-styles'
import type { ParsedProperties, ParsedPropertiesKeys } from './css-utils'
import { printCSSValue } from './css-utils'
import type { ReadonlyRef } from './inspector-utils'
import type { InspectorInfo, PathMappingFn } from './property-path-hooks'
import {
  InspectorPropsContext,
  ParsedValues,
  useGetOrderedPropertyKeys,
  useInspectorContext,
  useInspectorInfo,
} from './property-path-hooks'

function getShadowedLonghandShorthandValue<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys,
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
    return arrayEqualsByReference(propKeys, orderedPropKeys[0])
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
      if (shorthandValueObject != null && longhand in shorthandValueObject) {
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
  ShorthandKey extends ParsedPropertiesKeys,
> = Omit<InspectorInfo<ParsedProperties[LonghandKey]>, 'useSubmitValueFactory'> & {
  orderedPropKeys: Array<Array<LonghandKey | ShorthandKey>>
}

export function useInspectorInfoLonghandShorthand<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys,
>(
  longhands: Array<LonghandKey>,
  shorthand: ShorthandKey,
  pathMappingFn: PathMappingFn<LonghandKey | ShorthandKey>,
): {
  [longhand in LonghandKey]: InspectorInfoWithPropKeys<LonghandKey, ShorthandKey>
} {
  const dispatch = useDispatch()
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

  const allOrderedPropKeys = useGetOrderedPropertyKeys(pathMappingFn, [...longhands, shorthand])

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
        return arrayEqualsByReference(propKeys, orderedPropKeys[0])
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
        const updatedValue = {
          ...(currentValue as any),
          [longhand]: newTransformedValues, // VERY IMPORTANT here we assume that the longhand key is a valid key in the parsed shorthand value!!
        } as any as ParsedProperties[ShorthandKey]
        const longhandPropertyPath = pathMappingFn(longhand, inspectorTargetPath)
        const shorthandPropertyPath = pathMappingFn(shorthand, inspectorTargetPath)
        const printedValue = printCSSValue(shorthand, updatedValue)
        const actionsToDispatch = flatMapArray((selectedView) => {
          return [
            ...(doWeHaveToRemoveAShadowedLonghand
              ? [unsetProperty(selectedView, longhandPropertyPath)]
              : []),
            setProp_UNSAFE(selectedView, shorthandPropertyPath, printedValue),
          ]
        }, selectedViewsRef.current)
        dispatch(
          transient
            ? [transientActions(actionsToDispatch, selectedViewsRef.current)]
            : actionsToDispatch,
        )
      } else {
        // we either have a dominant longhand key, or we need to append a new one
        const propertyPath = pathMappingFn(longhand, inspectorTargetPath)
        const printedValue = printCSSValue(longhand, newTransformedValues)
        const actionsToDispatch = flatMapArray((selectedView) => {
          return [
            ...(doWeHaveToRemoveAShadowedLonghand
              ? [unsetProperty(selectedView, propertyPath)]
              : []),
            setProp_UNSAFE(selectedView, propertyPath, printedValue),
          ]
        }, selectedViewsRef.current)
        dispatch(
          transient
            ? [transientActions(actionsToDispatch, selectedViewsRef.current)]
            : actionsToDispatch,
        )
      }
    }

    const onTransientSubmitValue = (newTransformedValues: ParsedProperties[LonghandKey]) =>
      onSubmitValue(newTransformedValues, true)

    const controlStatus = getControlStatusFromPropertyStatus(propertyStatus)
    return {
      value: value,
      propertyStatus: propertyStatus,
      controlStatus: controlStatus,
      controlStyles: getControlStyles(controlStatus),
      onSubmitValue: onSubmitValue,
      onTransientSubmitValue: onTransientSubmitValue,
      onUnsetValues: NO_OP,
      orderedPropKeys: orderedPropKeys,
    }
  })

  const longhandResultsDictionary = mapArrayToDictionary(
    longhandResults,
    (_, index) => longhands[index],
    (result) => result,
  )

  const longhandResultsWithUnset = objectMap((longhandResult, longhandToUnset) => {
    const onUnsetValues = () => {
      const allPropKeysEqual = allOrderedPropKeys.every((propKeys) => {
        return arrayEqualsByReference(propKeys, allOrderedPropKeys[0])
      })
      if (!allPropKeysEqual) {
        // we do nothing for now. we cannot ensure that we can make a sensible update and surface it on the UI as well
        return
      }
      const propkeysToUse = allOrderedPropKeys[0]

      const actionsToDispatch = createUnsetActions<LonghandKey, ShorthandKey>(
        pathMappingFn,
        inspectorTargetPath,
        selectedViewsRef,
        propkeysToUse,
        shorthand,
        longhands,
        shorthandInfo,
        longhandResultsDictionary,
        longhandToUnset,
      )

      dispatch(actionsToDispatch)
    }

    return {
      ...longhandResult,
      onUnsetValues: onUnsetValues,
    }
  }, longhandResultsDictionary)

  return longhandResultsWithUnset
}

function createUnsetActions<
  LonghandKey extends ParsedPropertiesKeys,
  ShorthandKey extends ParsedPropertiesKeys,
>(
  pathMappingFn: PathMappingFn<LonghandKey | ShorthandKey>,
  inspectorTargetPath: readonly string[],
  selectedViewsRef: ReadonlyRef<ElementPath[]>,
  propkeysToUse: (LonghandKey | ShorthandKey)[],
  shorthand: ShorthandKey,
  longhands: LonghandKey[],
  shorthandInfo: InspectorInfo<ParsedProperties[ShorthandKey]>,
  longhandInfoDictionary: {
    [longhand in LonghandKey]: InspectorInfoWithPropKeys<LonghandKey, ShorthandKey>
  },
  longhandToUnset: LonghandKey,
) {
  const shorthandExists = propkeysToUse.includes(shorthand)
  const longhandPropertyPath = pathMappingFn(longhandToUnset, inspectorTargetPath)
  const shorthandPropertyPath = pathMappingFn(shorthand, inspectorTargetPath)

  if (shorthandExists) {
    const shorthandControlled = shorthandInfo.propertyStatus.controlled
    if (shorthandControlled) {
      // we want to remove existing uses of the longhand, and append a `{[longhand]: undefined}` at the end of the object
      return flatMapArray((selectedView) => {
        return [
          unsetProperty(selectedView, longhandPropertyPath),
          setProp_UNSAFE(
            selectedView,
            longhandPropertyPath,
            jsExpressionValue(undefined, emptyComments),
          ),
        ]
      }, selectedViewsRef.current)
    } else {
      // we have to split the shorthand into longhands 😭
      return flatMapArray((selectedView) => {
        const setPropsForLonghandstoKeep = longhands
          .filter((lh) => lh !== longhandToUnset)
          .map((longhandToKeep) => {
            // for every longhand, we figure out their current value
            const currentValue = longhandInfoDictionary[longhandToKeep].value
            const printedValue = printCSSValue(longhandToKeep, currentValue)
            const pathTouse = pathMappingFn(longhandToKeep, inspectorTargetPath)
            // we emit a setProperty for all of them. for some this will make a new prop, for others this will leave them in place
            return setProp_UNSAFE(selectedView, pathTouse, printedValue)
          })

        return [
          unsetProperty(selectedView, longhandPropertyPath),
          unsetProperty(selectedView, shorthandPropertyPath),
          ...setPropsForLonghandstoKeep,
        ]
      }, selectedViewsRef.current)
    }
  } else {
    return selectedViewsRef.current.map((selectedView) =>
      unsetProperty(selectedView, longhandPropertyPath),
    )
  }
}
