import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'

import deepEqual from 'fast-deep-equal'
import * as ObjectPath from 'object-path'
import React from 'react'
import { createContext, useContextSelector } from 'use-context-selector'
import {
  forUnderlyingTargetFromEditorState,
  withUnderlyingTarget,
} from '../../../components/editor/store/editor-state'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../../components/editor/store/store-hook'
import type {
  ControlStatus,
  PropertyStatus,
} from '../../../components/inspector/common/control-status'
import type { ControlStyles } from '../../../components/inspector/common/control-styles'

import {
  calculateMultiPropertyStatusForSelection,
  calculateMultiStringPropertyStatusForSelection,
  getControlStatusFromPropertyStatus,
} from '../../../components/inspector/common/control-status'
import { getControlStyles } from '../../../components/inspector/common/control-styles'
import type {
  ParsedCSSProperties,
  ParsedCSSPropertiesKeys,
  ParsedElementProperties,
  ParsedElementPropertiesKeys,
  ParsedProperties,
  ParsedPropertiesKeys,
  ParsedPropertiesValues,
} from '../../../components/inspector/common/css-utils'
import {
  emptyValues,
  isCSSUnknownFunctionParameters,
  isLayoutPropDetectedInCSS,
  maybePrintCSSValue,
  parseAnyParseableValue,
  printCSSValue,
  isTrivialDefaultValue,
} from '../../../components/inspector/common/css-utils'
import type { StyleLayoutProp } from '../../../core/layout/layout-helpers-new'
import { isHTMLComponent, isUtopiaAPIComponent } from '../../../core/model/project-file-utils'
import { stripNulls } from '../../../core/shared/array-utils'
import type { Either } from '../../../core/shared/either'
import { eitherToMaybe, flatMapEither, isRight, left, right } from '../../../core/shared/either'
import type {
  JSXAttributes,
  ComputedStyle,
  StyleAttributeMetadata,
  StyleAttributeMetadataEntry,
} from '../../../core/shared/element-template'
import {
  getJSXElementNameLastPart,
  isJSXElement,
  getJSXAttribute,
  isRegularJSXAttribute,
  clearExpressionUniqueIDs,
  getJSXElementNameAsString,
} from '../../../core/shared/element-template'
import type {
  GetModifiableAttributeResult,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import { getAllPathsFromAttributes } from '../../../core/shared/jsx-attributes'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import type {
  PropertyPath,
  ElementPath,
  PropertyPathPart,
} from '../../../core/shared/project-file-types'

import {
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../../utils/react-performance'
import { default as Utils } from '../../../utils/utils'
import type { ReadonlyRef } from './inspector-utils'
import type { MapLike } from 'typescript'
import { omitWithPredicate } from '../../../core/shared/object-utils'
import { UtopiaKeys } from '../../../core/model/utopia-constants'
import type { EditorAction } from '../../editor/action-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { eitherRight, fromTypeGuard } from '../../../core/shared/optics/optic-creators'
import { modify } from '../../../core/shared/optics/optic-utilities'
import { getActivePlugin } from '../../canvas/plugins/style-plugins'
import { isStyleInfoKey, type StyleInfo } from '../../canvas/canvas-types'
import { assertNever } from '../../../core/shared/utils'
import { maybeCssPropertyFromInlineStyle } from '../../canvas/commands/utils/property-utils'

export interface InspectorPropsContextData {
  selectedViews: Array<ElementPath>
  editedMultiSelectedProps: readonly JSXAttributes[]
  targetPath: readonly string[]
  spiedProps: ReadonlyArray<{ [key: string]: any }>
  computedStyles: ReadonlyArray<ComputedStyle>
  selectedAttributeMetadatas: ReadonlyArray<StyleAttributeMetadata>
}

export interface InspectorCallbackContextData {
  selectedViewsRef: ReadonlyRef<Array<ElementPath>>
  onSubmitValue: (newValue: any, propertyPath: PropertyPath, transient: boolean) => void
  onUnsetValue: (propertyPath: PropertyPath | Array<PropertyPath>, transient: boolean) => void
  collectActionsToSubmitValue: (
    propertyPath: PropertyPath,
    transient: boolean,
    newValuePrinter: () => any,
  ) => Array<EditorAction>
  collectActionsToUnsetValue: (
    propertyPath: PropertyPath | Array<PropertyPath>,
    transient: boolean,
  ) => Array<EditorAction>
}

export const InspectorPropsContext = createContext<InspectorPropsContextData>({
  selectedViews: [],
  editedMultiSelectedProps: [],
  targetPath: [],
  spiedProps: [],
  computedStyles: [],
  selectedAttributeMetadatas: [],
})

const emptyCollectActionsFn = () => []
export const InspectorCallbackContext = React.createContext<InspectorCallbackContextData>({
  selectedViewsRef: { current: [] },
  onSubmitValue: Utils.NO_OP,
  onUnsetValue: Utils.NO_OP,
  collectActionsToSubmitValue: emptyCollectActionsFn,
  collectActionsToUnsetValue: emptyCollectActionsFn,
})
InspectorCallbackContext.displayName = 'InspectorCallbackContext'

function extractSimpleValueFromMultiSelectAttribute(
  attrs: GetModifiableAttributeResult | undefined,
): Either<string, any> {
  if (attrs == null) {
    return left('Unknown attributes.')
  } else {
    return flatMapEither(jsxSimpleAttributeToValue, attrs)
  }
}

export type MultiselectAtProps<PropertiesToControl extends ParsedPropertiesKeys> = {
  [key in PropertiesToControl]: readonly Either<string, ModifiableAttribute>[]
}

export type MultiselectAtStringProps = {
  [key in string]: readonly Either<string, ModifiableAttribute>[]
}

export interface InspectorInfo<T> {
  value: T
  controlStatus: ControlStatus
  propertyStatus: PropertyStatus
  controlStyles: ControlStyles
  onUnsetValues: () => void
  onSubmitValue: (newTransformedValues: T, transient?: boolean) => void
  onTransientSubmitValue: (newTransformedValues: T) => void
  useSubmitValueFactory: UseSubmitValueFactory<T>
}

export type InspectorInfoWithRawValue<T> = InspectorInfo<T> & {
  attributeExpression: ModifiableAttribute | null
}

function getSpiedValues<P extends string | number>(
  key: P,
  selectedProps: { [keyValue in P]: ReadonlyArray<any> },
): ReadonlyArray<any> {
  if (key in selectedProps) {
    return selectedProps[key]
  } else {
    return []
  }
}

function getComputedStyleValues(
  key: string,
  selectedComputedStyles: { [k: string]: ReadonlyArray<string> },
): ReadonlyArray<string> {
  return selectedComputedStyles[key] ?? []
}

function getAttributeMetadatas(
  key: string,
  selectedAttributeMetadatas: {
    [key: string]: ReadonlyArray<StyleAttributeMetadataEntry>
  },
): ReadonlyArray<StyleAttributeMetadataEntry> {
  return selectedAttributeMetadatas[key] ?? []
}

// TODO also memoize me!
export function useInspectorInfoFromMultiselectMultiStyleAttribute<
  PropertiesToControl extends ParsedPropertiesKeys,
>(
  multiselectAtProps: MultiselectAtProps<PropertiesToControl>,
  selectedProps: { [key in PropertiesToControl]: ReadonlyArray<any> },
  selectedComputedStyles: { [key in PropertiesToControl]: ReadonlyArray<string> },
  selectedAttributeMetadatas: {
    [key in PropertiesToControl]: ReadonlyArray<StyleAttributeMetadataEntry>
  },
): {
  [key in PropertiesToControl]: {
    simpleValues: ReadonlyArray<Either<string, any>>
    rawValues: ReadonlyArray<Either<string, ModifiableAttribute>>
    spiedValues: ReadonlyArray<any>
    computedValues: ReadonlyArray<string>
    attributeMetadatas: ReadonlyArray<StyleAttributeMetadataEntry>
  }
} {
  const multiselectLength = useContextSelector(InspectorPropsContext, (c) => {
    return c.editedMultiSelectedProps.length
  })

  return React.useMemo(() => {
    const multiselectAtPropsKeys = Object.keys(multiselectAtProps)
    return Utils.mapArrayToDictionary(
      multiselectAtPropsKeys,
      (key) => key as PropertiesToControl,
      (key) => {
        if (multiselectLength === 0) {
          return {
            simpleValues: [left('No value')],
            rawValues: [left('Nothing selected')],
            spiedValues: [undefined],
            computedValues: [],
            attributeMetadatas: [],
          }
        }

        const rawValues = multiselectAtProps[key as PropertiesToControl]
        const simpleValues = rawValues.map((rawValue) =>
          extractSimpleValueFromMultiSelectAttribute(rawValue),
        )
        const spiedValues = getSpiedValues(key, selectedProps)
        const computedValues = getComputedStyleValues(key, selectedComputedStyles)
        const attributeMetadatas = getAttributeMetadatas(key, selectedAttributeMetadatas)

        return {
          simpleValues,
          rawValues,
          spiedValues,
          computedValues,
          attributeMetadatas,
        }
      },
    )
  }, [
    multiselectAtProps,
    multiselectLength,
    selectedProps,
    selectedComputedStyles,
    selectedAttributeMetadatas,
  ])
}

// FIXME: copy pasted for component prop section
export function useInspectorInfoFromMultiselectMultiPropAttribute(
  multiselectAtProps: MultiselectAtStringProps,
  selectedProps: { [key in string]: ReadonlyArray<any> },
): {
  [key in string]: {
    simpleValues: ReadonlyArray<Either<string, any>>
    rawValues: ReadonlyArray<Either<string, ModifiableAttribute>>
    spiedValues: ReadonlyArray<any>
  }
} {
  const multiselectLength = useContextSelector(InspectorPropsContext, (c) => {
    return c.editedMultiSelectedProps.length
  })

  return React.useMemo(() => {
    const multiselectAtPropsKeys = Object.keys(multiselectAtProps)
    return Utils.mapArrayToDictionary(
      multiselectAtPropsKeys,
      (key) => key,
      (key) => {
        if (multiselectLength === 0) {
          return {
            simpleValues: [left('No value')],
            rawValues: [left('Nothing selected')],
            spiedValues: [undefined],
          }
        }

        const rawValues = multiselectAtProps[key]
        const simpleValues = rawValues.map((rawValue) =>
          extractSimpleValueFromMultiSelectAttribute(rawValue),
        )
        const spiedValues = getSpiedValues(key, selectedProps)

        return {
          simpleValues,
          rawValues,
          spiedValues,
        }
      },
    )
  }, [multiselectAtProps, multiselectLength, selectedProps])
}

/* eslint-disable react-hooks/rules-of-hooks, react-hooks/exhaustive-deps */
/**
 * DO NOT USE useCallbackFactory outside of inspector-hooks.ts,
 * it is only exported for testing purposes
 *
 * useCallbackFactory is a meta-hook that returns a callback factory hook.
 * As you might see from the disabled linter, this goes a bit against the grain of the Rules of Hooks
 *
 * consult inspector-hooks.spec.ts to see what were the goals / known limitations
 */
export function useCallbackFactory<T>(
  oldValue: T,
  callback: (newValue: T, transient?: boolean) => void,
): <NT>(
  transform: (newValue: NT, old: T) => T,
) => [(newValue: NT) => void, (newValue: NT) => void] {
  return <NT>(transform: (newValue: NT, old: T) => T) => {
    return React.useMemo(() => {
      return [
        (newValue: NT, transient?: boolean) => callback(transform(newValue, oldValue), transient),
        (newValue: NT) => callback(transform(newValue, oldValue), true),
      ]
    }, [transform, oldValue, callback])
  }
}
/* eslint-enable react-hooks/rules-of-hooks, react-hooks/exhaustive-deps */

function elementPathMappingFn<P extends ParsedElementPropertiesKeys>(p: P) {
  return PP.create(p)
}

export function useInspectorElementInfo<P extends ParsedElementPropertiesKeys>(prop: P) {
  type T = ParsedElementProperties[P]
  const transformValue: (parsedValues: ParsedValues<P>) => T = (parsedValues) => parsedValues[prop]

  const untransformValue = (transformedType: T) =>
    ({
      [prop]: transformedType,
    } as Partial<ParsedValues<P>>)

  return useInspectorInfo([prop], transformValue, untransformValue, elementPathMappingFn)
}

export function stylePropPathMappingFn<
  P extends ParsedCSSPropertiesKeys,
  T extends PropertyPathPart,
>(p: P, target: readonly [T]): PropertyPath<[T, P]>
export function stylePropPathMappingFn<P extends ParsedCSSPropertiesKeys>(
  p: P,
  target: readonly string[],
): PropertyPath
export function stylePropPathMappingFn<P extends ParsedCSSPropertiesKeys>(
  p: P,
  target: readonly string[],
): PropertyPath {
  return PP.create(...target, p)
}

export function useInspectorStyleInfo<P extends ParsedCSSPropertiesKeys>(
  prop: P,
  transformValue: (parsedValues: ParsedValues<P>) => ParsedCSSProperties[P] = (parsedValues) =>
    parsedValues[prop],
  untransformValue: (transformedType: ParsedCSSProperties[P]) => Partial<ParsedValues<P>> = (
    transformedType,
  ) =>
    ({
      [prop]: transformedType,
    } as Partial<ParsedValues<P>>),
): InspectorInfo<ParsedCSSProperties[P]> {
  return useInspectorInfo([prop], transformValue, untransformValue, stylePropPathMappingFn)
}

export function useInspectorContext(): {
  selectedViewsRef: ReadonlyRef<Array<ElementPath>>
  onContextSubmitValue: (newValue: any, propertyPath: PropertyPath, transient: boolean) => void
  onContextUnsetValue: (
    propertyPath: PropertyPath | Array<PropertyPath>,
    transient: boolean,
  ) => void
  collectActionsToSubmitValue: (
    propertyPath: PropertyPath,
    transient: boolean,
    newValuePrinter: () => any,
  ) => Array<EditorAction>
  collectActionsToUnsetValue: (
    propertyPath: PropertyPath | Array<PropertyPath>,
    transient: boolean,
  ) => Array<EditorAction>
} {
  const {
    onSubmitValue,
    onUnsetValue,
    collectActionsToSubmitValue,
    collectActionsToUnsetValue,
    selectedViewsRef,
  } = React.useContext(InspectorCallbackContext)
  return React.useMemo(() => {
    return {
      onContextSubmitValue: onSubmitValue,
      onContextUnsetValue: onUnsetValue,
      collectActionsToSubmitValue: collectActionsToSubmitValue,
      collectActionsToUnsetValue: collectActionsToUnsetValue,
      selectedViewsRef: selectedViewsRef,
    }
  }, [
    onSubmitValue,
    onUnsetValue,
    collectActionsToSubmitValue,
    collectActionsToUnsetValue,
    selectedViewsRef,
  ])
}

function parseFinalValue<PropertiesToControl extends ParsedPropertiesKeys>(
  property: PropertiesToControl,
  simpleValue: Either<string, any>,
  rawValue: Either<string, ModifiableAttribute>,
  spiedValue: any,
  computedValue: string | undefined,
  attributeMetadataEntry: StyleAttributeMetadataEntry | undefined,
): {
  finalValue: ParsedPropertiesValues
  isUnknown: boolean
  trivialDefault: boolean
  usesComputedFallback: boolean
  setFromCssStyleSheet: boolean
} {
  const simpleValueAsMaybe = eitherToMaybe(simpleValue)
  const rawValueAsMaybe = eitherToMaybe(rawValue)
  const parsedValue = parseAnyParseableValue(property, simpleValueAsMaybe, rawValueAsMaybe)

  if (isRight(parsedValue)) {
    return {
      finalValue: parsedValue.value,
      isUnknown: isCSSUnknownFunctionParameters(parsedValue.value),
      trivialDefault: false,
      usesComputedFallback: false,
      setFromCssStyleSheet: false,
    }
  } else {
    const parsedSpiedValue = parseAnyParseableValue(property, spiedValue, null)

    if (isRight(parsedSpiedValue)) {
      return {
        finalValue: parsedSpiedValue.value,
        isUnknown: simpleValueAsMaybe != null,
        trivialDefault: false,
        usesComputedFallback: false,
        setFromCssStyleSheet: false,
      }
    } else {
      const parsedComputedValue = parseAnyParseableValue(property, computedValue, null)

      if (isRight(parsedComputedValue)) {
        const detectedComputedValue = parsedComputedValue.value
        const trivialDefault = isTrivialDefaultValue(property, detectedComputedValue)
        return {
          finalValue: detectedComputedValue,
          isUnknown: simpleValueAsMaybe != null,
          trivialDefault: trivialDefault,
          usesComputedFallback: true,
          setFromCssStyleSheet: attributeMetadataEntry?.fromStyleSheet ?? false,
        }
      } else {
        const defaultValue = emptyValues[property]
        const trivialDefault = isTrivialDefaultValue(property, defaultValue)
        return {
          finalValue: emptyValues[property],
          isUnknown: simpleValueAsMaybe != null,
          trivialDefault: trivialDefault,
          usesComputedFallback: false,
          setFromCssStyleSheet: false,
        }
      }
    }
  }
}

export type ParsedValues<P extends ParsedPropertiesKeys> = { [key in P]: ParsedProperties[key] }

export type SubmitValueFactoryReturn<T> = [(newValue: T) => void, (newValue: T) => void]

/**
 * @returns [onSubmitValue, onTransientSubmitValue]
 */
export type UseSubmitValueFactory<T> = <NewType>(
  transform: (newValue: NewType, oldValue: T) => T,
) => SubmitValueFactoryReturn<NewType>

export type OnUnsetValues = () => void

function useMemoizedPropKeys<T>(propKeys: Array<T>): Array<T> {
  return useKeepShallowReferenceEquality(propKeys)
}

export type PathMappingFn<P> = (propKey: P, targetPath: readonly string[]) => PropertyPath

export type TransformInspectorInfo<P extends ParsedPropertiesKeys, T = ParsedProperties[P]> = (
  parsedValues: ParsedValues<P>,
) => T
export type UntransformInspectorInfo<P extends ParsedPropertiesKeys, T = ParsedProperties[P]> = (
  transformedType: T,
) => Partial<ParsedValues<P>>

export function useInspectorInfo<P extends ParsedPropertiesKeys, T = ParsedProperties[P]>(
  propKeysIn: Array<P>,
  transformValue: TransformInspectorInfo<P, T>,
  untransformValue: UntransformInspectorInfo<P, T>,
  pathMappingFn: PathMappingFn<P>,
): InspectorInfo<T> {
  const propKeys = useMemoizedPropKeys(propKeysIn)
  const multiselectAtProps: MultiselectAtProps<P> = useGetMultiselectedProps<P>(
    pathMappingFn,
    propKeys,
  )

  const selectedProps = useGetSpiedProps<P>(pathMappingFn, propKeys)

  const selectedComputedStyles = useGetSelectedComputedStyles<P>(pathMappingFn, propKeys)

  const selectedAttributeMetadatas: {
    [key in P]: StyleAttributeMetadataEntry[]
  } = useGetSelectedAttributeMetadatas<P>(pathMappingFn, propKeys)

  const simpleAndRawValues = useInspectorInfoFromMultiselectMultiStyleAttribute(
    multiselectAtProps,
    selectedProps,
    selectedComputedStyles,
    selectedAttributeMetadatas,
  )

  const propertyStatus = calculateMultiPropertyStatusForSelection(
    multiselectAtProps,
    simpleAndRawValues,
  )

  const { values, isUnknown } = getParsedValues(propKeys, simpleAndRawValues, propertyStatus)

  const controlStatus = calculateControlStatusWithUnknown(propertyStatus, isUnknown)

  const {
    onContextSubmitValue: onSingleSubmitValue,
    onContextUnsetValue: onUnsetValue,
    collectActionsToSubmitValue,
    collectActionsToUnsetValue,
  } = useInspectorContext()

  const target = useKeepReferenceEqualityIfPossible(
    useContextSelector(InspectorPropsContext, (contextData) => contextData.targetPath, deepEqual),
  )
  const onUnsetValues = React.useCallback(() => {
    onUnsetValue(
      propKeys.map((propKey) => pathMappingFn(propKey, target)),
      false,
    )
  }, [onUnsetValue, propKeys, pathMappingFn, target])

  const transformedValue = transformValue(values)

  const onSubmitValue: (newValue: T, transient?: boolean) => void = useCreateOnSubmitValue<P, T>(
    untransformValue,
    propKeys,
    pathMappingFn,
    target,
    collectActionsToSubmitValue,
    collectActionsToUnsetValue,
  )

  const onTransientSubmitValue: (newValue: T) => void = React.useCallback(
    (newValue) => onSubmitValue(newValue, true),
    [onSubmitValue],
  )

  const useSubmitValueFactory = useCallbackFactory(transformedValue, onSubmitValue)

  const controlStyles = getControlStyles(controlStatus)
  const propertyStatusToReturn = useKeepReferenceEqualityIfPossible(propertyStatus)

  return {
    value: transformedValue,
    controlStatus,
    propertyStatus: propertyStatusToReturn,
    controlStyles,
    onSubmitValue,
    onTransientSubmitValue,
    onUnsetValues,
    useSubmitValueFactory,
  }
}

function useCreateOnSubmitValue<P extends ParsedPropertiesKeys, T = ParsedProperties[P]>(
  untransformValue: UntransformInspectorInfo<P, T>,
  propKeys: P[],
  pathMappingFn: PathMappingFn<P>,
  target: readonly string[],
  collectActionsToSubmitValue: (
    propertyPath: PropertyPath,
    transient: boolean,
    newValuePrinter: () => any,
  ) => Array<EditorAction>,
  collectActionsToUnsetValue: (
    propertyPath: PropertyPath | Array<PropertyPath>,
    transient: boolean,
  ) => Array<EditorAction>,
): (newValue: T, transient?: boolean | undefined) => void {
  const dispatch = useDispatch()
  return React.useCallback(
    (newValue, transient = false) => {
      const untransformedValue = untransformValue(newValue)
      let actions: Array<EditorAction> = []
      propKeys.forEach((propKey) => {
        const propertyPath = pathMappingFn(propKey, target)
        const valueToPrint = untransformedValue[propKey]
        if (valueToPrint != null) {
          actions.push(
            ...collectActionsToSubmitValue(propertyPath, transient, () =>
              printCSSValue(propKey, valueToPrint as ParsedProperties[P]),
            ),
          )
        } else {
          actions.push(...collectActionsToUnsetValue(propertyPath, transient))
        }
      })

      dispatch(actions)
    },
    [
      collectActionsToSubmitValue,
      collectActionsToUnsetValue,
      untransformValue,
      propKeys,
      pathMappingFn,
      target,
      dispatch,
    ],
  )
}

// TODO INTRODUCE UNKNOWN TO PROPERTY STATUS, ROLL THIS TO `getControlStatusFromPropertyStatus` PROPER
function calculateControlStatusWithUnknown(propertyStatus: PropertyStatus, isUnknown: boolean) {
  let controlStatus: ControlStatus = getControlStatusFromPropertyStatus(propertyStatus)
  if (isUnknown) {
    if (controlStatus === 'simple') {
      controlStatus = 'simple-unknown-css'
    } else if (
      controlStatus === 'multiselect-identical-simple' ||
      controlStatus === 'multiselect-identical-unset' ||
      controlStatus === 'multiselect-mixed-simple-or-unset'
    ) {
      controlStatus = 'multiselect-simple-unknown-css'
    }
  }
  return controlStatus
}

function useGetSelectedAttributeMetadatas<P extends ParsedPropertiesKeys>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: P[],
): { [key in P]: StyleAttributeMetadataEntry[] } {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propKey: P) => propKey
        const mapFn = (propKey: P): StyleAttributeMetadataEntry[] => {
          const path = PP.getElements(pathMappingFn(propKey, contextData.targetPath))
          const isStylePath = path[0] === 'style' || path[0] === 'css'
          if (isStylePath && path.length === 2) {
            return contextData.selectedAttributeMetadatas.map((attributeMetadata) => {
              return ObjectPath.get(attributeMetadata, path[1])
            })
          } else {
            return []
          }
        }
        return Utils.mapArrayToDictionary(propKeys, keyFn, mapFn)
      },
      deepEqual,
    ),
  )
}

function useGetSelectedComputedStyles<P extends ParsedPropertiesKeys>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: P[],
) {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propKey: P) => propKey
        const mapFn = (propKey: P): string[] => {
          const path = PP.getElements(pathMappingFn(propKey, contextData.targetPath))
          const isStylePath = path[0] === 'style' || path[0] === 'css'
          if (isStylePath && path.length === 2) {
            return contextData.computedStyles.map((computedStyle) => {
              return ObjectPath.get(computedStyle, path[1])
            })
          } else {
            return []
          }
        }
        return Utils.mapArrayToDictionary(propKeys, keyFn, mapFn)
      },
      deepEqual,
    ),
  )
}

function useGetSpiedProps<P extends ParsedPropertiesKeys>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: P[],
) {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propKey: P) => propKey
        const mapFn = (propKey: P) => {
          return contextData.spiedProps.map((props) => {
            return ObjectPath.get(
              props,
              PP.getElements(pathMappingFn(propKey, contextData.targetPath)),
            )
          })
        }
        return Utils.mapArrayToDictionary(propKeys, keyFn, mapFn)
      },
      deepEqual,
    ),
  )
}

const getModifiableAttributeResultToExpressionOptic = eitherRight<
  string,
  ModifiableAttribute
>().compose(fromTypeGuard(isRegularJSXAttribute))

function maybeStyleInfoKeyFromPropertyPath(propertyPath: PropertyPath): keyof StyleInfo | null {
  const maybeCSSProp = maybeCssPropertyFromInlineStyle(propertyPath)
  if (maybeCSSProp == null || !isStyleInfoKey(maybeCSSProp)) {
    return null
  }
  return maybeCSSProp
}

export function useGetMultiselectedProps<P extends ParsedPropertiesKeys>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: P[],
): MultiselectAtProps<P> {
  const styleInfoReaderRef = useRefEditorState(
    (store) =>
      (props: JSXAttributes, prop: keyof StyleInfo): GetModifiableAttributeResult => {
        const elementStyle = getActivePlugin(store.editor).readStyleFromElementProps(props, prop)
        if (elementStyle == null) {
          return left('not found')
        }
        switch (elementStyle.type) {
          case 'not-found':
            return right({ type: 'ATTRIBUTE_NOT_FOUND' })
          case 'not-parsable':
            return right(elementStyle.originalValue)
          case 'property':
            return right(elementStyle.propertyValue)
          default:
            assertNever(elementStyle)
        }
      },
  )

  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propKey: P) => propKey
        const mapFn = (propKey: P) => {
          return contextData.editedMultiSelectedProps.map((props) => {
            const targetPath = pathMappingFn(propKey, contextData.targetPath)
            const maybeStyleInfoKey = maybeStyleInfoKeyFromPropertyPath(targetPath)
            const result =
              maybeStyleInfoKey != null
                ? styleInfoReaderRef.current(props, maybeStyleInfoKey)
                : getModifiableJSXAttributeAtPath(props, targetPath)

            // This wipes the uid from any `JSExpression` values we may have retrieved,
            // as that can cause the deep equality check to fail for different elements
            // with the same value for a given property.
            return modify(
              getModifiableAttributeResultToExpressionOptic,
              clearExpressionUniqueIDs,
              result,
            )
          })
        }
        return Utils.mapArrayToDictionary(propKeys, keyFn, mapFn)
      },
      deepEqual,
    ),
  )
}

function getParsedValues<P extends ParsedPropertiesKeys>(
  propKeys: P[],
  simpleAndRawValues: {
    [key in P]: {
      simpleValues: readonly Either<string, any>[]
      rawValues: readonly Either<string, ModifiableAttribute>[]
      spiedValues: readonly any[]
      computedValues: readonly string[]
      attributeMetadatas: readonly StyleAttributeMetadataEntry[]
    }
  },
  propertyStatus: PropertyStatus,
): { values: ParsedValues<P>; isUnknown: boolean } {
  let isUnknownInner = false
  const valuesInner = Utils.mapArrayToDictionary(
    propKeys,
    (propKey) => propKey,
    (propKey) => {
      const { simpleValues, rawValues, spiedValues, computedValues, attributeMetadatas } =
        simpleAndRawValues[propKey]
      if (propertyStatus.identical) {
        const simpleValue: Either<string, any> = Utils.defaultIfNull(
          left('Simple value missing'),
          simpleValues[0],
        )
        const rawValue: Either<string, ModifiableAttribute> = Utils.defaultIfNull(
          left('Raw value missing'),
          rawValues[0],
        )
        const spiedValue: any = spiedValues[0]
        const computedValue = computedValues[0]
        const attributeMetadata = attributeMetadatas[0]
        const {
          finalValue,
          isUnknown: pathIsUnknown,
          usesComputedFallback,
          setFromCssStyleSheet,
          trivialDefault,
        } = parseFinalValue(
          propKey,
          simpleValue,
          rawValue,
          spiedValue,
          computedValue,
          attributeMetadata,
        )
        isUnknownInner = isUnknownInner || pathIsUnknown
        // setting the status to detected because it uses the fallback value
        // Mutates the parameter.
        propertyStatus.detected = usesComputedFallback
        propertyStatus.fromCssStyleSheet = setFromCssStyleSheet
        propertyStatus.trivialDefault = trivialDefault
        return finalValue
      } else {
        let firstFinalValue: ParsedPropertiesValues
        simpleValues.forEach((simpleValue, i) => {
          const rawValue: Either<string, ModifiableAttribute> = Utils.defaultIfNull(
            left('Raw value missing'),
            rawValues[i],
          )
          const spiedValue: any = spiedValues[i]
          const computedValue = computedValues[i]
          const attributeMetadata = attributeMetadatas[i]
          const {
            finalValue,
            isUnknown: pathIsUnknown,
            usesComputedFallback,
            setFromCssStyleSheet,
            trivialDefault,
          } = parseFinalValue(
            propKey,
            simpleValue,
            rawValue,
            spiedValue,
            computedValue,
            attributeMetadata,
          )
          if (i === 0) {
            firstFinalValue = finalValue
          }
          isUnknownInner = isUnknownInner || pathIsUnknown
          // setting the status to detected because it uses the fallback value
          // Mutates the parameter.
          propertyStatus.detected = propertyStatus.detected || usesComputedFallback
          propertyStatus.fromCssStyleSheet =
            propertyStatus.fromCssStyleSheet || setFromCssStyleSheet
          propertyStatus.trivialDefault = propertyStatus.trivialDefault && trivialDefault
        })
        return firstFinalValue
      }
    },
  ) as ParsedValues<P>
  return { isUnknown: isUnknownInner, values: valuesInner }
}

// TODO: Warning, this is just copy pasted code from useInspectorInfo. We needed a hook which
// works for any kind of prop (and not just css props). To properly do that we would need to refactor
// useInspectorInfo, but for now this is just a copy pasted version with less strict types and
// a default parser (which does nothing)
// FIXME: copy pasted for component prop section
export function useInspectorInfoSimpleUntyped(
  propertyPaths: ReadonlyArray<PropertyPath>,
  transformValue: (parsedValues: any) => any,
  untransformValue: (transformedType: any) => any,
): InspectorInfo<any> {
  const multiselectAtProps: MultiselectAtStringProps = useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const pathArray = (propertyPath: PropertyPath) =>
          propertyPath.propertyElements[propertyPath.propertyElements.length - 1]
        const keyFn = (propertyPath: PropertyPath) =>
          contextData.editedMultiSelectedProps.map((props) => {
            return getModifiableJSXAttributeAtPath(props, propertyPath)
          })
        return Utils.mapArrayToDictionary(propertyPaths, pathArray, keyFn)
      },
      deepEqual,
    ),
  )

  const selectedProps = useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propPath: PropertyPath) => PP.lastPart(propPath)
        const mapFn = (propPath: PropertyPath) => {
          return contextData.spiedProps.map((props) => {
            return ObjectPath.get(props, PP.getElements(propPath))
          })
        }
        return Utils.mapArrayToDictionary(propertyPaths, keyFn, mapFn)
      },
      deepEqual,
    ),
  )

  const simpleAndRawValues = useInspectorInfoFromMultiselectMultiPropAttribute(
    multiselectAtProps,
    selectedProps,
  )

  const propertyStatus = calculateMultiStringPropertyStatusForSelection(
    multiselectAtProps,
    simpleAndRawValues,
  )

  const values = Utils.mapArrayToDictionary(
    propertyPaths,
    (propertyPath) => propertyPath.propertyElements[propertyPath.propertyElements.length - 1],
    (propertyPath) => {
      const property = propertyPath.propertyElements[propertyPath.propertyElements.length - 1]
      const { simpleValues, spiedValues, rawValues } = simpleAndRawValues[property]
      const simpleValue = optionalMap(eitherToMaybe, simpleValues[0])
      if (spiedValues.length > 0) {
        return spiedValues[0]
      } else if (simpleValue != null) {
        return simpleValue
      } else {
        return optionalMap(eitherToMaybe, rawValues[0])
      }
    },
  )

  const controlStatus: ControlStatus = getControlStatusFromPropertyStatus(propertyStatus)

  const { onContextSubmitValue: onSingleSubmitValue, onContextUnsetValue: onSingleUnsetValue } =
    useInspectorContext()

  const onUnsetValues = React.useCallback(() => {
    for (const propertyPath of propertyPaths) {
      onSingleUnsetValue(propertyPath, false)
    }
  }, [onSingleUnsetValue, propertyPaths])

  const transformedValue = transformValue(values)
  const onSubmitValue = React.useCallback(
    (newValue: any, transient = false) => {
      const untransformedValue = untransformValue(newValue)
      const submitValue = (<Key extends string>(path: {
        propertyElements: ['style', Key] | ['layout', Key] | [Key]
      }) => {
        const property = path.propertyElements[path.propertyElements.length - 1] as Key
        if (property in untransformedValue) {
          const propertyToPrint = untransformedValue[property]
          const printedProperty = maybePrintCSSValue(property, propertyToPrint)
          onSingleSubmitValue(printedProperty, path, transient)
        } else {
          onSingleUnsetValue(path, transient)
        }
      }) as (path: PropertyPath) => void
      propertyPaths.forEach(submitValue)
    },
    [onSingleSubmitValue, untransformValue, propertyPaths, onSingleUnsetValue],
  )

  const onTransientSubmitValue = React.useCallback(
    (newValue: any) => onSubmitValue(newValue, true),
    [onSubmitValue],
  )

  const useSubmitValueFactory = useCallbackFactory(transformedValue, onSubmitValue)

  const controlStyles = getControlStyles(controlStatus)
  const propertyStatusToReturn = useKeepReferenceEqualityIfPossible(propertyStatus)

  return {
    value: transformedValue,
    controlStatus,
    propertyStatus: propertyStatusToReturn,
    controlStyles,
    onSubmitValue,
    onTransientSubmitValue,
    onUnsetValues,
    useSubmitValueFactory,
  }
}

export function useGetLayoutControlStatus<P extends StyleLayoutProp>(property: P): ControlStatus {
  const propKeys = useMemoizedPropKeys([property])
  const multiselectAtProps: MultiselectAtProps<P> = useGetMultiselectedProps<P>(
    stylePropPathMappingFn,
    propKeys,
  )

  const selectedProps = useGetSpiedProps<P>(stylePropPathMappingFn, propKeys)

  const selectedComputedStyles = useGetSelectedComputedStyles<P>(stylePropPathMappingFn, propKeys)

  const selectedAttributeMetadatas: {
    [key in P]: StyleAttributeMetadataEntry[]
  } = useGetSelectedAttributeMetadatas<P>(stylePropPathMappingFn, propKeys)

  const simpleAndRawValues = useInspectorInfoFromMultiselectMultiStyleAttribute(
    multiselectAtProps,
    selectedProps,
    selectedComputedStyles,
    selectedAttributeMetadatas,
  )

  const propertyStatus = calculateMultiPropertyStatusForSelection(
    multiselectAtProps,
    simpleAndRawValues,
  )

  return getControlStatusFromPropertyStatus(propertyStatus)
}

export function useInspectorLayoutInfo<P extends StyleLayoutProp>(
  property: P,
): InspectorInfo<ParsedProperties[P]> {
  function transformValue(parsedValues: ParsedValues<P>): ParsedProperties[P] {
    return parsedValues[property]
  }
  function untransformValue(transformedType: ParsedProperties[P]): Partial<ParsedValues<P>> {
    return { [property]: transformedType } as Partial<ParsedValues<P>>
  }

  let inspectorInfo = useInspectorInfo(
    [property],
    transformValue,
    untransformValue,
    stylePropPathMappingFn,
  )
  return inspectorInfo
}

export function useIsSubSectionVisible(sectionName: string): boolean {
  const selectedViews = useRefSelectedViews()

  return useEditorState(
    Substores.fullStore,
    (store) => {
      return selectedViews.current.every((view) => {
        const selectedViewType = withUnderlyingTarget(
          view,
          store.editor.projectContents,
          null,
          (underlyingSuccess, underlyingElement) => {
            if (isJSXElement(underlyingElement)) {
              if (isUtopiaAPIComponent(underlyingElement.name, underlyingSuccess.imports)) {
                return getJSXElementNameLastPart(underlyingElement.name).toString().toLowerCase()
              } else if (isHTMLComponent(underlyingElement.name, underlyingSuccess.imports)) {
                return underlyingElement.name.baseVariable
              } else {
                return getJSXElementNameAsString(underlyingElement.name)
              }
            } else {
              return null
            }
          },
        )

        const subSectionAvailable = StyleSubSectionForType[sectionName]
        if (subSectionAvailable == null) {
          return false
        } else {
          if (selectedViewType == null) {
            return true
          } else {
            switch (subSectionAvailable.type) {
              case 'permit':
                return subSectionAvailable.allowedElementTypes.includes(selectedViewType)
              case 'deny':
                return !subSectionAvailable.deniedElementTypes.includes(selectedViewType)
              case 'allowall':
                return true
              default:
                const _exhaustiveCheck: never = subSectionAvailable
                throw new Error(`Unhandled type ${JSON.stringify(subSectionAvailable)}`)
            }
          }
        }
      })
    },
    'useIsSubSectionVisible',
  )
}

interface SubSectionPermitList {
  type: 'permit'
  allowedElementTypes: Array<string>
}

function subSectionPermitList(allowedElementTypes: Array<string>): SubSectionPermitList {
  return {
    type: 'permit',
    allowedElementTypes: allowedElementTypes,
  }
}

interface SubSectionDenyList {
  type: 'deny'
  deniedElementTypes: Array<string>
}

function subSectionDenyList(deniedElementTypes: Array<string>): SubSectionDenyList {
  return {
    type: 'deny',
    deniedElementTypes: deniedElementTypes,
  }
}

interface SubSectionAllowAll {
  type: 'allowall'
}

const subSectionAllowAll: SubSectionAllowAll = {
  type: 'allowall',
}

type SubSectionAvailable = SubSectionPermitList | SubSectionDenyList | SubSectionAllowAll

const StyleSubSectionForType: { [key: string]: SubSectionAvailable } = {
  text: subSectionDenyList(['img', 'video']),
  textShadow: subSectionDenyList(['img', 'video']),
  shadow: subSectionAllowAll,
  border: subSectionAllowAll,
  opacity: subSectionAllowAll,
  background: subSectionAllowAll,
  img: subSectionPermitList(['img']),
  transform: subSectionAllowAll,
  overflow: subSectionDenyList(['img', 'video']),
}

const PropsToDiscard = [...UtopiaKeys, 'skipDeepFreeze', 'data-utopia-instance-path']
export function filterUtopiaSpecificProps(props: MapLike<any>) {
  return omitWithPredicate(props, (key) => typeof key === 'string' && PropsToDiscard.includes(key))
}

export function useInspectorWarningStatus(): boolean {
  const selectedViews = useSelectedViews()

  return useEditorState(
    Substores.fullStore,
    (store) => {
      let hasLayoutInCSSProp = false
      Utils.fastForEach(selectedViews, (view) => {
        if (hasLayoutInCSSProp) {
          return
        }

        forUnderlyingTargetFromEditorState(
          view,
          store.editor,
          (_underlyingSuccess, underlyingElement) => {
            if (isJSXElement(underlyingElement)) {
              const cssAttribute = getJSXAttribute(underlyingElement.props, 'css')
              if (cssAttribute != null) {
                const cssProps = Utils.defaultIfNull(
                  {},
                  eitherToMaybe(jsxSimpleAttributeToValue(cssAttribute)),
                )
                hasLayoutInCSSProp = isLayoutPropDetectedInCSS(cssProps)
              }
            }
          },
        )
      })
      return hasLayoutInCSSProp
    },
    'useInspectorWarningStatus',
  )
}

export function useSelectedViews(): ElementPath[] {
  const selectedViews = useContextSelector(
    InspectorPropsContext,
    (context) => context.selectedViews,
  )
  return selectedViews
}

export function useRefSelectedViews(): ReadonlyRef<ElementPath[]> {
  const { selectedViewsRef } = React.useContext(InspectorCallbackContext)
  return selectedViewsRef
}

export function useGetOrderedPropertyKeys<P>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: Readonly<Array<P>>,
): Array<Array<P>> {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        return contextData.editedMultiSelectedProps.map((props) =>
          stripNulls(
            getAllPathsFromAttributes(props).map((path) =>
              propKeys.find((propKey) =>
                PP.pathsEqual(path, pathMappingFn(propKey, contextData.targetPath)),
              ),
            ),
          ),
        )
      },
      deepEqual,
    ),
  )
}
