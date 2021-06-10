import * as deepEqual from 'fast-deep-equal'
import * as ObjectPath from 'object-path'
import * as React from 'react'
import { createContext, useContextSelector } from 'use-context-selector'
import { PropertyControls } from 'utopia-api'
import {
  forUnderlyingTargetFromEditorState,
  withUnderlyingTarget,
} from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import {
  calculateMultiPropertyStatusForSelection,
  calculateMultiStringPropertyStatusForSelection,
  ControlStatus,
  ControlStyles,
  getControlStatusFromPropertyStatus,
  getControlStyles,
  PropertyStatus,
} from '../../../components/inspector/common/control-status'
import {
  emptyValues,
  isCSSUnknownFunctionParameters,
  isLayoutPropDetectedInCSS,
  maybePrintCSSValue,
  parseAnyParseableValue,
  ParsedCSSProperties,
  ParsedCSSPropertiesKeys,
  ParsedCSSPropertiesKeysNoLayout,
  ParsedElementProperties,
  ParsedElementPropertiesKeys,
  ParsedProperties,
  ParsedPropertiesKeys,
  ParsedPropertiesValues,
  printCSSValue,
  cssNumber,
  isTrivialDefaultValue,
} from '../../../components/inspector/common/css-utils'
import {
  createLayoutPropertyPath,
  LayoutProp,
  StyleLayoutProp,
} from '../../../core/layout/layout-helpers-new'
import { findElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getFilePathForImportedComponent,
  getUtopiaJSXComponentsFromSuccess,
  isHTMLComponent,
  isUtopiaAPIComponent,
} from '../../../core/model/project-file-utils'
import {
  ParsedPropertyControls,
  parsePropertyControls,
} from '../../../core/property-controls/property-controls-parser'
import {
  filterSpecialProps,
  findMissingDefaults,
  getDefaultPropsFromParsedControls,
  removeIgnored,
  getPropertyControlsForTargetFromEditor,
} from '../../../core/property-controls/property-controls-utils'
import { addUniquely, mapDropNulls, stripNulls } from '../../../core/shared/array-utils'
import {
  defaultEither,
  Either,
  eitherToMaybe,
  flatMapEither,
  foldEither,
  isRight,
  left,
  mapEither,
} from '../../../core/shared/either'
import {
  getJSXElementNameLastPart,
  getJSXElementNameNoPathName,
  isJSXElement,
  JSXAttributes,
  UtopiaJSXComponent,
  ComputedStyle,
  getJSXAttribute,
  StyleAttributeMetadata,
  StyleAttributeMetadataEntry,
} from '../../../core/shared/element-template'
import {
  getAllPathsFromAttributes,
  GetModifiableAttributeResult,
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import { forEachOptional, optionalMap } from '../../../core/shared/optional-utils'
import { PropertyPath, ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import { fastForEach } from '../../../core/shared/utils'
import { KeepDeepEqualityCall } from '../../../utils/deep-equality'
import {
  keepDeepReferenceEqualityIfPossible,
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../../utils/react-performance'
import { default as Utils } from '../../../utils/utils'
import { ParseResult } from '../../../utils/value-parser-utils'
import type { ReadonlyRef } from './inspector-utils'
import { findUnderlyingTargetComponentImplementation } from '../../custom-code/code-file'
import type { MapLike } from 'typescript'
import { omitWithPredicate } from '../../../core/shared/object-utils'
import { UtopiaKeys } from '../../../core/model/utopia-constants'

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
  onUnsetValue: (propertyPath: PropertyPath | Array<PropertyPath>) => void
}

export const InspectorPropsContext = createContext<InspectorPropsContextData>({
  selectedViews: [],
  editedMultiSelectedProps: [],
  targetPath: [],
  spiedProps: [],
  computedStyles: [],
  selectedAttributeMetadatas: [],
})

export const InspectorCallbackContext = React.createContext<InspectorCallbackContextData>({
  selectedViewsRef: { current: [] },
  onSubmitValue: Utils.NO_OP,
  onUnsetValue: Utils.NO_OP,
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
  PropertiesToControl extends ParsedPropertiesKeys
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
  return PP.create([p])
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

export function stylePropPathMappingFn<P extends ParsedCSSPropertiesKeys>(
  p: P,
  target: readonly string[],
) {
  return PP.create([...target, p])
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
) {
  return useInspectorInfo([prop], transformValue, untransformValue, stylePropPathMappingFn)
}

export function useInspectorContext() {
  const { onSubmitValue, onUnsetValue, selectedViewsRef } = React.useContext(
    InspectorCallbackContext,
  )
  return React.useMemo(() => {
    return {
      onContextSubmitValue: onSubmitValue,
      onContextUnsetValue: onUnsetValue,
      selectedViewsRef: selectedViewsRef,
    }
  }, [onSubmitValue, onUnsetValue, selectedViewsRef])
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
  const parsedSpiedValue = parseAnyParseableValue(property, spiedValue, null)
  const parsedComputedValue = parseAnyParseableValue(property, computedValue, null)
  if (isRight(parsedValue)) {
    return {
      finalValue: parsedValue.value,
      isUnknown: isCSSUnknownFunctionParameters(parsedValue.value),
      trivialDefault: false,
      usesComputedFallback: false,
      setFromCssStyleSheet: false,
    }
  } else if (isRight(parsedSpiedValue)) {
    return {
      finalValue: parsedSpiedValue.value,
      isUnknown: simpleValueAsMaybe != null,
      trivialDefault: false,
      usesComputedFallback: false,
      setFromCssStyleSheet: false,
    }
  } else if (isRight(parsedComputedValue)) {
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
  } = useInspectorContext()

  const target = useKeepReferenceEqualityIfPossible(
    useContextSelector(InspectorPropsContext, (contextData) => contextData.targetPath, deepEqual),
  )
  const onUnsetValues = React.useCallback(() => {
    onUnsetValue(propKeys.map((propKey) => pathMappingFn(propKey, target)))
  }, [onUnsetValue, propKeys, pathMappingFn, target])

  const transformedValue = transformValue(values)
  const onSubmitValue: (newValue: T, transient?: boolean) => void = useCreateOnSubmitValue<P, T>(
    untransformValue,
    propKeys,
    pathMappingFn,
    target,
    onSingleSubmitValue,
    onUnsetValue,
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
  onSingleSubmitValue: (newValue: any, propertyPath: PropertyPath, transient: boolean) => void,
  onUnsetValue: (propertyPath: PropertyPath | Array<PropertyPath>) => void,
): (newValue: T, transient?: boolean | undefined) => void {
  return React.useCallback(
    (newValue, transient = false) => {
      const untransformedValue = untransformValue(newValue)
      propKeys.forEach((propKey) => {
        const propertyPath = pathMappingFn(propKey, target)
        const valueToPrint = untransformedValue[propKey]
        if (valueToPrint != null) {
          const printedProperty = printCSSValue(propKey, valueToPrint as ParsedProperties[P])
          onSingleSubmitValue(printedProperty, propertyPath, transient)
        } else {
          onUnsetValue(propertyPath)
        }
      })
    },
    [onSingleSubmitValue, untransformValue, propKeys, onUnsetValue, pathMappingFn, target],
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

function useGetMultiselectedProps<P extends ParsedPropertiesKeys>(
  pathMappingFn: PathMappingFn<P>,
  propKeys: P[],
): MultiselectAtProps<P> {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const keyFn = (propKey: P) => propKey
        const mapFn = (propKey: P) =>
          contextData.editedMultiSelectedProps.map((props) => {
            return getModifiableJSXAttributeAtPath(
              props,
              pathMappingFn(propKey, contextData.targetPath),
            )
          })
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
      const {
        simpleValues,
        rawValues,
        spiedValues,
        computedValues,
        attributeMetadatas,
      } = simpleAndRawValues[propKey]
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

  let controlStatus: ControlStatus = getControlStatusFromPropertyStatus(propertyStatus)

  const {
    onContextSubmitValue: onSingleSubmitValue,
    onContextUnsetValue: onSingleUnsetValue,
  } = useInspectorContext()

  const onUnsetValues = React.useCallback(() => {
    for (const propertyPath of propertyPaths) {
      onSingleUnsetValue(propertyPath)
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
          onSingleUnsetValue(path)
        }
      }) as (path: PropertyPath) => void
      propertyPaths.forEach(submitValue)
    },
    [onSingleSubmitValue, untransformValue, propertyPaths, onSingleUnsetValue],
  )

  const onTransientSubmitValue = React.useCallback((newValue) => onSubmitValue(newValue, true), [
    onSubmitValue,
  ])

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

export function useInspectorLayoutInfo<P extends LayoutProp | StyleLayoutProp>(
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
    createLayoutPropertyPath,
  )
  return inspectorInfo
}

export function useIsSubSectionVisible(sectionName: string): boolean {
  const selectedViews = useRefSelectedViews()

  return useEditorState((store) => {
    const types = selectedViews.current.map((view) => {
      return withUnderlyingTarget(
        view,
        store.editor.projectContents,
        store.editor.nodeModules.files,
        store.editor.canvas.openFile?.filename ?? null,
        null,
        (underlyingSuccess, underlyingElement) => {
          if (isJSXElement(underlyingElement)) {
            if (isUtopiaAPIComponent(underlyingElement.name, underlyingSuccess.imports)) {
              return getJSXElementNameLastPart(underlyingElement.name).toString().toLowerCase()
            } else if (isHTMLComponent(underlyingElement.name, underlyingSuccess.imports)) {
              return underlyingElement.name.baseVariable
            } else {
              return null
            }
          } else {
            return null
          }
        },
      )
    })
    return types.every((type) => {
      const allowedTypes = StyleSubSectionForType[sectionName]
      if (allowedTypes == null) {
        return false
      } else {
        if (typeof allowedTypes === 'boolean') {
          return allowedTypes
        } else {
          return allowedTypes.find((t) => t === type)
        }
      }
    })
  }, 'useIsSubSectionVisible')
}

const StyleSubSectionForType: { [key: string]: string[] | boolean } = {
  text: ['div', 'span', 'text'],
  textShadow: ['div', 'span', 'text'],
  shadow: true,
  border: true,
  opacity: true,
  background: true,
  img: ['img'],
  transform: true,
}

export function useSelectedPropertyControls(
  includeIgnored: boolean,
): ParseResult<ParsedPropertyControls> {
  const selectedViews = useRefSelectedViews()

  const propertyControls = useEditorState((store) => {
    const { codeResultCache } = store.editor

    let selectedPropertyControls: PropertyControls | null = {}
    if (codeResultCache != null) {
      Utils.fastForEach(selectedViews.current, (path) => {
        // TODO multiselect
        // TODO use getPropertyControlsForTarget and reselect selectors
        selectedPropertyControls = getPropertyControlsForTargetFromEditor(path, store.editor) ?? {}
      })
    }

    return selectedPropertyControls
  }, 'useSelectedPropertyControls')

  // Strip ignored property controls here.
  const parsed = parsePropertyControls(propertyControls)
  if (includeIgnored) {
    return parsed
  } else {
    return mapEither(removeIgnored, parsed)
  }
}

export function useUsedPropsWithoutControls(propsGiven: Array<string>): Array<string> {
  const parsedPropertyControls = useSelectedPropertyControls(true)

  const selectedViews = useRefSelectedViews()

  const selectedComponents = useEditorState((store) => {
    let components: Array<UtopiaJSXComponent> = []
    fastForEach(selectedViews.current, (path) => {
      const openStoryboardFile = store.editor.canvas.openFile?.filename ?? null
      if (openStoryboardFile != null) {
        const component = findUnderlyingTargetComponentImplementation(
          store.editor.projectContents,
          store.editor.nodeModules.files,
          openStoryboardFile,
          path,
        )
        if (component != null) {
          components.push(component)
        }
      }
    })
    return components
  }, 'useUsedPropsWithoutControls')

  const propertiesWithControls = Object.keys(eitherToMaybe(parsedPropertyControls) ?? {})
  let propertiesWithoutControls: Array<string> = []
  fastForEach(selectedComponents, (component) => {
    if (isJSXElement(component.rootElement)) {
      fastForEach(component.propsUsed, (propUsed) => {
        if (!propertiesWithControls.includes(propUsed) && !propsGiven.includes(propUsed)) {
          propertiesWithoutControls = addUniquely(propertiesWithoutControls, propUsed)
        }
      })
    }
  })

  return propertiesWithoutControls
}

const PropsToDiscard = [...UtopiaKeys, 'skipDeepFreeze', 'data-utopia-instance-path']
function filterUtopiaSpecificProps(props: MapLike<any>) {
  return omitWithPredicate(props, (key) => typeof key === 'string' && PropsToDiscard.includes(key))
}

export function useGivenPropsWithoutControls(): Array<string> {
  const parsedPropertyControls = useSelectedPropertyControls(true)
  const selectedViews = useRefSelectedViews()

  const selectedElements = useEditorState((store) => {
    return mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path),
      selectedViews.current,
    )
  }, 'useGivenPropsWithoutControls')

  const propertiesWithControls = filterSpecialProps(
    Object.keys(eitherToMaybe(parsedPropertyControls) ?? {}),
  )
  let givenProps: Array<string> = []
  fastForEach(selectedElements, (element) => {
    const elementProps = filterUtopiaSpecificProps(element.props)
    fastForEach(Object.keys(elementProps), (propName) => {
      if (!propertiesWithControls.includes(propName)) {
        givenProps = addUniquely(givenProps, propName)
      }
    })
  })

  return givenProps
}

export function useUsedPropsWithoutDefaults(): Array<string> {
  const parsedPropertyControls = useSelectedPropertyControls(false)

  const selectedViews = useRefSelectedViews()

  const selectedComponentProps = useEditorState((store) => {
    let propsUsed: Array<string> = []
    fastForEach(selectedViews.current, (path) => {
      forUnderlyingTargetFromEditorState(
        path,
        store.editor,
        (underlyingSuccess, underlyingElement) => {
          const rootComponents = getUtopiaJSXComponentsFromSuccess(underlyingSuccess)
          if (underlyingElement != null && isJSXElement(underlyingElement)) {
            const noPathName = getJSXElementNameNoPathName(underlyingElement.name)
            for (const component of rootComponents) {
              if (component.name === noPathName) {
                propsUsed.push(...component.propsUsed)
                break
              }
            }
          }
        },
      )
    })
    return propsUsed
  }, 'useUsedPropsWithoutDefaults')

  const defaultProps = getDefaultPropsFromParsedControls(parsedPropertyControls)
  return findMissingDefaults(selectedComponentProps, defaultProps)
}

export function useInspectorWarningStatus(): boolean {
  const selectedViews = useSelectedViews()

  return useEditorState((store) => {
    let hasLayoutInCSSProp = false
    Utils.fastForEach(selectedViews, (view) => {
      if (hasLayoutInCSSProp) {
        return
      }

      forUnderlyingTargetFromEditorState(
        view,
        store.editor,
        (underlyingSuccess, underlyingElement) => {
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
  }, 'useInspectorWarningStatus')
}

export function useSelectedViews() {
  const selectedViews = useContextSelector(
    InspectorPropsContext,
    (context) => context.selectedViews,
  )
  return selectedViews
}

export function useRefSelectedViews() {
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
