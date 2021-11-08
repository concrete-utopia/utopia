import deepEqual from 'fast-deep-equal'
import * as ObjectPath from 'object-path'
import React from 'react'
import { useContextSelector } from 'use-context-selector'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import {
  printerForPropertyControl,
  unwrapperAndParserForPropertyControl,
  controlToUseForUnion,
  getPropertyControlNames,
} from '../../../core/property-controls/property-control-values'
import {
  BaseControlDescription,
  UnionControlDescription,
  ControlDescription,
  ArrayControlDescription,
  HigherLevelControlDescription,
  RegularControlDescription,
} from 'utopia-api'
import {
  filterUtopiaSpecificProps,
  InspectorInfo,
  InspectorPropsContext,
  useCallbackFactory,
  useInspectorContext,
  useRefSelectedViews,
} from './property-path-hooks'
import {
  getModifiableJSXAttributeAtPath,
  ModifiableAttribute,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attributes'
import * as PP from '../../../core/shared/property-path'
import {
  Either,
  eitherToMaybe,
  flatMapEither,
  foldEither,
  mapEither,
  right,
  unwrapEither,
} from '../../../core/shared/either'
import {
  calculatePropertyStatusForSelection,
  ControlStatus,
  getControlStatusFromPropertyStatus,
  getControlStyles,
} from './control-status'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXAttributes,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { addUniquely, mapArrayToDictionary, mapDropNulls } from '../../../core/shared/array-utils'
import { ParseError, ParseResult } from '../../../utils/value-parser-utils'
import { ParsedPropertyControls } from '../../../core/property-controls/property-controls-parser'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getPropertyControlsForTargetFromEditor } from '../../../core/property-controls/property-controls-utils'
import { fastForEach } from '../../../core/shared/utils'
import { findUnderlyingTargetComponentImplementation } from '../../custom-code/code-file'
import {
  ElementInstanceMetadataKeepDeepEquality,
  UtopiaJSXComponentKeepDeepEquality,
} from '../../editor/store/store-deep-equality-instances'
import { arrayDeepEquality } from '../../../utils/deep-equality'
import { omit } from '../../../core/shared/object-utils'

type RawValues = Either<string, ModifiableAttribute>[]
type RealValues = unknown[]

const propsToOmit = ['style', 'css', 'className']

function filterSpecialProp(propKey: string): boolean {
  return !propsToOmit.includes(propKey)
}

function filterSpecialProps(props: Array<string>): Array<string> {
  return props.filter(filterSpecialProp)
}

export function useInspectorInfoForPropertyControl(
  propertyPath: PropertyPath,
  control: RegularControlDescription,
): InspectorInfo<any> {
  const rawValues: RawValues = useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        return contextData.editedMultiSelectedProps.map((props) => {
          return getModifiableJSXAttributeAtPath(props, propertyPath)
        })
      },
      deepEqual,
    ),
  )

  const realValues: RealValues = useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        return contextData.spiedProps.map((props) => {
          return ObjectPath.get(props, PP.getElements(propertyPath))
        })
      },
      deepEqual,
    ),
  )

  const propertyStatus = calculatePropertyStatusForSelection(rawValues, realValues)
  const controlStatus = getControlStatusFromPropertyStatus(propertyStatus)
  const controlStyles = getControlStyles(controlStatus)
  const propertyStatusToReturn = useKeepReferenceEqualityIfPossible(propertyStatus)

  const {
    onContextSubmitValue: onSingleSubmitValue,
    onContextUnsetValue: onSingleUnsetValue,
  } = useInspectorContext()

  const parserFn = unwrapperAndParserForPropertyControl(control)
  const printerFn = printerForPropertyControl(control)
  let parsedValue: unknown = null
  if (0 in rawValues && 0 in realValues) {
    parsedValue = eitherToMaybe(parserFn(rawValues[0], realValues[0])) // TODO We need a way to surface these errors to the users
  }

  const onSubmitValue = React.useCallback(
    (newValue: any, transient = false) => {
      if (newValue == null) {
        onSingleUnsetValue(propertyPath, transient)
      } else {
        const printedValue = printerFn(newValue)
        onSingleSubmitValue(printedValue, propertyPath, transient)
      }
    },
    [onSingleSubmitValue, printerFn, propertyPath, onSingleUnsetValue],
  )

  const onTransientSubmitValue = React.useCallback((newValue) => onSubmitValue(newValue, true), [
    onSubmitValue,
  ])

  const useSubmitValueFactory = useCallbackFactory(parsedValue, onSubmitValue)

  const onUnsetValues = React.useCallback(() => {
    onSingleUnsetValue(propertyPath, false)
  }, [onSingleUnsetValue, propertyPath])

  return {
    value: parsedValue,
    controlStatus,
    propertyStatus: propertyStatusToReturn,
    controlStyles,
    onSubmitValue,
    onTransientSubmitValue,
    onUnsetValues,
    useSubmitValueFactory,
  }
}

function useFirstRawValue(propertyPath: PropertyPath): Either<string, ModifiableAttribute> {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const firstElemProps = contextData.editedMultiSelectedProps[0] ?? {}
        return getModifiableJSXAttributeAtPath(firstElemProps, propertyPath)
      },
      deepEqual,
    ),
  )
}

function useFirstRealValue(propertyPath: PropertyPath): unknown {
  return useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => {
        const firstElemProps = contextData.spiedProps[0] ?? {}
        return ObjectPath.get(firstElemProps, PP.getElements(propertyPath))
      },
      deepEqual,
    ),
  )
}

export function useControlForUnionControl(
  propertyPath: PropertyPath,
  control: UnionControlDescription,
): RegularControlDescription | null {
  const firstRawValue = useFirstRawValue(propertyPath)
  const firstRealValue = useFirstRealValue(propertyPath)

  return controlToUseForUnion(control, firstRawValue, firstRealValue)
}

type ParsedPropertyControlsAndTargets = {
  controls: ParseResult<ParsedPropertyControls>
  targets: ElementPath[]
}

type PropertyControlsAndTargets = {
  controls: ParseResult<ParsedPropertyControls>
  targets: ElementPath[]
  propsWithControlsButNoValue: string[]
  detectedPropsWithNoValue: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
}

const emptyControls: ParseResult<ParsedPropertyControls> = right({})

export function useGetPropertyControlsForSelectedComponents(): Array<PropertyControlsAndTargets> {
  const selectedViews = useRefSelectedViews()

  const selectedPropertyControls = useEditorState(
    (store) => {
      let parsedPropertyControls: Array<ParsedPropertyControlsAndTargets> = []
      fastForEach(selectedViews.current, (path) => {
        const propertyControls = getPropertyControlsForTargetFromEditor(path, store.editor)
        if (propertyControls == null) {
          parsedPropertyControls.push({
            controls: emptyControls,
            targets: [path],
          })
        } else {
          const withFilteredProps = mapEither(
            (parsedControls) => omit(propsToOmit, parsedControls),
            propertyControls,
          )

          const foundMatch = parsedPropertyControls.findIndex((existing) =>
            areMatchingPropertyControls(existing.controls, withFilteredProps),
          )
          if (foundMatch > -1) {
            parsedPropertyControls[foundMatch].targets.push(path)
          } else {
            parsedPropertyControls.push({
              controls: withFilteredProps,
              targets: [path],
            })
          }
        }
      })

      return parsedPropertyControls
    },
    'useSelectedPropertyControls',
    (oldResult, newResult) => {
      return deepEqual(oldResult, newResult) // TODO better equality
    },
  )

  const selectedElementsFIXME = useEditorState(
    (store) => {
      return selectedPropertyControls.map(({ targets }) =>
        mapDropNulls(
          (path) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path),
          targets,
        ),
      )
    },
    'useGetPropertyControlsForSelectedComponents selectedElements',
    (a, b) =>
      arrayDeepEquality(arrayDeepEquality(ElementInstanceMetadataKeepDeepEquality()))(a, b)
        .areEqual,
  )

  const selectedComponentsFIXME = useEditorState(
    (store) => {
      return selectedPropertyControls.map(({ targets }) => {
        // TODO mapDropNulls
        let components: Array<UtopiaJSXComponent> = []
        fastForEach(targets, (path) => {
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
      })
    },
    'useUsedPropsWithoutControls',
    (a, b) =>
      arrayDeepEquality(arrayDeepEquality(UtopiaJSXComponentKeepDeepEquality))(a, b).areEqual,
  )

  return selectedPropertyControls.map(({ controls, targets }, index) => {
    ////////////////////////
    // useGivenPropsWithoutControls
    const parsedPropertyControls = controls
    const selectedElements = selectedElementsFIXME[index]
    const selectedComponents = selectedComponentsFIXME[index]

    const propertiesWithControls = foldEither(
      () => [],
      (success) =>
        filterSpecialProps(
          // TODO fix having to rely on getPropertyControlNames
          getPropertyControlNames(success),
        ),
      parsedPropertyControls,
    )
    let detectedPropsWithoutControls: Array<string> = []
    let definedControlsWithoutValues: Set<string> = new Set(propertiesWithControls)
    fastForEach(selectedElements, (element) => {
      const elementProps = Object.keys(filterUtopiaSpecificProps(element.props))
      fastForEach(elementProps, (propName) => {
        if (!propertiesWithControls.includes(propName)) {
          detectedPropsWithoutControls = addUniquely(detectedPropsWithoutControls, propName)
        }
      })
      fastForEach(propertiesWithControls, (definedControlProperty) => {
        if (elementProps.includes(definedControlProperty)) {
          definedControlsWithoutValues.delete(definedControlProperty)
        }
      })
    })

    ////////////////////////
    // useUsedPropsWithoutControls

    const propertiesWithControlsKeys_MAYBE_KILLME: Array<string> = Object.keys(
      eitherToMaybe(parsedPropertyControls) ?? {},
    )
    let detectedPropsWithNoValue: Array<string> = []
    fastForEach(selectedComponents, (component) => {
      if (isJSXElement(component.rootElement)) {
        fastForEach(component.propsUsed, (propUsed) => {
          if (
            !propertiesWithControlsKeys_MAYBE_KILLME.includes(propUsed) &&
            !detectedPropsWithoutControls.includes(propUsed)
          ) {
            detectedPropsWithNoValue = addUniquely(detectedPropsWithNoValue, propUsed)
          }
        })
      }
    })

    ////////////////////////
    // useGivenPropsAndValuesWithoutControls

    let detectedPropsAndValuesWithoutControls: Record<string, unknown> = {}
    fastForEach(selectedElements, (element) => {
      const elementProps = filterUtopiaSpecificProps(element.props)
      fastForEach(Object.keys(elementProps), (propName) => {
        if (!propertiesWithControls.includes(propName)) {
          detectedPropsAndValuesWithoutControls[propName] = elementProps[propName]
        }
      })
    })

    ////////////////////////

    return {
      controls: controls,
      propsWithControlsButNoValue: Array.from(definedControlsWithoutValues),
      detectedPropsWithNoValue: detectedPropsWithNoValue,
      detectedPropsAndValuesWithoutControls: detectedPropsAndValuesWithoutControls,
      targets: targets,
    }
  })
}

function areMatchingPropertyControls(
  a: ParseResult<ParsedPropertyControls>,
  b: ParseResult<ParsedPropertyControls>,
): boolean {
  // TODO create equality call
  return deepEqual(a, b)
}
