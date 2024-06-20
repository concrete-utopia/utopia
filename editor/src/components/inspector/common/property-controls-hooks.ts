import deepEqual from 'fast-deep-equal'
import * as ObjectPath from 'object-path'
import React from 'react'
import { useContextSelector } from 'use-context-selector'
import type { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import {
  printerForPropertyControl,
  unwrapperAndParserForPropertyControl,
  controlToUseForUnion,
  getPropertyControlNames,
} from '../../../core/property-controls/property-control-values'
import type {
  UnionControlDescription,
  RegularControlDescription,
} from '../../custom-code/internal-property-controls'
import type { InspectorInfoWithRawValue } from './property-path-hooks'
import {
  filterUtopiaSpecificProps,
  InspectorPropsContext,
  useCallbackFactory,
  useInspectorContext,
} from './property-path-hooks'
import type { ModifiableAttribute } from '../../../core/shared/jsx-attributes'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attribute-utils'
import * as PP from '../../../core/shared/property-path'
import type { Either } from '../../../core/shared/either'
import { eitherToMaybe, maybeEitherToMaybe } from '../../../core/shared/either'
import {
  calculatePropertyStatusForSelection,
  getControlStatusFromPropertyStatus,
} from './control-status'
import { getControlStyles } from './control-styles'
import {
  keepDeepReferenceEqualityIfPossible,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import type { UtopiaJSXComponent } from '../../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  isImportedOrigin,
  isJSXElement,
} from '../../../core/shared/element-template'
import { addUniquely, mapDropNulls } from '../../../core/shared/array-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getPropertyControlsForTargetFromEditor,
  getRegisteredComponent,
} from '../../../core/property-controls/property-controls-utils'
import { fastForEach } from '../../../core/shared/utils'
import { findUnderlyingTargetComponentImplementationFromImportInfo } from '../../custom-code/code-file'
import {
  ElementInstanceMetadataKeepDeepEquality,
  UtopiaJSXComponentKeepDeepEquality,
} from '../../editor/store/store-deep-equality-instances'
import { arrayDeepEquality } from '../../../utils/deep-equality'
import { omit } from '../../../core/shared/object-utils'
import type { AllElementProps } from '../../../components/editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import type { PropertyControls } from '../../custom-code/internal-property-controls'

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
  elementPath: ElementPath,
  propertyPath: PropertyPath,
  control: RegularControlDescription,
): InspectorInfoWithRawValue<any> {
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

  const { onContextSubmitValue: onSingleSubmitValue, onContextUnsetValue: onSingleUnsetValue } =
    useInspectorContext()

  const parserFn = unwrapperAndParserForPropertyControl(control)
  const printerFn = printerForPropertyControl(control)

  const allElementProps = useEditorState(
    Substores.metadata,
    (store) => store.editor.allElementProps,
    'allElementProps',
  )

  let parsedValue: unknown = null
  if (propertyPath.propertyElements[0] === 'children') {
    const fromAllElementProps = allElementProps[EP.toString(elementPath)]?.children ?? null
    switch (typeof fromAllElementProps) {
      case 'number':
        parsedValue = fromAllElementProps
        break
    }
  } else if (0 in rawValues && 0 in realValues) {
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

  const onTransientSubmitValue = React.useCallback(
    (newValue: any) => onSubmitValue(newValue, true),
    [onSubmitValue],
  )

  const useSubmitValueFactory = useCallbackFactory(parsedValue, onSubmitValue)

  const onUnsetValues = React.useCallback(() => {
    onSingleUnsetValue(propertyPath, false)
  }, [onSingleUnsetValue, propertyPath])

  const attributeExpression = maybeEitherToMaybe(rawValues[0]) // TODO handle multiselection

  return {
    value: parsedValue,
    attributeExpression: attributeExpression,
    controlStatus: controlStatus,
    propertyStatus: propertyStatusToReturn,
    controlStyles: controlStyles,
    onSubmitValue: onSubmitValue,
    onTransientSubmitValue: onTransientSubmitValue,
    onUnsetValues: onUnsetValues,

    useSubmitValueFactory: useSubmitValueFactory,
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

type PropertyControlsAndTargets = {
  controls: PropertyControls
  targets: ElementPath[]
}

type FullPropertyControlsAndTargets = {
  controls: PropertyControls
  targets: ElementPath[]
  propsWithControlsButNoValue: string[]
  detectedPropsWithNoValue: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
}

const emptyControls: PropertyControls = {}

// if any of these keys is found as a prop name, it will be omitted from the property controls
const ignoredPropNames: string[] = ['data-constraints']

export function useGetPropertyControlsForSelectedComponents(): Array<FullPropertyControlsAndTargets> {
  const selectedPropertyControls = useEditorState(
    Substores.fullStore,
    (store) => {
      let propertyControlsAndTargets: Array<PropertyControlsAndTargets> = []
      fastForEach(store.editor.selectedViews, (path) => {
        const propertyControls = getPropertyControlsForTargetFromEditor(path, store.editor)
        if (propertyControls == null) {
          propertyControlsAndTargets.push({
            controls: emptyControls,
            targets: [path],
          })
        } else {
          const withFilteredProps = omit(propsToOmit, propertyControls)

          const foundMatch = propertyControlsAndTargets.findIndex((existing) =>
            areMatchingPropertyControls(existing.controls, withFilteredProps),
          )
          if (foundMatch > -1) {
            propertyControlsAndTargets[foundMatch].targets.push(path)
          } else {
            propertyControlsAndTargets.push({
              controls: withFilteredProps,
              targets: [path],
            })
          }
        }
      })

      return propertyControlsAndTargets
    },
    'useSelectedPropertyControls',
    (oldResult, newResult) => {
      return deepEqual(oldResult, newResult) // TODO better equality
    },
  )

  const selectedElementsProps = useEditorState(
    Substores.metadata,
    (store) => {
      let result: AllElementProps = {}
      fastForEach(selectedPropertyControls, ({ targets }) => {
        fastForEach(targets, (target) => {
          const pathString = EP.toString(target)
          result[pathString] = store.editor.allElementProps[pathString]
        })
      })
      return result
    },
    'useGetPropertyControlsForSelectedComponents selectedElementsProps',
    (a, b) => keepDeepReferenceEqualityIfPossible(a, b) === a,
  )

  const selectedElementsFIXME = useEditorState(
    Substores.metadata,
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
      arrayDeepEquality(arrayDeepEquality(ElementInstanceMetadataKeepDeepEquality))(a, b).areEqual,
  )

  const selectedComponentsFIXME = useEditorState(
    Substores.fullStore,
    (store) => {
      return selectedPropertyControls.map(({ targets }) => {
        // TODO mapDropNulls
        let components: Array<UtopiaJSXComponent> = []
        fastForEach(targets, (path) => {
          const openStoryboardFile = store.editor.canvas.openFile?.filename ?? null
          if (openStoryboardFile != null) {
            const component = findUnderlyingTargetComponentImplementationFromImportInfo(
              store.editor.projectContents,
              MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)?.importInfo ??
                null,
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
    const selectedElements = selectedElementsFIXME[index]
    const selectedComponents = selectedComponentsFIXME[index]

    const propertiesWithControls = filterSpecialProps(
      // TODO fix having to rely on getPropertyControlNames
      getPropertyControlNames(controls),
    )
    let detectedPropsWithoutControls: Array<string> = []
    let definedControlsWithoutValues: Set<string> = new Set(propertiesWithControls)
    fastForEach(selectedElements, (element) => {
      const elementProps = Object.keys(
        filterUtopiaSpecificProps(selectedElementsProps[EP.toString(element.elementPath)] ?? {}),
      )
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

    const propertiesWithControlsKeys_MAYBE_KILLME: Array<string> = Object.keys(controls ?? {})
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
      const elementProps = filterUtopiaSpecificProps(
        selectedElementsProps[EP.toString(element.elementPath)] ?? {},
      )
      fastForEach(Object.keys(elementProps), (propName) => {
        if (!propertiesWithControls.includes(propName) && !ignoredPropNames.includes(propName)) {
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

function areMatchingPropertyControls(a: PropertyControls, b: PropertyControls): boolean {
  // TODO create equality call
  return deepEqual(a, b)
}

export function useGetComponentDataForElementPath(elementPath: ElementPath | null) {
  return useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      if (elementPath == null) {
        return null
      }

      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath)

      const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(element)
      const elementImportInfo = element?.importInfo
      if (elementImportInfo == null || targetJSXElement == null) {
        return null
      }

      const elementName = getJSXElementNameAsString(targetJSXElement.name)

      const exportedName = isImportedOrigin(elementImportInfo)
        ? elementImportInfo.exportedName ?? elementName
        : elementName

      const registeredComponent = getRegisteredComponent(
        exportedName,
        elementImportInfo.filePath,
        store.editor.propertyControlsInfo,
      )

      const descriptorFile =
        registeredComponent?.source.type === 'DESCRIPTOR_FILE' ? registeredComponent.source : null

      if (registeredComponent?.label == null) {
        return {
          displayName: elementName,
          descriptorFile: descriptorFile,
          isRegisteredComponent: registeredComponent != null,
        }
      }

      return {
        displayName: registeredComponent.label,
        descriptorFile: descriptorFile,
        isRegisteredComponent: registeredComponent != null,
        secondaryName: elementName,
      }
    },
    'useGetComponentDataForElementPath componentName',
  )
}
