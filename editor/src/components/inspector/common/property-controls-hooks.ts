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
  useSelectedPropertyControls,
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
  isJSXElement,
  JSXAttributes,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { addUniquely, mapArrayToDictionary, mapDropNulls } from '../../../core/shared/array-utils'
import { ParseError, ParseResult } from '../../../utils/value-parser-utils'
import { ParsedPropertyControls } from '../../../core/property-controls/property-controls-parser'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { filterSpecialProps } from '../../../core/property-controls/property-controls-utils'
import { fastForEach } from '../../../core/shared/utils'
import { findUnderlyingTargetComponentImplementation } from '../../custom-code/code-file'

type RawValues = Either<string, ModifiableAttribute>[]
type RealValues = unknown[]

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

export function useControlStatusForPaths(paths: PropertyPath[]): { [path: string]: ControlStatus } {
  const attributes: readonly JSXAttributes[] = useKeepReferenceEqualityIfPossible(
    useContextSelector(
      InspectorPropsContext,
      (contextData) => contextData.editedMultiSelectedProps,
      deepEqual,
    ),
  )

  const spiedProps = useKeepReferenceEqualityIfPossible(
    useContextSelector(InspectorPropsContext, (contextData) => contextData.spiedProps, deepEqual),
  )

  return mapArrayToDictionary(
    paths.map((path) => {
      const rawValues: RawValues = attributes.map((props) => {
        return getModifiableJSXAttributeAtPath(props, path)
      })
      const realValues: RealValues = spiedProps.map((props) => {
        return ObjectPath.get(props, PP.getElements(path))
      })

      const propertyStatus = calculatePropertyStatusForSelection(rawValues, realValues)
      const controlStatus = getControlStatusFromPropertyStatus(propertyStatus)

      return {
        path: path,
        controlStatus: controlStatus,
      }
    }),
    (k) => PP.toString(k.path),
    (v) => v.controlStatus,
  )
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

// kontrolkent mutatni:
// 1. van hozza propertyControls es van erteke
// 2. nincs hozza propertyControls de van erteke

// listan mutatni:
// 1. van hozza propertyControls de nincs erteke
// 2. nincs erteke de detektaltuk a kodban hogy a komponens hasznalja (pl props.kisCica)

type PropertyControlsAndTargets = {
  controls: ParseResult<ParsedPropertyControls>
  targets: ElementPath[]
  detectedPropsWithNoValue: string[]
  detectedPropsAndValuesWithoutControls: Record<string, unknown>
}

export function useGetPropertyControlsForSelectedComponents(): Array<PropertyControlsAndTargets> {
  const selectedPropertyControls = useSelectedPropertyControls(true) // TODO BEFORE MERGE the final version should ignore ignore controls
  const selectedElementsFIXME = useEditorState((store) => {
    return selectedPropertyControls.map(({ targets }) =>
      mapDropNulls(
        (path) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path),
        targets,
      ),
    )
  }, 'useGetPropertyControlsForSelectedComponents selectedElements')

  const selectedComponentsFIXME = useEditorState((store) => {
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
  }, 'useUsedPropsWithoutControls')

  return selectedPropertyControls.map(({ controls, targets }, index) => {
    ////////////////////////
    // useGivenPropsWithoutControls
    const parsedPropertyControls = controls
    const selectedElements = selectedElementsFIXME[index]

    const propertiesWithControls_MAYBE_FIXME = foldEither(
      () => [],
      (success) =>
        filterSpecialProps(
          // TODO fix having to rely on getPropertyControlNames
          getPropertyControlNames(success),
        ),
      parsedPropertyControls,
    )
    let givenProps: Array<string> = []
    fastForEach(selectedElements, (element) => {
      const elementProps = filterUtopiaSpecificProps(element.props)
      fastForEach(Object.keys(elementProps), (propName) => {
        if (!propertiesWithControls_MAYBE_FIXME.includes(propName)) {
          givenProps = addUniquely(givenProps, propName)
        }
      })
    })

    const detectedPropsWithoutControls = givenProps

    ////////////////////////

    //useUsedPropsWithoutControls
    const selectedComponents = selectedComponentsFIXME[index]

    const propertiesWithControlsKeys_MAYBE_KILLME: Array<string> = Object.keys(
      eitherToMaybe(parsedPropertyControls) ?? {},
    )
    let propertiesWithoutControls: Array<string> = []
    fastForEach(selectedComponents, (component) => {
      if (isJSXElement(component.rootElement)) {
        fastForEach(component.propsUsed, (propUsed) => {
          if (
            !propertiesWithControlsKeys_MAYBE_KILLME.includes(propUsed) &&
            !detectedPropsWithoutControls.includes(propUsed)
          ) {
            propertiesWithoutControls = addUniquely(propertiesWithoutControls, propUsed)
          }
        })
      }
    })

    const detectedPropsWithNoValue = propertiesWithoutControls

    ////////////////////////
    // eslint-disable-next-line react-hooks/rules-of-hooks

    //useGivenPropsAndValuesWithoutControls

    const propertiesWithControls = foldEither(
      () => [],
      (success) => filterSpecialProps(getPropertyControlNames(success)),
      parsedPropertyControls,
    )

    let propsWithValueWithoutControls: Record<string, unknown> = {}
    fastForEach(selectedElements, (element) => {
      const elementProps = filterUtopiaSpecificProps(element.props)
      fastForEach(Object.keys(elementProps), (propName) => {
        if (!propertiesWithControls.includes(propName)) {
          propsWithValueWithoutControls[propName] = elementProps[propName]
        }
      })
    })

    const detectedPropsAndValuesWithoutControls = propsWithValueWithoutControls

    ////////////////////////

    return {
      controls: controls,
      detectedPropsWithoutControls: detectedPropsWithoutControls,
      detectedPropsWithNoValue: detectedPropsWithNoValue,
      detectedPropsAndValuesWithoutControls: detectedPropsAndValuesWithoutControls,
      targets: targets,
    }
  })
}
