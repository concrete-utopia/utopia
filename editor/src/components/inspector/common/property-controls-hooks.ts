import * as deepEqual from 'fast-deep-equal'
import * as ObjectPath from 'object-path'
import * as React from 'react'
import { useContextSelector } from 'use-context-selector'
import { PropertyPath } from '../../../core/shared/project-file-types'
import {
  printerForBasePropertyControl,
  unwrapperAndParserForBaseControl,
} from '../../../core/property-controls/property-control-values'
import { BaseControlDescription } from 'utopia-api'
import {
  InspectorInfo,
  useKeepReferenceEqualityIfPossible,
  InspectorPropsContext,
  useCallbackFactory,
  useInspectorContext,
} from './property-path-hooks'
import {
  getModifiableJSXAttributeAtPath,
  ModifiableAttribute,
} from '../../../core/shared/jsx-attributes'
import * as PP from '../../../core/shared/property-path'
import { Either } from '../../../core/shared/either'
import {
  calculatePropertyStatusForSelection,
  getControlStatusFromPropertyStatus,
  getControlStyles,
} from './control-status'

type RawValues = Either<string, ModifiableAttribute>[]
type RealValues = unknown[]

export function useInspectorInfoForPropertyControl(
  propertyPath: PropertyPath,
  control: BaseControlDescription,
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
        return contextData.realValues.map((props) => {
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

  const parserFn = unwrapperAndParserForBaseControl(control)
  const printerFn = printerForBasePropertyControl(control)
  const parsedValue = parserFn(rawValues[0], realValues[0])

  const onSubmitValue = React.useCallback(
    (newValue: any, transient = false) => {
      if (newValue == null) {
        onSingleUnsetValue(propertyPath)
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
    onSingleUnsetValue(propertyPath)
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
