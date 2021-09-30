import fastDeepEquals from 'fast-deep-equal'
import React from 'react'
import { betterReactMemo, SliderControl } from '../../../../uuiui-deps'
import {
  BaseControlDescription,
  BooleanControlDescription,
  ColorControlDescription,
  ComponentInstanceDescription,
  EnumControlDescription,
  EventHandlerControlDescription,
  ExpressionEnumControlDescription,
  ImageControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  SliderControlDescription,
  StringControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
} from 'utopia-api'
import { InspectorInfo } from '../../common/property-path-hooks'
import { BooleanControl } from '../../controls/boolean-control'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  SimpleNumberInput,
  PopupList,
  NumberInputProps,
  ChainedNumberInput,
} from '../../../../uuiui'
import {
  parseColor,
  CSSColor,
  printColor,
  printCSSNumber,
  CSSNumber,
  cssNumber,
} from '../../common/css-utils'
import * as PP from '../../../../core/shared/property-path'
import { foldEither } from '../../../../core/shared/either'
import { ColorControl } from '../../controls/color-control'
import { StringControl } from '../../controls/string-control'
import { NO_OP } from '../../../../core/shared/utils'
import { SelectControl, SelectOption } from '../../controls/select-control'
import { EventHandlerControl } from '../event-handlers-section/event-handlers-section'
import { OptionChainControl } from '../../controls/option-chain-control'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { importDetails, Imports, PropertyPath } from '../../../../core/shared/project-file-types'
import { useEditorState } from '../../../editor/store/store-hook'
import { addImports, setProp_UNSAFE } from '../../../editor/actions/action-creators'
import { jsxAttributeOtherJavaScript } from '../../../../core/shared/element-template'
import { EditorAction } from '../../../editor/action-types'
import { forceNotNull } from '../../../../core/shared/optional-utils'

export interface ControlForPropProps<T extends BaseControlDescription> {
  propPath: PropertyPath
  propName: string
  controlDescription: T
  propMetadata: InspectorInfo<any>
}

export const ControlForBooleanProp = betterReactMemo(
  'ControlForBooleanProp',
  (props: ControlForPropProps<BooleanControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-boolean-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    return (
      <BooleanControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value}
        onSubmitValue={propMetadata.onSubmitValue}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
      />
    )
  },
)

export const ControlForColorProp = betterReactMemo(
  'ControlForColorProp',
  (props: ControlForPropProps<ColorControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-color-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    const parsedColor = parseColor(value)
    return foldEither(
      (failureReason) => {
        return <div>{failureReason}</div>
      },
      (validColor) => {
        function transientSubmitValue(color: CSSColor): void {
          propMetadata.onTransientSubmitValue(printColor(color))
        }
        function submitValue(color: CSSColor): void {
          propMetadata.onSubmitValue(printColor(color))
        }
        return (
          <ColorControl
            key={controlId}
            id={controlId}
            testId={controlId}
            value={validColor}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
            onTransientSubmitValue={transientSubmitValue}
            onSubmitValue={submitValue}
          />
        )
      },
      parsedColor,
    )
  },
)

export const ControlForComponentInstanceProp = betterReactMemo(
  'ControlForComponentInstanceProp',
  (props: ControlForPropProps<ComponentInstanceDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-componentinstance-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue
    const controlStyles = useKeepReferenceEqualityIfPossible({
      ...propMetadata.controlStyles,
      showContent: true,
    })

    return (
      <StringControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value ?? ''}
        onSubmitValue={
          propMetadata.controlStatus === 'controlled' ? NO_OP : propMetadata.onSubmitValue
        }
        controlStatus={propMetadata.controlStatus}
        controlStyles={controlStyles}
      />
    )
  },
)

export const ControlForEnumProp = betterReactMemo(
  'ControlForEnumProp',
  (props: ControlForPropProps<EnumControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    const options: Array<SelectOption> = useKeepReferenceEqualityIfPossible(
      controlDescription.options.map((option, index) => {
        return {
          value: option as string, // TODO cheating with type
          label:
            controlDescription.optionTitles == null ||
            typeof controlDescription.optionTitles === 'function'
              ? (option as string)
              : (controlDescription.optionTitles[index] as string),
        }
      }),
    )
    const currentValue = options.find((option) => {
      return fastDeepEquals(option.value, value)
    })

    function submitValue(option: SelectOption): void {
      propMetadata.onSubmitValue(option.value)
    }

    return (
      <PopupList
        disabled={!propMetadata.controlStyles.interactive}
        value={currentValue}
        onSubmitValue={submitValue}
        options={options}
        containerMode={'default'}
      />
    )
  },
)

export const ControlForExpressionEnumProp = betterReactMemo(
  'ControlForEnumProp',
  (props: ControlForPropProps<ExpressionEnumControlDescription>) => {
    const dispatch = useEditorState(
      (store) => store.dispatch,
      'ControlForExpressionEnumProp dispatch',
    )
    const selectedViews = useEditorState(
      (store) => store.editor.selectedViews,
      'ControlForExpressionEnumProp selectedViews',
    )
    const target = forceNotNull('Inspector control without selected element', selectedViews[0])
    const { propMetadata, controlDescription } = props

    const detectedExpression = controlDescription.options.find((option) =>
      fastDeepEquals(option.value, propMetadata.value),
    )?.expression

    const selectedExpression =
      propMetadata.propertyStatus.set && detectedExpression != null
        ? detectedExpression
        : controlDescription.defaultValue?.expression

    const options: Array<SelectOption> = useKeepReferenceEqualityIfPossible(
      controlDescription.options.map((option, index) => {
        return {
          value: option.expression,
          label:
            controlDescription.optionTitles == null ||
            typeof controlDescription.optionTitles === 'function'
              ? option.expression
              : (controlDescription.optionTitles[index] as string),
        }
      }),
    )
    const currentValue = options.find((option) => {
      return fastDeepEquals(option.value, selectedExpression)
    })

    function submitValue(option: SelectOption): void {
      const actions: EditorAction[] = [
        setProp_UNSAFE(
          target,
          props.propPath,
          jsxAttributeOtherJavaScript(option.value, `return ${option.value}`, [], null, {}),
        ),
      ]
      const expressionOption = controlDescription.options.find((o) => o.expression === option.value)
      if (expressionOption != null && expressionOption.import != null) {
        const importOption = expressionOption.import
        const importToAdd: Imports = {
          [importOption.source]: importDetails(
            importOption.type === 'default' ? importOption.name : null,
            importOption.type == null
              ? [{ name: importOption.name, alias: importOption.name }]
              : [],
            importOption.type === 'star' ? importOption.name : null,
          ),
        }
        actions.push(addImports(importToAdd, target))
      }
      dispatch(actions, 'everyone')
    }

    return (
      <PopupList
        disabled={!propMetadata.controlStyles.interactive}
        value={currentValue}
        onSubmitValue={submitValue}
        options={options}
        containerMode={'default'}
      />
    )
  },
)

export const ControlForEventHandlerProp = betterReactMemo(
  'ControlForEventHandlerProp',
  (props: ControlForPropProps<EventHandlerControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    return <EventHandlerControl handlerName={propName} value={value ?? ''} />
  },
)

export const ControlForImageProp = betterReactMemo(
  'ControlForImageProp',
  (props: ControlForPropProps<ImageControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-image-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue
    const controlStyles = useKeepReferenceEqualityIfPossible({
      ...propMetadata.controlStyles,
      showContent: true,
    })

    return (
      <StringControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value ?? ''}
        onSubmitValue={
          propMetadata.controlStatus === 'controlled' ? NO_OP : propMetadata.onSubmitValue
        }
        controlStatus={propMetadata.controlStatus}
        controlStyles={controlStyles}
      />
    )
  },
)

export const ControlForNumberProp = betterReactMemo(
  'ControlForNumberProp',
  (props: ControlForPropProps<NumberControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      propMetadata.onSubmitValue,
      propMetadata.onUnsetValues,
    )
    const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      propMetadata.onTransientSubmitValue,
      propMetadata.onUnsetValues,
    )

    const controlId = `${propName}-number-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    return (
      <SimpleNumberInput
        id={controlId}
        testId={controlId}
        value={value}
        onSubmitValue={wrappedOnSubmit}
        onTransientSubmitValue={wrappedOnTransientSubmit}
        onForcedSubmitValue={wrappedOnSubmit}
        controlStatus={propMetadata.controlStatus}
        incrementControls={controlDescription.displayStepper}
        stepSize={controlDescription.step}
        minimum={controlDescription.min}
        maximum={controlDescription.max}
        labelInner={controlDescription.unit}
        defaultUnitToHide={'px'}
      />
    )
  },
)

export const ControlForOptionsProp = betterReactMemo(
  'ControlForOptionsProp',
  (props: ControlForPropProps<OptionsControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-options-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    return (
      <OptionChainControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
        onSubmitValue={propMetadata.onSubmitValue}
        options={controlDescription.options}
      />
    )
  },
)

export const ControlForPopupListProp = betterReactMemo(
  'ControlForPopupListProp',
  (props: ControlForPropProps<PopUpListControlDescription>) => {
    const { propMetadata, controlDescription } = props

    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    function submitValue(option: SelectOption): void {
      propMetadata.onSubmitValue(option.value)
    }
    const currentValue = controlDescription.options.find((option) => {
      return fastDeepEquals(option.value, value)
    })

    return (
      <PopupList
        disabled={!propMetadata.controlStyles.interactive}
        value={currentValue}
        onSubmitValue={submitValue}
        options={controlDescription.options}
        containerMode={'default'}
      />
    )
  },
)

export const ControlForSliderProp = betterReactMemo(
  'ControlForSliderProp',
  (props: ControlForPropProps<SliderControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-slider-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    const controlOptions = useKeepReferenceEqualityIfPossible({
      minimum: controlDescription.min,
      maximum: controlDescription.max,
      stepSize: controlDescription.step,
    })

    return (
      <UIGridRow padded={false} variant={'<--------auto-------->|--45px--|'}>
        <SliderControl
          key={`${controlId}-slider`}
          id={`${controlId}-slider`}
          testId={`${controlId}-slider`}
          value={value}
          onTransientSubmitValue={propMetadata.onTransientSubmitValue}
          onForcedSubmitValue={propMetadata.onSubmitValue}
          onSubmitValue={propMetadata.onSubmitValue}
          controlStatus={propMetadata.controlStatus}
          controlStyles={propMetadata.controlStyles}
          DEPRECATED_controlOptions={controlOptions}
        />
        <SimpleNumberInput
          id={`${controlId}-number`}
          testId={`${controlId}-number`}
          key={`${controlId}-number`}
          value={value}
          onTransientSubmitValue={propMetadata.onTransientSubmitValue}
          onForcedSubmitValue={propMetadata.onSubmitValue}
          onSubmitValue={propMetadata.onSubmitValue}
          controlStatus={propMetadata.controlStatus}
          stepSize={controlDescription.step}
          minimum={controlDescription.min}
          maximum={controlDescription.max}
          defaultUnitToHide={'px'}
        />
      </UIGridRow>
    )
  },
)

export const ControlForStringProp = betterReactMemo(
  'ControlForStringProp',
  (props: ControlForPropProps<StringControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-string-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    return (
      <StringControl
        key={propName}
        id={controlId}
        testId={controlId}
        value={value ?? ''}
        onSubmitValue={propMetadata.onSubmitValue}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
      />
    )
  },
)

export const ControlForVectorProp = betterReactMemo(
  'ControlForVectorProp',
  (props: ControlForPropProps<Vector2ControlDescription | Vector3ControlDescription>) => {
    const { propPath, propMetadata, controlDescription } = props

    const vectorValue =
      (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ?? []

    const keys = controlDescription.type === 'vector3' ? ['x', 'y', 'z'] : ['x', 'y']
    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = keys.map(
      (propName: string, index: number) => {
        const innerValue = vectorValue[index]

        const innerPropPath = PP.appendPropertyPathElems(propPath, [propName])
        const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue((rawValue: CSSNumber) => {
          const newValue = printCSSNumber(rawValue, null)
          let updatedValues = vectorValue.length === 0 ? keys.map((k) => 0) : [...vectorValue]
          updatedValues[index] = newValue
          propMetadata.onSubmitValue(updatedValues)
        }, propMetadata.onUnsetValues)
        const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
          (rawValue: CSSNumber) => {
            const newValue = printCSSNumber(rawValue, null)
            let updatedValues = vectorValue.length === 0 ? keys.map((k) => 0) : [...vectorValue]
            updatedValues[index] = newValue
            propMetadata.onTransientSubmitValue(updatedValues)
          },
          propMetadata.onUnsetValues,
        )
        return {
          value:
            innerValue == null || typeof innerValue !== 'number' ? null : cssNumber(innerValue),
          DEPRECATED_labelBelow: propName,
          testId: `component-section-${PP.toString(innerPropPath)}`,
          controlStatus: propMetadata.controlStatus,
          onSubmitValue: wrappedOnSubmit,
          onTransientSubmitValue: wrappedOnTransientSubmit,
          numberType: 'Unitless' as const,
          defaultUnitToHide: null,
        }
      },
    )

    return <ChainedNumberInput idPrefix={'vector'} propsArray={propsArray} />
  },
)
