import fastDeepEquals from 'fast-deep-equal'
import React from 'react'
import { betterReactMemo, CSSCursor, SliderControl } from '../../../../uuiui-deps'
import {
  BaseControlDescription,
  BooleanControlDescription,
  ColorControlDescription,
  EnumControlDescription,
  EulerControlDescription,
  ExpressionEnumControlDescription,
  ImageControlDescription,
  Matrix3ControlDescription,
  Matrix4ControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  QuaternionControlDescription,
  RawJSControlDescription,
  StringControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  Vector4ControlDescription,
} from 'utopia-api'
import { InspectorInfo } from '../../common/property-path-hooks'
import { BooleanControl } from '../../controls/boolean-control'
import {
  useWrappedEmptyOrUnknownOnSubmitValue,
  SimpleNumberInput,
  PopupList,
  NumberInputProps,
  ChainedNumberInput,
  wrappedEmptyOrUnknownOnSubmitValue,
  FlexColumn,
} from '../../../../uuiui'
import {
  parseColor,
  CSSColor,
  printColor,
  printCSSNumber,
  CSSNumber,
  cssNumber,
  defaultCSSColor,
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
import { addImports, forceParseFile, setProp_UNSAFE } from '../../../editor/actions/action-creators'
import { jsxAttributeOtherJavaScript } from '../../../../core/shared/element-template'
import { EditorAction } from '../../../editor/action-types'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { DEPRECATEDSliderControlOptions } from '../../controls/slider-control'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../../../custom-code/code-file'

export interface ControlForPropProps<T extends BaseControlDescription> {
  propPath: PropertyPath
  propName: string
  controlDescription: T
  propMetadata: InspectorInfo<any>
  setGlobalCursor: (cursor: CSSCursor | null) => void
  focusOnMount: boolean
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
        focusOnMount={props.focusOnMount}
      />
    )
  },
)

export const ControlForColorProp = betterReactMemo(
  'ControlForColorProp',
  (props: ControlForPropProps<ColorControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-color-property-control`
    const rawValue = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

    const value = rawValue ?? defaultCSSColor

    return (
      <ColorControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
        onSubmitValue={propMetadata.onSubmitValue}
        onTransientSubmitValue={propMetadata.onTransientSubmitValue}
      />
    )
  },
)

export const ControlForRawJSProp = betterReactMemo(
  'ControlForRawJSProp',
  (props: ControlForPropProps<RawJSControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props
    const dispatch = useEditorState((store) => store.dispatch, 'ControlForRawJSProp dispatch')

    const targetFilePaths = useEditorState((store) => {
      const currentFilePath = forceNotNull(
        'Missing open file',
        store.editor.canvas.openFile?.filename,
      )
      return store.editor.selectedViews.map((selectedView) => {
        const normalisedPath = normalisePathToUnderlyingTarget(
          store.editor.projectContents,
          store.editor.nodeModules.files,
          currentFilePath,
          selectedView,
        )
        return normalisePathSuccessOrThrowError(normalisedPath).filePath
      })
    }, 'ControlForRawJSProp targetFilePaths')

    const controlId = `${propName}-rawjs-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue
    const controlStyles = useKeepReferenceEqualityIfPossible({
      ...propMetadata.controlStyles,
      showContent: true,
    })

    const baseOnSubmit = propMetadata.onSubmitValue

    const submitValue = React.useCallback(
      (newValue: string): void => {
        baseOnSubmit(newValue)
        const actions = targetFilePaths.map((filePath) => forceParseFile(filePath))
        dispatch(actions, 'everyone')
      },
      [dispatch, baseOnSubmit, targetFilePaths],
    )

    return (
      <StringControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value ?? ''}
        onSubmitValue={submitValue}
        controlStatus={propMetadata.controlStatus}
        controlStyles={controlStyles}
        focus={props.focusOnMount}
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
        autoFocus={props.focusOnMount}
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
        autoFocus={props.focusOnMount}
      />
    )
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
        focus={props.focusOnMount}
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

    if (controlDescription.min != null && controlDescription.max != null) {
      const controlOptions: DEPRECATEDSliderControlOptions = useKeepReferenceEqualityIfPossible({
        minimum: controlDescription.min,
        maximum: controlDescription.max,
        stepSize: controlDescription.step,
      })
      return <NumberWithSliderControl {...props} controlOptions={controlOptions} />
    } else {
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
          focusOnMount={props.focusOnMount}
        />
      )
    }
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
        autoFocus={props.focusOnMount}
      />
    )
  },
)

export const NumberWithSliderControl = betterReactMemo(
  'NumberWithSliderControl',
  (
    props: ControlForPropProps<NumberControlDescription> & {
      controlOptions: DEPRECATEDSliderControlOptions
    },
  ) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-slider-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : controlDescription.defaultValue

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
          DEPRECATED_controlOptions={props.controlOptions}
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
          focusOnMount={props.focusOnMount}
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
        focus={props.focusOnMount}
      />
    )
  },
)

function keysForVectorOfType(vectorType: 'vector2' | 'vector3' | 'vector4'): Array<string> {
  switch (vectorType) {
    case 'vector2':
      return ['x', 'y']
    case 'vector3':
      return ['x', 'y', 'z']
    case 'vector4':
      return ['x', 'y', 'z', 'w']
    default:
      const _exhaustiveCheck: never = vectorType
      throw new Error(`Unhandled vector type ${vectorType}`)
  }
}

function propsArrayForCSSNumberArray(
  values: Array<string | number>,
  keys: Array<string>,
  propPath: PropertyPath,
  propMetadata: InspectorInfo<any>,
): Array<Omit<NumberInputProps, 'id' | 'chained'>> {
  return keys.map((propName: string, index: number) => {
    const innerValue = values[index]

    const innerPropPath = PP.appendPropertyPathElems(propPath, [propName])
    const wrappedOnSubmit = wrappedEmptyOrUnknownOnSubmitValue((rawValue: CSSNumber) => {
      const newValue = printCSSNumber(rawValue, null)
      let updatedValues = values.length === 0 ? keys.map((k) => 0) : [...values]
      updatedValues[index] = newValue
      propMetadata.onSubmitValue(updatedValues)
    }, propMetadata.onUnsetValues)
    const wrappedOnTransientSubmit = wrappedEmptyOrUnknownOnSubmitValue((rawValue: CSSNumber) => {
      const newValue = printCSSNumber(rawValue, null)
      let updatedValues = values.length === 0 ? keys.map((k) => 0) : [...values]
      updatedValues[index] = newValue
      propMetadata.onTransientSubmitValue(updatedValues)
    }, propMetadata.onUnsetValues)
    return {
      value: innerValue == null || typeof innerValue !== 'number' ? null : cssNumber(innerValue),
      DEPRECATED_labelBelow: propName,
      testId: `component-section-${PP.toString(innerPropPath)}`,
      controlStatus: propMetadata.controlStatus,
      onSubmitValue: wrappedOnSubmit,
      onTransientSubmitValue: wrappedOnTransientSubmit,
      numberType: 'Unitless' as const,
      defaultUnitToHide: null,
    }
  })
}

export const ControlForVectorProp = betterReactMemo(
  'ControlForVectorProp',
  (
    props: ControlForPropProps<
      Vector2ControlDescription | Vector3ControlDescription | Vector4ControlDescription
    >,
  ) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const vectorValue =
        (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ??
        []

      const keys = keysForVectorOfType(controlDescription.type)
      return propsArrayForCSSNumberArray(vectorValue, keys, propPath, propMetadata)
    }, [controlDescription.type, controlDescription.defaultValue, propMetadata, propPath])

    return (
      <ChainedNumberInput
        idPrefix={'vector'}
        propsArray={propsArray}
        setGlobalCursor={setGlobalCursor}
      />
    )
  },
)

export const ControlForEulerProp = betterReactMemo(
  'ControlForEulerProp',
  (props: ControlForPropProps<EulerControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const values = React.useMemo(
      () =>
        (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ??
        [],
      [propMetadata.propertyStatus.set, propMetadata.value, controlDescription.defaultValue],
    )

    const numericPropsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      return propsArrayForCSSNumberArray(values, ['x', 'y', 'z'], propPath, propMetadata)
    }, [values, propMetadata, propPath])

    const orderValue = values[3] ?? 'XYZ'

    const orderWrappedOnSubmit = wrappedEmptyOrUnknownOnSubmitValue((newValue: string) => {
      let updatedValues = values.length === 0 ? [0, 0, 0, 'XYZ'] : [...values]
      updatedValues[3] = newValue
      propMetadata.onSubmitValue(updatedValues)
    }, propMetadata.onUnsetValues)
    const orderControlId = `euler-order-control`

    return (
      <FlexColumn>
        <ChainedNumberInput
          idPrefix={'euler-xyz'}
          propsArray={numericPropsArray}
          setGlobalCursor={setGlobalCursor}
        />
        <StringControl
          key={'euler-order'}
          id={orderControlId}
          testId={orderControlId}
          value={orderValue}
          onSubmitValue={orderWrappedOnSubmit}
          controlStatus={propMetadata.controlStatus}
          controlStyles={propMetadata.controlStyles}
          DEPRECATED_controlOptions={{
            DEPRECATED_labelBelow: 'order',
          }}
        />
      </FlexColumn>
    )
  },
)

export const ControlForQuaternionProp = betterReactMemo(
  'ControlForQuaternionProp',
  (props: ControlForPropProps<QuaternionControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const quaternion =
        (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ??
        []

      return propsArrayForCSSNumberArray(quaternion, ['x', 'y', 'z', 'w'], propPath, propMetadata)
    }, [controlDescription.defaultValue, propMetadata, propPath])

    return (
      <ChainedNumberInput
        idPrefix={'quaternion'}
        propsArray={propsArray}
        setGlobalCursor={setGlobalCursor}
      />
    )
  },
)

export const ControlForMatrix3Prop = betterReactMemo(
  'ControlForMatrix3Prop',
  (props: ControlForPropProps<Matrix3ControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const value =
        (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ??
        []

      // prettier-ignore
      const keys = [
        'n11', 'n12', 'n13',
        'n21', 'n22', 'n23',
        'n31', 'n32', 'n33',
      ]
      return propsArrayForCSSNumberArray(value, keys, propPath, propMetadata)
    }, [controlDescription.defaultValue, propMetadata, propPath])

    return (
      <FlexColumn>
        <ChainedNumberInput
          idPrefix={'matrix3-row1'}
          propsArray={propsArray.slice(0, 3)}
          setGlobalCursor={setGlobalCursor}
        />
        <ChainedNumberInput
          idPrefix={'matrix3-row2'}
          propsArray={propsArray.slice(3, 6)}
          setGlobalCursor={setGlobalCursor}
        />
        <ChainedNumberInput
          idPrefix={'matrix3-row3'}
          propsArray={propsArray.slice(6, 9)}
          setGlobalCursor={setGlobalCursor}
        />
      </FlexColumn>
    )
  },
)

export const ControlForMatrix4Prop = betterReactMemo(
  'ControlForMatrix4Prop',
  (props: ControlForPropProps<Matrix4ControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const value =
        (propMetadata.propertyStatus.set ? propMetadata.value : controlDescription.defaultValue) ??
        []

      // prettier-ignore
      const keys = [
        'n11', 'n12', 'n13', 'n14',
        'n21', 'n22', 'n23', 'n24',
        'n31', 'n32', 'n33', 'n34',
        'n41', 'n42', 'n43', 'n44',
      ]
      return propsArrayForCSSNumberArray(value, keys, propPath, propMetadata)
    }, [controlDescription.defaultValue, propMetadata, propPath])

    return (
      <FlexColumn>
        <ChainedNumberInput
          idPrefix={'matrix4-row1'}
          propsArray={propsArray.slice(0, 4)}
          setGlobalCursor={setGlobalCursor}
        />
        <ChainedNumberInput
          idPrefix={'matrix4-row2'}
          propsArray={propsArray.slice(4, 8)}
          setGlobalCursor={setGlobalCursor}
        />
        <ChainedNumberInput
          idPrefix={'matrix4-row3'}
          propsArray={propsArray.slice(8, 12)}
          setGlobalCursor={setGlobalCursor}
        />
        <ChainedNumberInput
          idPrefix={'matrix4-row4'}
          propsArray={propsArray.slice(12, 16)}
          setGlobalCursor={setGlobalCursor}
        />
      </FlexColumn>
    )
  },
)
