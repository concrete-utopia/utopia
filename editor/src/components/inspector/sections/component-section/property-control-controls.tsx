import fastDeepEquals from 'fast-deep-equal'
import React from 'react'
import { CSSCursor, SliderControl } from '../../../../uuiui-deps'
import {
  AllowedEnumType,
  BaseControlDescription,
  BasicControlOption,
  CheckboxControlDescription,
  ColorControlDescription,
  EulerControlDescription,
  ExpressionInputControlDescription,
  ExpressionPopUpListControlDescription,
  Matrix3ControlDescription,
  Matrix4ControlDescription,
  NumberInputControlDescription,
  PopUpListControlDescription,
  RadioControlDescription,
  StringInputControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  Vector4ControlDescription,
} from 'utopia-api/core'
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
  FlexRow,
  UtopiaTheme,
} from '../../../../uuiui'
import { printCSSNumber, CSSNumber, cssNumber, defaultCSSColor } from '../../common/css-utils'
import * as PP from '../../../../core/shared/property-path'
import { ColorControl } from '../../controls/color-control'
import { StringControl } from '../../controls/string-control'
import { SelectOption } from '../../controls/select-control'
import { OptionChainControl } from '../../controls/option-chain-control'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import { UIGridRow } from '../../widgets/ui-grid-row'
import {
  importDetailsFromImportOption,
  Imports,
  PropertyPath,
} from '../../../../core/shared/project-file-types'
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

export const CheckboxPropertyControl = React.memo(
  (props: ControlForPropProps<CheckboxControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-checkbox-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined

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

export const ColorPropertyControl = React.memo(
  (props: ControlForPropProps<ColorControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-color-property-control`
    const rawValue = propMetadata.propertyStatus.set ? propMetadata.value : undefined

    const value = rawValue ?? defaultCSSColor

    return (
      <ColorControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={value}
        showString={true}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
        onSubmitValue={propMetadata.onSubmitValue}
        onTransientSubmitValue={propMetadata.onTransientSubmitValue}
      />
    )
  },
)

export const ExpressionInputPropertyControl = React.memo(
  (props: ControlForPropProps<ExpressionInputControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props
    const dispatch = useEditorState(
      (store) => store.dispatch,
      'ExpressionInputPropertyControl dispatch',
    )

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
    }, 'ExpressionInputPropertyControl targetFilePaths')

    const controlId = `${propName}-expression-input-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined
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

type IndividualOption = AllowedEnumType | BasicControlOption<unknown>

function valueForIndividualOption(option: IndividualOption): unknown {
  if (typeof option === 'object' && option != null) {
    return option.value
  } else {
    return option
  }
}

function labelForIndividualOption(option: IndividualOption): string {
  if (typeof option === 'object' && option != null) {
    return option.label
  } else {
    return `${option}`
  }
}

export const PopUpListPropertyControl = React.memo(
  (props: ControlForPropProps<PopUpListControlDescription>) => {
    const { propMetadata, controlDescription } = props
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined

    // TS baulks at the map below for some reason if we don't first do this
    const controlOptions: Array<IndividualOption> = controlDescription.options

    const options: Array<SelectOption> = useKeepReferenceEqualityIfPossible(
      controlOptions.map((option) => {
        return {
          value: valueForIndividualOption(option),
          label: labelForIndividualOption(option),
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

export const ExpressionPopUpListPropertyControl = React.memo(
  (props: ControlForPropProps<ExpressionPopUpListControlDescription>) => {
    const dispatch = useEditorState(
      (store) => store.dispatch,
      'ExpressionPopUpListPropertyControl dispatch',
    )
    const selectedViews = useEditorState(
      (store) => store.editor.selectedViews,
      'ExpressionPopUpListPropertyControl selectedViews',
    )

    const targetFilePaths = useEditorState((store) => {
      const currentFilePath = forceNotNull(
        'Missing open file',
        store.editor.canvas.openFile?.filename,
      )
      return selectedViews.map((selectedView) => {
        const normalisedPath = normalisePathToUnderlyingTarget(
          store.editor.projectContents,
          store.editor.nodeModules.files,
          currentFilePath,
          selectedView,
        )
        return normalisePathSuccessOrThrowError(normalisedPath).filePath
      })
    }, 'ExpressionPopUpListPropertyControl targetFilePaths')

    const target = forceNotNull('Inspector control without selected element', selectedViews[0])
    const { propMetadata, controlDescription } = props

    const detectedExpression = controlDescription.options.find((option) =>
      fastDeepEquals(option.value, propMetadata.value),
    )?.expression

    const selectedExpression =
      propMetadata.propertyStatus.set && detectedExpression != null ? detectedExpression : undefined

    const options: Array<SelectOption> = useKeepReferenceEqualityIfPossible(
      controlDescription.options.map((option, index) => {
        return {
          value: option.expression,
          label: option.label ?? option.expression,
        }
      }),
    )
    const currentValue = options.find((option) => {
      return fastDeepEquals(option.value, selectedExpression)
    })

    const baseOnSubmit = propMetadata.onSubmitValue

    const submitValue = React.useCallback(
      (option: SelectOption): void => {
        baseOnSubmit(option.value)
        let actions: EditorAction[] = targetFilePaths.map((filePath) => forceParseFile(filePath))

        const expressionOption = controlDescription.options.find(
          (o) => o.expression === option.value,
        )
        if (expressionOption != null && expressionOption.requiredImport != null) {
          const importOption = expressionOption.requiredImport
          const importToAdd: Imports = {
            [importOption.source]: importDetailsFromImportOption(importOption),
          }
          actions.push(addImports(importToAdd, target))
        }
        dispatch(actions, 'everyone')
      },
      [dispatch, baseOnSubmit, targetFilePaths, target, controlDescription.options],
    )

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

export const NumberInputPropertyControl = React.memo(
  (props: ControlForPropProps<NumberInputControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      propMetadata.onSubmitValue,
      propMetadata.onUnsetValues,
    )
    const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
      propMetadata.onTransientSubmitValue,
      propMetadata.onUnsetValues,
    )

    const controlId = `${propName}-number-input-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined
    const controlOptions = useKeepReferenceEqualityIfPossible({
      minimum: controlDescription.min,
      maximum: controlDescription.max,
      stepSize: controlDescription.step,
    })
    if (controlOptions.minimum != null && controlOptions.maximum != null) {
      return (
        <NumberWithSliderControl
          {...props}
          controlOptions={controlOptions as DEPRECATEDSliderControlOptions}
        />
      )
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

export const RadioPropertyControl = React.memo(
  (props: ControlForPropProps<RadioControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-radio-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined

    // TS baulks at the map below for some reason if we don't first do this
    const controlOptions: Array<IndividualOption> = controlDescription.options

    const options: Array<SelectOption> = useKeepReferenceEqualityIfPossible(
      controlOptions.map((option) => {
        return {
          value: valueForIndividualOption(option),
          label: labelForIndividualOption(option),
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
      <OptionChainControl
        key={controlId}
        id={controlId}
        testId={controlId}
        value={currentValue}
        controlStatus={propMetadata.controlStatus}
        controlStyles={propMetadata.controlStyles}
        onSubmitValue={submitValue}
        options={options}
      />
    )
  },
)

const NumberWithSliderControl = React.memo(
  (
    props: ControlForPropProps<NumberInputControlDescription> & {
      controlOptions: DEPRECATEDSliderControlOptions
    },
  ) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-slider-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined

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

export const StringInputPropertyControl = React.memo(
  (props: ControlForPropProps<StringInputControlDescription>) => {
    const { propName, propMetadata, controlDescription } = props

    const controlId = `${propName}-string-input-property-control`
    const value = propMetadata.propertyStatus.set ? propMetadata.value : undefined

    return (
      <StringControl
        key={controlId}
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

export const VectorPropertyControl = React.memo(
  (
    props: ControlForPropProps<
      Vector2ControlDescription | Vector3ControlDescription | Vector4ControlDescription
    >,
  ) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const vectorValue = (propMetadata.propertyStatus.set ? propMetadata.value : undefined) ?? []

      const keys = keysForVectorOfType(controlDescription.control)
      return propsArrayForCSSNumberArray(vectorValue, keys, propPath, propMetadata)
    }, [controlDescription.control, propMetadata, propPath])

    return (
      <FlexRow style={{ alignItems: 'flex-start', paddingTop: 3, paddingBottom: 3 }}>
        <ChainedNumberInput
          idPrefix={'vector'}
          propsArray={propsArray}
          setGlobalCursor={setGlobalCursor}
        />
      </FlexRow>
    )
  },
)

export const EulerPropertyControl = React.memo(
  (props: ControlForPropProps<EulerControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const values = React.useMemo(
      () => (propMetadata.propertyStatus.set ? propMetadata.value : undefined) ?? [],
      [propMetadata.propertyStatus.set, propMetadata.value],
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
      <FlexColumn style={{ paddingTop: 3, paddingBottom: 3 }}>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.max }}>
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
        </FlexRow>
      </FlexColumn>
    )
  },
)

export const Matrix3PropertyControl = React.memo(
  (props: ControlForPropProps<Matrix3ControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const value = (propMetadata.propertyStatus.set ? propMetadata.value : undefined) ?? []

      // prettier-ignore
      const keys = [
        'n11', 'n12', 'n13',
        'n21', 'n22', 'n23',
        'n31', 'n32', 'n33',
      ]
      return propsArrayForCSSNumberArray(value, keys, propPath, propMetadata)
    }, [propMetadata, propPath])

    return (
      <FlexColumn style={{ paddingTop: 3, paddingBottom: 3 }}>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix3-row1'}
            propsArray={propsArray.slice(0, 3)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix3-row2'}
            propsArray={propsArray.slice(3, 6)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix3-row3'}
            propsArray={propsArray.slice(6, 9)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
      </FlexColumn>
    )
  },
)

export const Matrix4PropertyControl = React.memo(
  (props: ControlForPropProps<Matrix4ControlDescription>) => {
    const { propPath, propMetadata, controlDescription, setGlobalCursor } = props

    const propsArray: Array<Omit<NumberInputProps, 'id' | 'chained'>> = React.useMemo(() => {
      const value = (propMetadata.propertyStatus.set ? propMetadata.value : undefined) ?? []

      // prettier-ignore
      const keys = [
        'n11', 'n12', 'n13', 'n14',
        'n21', 'n22', 'n23', 'n24',
        'n31', 'n32', 'n33', 'n34',
        'n41', 'n42', 'n43', 'n44',
      ]
      return propsArrayForCSSNumberArray(value, keys, propPath, propMetadata)
    }, [propMetadata, propPath])

    return (
      <FlexColumn style={{ paddingTop: 3, paddingBottom: 3 }}>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix4-row1'}
            propsArray={propsArray.slice(0, 4)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix4-row2'}
            propsArray={propsArray.slice(4, 8)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix4-row3'}
            propsArray={propsArray.slice(8, 12)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
        <FlexRow style={{ height: UtopiaTheme.layout.rowHeight.large }}>
          <ChainedNumberInput
            idPrefix={'matrix4-row4'}
            propsArray={propsArray.slice(12, 16)}
            setGlobalCursor={setGlobalCursor}
          />
        </FlexRow>
      </FlexColumn>
    )
  },
)
