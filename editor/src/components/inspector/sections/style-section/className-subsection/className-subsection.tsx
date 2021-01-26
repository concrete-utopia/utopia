import * as React from 'react'
import { ValueType } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import { IndicatorContainerProps } from 'react-select/src/components/containers'
import { MultiValueRemoveProps } from 'react-select/src/components/MultiValue'
import { GridRow } from '../../../widgets/grid-row'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { useInspectorElementInfo } from '../../../common/property-path-hooks'
import { styleFn } from 'react-select/src/styles'
import { CustomReactSelectInput, SelectOption } from '../../../controls/select-control'
import {
  UtopiaTheme,
  UNSAFE_getIconURL,
  Section,
  InspectorSubsectionHeader,
  InspectorSectionHeader,
} from '../../../../../uuiui'
import { ControlStyles, betterReactMemo, Utils } from '../../../../../uuiui-deps'

const IndicatorsContainer: React.FunctionComponent<IndicatorContainerProps<SelectOption>> = () =>
  null

const MultiValueRemove: React.FunctionComponent<MultiValueRemoveProps<SelectOption>> = (props) => (
  <div {...props.innerProps} />
)

interface ClassNameControlProps<T extends string> {
  controlStyles: ControlStyles
  values: ReadonlyArray<SelectOption>
  onSubmitValue: (newTransformedValues: string, transient?: boolean) => void
  onUnsetValues: () => void
}

const valueContainer: styleFn = (base) => ({
  ...base,
  padding: 0,
  height: '100%',
  width: '100%',
})

const multiValueLabel: styleFn = (base) => ({
  ...base,
  label: 'multiValueLabel',
  display: 'flex',
  alignItems: 'center',
  paddingTop: 2,
  paddingBottom: 2,
  paddingLeft: 6,
  paddingRight: 2,
  fontSize: 9,
})

const multiValueRemove: styleFn = (base, state) => ({
  label: 'multiValueRemove',
  width: 16,
  height: UtopiaTheme.layout.inputHeight.small,
  display: 'flex',
  alignItems: 'center',
  padding: 0,
  overflow: 'hidden',
  marginRight: 2,
  backgroundImage: `url(${
    state.isFocused
      ? UNSAFE_getIconURL('cross-in-translucent-circle', 'blue')
      : UNSAFE_getIconURL('cross-small')
  })`,
  backgroundSize: 16,
  backgroundPosition: 'center center',
  ':hover': {
    backgroundImage: `url(${UNSAFE_getIconURL('cross-in-translucent-circle', 'blue')})`,
  },
})

const placeholder: styleFn = (base) => ({
  ...base,
  paddingTop: 2,
  paddingBottom: 2,
  paddingLeft: 6,
  paddingRight: 6,
})

const menu: styleFn = () => ({ display: 'none' })

const ClassNameControl = betterReactMemo(
  'ClassNameControl',
  <T extends string>({
    controlStyles,
    values,
    onSubmitValue,
    onUnsetValues,
  }: ClassNameControlProps<T>) => {
    const onChange = React.useCallback(
      (newValues: ValueType<SelectOption>) => {
        if (Array.isArray(newValues) && newValues.length > 0) {
          onSubmitValue(
            Utils.stripNulls(
              newValues.map((newValue) => {
                const newValueValue = newValue.value
                if (typeof newValueValue === 'string' && newValueValue.length > 0) {
                  const trimmed = newValueValue.trim()
                  return trimmed.startsWith('.') ? trimmed.slice(1) : trimmed
                } else {
                  return null
                }
              }),
            ).join(' '),
          )
        } else if (newValues === null) {
          onUnsetValues()
        }
      },
      [onSubmitValue, onUnsetValues],
    )

    const valuesLength = values.length

    const input: styleFn = React.useCallback(
      (base) => ({
        ...base,
        borderRadius: UtopiaTheme.inputBorderRadius,
        fontSize: 9,
        height: UtopiaTheme.layout.inputHeight.small,
        paddingTop: 0,
        paddingBottom: 2,
        paddingLeft: 6,
        paddingRight: 6,
        minWidth: 90,
        display: 'flex',
        alignItems: 'center',
        '.className-inspector-control:hover &, &:focus-within': {
          boxShadow: `inset 0 0 0 1px ${controlStyles.borderColor}`,
        },
        '& input': {
          fontFamily: 'Consolas, Menlo, monospace',
          color: UtopiaTheme.color.emphasizedForeground.value,
        },
      }),
      [controlStyles],
    )

    const container: styleFn = React.useCallback(
      (base) => ({
        ...base,
        transform: valuesLength === 0 && !controlStyles.mixed ? 'translateX(-8px)' : undefined,
        minHeight: UtopiaTheme.layout.inputHeight.default,
        paddingTop: 2,
        paddingBottom: 2,
      }),
      [controlStyles, valuesLength],
    )
    const control: styleFn = React.useCallback(
      () => ({
        label: 'control',
        alignItems: 'center',
        backgroundColor: controlStyles.backgroundColor,
        boxSizing: 'border-box',
        cursor: 'default',
        display: 'flex',
        flexWrap: 'wrap',
        justifyContent: 'space-between',
        position: 'relative',
        transition: 'all 100ms',
        minHeight: UtopiaTheme.layout.inputHeight.default,
      }),
      [controlStyles],
    )
    const multiValue: styleFn = React.useCallback(
      (base, state) => ({
        label: 'multiValue',
        fontFamily: 'Consolas, Menlo, monospace',
        color: UtopiaTheme.color.emphasizedForeground.value,
        borderRadius: UtopiaTheme.inputBorderRadius,
        display: 'flex',
        marginRight: 4,
        marginTop: 2,
        marginBottom: 2,
        minWidth: 0,
        height: UtopiaTheme.layout.inputHeight.small,
        boxShadow: `inset 0 0 0 1px ${
          state.isFocused
            ? UtopiaTheme.color.inspectorFocusedColor.value
            : controlStyles.borderColor
        }`,
        overflow: 'hidden',
      }),
      [controlStyles],
    )

    return (
      <CreatableSelect
        placeholder='Add class…'
        isMulti
        value={
          controlStyles.mixed
            ? [
                {
                  value: '',
                  label: 'mixed',
                  style: { fontFamily: controlStyles.fontStyle },
                },
              ]
            : values
        }
        isDisabled={!controlStyles.interactive}
        onChange={onChange}
        components={{
          IndicatorsContainer,
          MultiValueRemove,
          Input: CustomReactSelectInput,
        }}
        className='className-inspector-control'
        styles={{
          container,
          control,
          valueContainer,
          multiValue,
          multiValueLabel,
          multiValueRemove,
          input,
          placeholder,
          menu,
        }}
      />
    )
  },
)

export const ClassNameSubsection = betterReactMemo('ClassNameSubSection', () => {
  const {
    value,
    onSubmitValue,
    onUnsetValues,
    controlStyles,
    controlStatus,
  } = useInspectorElementInfo('className')

  const values: ReadonlyArray<SelectOption> =
    value === '' ? [] : value.split(' ').map((v) => ({ value: v, label: `.${v}` }))

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  return (
    <Section>
      <InspectorSectionHeader style={{ ...headerStyle, height: 22 }}>
        Class names
      </InspectorSectionHeader>
      <GridRow padded type='<-------------1fr------------->' style={{ height: undefined }}>
        <ClassNameControl
          values={values}
          controlStyles={controlStyles}
          onSubmitValue={onSubmitValue}
          onUnsetValues={onUnsetValues}
        />
      </GridRow>
    </Section>
  )
})
