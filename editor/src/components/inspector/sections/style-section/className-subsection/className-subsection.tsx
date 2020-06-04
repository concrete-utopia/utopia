import { ObjectInterpolation } from '@emotion/core'
import * as Chroma from 'chroma-js'
import * as React from 'react'
import { ValueType } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import { IndicatorContainerProps } from 'react-select/src/components/containers'
import { MultiValueRemoveProps } from 'react-select/src/components/MultiValue'
import { styleFn } from 'react-select/src/styles'
import { Section, UNSAFE_getIconURL, UtopiaTheme } from 'uuiui'
import { betterReactMemo, ControlStyles, SelectOption, Utils } from 'uuiui-deps'
import { utopionsStylesOptions } from '../../../../../experimental/pseudo-utopions-css'
import { useInspectorElementInfo } from '../../../common/property-path-hooks'
import { GridRow } from '../../../widgets/grid-row'

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

// a row in the dropdown
const option = (((
  styles: ObjectInterpolation<any>,
  { data, isDisabled, isFocused, isSelected }: any,
): ObjectInterpolation<any> => {
  let color = Chroma('red')
  try {
    color = Chroma(data.color)
  } catch (err) {
    color = Chroma('green')
  }
  return {
    ...styles,
    backgroundColor: isDisabled
      ? null
      : isSelected
      ? data.color
      : isFocused
      ? data.color + '33'
      : null,
    color: isDisabled
      ? '#ccc'
      : isSelected
      ? Chroma.contrast(color, 'white') > 2
        ? 'white'
        : 'black'
      : data.color,
    cursor: isDisabled ? 'not-allowed' : 'default',
    ':active': {
      ...styles[':active'],
      backgroundColor: !isDisabled && (isSelected ? data.color : color.alpha(0.3).css()),
    },
  }
}) as unknown) as styleFn

// the outer container for the chiclets / text entry
const valueContainer: styleFn = (base) => ({
  ...base,
  padding: 0,
  height: '100%',
  width: '100%',
})

// chiclet
const multiValue: styleFn = (base, { data }) => ({
  label: 'multiValue',
  color: UtopiaTheme.color.emphasizedForeground.value,
  borderRadius: UtopiaTheme.inputBorderRadius,
  fontWeight: 600,
  backgroundColor: data.color,
  display: 'flex',
  marginRight: 4,
  marginTop: 2,
  marginBottom: 2,
  minWidth: 0,
  height: UtopiaTheme.layout.inputHeight.small,
  overflow: 'hidden',
  border: '1px solid black',
  fontSize: 10,
})

// label inside chiclet
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

// x-button right side of chiclet
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

// dropdown part of the menu
const menu: styleFn = () => ({ zIndex: 999999, boxShadow: '0px 2px 4px 1px #00000022' })

const ClassNameControl = betterReactMemo(
  'ClassNameControl',
  <T extends string>({
    controlStyles,
    values,
    onSubmitValue,
    onUnsetValues,
  }: ClassNameControlProps<T>) => {
    const optionsMappedValues = values.map((value) => {
      const match = utopionsStylesOptions.find((element) => element.value === value.value)
      return match ?? value
    })

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
        '& input': {
          fontFamily: 'Inter',
          color: UtopiaTheme.color.emphasizedForeground.value,
        },
      }),
      [],
    )

    const container: styleFn = React.useCallback(
      (base) => ({
        ...base,
        transform: valuesLength === 0 && !controlStyles.mixed ? 'translateX(-8px)' : undefined,
        minHeight: UtopiaTheme.layout.inputHeight.default,
        backgroundColor: 'white',
        paddingTop: 2,
        paddingBottom: 2,
      }),
      [controlStyles, valuesLength],
    )

    // outermost container
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

    return (
      <CreatableSelect
        isClearable
        placeholder='add css class name'
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
            : optionsMappedValues
        }
        options={utopionsStylesOptions}
        isDisabled={!controlStyles.interactive}
        onChange={onChange}
        components={{
          IndicatorsContainer,
          MultiValueRemove,
        }}
        className='className-inspector-control'
        styles={{
          container,
          control,
          option,
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
  const { value, onSubmitValue, onUnsetValues, controlStyles } = useInspectorElementInfo(
    'className',
  )

  const values: ReadonlyArray<SelectOption> =
    value === '' ? [] : value.split(' ').map((v) => ({ value: v, label: `.${v}` }))

  // const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  return (
    <Section>
      <GridRow
        padded
        type='<-------------1fr------------->'
        style={{ height: undefined, overflow: 'visible' }}
      >
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
