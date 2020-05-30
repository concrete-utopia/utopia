import * as React from 'react'
import { ValueType } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import { IndicatorContainerProps } from 'react-select/src/components/containers'
import { MultiValueRemoveProps } from 'react-select/src/components/MultiValue'
import { UNSAFE_getIconURL, InspectorSubsectionHeader, Section, UtopiaTheme } from 'uuiui'
import { betterReactMemo, SelectOption, Utils } from 'uuiui-deps'
import { GridRow } from '../../../widgets/grid-row'
import { useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { useInspectorMetadataInfo } from '../../../common/property-path-hooks'

const IndicatorsContainer: React.FunctionComponent<IndicatorContainerProps<SelectOption>> = () =>
  null

const MultiValueRemove: React.FunctionComponent<MultiValueRemoveProps<SelectOption>> = (props) => (
  <div {...props.innerProps} />
)

export const ClassNameControl = betterReactMemo('ClassNameSection', () => {
  const {
    value,
    onSubmitValue,
    onUnsetValues,
    controlStyles,
    controlStatus,
  } = useInspectorMetadataInfo('className')
  const onChange = (newValues: ValueType<SelectOption>) => {
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
  }
  const selectValues: ReadonlyArray<SelectOption> =
    value === '' ? [] : value.split(' ').map((v) => ({ value: v, label: `.${v}` }))

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  const mixed = controlStyles.mixed

  return (
    <Section>
      <InspectorSubsectionHeader style={{ ...headerStyle, height: 22 }}>
        Class names
      </InspectorSubsectionHeader>
      <GridRow padded type='<-------------1fr------------->' style={{ height: undefined }}>
        <CreatableSelect
          placeholder='Add class…'
          isMulti
          value={
            mixed
              ? [
                  {
                    value: '',
                    label: 'mixed',
                    style: { fontFamily: controlStyles.fontStyle },
                  },
                ]
              : selectValues
          }
          isDisabled={!controlStyles.interactive}
          onChange={onChange}
          components={{
            IndicatorsContainer,
            MultiValueRemove,
          }}
          className='className-inspector-control'
          styles={{
            container: (base) => ({
              ...base,
              transform:
                selectValues.length === 0 && !controlStyles.mixed ? 'translateX(-8px)' : undefined,
              minHeight: UtopiaTheme.layout.inputHeight.default,
              paddingTop: 2,
              paddingBottom: 2,
            }),
            control: () => ({
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
            valueContainer: (base) => ({
              ...base,
              padding: 0,
              height: '100%',
              width: '100%',
            }),
            multiValue: (base, state) => ({
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
            multiValueLabel: (base) => ({
              ...base,
              label: 'multiValueLabel',
              display: 'flex',
              alignItems: 'center',
              paddingTop: 2,
              paddingBottom: 2,
              paddingLeft: 6,
              paddingRight: 2,
              fontSize: 9,
            }),
            multiValueRemove: (base, state) => ({
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
            }),
            input: (base) => ({
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
            placeholder: (base) => ({
              ...base,
              paddingTop: 2,
              paddingBottom: 2,
              paddingLeft: 6,
              paddingRight: 6,
            }),
            menu: () => ({ display: 'none' }),
          }}
        />
      </GridRow>
    </Section>
  )
})
