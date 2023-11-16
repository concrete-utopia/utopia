import React from 'react'
import type { InputProps } from 'react-select'
import Select, { components, createFilter } from 'react-select'
import type { Props as SelectProps } from 'react-select/creatable'
import CreatableSelect from 'react-select/creatable'
import type { IndicatorProps } from 'react-select/src/components/indicators'
import Utils from '../../../utils/utils'
import type { DEPRECATEDControlProps, DEPRECATEDGenericControlOptions } from './control'
import { isControlledStatus } from '../common/control-status'
import { getControlStyles } from '../common/control-styles'
import type { ValueType } from 'react-select/src/types'
import { PortalTargetID } from '../../../core/shared/utils'
import type { IcnProps } from '../../../uuiui'
import { Icons, useColorTheme, UtopiaTheme } from '../../../uuiui'
import { styleStringInArray } from '../../../utils/common-constants'

export interface DEPRECATEDSelectControlOptions extends DEPRECATEDGenericControlOptions {
  creatable?: boolean
  dropdownWidth?: number
  horizontalOrigin?: 'left' | 'right'
  placeholder?: string
  focusOnMount?: boolean
  onInputSubmitValue?: (newValue: string) => void
  selectRegularRef?: React.RefObject<Select<any>>
  selectCreatableRef?: React.RefObject<CreatableSelect<any>>
  onKeyDown?: (e: React.KeyboardEvent<HTMLElement>) => void
}

export interface SelectOption<T = any> {
  value: T
  icon?: Omit<IcnProps, 'color' | 'width' | 'height'>
  label?: string | React.ReactElement
  style?: React.CSSProperties
  options?: SelectOption<any>[]
  disabled?: boolean
  tooltip?: string
  invalid?: boolean
}

const DropdownIndicator: React.FunctionComponent<
  React.PropsWithChildren<IndicatorProps<SelectOption>>
> = (indicatorProps) => {
  return components.DropdownIndicator == null ? null : (
    <components.DropdownIndicator {...indicatorProps}>
      <Icons.ExpansionArrow />
    </components.DropdownIndicator>
  )
}

const ControlledDropdownIndicator: React.FunctionComponent<
  React.PropsWithChildren<IndicatorProps<SelectOption>>
> = (indicatorProps) => {
  return components.DropdownIndicator == null ? null : (
    <components.DropdownIndicator {...indicatorProps}>
      <Icons.ExpansionArrowControlled />
    </components.DropdownIndicator>
  )
}

export const CustomReactSelectInput = (props: InputProps) => {
  const inputStyle = React.useMemo(() => {
    return {
      label: 'input',
      background: 'transparent',
      border: 0,
      fontSize: 'inherit',
      opacity: props.isHidden ? 0 : 1,
      outline: 0,
      paddingLeft: '4px',
      paddingRight: '4px',
      width: '100%',
      color: 'inherit',
      // default input height minus 1px padding and the border
      // on the outer container
      height: UtopiaTheme.layout.inputHeight.default - 2,
    }
  }, [props.isHidden])

  let strippedProps: any = { ...props }
  delete strippedProps['getStyles']
  delete strippedProps['innerRef']
  delete strippedProps['isHidden']
  delete strippedProps['isDisabled']
  delete strippedProps['cx']
  delete strippedProps['selectProps']

  return (
    <input
      className={props.className}
      style={inputStyle}
      disabled={props.isDisabled}
      {...strippedProps}
    />
  )
}

export const SelectControl: React.FunctionComponent<
  React.PropsWithChildren<DEPRECATEDControlProps<any>>
> = (props) => {
  const colorTheme = useColorTheme()
  const options = props.options != null ? (props.options as Array<SelectOption>) : []
  const controlOptions = {
    creatable: false,
    horizontalOrigin: 'left',
    focusOnMount: false,
    ...props.DEPRECATED_controlOptions,
  } as DEPRECATEDSelectControlOptions

  const selectedOption = options.find((option) => option.value === props.value)

  let value: SelectOption = {
    value: undefined,
    label: undefined,
  }
  if (selectedOption != null) {
    value.value = props.value
    value.label = selectedOption.label != null ? selectedOption.label : props.value
    value.style = selectedOption.style != null ? selectedOption.style : undefined
    value.icon = selectedOption.icon != null ? selectedOption.icon : undefined
  } else if (typeof props.value === 'object' && value.hasOwnProperty('value')) {
    value = props.value
  }

  const mixed = props.controlStyles.mixed

  const createableSelectOnSubmitValue = (newValue: ValueType<SelectOption>) => {
    if (newValue != null && !Array.isArray(newValue)) {
      props.onSubmitValue((newValue as any).value)
    }
  }

  const formatCreateLabel = (inputValue: string) => `Add ${inputValue}`

  const controlledStatus =
    props.controlStatus == null ? false : isControlledStatus(props.controlStatus)

  let selectProperties: SelectProps<SelectOption> = {
    formatCreateLabel: formatCreateLabel,
    className: `${Utils.pathOr('', ['controlClassName'], props)} flex-auto`,
    classNamePrefix: 'inspector-select',
    isMulti: false,
    openMenuOnFocus: true,
    menuShouldScrollIntoView: true,
    menuPlacement: 'auto',
    menuPortalTarget: document.getElementById(PortalTargetID),
    value: mixed
      ? ({
          value,
          label: 'mixed',
          style: { fontFamily: props.controlStyles.fontStyle },
        } as SelectOption)
      : value,
    isClearable: false,
    isDisabled: !props.controlStyles.interactive,
    components: {
      DropdownIndicator: controlledStatus ? ControlledDropdownIndicator : DropdownIndicator,
      Input: CustomReactSelectInput,
      ...props.reactSelectComponents,
    },
    onChange: createableSelectOnSubmitValue,
    onInputChange: controlOptions.onInputSubmitValue,
    options: options,
    placeholder: controlOptions.placeholder,
    autoFocus: controlOptions.focusOnMount,
    onKeyDown: controlOptions.onKeyDown,
    filterOption: createFilter({ ignoreAccents: true }),
    styles: {
      control: (base, state) => {
        return {
          ...base,
          height: '100%',
          backgroundColor: (state.isFocused as boolean)
            ? props.controlStyles.focusedBackgroundColor
            : props.controlStyles.backgroundColor,
          boxShadow: `0 0 0 1px ${
            (state.isFocused as boolean)
              ? colorTheme.inspectorFocusedColor.value
              : props.controlStyles.borderColor
          } inset`,
          borderRadius: UtopiaTheme.inputBorderRadius,
          borderWidth: 0,
          minHeight: 0,
          padding: '1px',
          '&:hover': {
            boxShadow: `0 0 0 1px ${colorTheme.inspectorFocusedColor.value}`,
          },
        }
      },
      container: (base: React.CSSProperties) => ({
        ...base,
      }),
      option: (base: React.CSSProperties, state: any) => {
        const optionStyle = Utils.path(
          styleStringInArray,
          options.find((option) => option.value === state.value),
        )
        return {
          ...base,
          minHeight: UtopiaTheme.layout.inputHeight.default,
          padding: 8,
          display: 'grid',
          gridTemplateColumns: 'auto 14px',
          ...(optionStyle as React.CSSProperties),
        }
      },
      singleValue: (base: React.CSSProperties) => {
        return {
          ...base,
          padding: '0px 4px',
          color: props.controlStyles.mainColor,
          overflow: 'visible',
          ...value.style,
        }
      },
      placeholder: (base) => ({
        ...base,
        padding: '0 4px',
      }),
      menu: (base: React.CSSProperties) => {
        return {
          ...base,
          overflow: 'hidden',
          zIndex: 2,
          width:
            controlOptions.dropdownWidth != null
              ? `${controlOptions.dropdownWidth}px !important`
              : '100%',
          [controlOptions.horizontalOrigin as 'left' | 'right']: 0,
        }
      },
      menuList: (base) => ({
        ...base,
        padding: 0,
        maxHeight: 120,
      }),
      indicatorSeparator: () => ({
        display: 'none',
      }),
      dropdownIndicator: (base) => ({
        ...base,
        opacity: props.controlStyles.interactive ? 1 : 0,
        padding: '0px 2px',
      }),
      valueContainer: (base) => ({
        ...base,
        padding: 0,
        height: '100%',
        width: 'calc(100% - 18px)',
        overflow: 'hidden',
      }),
    },
  }

  return (
    <div
      onMouseDownCapture={(e) => {
        if (e.button === 2) {
          e.stopPropagation()
        }
      }}
      style={props.style}
    >
      {controlOptions.creatable ? (
        <CreatableSelect {...selectProperties} ref={controlOptions.selectCreatableRef} />
      ) : (
        <Select {...selectProperties} ref={controlOptions.selectRegularRef} />
      )}
    </div>
  )
}
