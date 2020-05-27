import * as R from 'ramda'
import * as React from 'react'
import Select, { components, createFilter } from 'react-select'
import CreatableSelect, { Props as SelectProps } from 'react-select/creatable'
import { IndicatorProps } from 'react-select/src/components/indicators'
import Utils from '../../../utils/utils'
import { Icn, IcnProps } from 'uuiui'
import { colorTheme, UtopiaTheme } from 'uuiui'
import { ControlProps, GenericControlOptions } from './control'
import { getControlStyles, ControlStatus, isControlledStatus } from '../widgets/control-status'
import { ValueType } from 'react-select/src/types'
import { Icons } from 'uuiui'
import { PortalTargetID } from '../../../core/shared/utils'

export interface SelectControlOptions extends GenericControlOptions {
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

export interface SelectOption {
  value: any
  icon?: IcnProps
  label?: string
  style?: React.CSSProperties
  options?: SelectOption[]
}

const DropdownIndicator: React.FunctionComponent<IndicatorProps<SelectOption>> = (
  indicatorProps,
) => {
  return (
    components.DropdownIndicator && (
      <components.DropdownIndicator {...indicatorProps}>
        <Icons.ExpansionArrow />
      </components.DropdownIndicator>
    )
  )
}

const ControlledDropdownIndicator: React.FunctionComponent<IndicatorProps<SelectOption>> = (
  indicatorProps,
) => {
  return (
    components.DropdownIndicator && (
      <components.DropdownIndicator {...indicatorProps}>
        <Icons.ExpansionArrowControlled />
      </components.DropdownIndicator>
    )
  )
}

export const SelectControl: React.StatelessComponent<ControlProps<any>> = (props) => {
  const options = props.options != null ? (props.options as Array<SelectOption>) : []
  const controlOptions = {
    creatable: false,
    horizontalOrigin: 'left',
    focusOnMount: false,
    ...props.controlOptions,
  } as SelectControlOptions

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

  const formatCreateLabel = (inputValue: string) => inputValue

  const controlledStatus =
    props.controlStatus == null ? false : isControlledStatus(props.controlStatus)

  let selectProperties: SelectProps<SelectOption> = {
    formatCreateLabel: formatCreateLabel,
    className: `${R.pathOr('', ['controlClassName'], props)} flex-auto`,
    classNamePrefix: 'inspector-select',
    isMulti: false,
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
          backgroundColor: props.controlStyles.backgroundColor,
          boxShadow: `0 0 0 1px ${
            state.isFocused
              ? colorTheme.inspectorFocusedColor.value
              : props.controlStyles.borderColor
          } inset`,
          borderRadius: UtopiaTheme.inputBorderRadius,
          borderWidth: 0,
          minHeight: 0,
          padding: '2px 0',
        }
      },
      container: (base: React.CSSProperties) => ({
        ...base,
        height: '100%',
      }),
      option: (base: React.CSSProperties, state: any) => {
        const optionStyle = Utils.path(
          ['style'],
          options.find((option) => option.value === state.value),
        )
        return {
          ...base,
          minHeight: 22,
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
      input: (base) => ({
        ...base,
        padding: '0 4px',
      }),
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

interface BasicSelectControlParams {
  value: string
  id: string
  options: Array<SelectOption>
  onSubmitValue: ControlProps<any>['onSubmitValue']
  onTransientSubmitValue?: ControlProps<any>['onTransientSubmitValue']
  onForcedSubmitValue?: ControlProps<any>['onForcedSubmitValue']
  controlOptions?: SelectControlOptions
  controlClassName?: string
  htmlFor?: string
  style?: React.CSSProperties
  placeholder?: string
  inputRef?: React.RefObject<HTMLInputElement>
}

export const BasicSelectControl = ({
  value,
  id,
  options,
  onSubmitValue,
  onTransientSubmitValue,
  onForcedSubmitValue,
  controlOptions,
  controlClassName = '',
  htmlFor = '',
  style = {},
  inputRef,
}: BasicSelectControlParams) => {
  return (
    <SelectControl
      id={id}
      key={id}
      controlClassName={controlClassName}
      onSubmitValue={onSubmitValue}
      onTransientSubmitValue={onTransientSubmitValue}
      onForcedSubmitValue={onForcedSubmitValue}
      value={value}
      controlOptions={controlOptions}
      controlStatus={'simple'}
      controlStyles={getControlStyles('simple')}
      htmlFor={htmlFor}
      style={style}
      options={options}
    />
  )
}
