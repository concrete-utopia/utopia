/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as Chroma from 'chroma-js'
import * as React from 'react'
import Select, { components } from 'react-select'
import { IndicatorProps } from 'react-select/src/components/indicators'
import { MenuProps } from 'react-select/src/components/Menu'
import {
  MultiValueGenericProps,
  MultiValueProps,
  MultiValueRemoveProps,
} from 'react-select/src/components/MultiValue'
import { OptionProps } from 'react-select/src/components/Option'
import { SingleValueProps } from 'react-select/src/components/SingleValue'
import { ActionMeta, ValueType } from 'react-select/src/types'
import { Icn } from 'uuiui'
import { colorTheme, UtopiaTheme } from 'uuiui'
import { FlexColumn } from 'uuiui'
import { ControlStyles } from '../widgets/control-status'
import utils from '../../../utils/utils'
import { PortalTargetID } from '../../../core/shared/utils'

type ColorSwatchProps = {
  color: string
  isValid: boolean
  style: React.CSSProperties
}

export const ColorSwatch: React.FunctionComponent<ColorSwatchProps> = (props) => {
  return (
    <div
      css={{
        position: 'relative',
        display: 'flex',
        width: '18px',
        height: '18px',
        backgroundImage:
          props.color && props.isValid
            ? 'linear-gradient(45deg, hsl(0,0%,80%) 25%, transparent 25%, transparent 75%, hsl(0,0%,80%) 75%, hsl(0,0%,70%)), linear-gradient(45deg, hsl(0,0%,80%) 25%, transparent 25%, transparent 75%, hsl(0,0%,80%) 75%, hsl(0,0%,80%))'
            : 'white',
        backgroundSize: '9px 9px',
        backgroundPosition: '0 0, 4px 4px',
        borderRadius: '50%',
        overflow: 'clip',
        '&:before': {
          ...props.style,
          backgroundColor: props.color && props.isValid ? props.color : 'transparent',
          content: '" "',
          position: 'absolute',
          left: '0px',
          top: '0px',
          bottom: '0px',
          right: '0px',
          borderRadius: '50%',
        },
      }}
    >
      <span style={{ margin: 'auto', fontWeight: 600, color: 'red' }}>
        {props.isValid ? null : '?'}
      </span>

      {props.children}
    </div>
  )
}

export const FontThemeMenuOptions: Array<FontThemeMenuOption> = [
  {
    label: 'base.primary.headline',
    value: {
      fontWeight: 'bold',
      fontSize: '64px',
      textTransform: 'uppercase',
      fontFamily: 'San Francisco',
      color: '#007AFF',
    },
  },
  {
    label: 'base.primary.h1',
    value: {
      fontWeight: 300,
      fontSize: '27px',
      fontFamily: 'Papyrus',
      color: '#007AFF',
    },
  },
  {
    label: 'base.primary.h2',
    value: {
      fontWeight: 400,
      fontSize: '22px',
      letterSpacing: '29px',
      lineHeight: '30px',
      color: '#007AFF',
    },
  },
  {
    label: 'base.primary.h3',
    value: {
      fontWeight: 500,
      fontSize: '17px',
      textTransform: 'uppercase',
      color: 'black',
    },
  },
  {
    label: 'base.primary.normal',
    value: {
      fontWeight: 'bold',
      fontSize: '11px',
      textTransform: 'uppercase',
      color: 'black',
    },
  },
  {
    label: 'base.primary.subdued',
    value: {
      fontWeight: 'bold',
      fontFamily: 'Arial, Helvetica Neue',
      fontSize: '10px',
      textTransform: 'uppercase',
      color: 'lightgrey',
    },
  },
]

export type FontThemeMenuOption = {
  value: React.CSSProperties
  label: string
}

const generateReplacementMenu = (value: ValueType<FontThemeMenuOption>) => {
  let TypographicDetail: React.ReactElement
  if (value == null) {
    TypographicDetail = (
      <div>
        Nothing selected. Check the <a href='/'>theme file</a>.
      </div>
    ) // Todo can we add a link?
  } else if (!Array.isArray(value)) {
    const castValue = value as FontThemeMenuOption // wtf why can't typescript understand this isn't an array here?
    TypographicDetail = (
      <div style={{ display: 'flex', flexDirection: 'column' }}>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
          }}
        >
          <span style={{ fontWeight: 600 }}>{castValue.label}</span>
        </div>
        <div>
          <span style={{ color: '#aaa', fontWeight: 400 }}>
            {Object.keys(castValue.value).length}
            {Object.keys(castValue.value).length == 1 ? ' Property' : ' Properties'}
            <br />
            {Object.keys(castValue.value).map((x) => x + ' ')}
          </span>
        </div>
      </div>
    )
  } else if (Array.isArray(value)) {
    TypographicDetail = <div>Multiple values selected.</div>
  }
  const ReplacementMenu: React.FunctionComponent<MenuProps<FontThemeMenuOption>> = (props) => {
    return (
      <components.Menu {...props}>
        <div style={{ backgroundColor: 'white' }}>
          <div
            style={{
              padding: '4px 8px',
              fontSize: '11px',
              display: 'flex',
              verticalAlign: 'center',
            }}
          >
            <span style={{ fontWeight: 'bold', marginRight: '12px' }}>Fonts</span> Complex Styles
          </div>
          <div className='wrapperForInnerMenu'>{props.children}</div>
          <div
            className='focusedElementMetadata'
            style={{
              overflow: 'hidden',
              backgroundColor: '#FCFCFC',
              boxShadow: 'inset 0px 1px 0px 0px rgba(0,0,0,.1)',
              padding: '8px 8px',
              fontSize: '11px',
              pointerEvents: 'none',
            }}
          >
            {TypographicDetail}
          </div>
        </div>
      </components.Menu>
    )
  }
  return ReplacementMenu
}

const ReplacementOption: React.FunctionComponent<OptionProps<FontThemeMenuOption>> = (props) => {
  // Color fallbacks and backdrop computation
  const appliedColor = utils.defaultIfNull('#111', props.data.value.color)
  const backgroundColor = Chroma.contrast(appliedColor, '#FFFFFF') < 4 ? '#333' : 'white'
  const pathValueColor = props.isSelected ? '#fff' : 'hsl(0,0%,50%)'
  const highlightDataColor = props.isSelected ? '#fff' : 'inherit'

  return (
    <components.Option {...props}>
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          fontFamily: 'Inter',
          fontSize: '11px',
          color: '#111',
          backgroundColor: 'transparent',
        }}
      >
        <div
          style={{
            fontSize: props.data.value.fontSize,
            fontFamily: props.data.value.fontFamily,
            fontWeight: props.data.value.fontWeight,
            color: props.data.value.color,
            backgroundColor: backgroundColor,
            padding: '4px 4px',
            overflow: 'clip',
          }}
        >
          {props.data.label}
        </div>

        <div
          className='pathValue'
          style={{ padding: '4px 4px', color: pathValueColor, fontWeight: 600 }}
        >
          {props.data.label}
        </div>
        <div
          className='highlightData'
          style={{
            color: highlightDataColor,
            padding: '4px 4px',
            textTransform: 'capitalize',
          }}
        >
          getPreviewedProperties(props.data.value)
        </div>
      </div>
    </components.Option>
  )
}

export const ReplacementDropdownIndicator: React.FunctionComponent<IndicatorProps<
  FontThemeMenuOption
>> = (props) =>
  components.DropdownIndicator && (
    <components.DropdownIndicator {...props}>
      <Icn category='select' type='downIndicator' color='gray' width={9} height={5} />
    </components.DropdownIndicator>
  )

const ReplacementClearIndicator: React.FunctionComponent<IndicatorProps<FontThemeMenuOption>> = (
  props,
) =>
  components.ClearIndicator && (
    <components.ClearIndicator {...props}>
      <Icn category='select' type='clearIndicator' color='gray' width={15} height={15} />
    </components.ClearIndicator>
  )

const ReplacementSingleValue: React.FunctionComponent<SingleValueProps<FontThemeMenuOption>> = (
  props,
) => (
  <components.SingleValue {...props}>
    <div style={{ display: 'flex', flexDirection: 'row', fontSize: '11px' }}>
      <ColorSwatch
        color={Chroma.valid(props.data.value) ? String(props.data.value) : 'white'}
        isValid={Chroma.valid(props.data.value)}
        style={{ boxShadow: 'inset 0px 0px 0px 1px rgba(0,0,0, .15)' }}
      />
      <span style={{ marginLeft: '8px', flexGrow: 1 }}>{props.children}</span>
    </div>
  </components.SingleValue>
)

const ReplacementMultiValueContainer: React.FunctionComponent<MultiValueProps<
  FontThemeMenuOption
>> = (props) => (
  <components.MultiValueContainer {...props}>
    <div className='MultiValueContainer' style={{ position: 'relative', width: '100%' }}>
      {props.children}
    </div>
  </components.MultiValueContainer>
)

const getPreviewedProperties = (properties: React.CSSProperties) => {
  let numberOfUnPreviewedProperties = 0
  const previewItemKeys = (Object.keys(properties) as Array<keyof React.CSSProperties>).filter(
    (property) => {
      if (property === 'fontSize' || property === 'fontWeight' || property === 'fontFamily') {
        return true
      } else {
        numberOfUnPreviewedProperties++
        return false
      }
    },
  )
  const previewItems = (['fontSize', 'fontWeight', 'fontFamily'] as Array<
    keyof React.CSSProperties
  >)
    .filter((element) => previewItemKeys.indexOf(element) >= 0)
    .map((key) => (
      <span
        key={key}
        style={{
          marginRight: 6,
        }}
      >
        {properties[key]}
      </span>
    ))
  if (numberOfUnPreviewedProperties > 0) {
    previewItems.push(<span key='others'>+{numberOfUnPreviewedProperties} others</span>)
  }
  return previewItems
}

const ReplacementMultiValueLabel: React.FunctionComponent<MultiValueGenericProps<
  FontThemeMenuOption
>> = (props) => {
  const previewItems = getPreviewedProperties(props.data.value)
  return (
    <div
      key={props.data.label}
      className='MultiValueLabel'
      style={{
        height: '100%',
        width: '100%',
        display: 'flex',
        flexDirection: 'row',
        padding: 0,
      }}
    >
      <div
        style={{
          ...props.data.value,
          textAlign: 'center',
          fontSize: 16,
          margin: 2,
          width: 32,
          height: 32,
          backgroundColor: '#fff',
          position: 'relative',
          overflow: 'hidden',
          borderRadius: 3,
        }}
      >
        <div
          style={{
            width: '100%',
            position: 'absolute',
            top: '50%',
            left: '50%',
            marginRight: '-50%',
            transform: 'translate(-50%, -50%)',
          }}
        >
          Aa
        </div>
      </div>
      <FlexColumn
        className='preview'
        style={{
          padding: '4px 6px',
          width: 'calc(100% - 36px)',
        }}
      >
        <div
          style={{
            fontWeight: 500,
            overflow: 'hidden',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
          }}
        >
          {props.data.label}
        </div>
        <div
          style={{
            overflow: 'hidden',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
            lineHeight: '1.5em',
            fontSize: 9,
          }}
        >
          {previewItems}
        </div>
      </FlexColumn>
    </div>
  )
}

const ReplacementMultiValueRemove: React.FunctionComponent<MultiValueRemoveProps<
  FontThemeMenuOption
>> = (props) => {
  return (
    <components.MultiValueRemove {...props}>
      <Icn category='select' type='remove' width={9} height={9} color='gray' />
    </components.MultiValueRemove>
  )
}

export type FontThemeMenuProps = {
  options: Array<FontThemeMenuOption>
  value: ValueType<FontThemeMenuOption>
  style: React.CSSProperties
  onChange?: (value: ValueType<FontThemeMenuOption>, action?: ActionMeta) => void
  controlStyles: ControlStyles
}

export const FontThemeMenu: React.FunctionComponent<FontThemeMenuProps> = (props) => {
  return (
    <div style={props.style}>
      <Select
        isClearable={false}
        isMulti
        closeMenuOnSelect={false}
        menuPortalTarget={document.getElementById(PortalTargetID)}
        components={{
          SingleValue: ReplacementSingleValue,
          ClearIndicator: ReplacementClearIndicator,
          DownChevron: ReplacementDropdownIndicator, // todo: should this be different?
          DropdownIndicator: ReplacementDropdownIndicator,
          Option: ReplacementOption,
          MultiValueContainer: ReplacementMultiValueContainer,
          MultiValueLabel: ReplacementMultiValueLabel,
          MultiValueRemove: ReplacementMultiValueRemove,
          Menu: generateReplacementMenu(props.value),
        }}
        value={props.value}
        styles={{
          control: (base, state) => ({
            ...base,
            width: '100%',
            minHeight: 35,
            border: 0,
            boxShadow: `0 0 0 1px ${
              state.isFocused
                ? colorTheme.inspectorFocusedColor.value
                : props.controlStyles.borderColor
            } inset`,
            borderRadius: UtopiaTheme.inputBorderRadius,
            display: 'flex', // needed to align input and label centered
            alignItems: 'center', // same
            fontSize: 11, //
            fontWeight: 400,
          }),
          option: (base) => ({
            ...base,
            ':hover': {
              color: 'white',
              pointer: 'default !important',
            },
          }),
          menu: (base) => ({
            ...base,
            maxWidth: '250px',
          }),
          menuList: (base) => ({
            ...base,

            maxWidth: 260,
            backgroundColor: '#fff',
          }),
          indicatorSeparator: (base) => ({
            display: 'none',
          }),
          dropdownIndicator: (base) => ({
            ...base,
            padding: '8px 6px 8px 0',
          }),
          valueContainer: (base) => ({
            ...base,
            label: 'valueContainer',
            padding: '4px 0 4px 8px',
          }),
          multiValue: (base) => ({
            ...base,
            backgroundColor: props.controlStyles.borderColor,
            overflow: 'hidden',
            margin: '4px 0',
            width: '100%',
            borderRadius: UtopiaTheme.inputBorderRadius,
          }),
          multiValueRemove: (base) => ({
            ...base,
            backgroundImage: `linear-gradient(to left, ${
              props.controlStyles.borderColor
            } 75%, ${Chroma(props.controlStyles.borderColor)
              .alpha(0)
              .css()})`,
            width: 36,
            height: 32,
            paddingLeft: 12,
            justifyContent: 'center',
            position: 'absolute',
            top: 0,
            right: 0,
            opacity: 0,
            transition: 'opacity 200ms',
            '.MultiValueContainer:hover &': {
              opacity: 1,
            },
            '&:hover': {
              backgroundColor: 'transparent',
            },
          }),
        }}
        options={props.options}
        onChange={props.onChange}
      />
    </div>
  )
}
