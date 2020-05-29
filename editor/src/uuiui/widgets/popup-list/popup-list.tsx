/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'
import useInterval from '@use-it/interval'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import Select, {
  components,
  createFilter,
  MenuListComponentProps,
  OptionProps,
  OptionsType,
  SingleValueProps,
  ValueType,
} from 'react-select'
import { MenuPortalProps } from 'react-select/src/components/Menu'
import { styleFn } from 'react-select/src/styles'
import {
  betterReactMemo,
  CommonUtils,
  ControlStyles,
  getControlStyles,
  SelectOption,
} from 'uuiui-deps'
import { Icn, IcnProps, IcnSpacer } from '../../icn'
import { colorTheme, UtopiaTheme } from '../../styles/theme'
import { FlexRow } from '../layout/flex-row'

type ContainerMode = 'default' | 'showBorderOnHover' | 'noBorder'

interface PopupListProps {
  id?: string
  options: OptionsType<SelectOption>
  value: SelectOption | undefined
  onSubmitValue: (option: SelectOption) => void
  style?: React.CSSProperties
  containerMode?: ContainerMode
  controlStyles?: ControlStyles
  disabled?: boolean
  icon?: IcnProps
}

const WindowEdgePadding = 4
const OptionHeight = UtopiaTheme.layout.inputHeight.default
const CheckboxPadding = 4
const CheckboxWidth = 16
const CheckboxInset = CheckboxPadding + CheckboxWidth
const ValueContainerLeftPadding = 8

const getValueOfValueType = (value: ValueType<SelectOption>): SelectOption['value'] => {
  if (Array.isArray(value)) {
    if (value.length > 0) {
      return value[0].value
    } else {
      return undefined
    }
  } else {
    return ((value as unknown) as SelectOption).value
  }
}

const getIndexOfValue = (
  value: ValueType<SelectOption>,
  options: OptionsType<SelectOption>,
): number => {
  const firstValue = getValueOfValueType(value)

  let allOptions: SelectOption[] = []
  options.forEach((option) => {
    allOptions.push(option)
    if (option.options != null) {
      allOptions.push(...option.options)
    }
  })

  const index = allOptions.findIndex((option) => option.value === firstValue)
  return Math.max(0, index)
}

const Option = (props: OptionProps<SelectOption>) => {
  const selectOption = props.selectOption
  const data: SelectOption = props.data

  const onMouseUp = React.useCallback(() => {
    selectOption(data)
  }, [data, selectOption])

  return (
    <FlexRow {...props.innerProps} onMouseUp={onMouseUp} style={props.getStyles('option', props)}>
      <FlexRow style={{ width: CheckboxWidth, padding: CheckboxPadding, flexShrink: 0 }}>
        {props.isSelected ? '✓' : ''}
      </FlexRow>
      {props.data.icon == null ? <IcnSpacer /> : <Icn {...props.data.icon} />}
      {props.children}
    </FlexRow>
  )
}

const calculateMenuScrollPosition = (index: number, menuHeight: number) => {
  return index * OptionHeight - menuHeight / 2 + OptionHeight / 2
}

const calculateOptionsToCutOff = (
  optionsLength: number,
  windowHeightAboveOrBelowReference: number,
  bottomOrTop: 'bottom' | 'top',
  index: number = 0,
) => {
  return Math.min(
    optionsLength,
    Math.max(
      0,
      Math.round(optionsLength - index - windowHeightAboveOrBelowReference / OptionHeight),
    ),
  )
}

const getPortalPosition = (
  referenceTop: number,
  options: OptionsType<SelectOption>,
  value: ValueType<SelectOption>,
  scrollIndexOffset: number,
): {
  menuTop: number
  menuHeight: number
  scrollTop: number
  croppedTop: boolean
  croppedBottom: boolean
} => {
  const optionsLength = options.reduce((working, o) => {
    if (o.options == null) {
      return working + 1
    } else {
      return working + 1 + o.options.length
    }
  }, 0)
  const windowHeight = window.innerHeight
  const indexOfValue = getIndexOfValue(value, options)
  const centredIndex = indexOfValue + scrollIndexOffset

  const windowHeightAboveReference = referenceTop - WindowEdgePadding
  const windowHeightBelowReference =
    windowHeight - (windowHeightAboveReference + OptionHeight) - WindowEdgePadding

  const optionPaddingElements = 1
  const optionPaddingAboveSelected = OptionHeight * Math.min(optionPaddingElements, optionsLength)
  const optionPaddingBelowSelected =
    OptionHeight * Math.min(optionPaddingElements, optionsLength - centredIndex)

  if (
    windowHeightAboveReference > optionPaddingAboveSelected &&
    windowHeightBelowReference > optionPaddingBelowSelected
  ) {
    const numberCroppedTop = calculateOptionsToCutOff(
      optionsLength,
      windowHeightAboveReference,
      'top',
      optionsLength - centredIndex - 1,
    )
    const howManyElementsToShowAboveSelected = centredIndex - numberCroppedTop
    const numberCroppedBottom = calculateOptionsToCutOff(
      optionsLength,
      windowHeightBelowReference,
      'bottom',
      centredIndex,
    )
    const howManyElementsToShowBelowSelected =
      optionsLength - 1 - centredIndex - numberCroppedBottom
    const croppedMenuHeight =
      (howManyElementsToShowAboveSelected + howManyElementsToShowBelowSelected + 1) * OptionHeight
    return {
      menuTop: referenceTop - howManyElementsToShowAboveSelected * OptionHeight,
      menuHeight: croppedMenuHeight,
      scrollTop: numberCroppedTop * OptionHeight,
      croppedTop: numberCroppedTop > 0,
      croppedBottom: numberCroppedBottom > 0,
    }
  } else {
    if (windowHeightAboveReference > windowHeightBelowReference) {
      const numberCroppedTop = calculateOptionsToCutOff(
        optionsLength,
        windowHeightAboveReference,
        'top',
      )
      const numberCroppedBottom = calculateOptionsToCutOff(
        optionsLength,
        windowHeightBelowReference,
        'bottom',
      )
      const menuHeight = Math.min(
        optionsLength * OptionHeight,
        windowHeightAboveReference - numberCroppedTop * OptionHeight,
      )
      return {
        menuTop: referenceTop - menuHeight,
        menuHeight: menuHeight,
        scrollTop: 0,
        croppedTop: numberCroppedTop > 0,
        croppedBottom: numberCroppedBottom > 0,
      }
    } else {
      const numberCroppedTop = calculateOptionsToCutOff(
        optionsLength,
        windowHeightAboveReference,
        'top',
      )
      const numberCroppedBottom = calculateOptionsToCutOff(
        optionsLength,
        windowHeightBelowReference,
        'bottom',
      )
      const menuHeight = Math.min(
        optionsLength * OptionHeight,
        windowHeightBelowReference - numberCroppedBottom * OptionHeight,
      )
      return {
        menuTop: referenceTop + OptionHeight,
        menuHeight: menuHeight,
        scrollTop: 0,
        croppedTop: numberCroppedTop > 0,
        croppedBottom: numberCroppedBottom > 0,
      }
    }
  }
}

const MenuPortal = (props: MenuPortalProps<SelectOption>) => {
  const ref = React.useRef<HTMLDivElement>(null)
  const [popupHeight, setPopupHeight] = React.useState(0)
  const [popupTop, setPopupTop] = React.useState(0)
  const [popupLeft, setPopupLeft] = React.useState(0)
  const [deltaSinceMouseDown, setDeltaSinceMouseDown] = React.useState(0)
  const [croppedTop, setCroppedTop] = React.useState(false)
  const [croppedBottom, setCroppedBottom] = React.useState(false)
  const [scrollIndexOffset, setScrollIndexOffset] = React.useState(0)
  const [mouseInCropTopArea, setMouseInCropTopArea] = React.useState(false)
  const [mouseInCropBottomArea, setMouseInCropBottomArea] = React.useState(false)

  useInterval(
    () => {
      setScrollIndexOffset((value) => {
        return value - 1
      })
    },
    mouseInCropTopArea && croppedTop ? 50 : null,
  )

  useInterval(
    () => {
      setScrollIndexOffset((value) => {
        return value + 1
      })
    },
    mouseInCropBottomArea && croppedBottom ? 50 : null,
  )

  const onCroppedTopMouseOver = React.useCallback(() => {
    setMouseInCropTopArea(true)
  }, [])

  const onCroppedTopMouseOut = React.useCallback(() => {
    setMouseInCropTopArea(false)
  }, [])

  const onCroppedBottomMouseOver = React.useCallback(() => {
    setMouseInCropBottomArea(true)
  }, [])

  const onCroppedBottomMouseOut = React.useCallback(() => {
    setMouseInCropBottomArea(false)
  }, [])

  const onMouseMove = React.useCallback(() => {
    setDeltaSinceMouseDown((value) => value + 1)
  }, [setDeltaSinceMouseDown])

  const onMouseUp = React.useCallback(
    (e: React.MouseEvent) => {
      if (deltaSinceMouseDown < 3) {
        e.stopPropagation()
        e.nativeEvent.stopImmediatePropagation()
      }
    },
    [deltaSinceMouseDown],
  )

  const propsOptions = props.options
  const propsGetValue = props.getValue
  const refCurrent = ref.current
  const referenceElement = props.controlElement
  const updateLayout = React.useCallback(() => {
    if (referenceElement != null) {
      const referenceRect = referenceElement.getBoundingClientRect()
      const {
        menuTop,
        menuHeight,
        scrollTop,
        croppedTop: isCroppedTop,
        croppedBottom: isCroppedBottom,
      } = getPortalPosition(referenceRect.top, propsOptions, propsGetValue(), scrollIndexOffset)
      if (refCurrent != null) {
        refCurrent.scrollTo({
          top: scrollTop,
        })
      }
      setPopupHeight(menuHeight)
      setPopupTop(menuTop)
      setPopupLeft(referenceRect.left)
      setCroppedTop(isCroppedTop)
      setCroppedBottom(isCroppedBottom)
    }
  }, [propsOptions, propsGetValue, refCurrent, referenceElement, scrollIndexOffset])

  React.useLayoutEffect(updateLayout, [updateLayout])

  React.useEffect(() => {
    window.addEventListener('resize', updateLayout)
    return () => {
      window.removeEventListener('resize', updateLayout)
    }
  }, [updateLayout])

  if (props.selectProps.menuPortalTarget != null) {
    return ReactDOM.createPortal(
      <div
        className='ignore-react-onclickoutside'
        onMouseMove={onMouseMove}
        onMouseUpCapture={onMouseUp}
        id='menuPortal'
        style={{
          backgroundColor: colorTheme.neutralBackground.value,
          minWidth: 150,
          maxWidth: 250,
          boxShadow: 'rgba(0, 0, 0, 0.1) 0px 0px 0px 1px, rgba(0, 0, 0, 0.1) 0px 4px 11px',
          zIndex: 1,
          boxSizing: 'border-box',
          borderRadius: UtopiaTheme.inputBorderRadius,
          position: 'absolute',
          height: popupHeight,
          top: popupTop,
          left: popupLeft - CheckboxInset + ValueContainerLeftPadding,
          overflow: 'hidden',
        }}
      >
        <div
          ref={ref}
          style={{
            minWidth: 150,
            maxWidth: 250,
            height: popupHeight,
            overflow: 'hidden',
          }}
        >
          {props.children}
        </div>
        {croppedTop ? (
          <OverflowIndicator
            style={{
              top: 0,
            }}
            onMouseOver={onCroppedTopMouseOver}
            onMouseOut={onCroppedTopMouseOut}
          >
            …
          </OverflowIndicator>
        ) : null}
        {croppedBottom ? (
          <OverflowIndicator
            style={{
              bottom: 0,
            }}
            onMouseOver={onCroppedBottomMouseOver}
            onMouseOut={onCroppedBottomMouseOut}
          >
            …
          </OverflowIndicator>
        ) : null}
      </div>,
      props.selectProps.menuPortalTarget,
    )
  } else {
    return null
  }
}

const MenuList = (props: MenuListComponentProps<SelectOption>) => {
  const ref = React.useRef<HTMLDivElement>(null)
  const refCurrent = ref.current
  const propsValue = props.getValue()
  React.useEffect(() => {
    if (refCurrent != null) {
      const index = getIndexOfValue(propsValue, [])
      refCurrent.scrollTo({
        top: calculateMenuScrollPosition(index, refCurrent.clientHeight),
      })
    }
  }, [refCurrent, propsValue])

  return <components.MenuList {...props} innerRef={ref} />
}

const DropdownIndicator = () => {
  return <Icn category='controls/dropdown' type='carats' color='gray' width={11} height={11} />
}

const SingleValue = (props: SingleValueProps<SelectOption>) => {
  return (
    <components.SingleValue {...props}>
      {props.data.icon == null ? null : <Icn {...props.data.icon} />}
      {props.children}
    </components.SingleValue>
  )
}

const displayNone: styleFn = () => ({
  display: 'none',
})

const getDefaultContainer = (
  controlStyles: ControlStyles,
  propsStyle?: React.CSSProperties,
): styleFn => () => ({
  width: '100%',
  height: OptionHeight,
  borderRadius: UtopiaTheme.inputBorderRadius,
  boxShadow: `inset 0 0 0 1px ${controlStyles.borderColor}`,
  backgroundColor: controlStyles.backgroundColor,
  color: controlStyles.mainColor,
  ...propsStyle,
})

const getShowBorderOnHoverContainer = (
  controlStyles: ControlStyles,
  propsStyle?: React.CSSProperties,
): styleFn => () => {
  return ({
    width: '100%',
    height: OptionHeight,
    borderRadius: UtopiaTheme.inputBorderRadius,
    color: controlStyles.mainColor,
    '&:hover': {
      boxShadow: `inset 0 0 0 1px ${controlStyles.borderColor}`,
      backgroundColor: controlStyles.backgroundColor,
    },
    ...propsStyle,
  } as unknown) as React.CSSProperties // incorrect react-select type. it actually accepts an emotion style object
}

const getNoBorderContainer = (
  controlStyles: ControlStyles,
  propsStyle?: React.CSSProperties,
): styleFn => () => ({
  width: '100%',
  height: OptionHeight,
  color: controlStyles.mainColor,
  ...propsStyle,
})

const getContainer = (
  containerMode: ContainerMode,
  controlStyles: ControlStyles,
  style: React.CSSProperties | undefined,
): styleFn => {
  switch (containerMode) {
    case 'default': {
      return getDefaultContainer(controlStyles, style)
    }
    case 'showBorderOnHover': {
      return getShowBorderOnHoverContainer(controlStyles, style)
    }
    case 'noBorder': {
      return getNoBorderContainer(controlStyles, style)
    }
  }
}

export const PopupList = betterReactMemo<PopupListProps>(
  'PopupList',
  ({
    id,
    options,
    value,
    onSubmitValue,
    style,
    containerMode = 'default',
    controlStyles = getControlStyles('simple'),
    disabled = !controlStyles.interactive,
  }) => {
    const selectOnSubmitValue = React.useCallback(
      (newValue: ValueType<SelectOption>) => {
        if (newValue != null && !Array.isArray(newValue)) {
          onSubmitValue(newValue as SelectOption)
        }
      },
      [onSubmitValue],
    )

    const container: styleFn = getContainer(containerMode, controlStyles, style)

    return (
      <Select
        id={id}
        components={{
          Option,
          MenuList,
          MenuPortal,
          DropdownIndicator,
          SingleValue,
        }}
        value={value}
        onChange={selectOnSubmitValue}
        options={options}
        menuPortalTarget={document.getElementById(CommonUtils.PortalTargetID)}
        filterOption={createFilter({ ignoreAccents: true })}
        isDisabled={disabled}
        styles={{
          container,
          indicatorsContainer: (base) => ({
            ...base,
            width: 11,
            height: OptionHeight,
            padding: 0,
            marginRight: 6,
            alignItems: 'center',
            justifyContent: 'center',
            flexShrink: 0,
          }),
          control: () => ({
            minHeight: OptionHeight,
            height: OptionHeight,
            backgroundColor: 'transparent',
            alignItems: 'center',
            cursor: 'default',
            display: 'flex',
            label: 'control',
            outline: '0 !important',
            position: 'relative',
            transition: 'all 100ms',
          }),
          singleValue: () => ({
            label: 'singleValue',
            color: controlStyles.mainColor,
            display: 'flex',
            alignItems: 'center',
          }),
          menu: () => ({
            label: 'menu',
            boxSizing: 'border-box',
            height: '100%',
            width: '100%',
          }),
          menuList: (_, menuListProps) => {
            return {
              backgroundColor: controlStyles.backgroundColor,
              padding: 0,
              boxSizing: 'border-box',
              label: 'menuList',
              height: menuListProps.children.length * OptionHeight,
            }
          },
          option: (_, optionProps) => ({
            paddingBottom: 0,
            paddingRight: '24px',
            paddingLeft: '4px',
            fontWeight: 500,
            userSelect: 'none',
            fontSize: 11,
            backgroundColor: optionProps.isFocused
              ? colorTheme.contextMenuHighlightBackground.value
              : 'transparent',
            color: optionProps.isFocused
              ? colorTheme.contextMenuHighlightForeground.value
              : colorTheme.contextMenuForeground.value,
            height: OptionHeight,
          }),
          input: () => ({
            margin: 2,
            paddingBottom: 2,
            paddingTop: 2,
            visibility: 'visible',
            color: controlStyles.mainColor,
            boxSizing: 'border-box',
            position: 'absolute',
            top: 0,
            left: 0,
          }),
          valueContainer: () => ({
            label: 'valueContainer',
            height: OptionHeight,
            boxSizing: 'border-box',
            overflow: 'hidden',
            padding: `2px 2px 2px ${ValueContainerLeftPadding}px`,
            position: 'relative',
            borderRadius: UtopiaTheme.inputBorderRadius,
            display: 'flex',
            alignItems: 'center',
            flexGrow: containerMode === 'noBorder' ? 0 : 1,
          }),
          indicatorSeparator: displayNone,
          clearIndicator: displayNone,
          loadingIndicator: displayNone,
        }}
      />
    )
  },
)

const OverflowIndicator = styled(FlexRow)({
  position: 'absolute',
  left: 0,
  width: '100%',
  height: OptionHeight,
  backgroundColor: colorTheme.neutralBackground.value,
  color: colorTheme.emphasizedForeground.value,
  display: 'flex',
  justifyContent: 'center',
  zIndex: 1,
})
