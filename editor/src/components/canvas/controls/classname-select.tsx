/** @jsx jsx */

import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import React from 'react'
import type { StylesConfig } from 'react-select'
import WindowedSelect, {
  components,
  FormatOptionLabelMeta,
  IndicatorProps,
  InputActionMeta,
  InputProps,
  MenuProps,
  MultiValueProps,
  ValueContainerProps,
} from 'react-windowed-select'
import { findElementAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../core/shared/atom-with-pub-sub'
import { eitherToMaybe } from '../../../core/shared/either'
import {
  isJSXAttributeNotFound,
  isJSXAttributeValue,
  isJSXElement,
  jsxAttributeValue,
  JSXElementChild,
} from '../../../core/shared/element-template'
import { getModifiableJSXAttributeAtPath } from '../../../core/shared/jsx-attributes'
import { isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import {
  MatchHighlighter,
  TailWindOption,
  useFilteredOptions,
  useGetSelectedTailwindOptions,
} from '../../../core/tailwind/tailwind-options'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { colorTheme, FlexColumn, FlexRow, useColorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { getContentsTreeFileFromString } from '../../assets'
import { normalisePathToUnderlyingTarget } from '../../custom-code/code-file'
import * as EditorActions from '../../editor/actions/action-creators'
import { getOpenUIJSFileKey } from '../../editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'

const DropdownIndicator = betterReactMemo(
  'DropdownIndicator',
  (props: IndicatorProps<TailWindOption, true>) => (
    <components.DropdownIndicator {...props}>
      <span style={{ lineHeight: '20px', opacity: props.isDisabled ? 0 : 1 }}> ↓ </span>
    </components.DropdownIndicator>
  ),
)

const ClearIndicator = () => null
const IndicatorSeparator = () => null

const NoOptionsMessage = betterReactMemo('NoOptionsMessage', (props: any) => (
  <span {...props}>No results found</span>
))

const AngledStripe = styled.div({
  width: 5,
  height: 22,
  borderRadius: 0,
  transform: 'skewX(-11deg)',
})

const getColorForCategory = (category: string) => {
  if (category === 'layout') {
    return '#5FACFF'
  } else if (category === 'typography') {
    return '#F7B500'
  } else if (category === 'interaction') {
    return '#B620E0'
  } else return 'pink'
}

const getOptionColors = (
  theme: typeof colorTheme,
  isFocused: boolean,
  isSelected: boolean,
  isDisabled: boolean,
  data: any,
) => {
  let color: string | undefined = theme.inverted.textColor.value
  let selectedColor = theme.inverted.primary.value
  let backgroundColor: string | undefined = theme.inverted.bg1.value
  let activeBackgroundColor: string | undefined = theme.primary.value
  if (isFocused) {
    backgroundColor = theme.inverted.primary.value
  } else if (isSelected) {
    backgroundColor = selectedColor
    activeBackgroundColor = selectedColor
  } else if (isDisabled) {
    backgroundColor = undefined
    activeBackgroundColor = undefined
    color = undefined
  }

  return {
    color: color,
    backgroundColor: backgroundColor,
    activeBackgroundColor: activeBackgroundColor,
  }
}

const focusedOptionAtom = atomWithPubSub<TailWindOption | null>({
  key: 'classNameSelectFocusedOption',
  defaultValue: null,
})

function formatOptionLabel(
  { label }: TailWindOption,
  { context, inputValue }: FormatOptionLabelMeta<TailWindOption, true>,
) {
  return context === 'menu' ? <MatchHighlighter text={label} searchString={inputValue} /> : label
}

const Menu = betterReactMemo('Menu', (props: MenuProps<TailWindOption, true>) => {
  const theme = useColorTheme()
  const focusedOption = usePubSubAtomReadOnly(focusedOptionAtom)
  const showFooter = props.options.length > 0
  const joinedAttributes = focusedOption?.attributes?.join(', ')
  const attributesText =
    joinedAttributes == null || joinedAttributes === '' ? '\u00a0' : `Sets: ${joinedAttributes}`

  return (
    <components.Menu {...props}>
      <React.Fragment>
        {props.children}
        {showFooter ? (
          <div
            css={{
              label: 'focusedElementMetadata',
              overflow: 'hidden',
              boxShadow: 'inset 0px 1px 0px 0px rgba(0,0,0,.1)',
              padding: '8px 8px',
              fontSize: '10px',
              pointerEvents: 'none',
              color: theme.inverted.textColor.value,
            }}
          >
            <FlexColumn>
              <FlexRow>
                <span>
                  <MatchHighlighter
                    text={attributesText}
                    searchString={props.selectProps.inputValue}
                  />
                </span>
              </FlexRow>
            </FlexColumn>
          </div>
        ) : null}
      </React.Fragment>
    </components.Menu>
  )
})

const MultiValueContainer = betterReactMemo(
  'MultiValueContainer',
  (props: MultiValueProps<TailWindOption>) => {
    const theme = useColorTheme()
    const { data } = props
    const stripes: jsx.JSX.Element[] = React.useMemo(() => {
      const categories = data.categories ?? []
      if (categories.length > 0) {
        return categories.map((category, index) => (
          <AngledStripe
            key={data.label ?? index}
            style={{ backgroundColor: getColorForCategory(category) }}
          />
        ))
      } else {
        return []
      }
    }, [data.label, data.categories])

    return (
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          backgroundColor: theme.inverted.bg1.value,
        }}
      >
        <components.MultiValueContainer {...props} />
        <div
          style={{
            display: 'flex',
            height: 16,
            paddingRight: 4,
            paddingLeft: 2,
          }}
        >
          {stripes}
        </div>
      </div>
    )
  },
)

const ValueContainer = betterReactMemo(
  'ValueContainer',
  (props: ValueContainerProps<TailWindOption, true>) => {
    return (
      <div style={{ overflowX: 'scroll', flex: 1 }}>
        <components.ValueContainer {...props} />
      </div>
    )
  },
)

const filterOption = () => true
const MaxResults = 500

const Input = (props: InputProps) => {
  const value = (props as any).value
  const isHidden = value.length !== 0 ? false : props.isHidden
  return <components.Input {...props} isHidden={isHidden} />
}
let queuedDispatchTimeout: number | undefined = undefined

export const ClassNameSelect = betterReactMemo(
  'ClassNameSelect',
  React.forwardRef<HTMLInputElement>((_, ref) => {
    const theme = useColorTheme()
    const targets = useEditorState((store) => store.editor.selectedViews, 'ClassNameSelect targets')
    const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSelect dispatch')
    const [input, setInput] = React.useState('')
    const focusedValueRef = React.useRef<string | null>(null)
    const updateFocusedOption = usePubSubAtomWriteOnly(focusedOptionAtom)
    const clearFocusedOption = React.useCallback(() => {
      updateFocusedOption(null)
      dispatch([EditorActions.clearTransientProps()], 'canvas')
    }, [updateFocusedOption, dispatch])

    const isMenuOpenRef = React.useRef(false)
    const onMenuClose = React.useCallback(() => {
      isMenuOpenRef.current = false
      clearFocusedOption()
    }, [clearFocusedOption])
    const onMenuOpen = React.useCallback(() => {
      isMenuOpenRef.current = true
    }, [])

    const filteredOptions = useFilteredOptions(input, MaxResults, clearFocusedOption)

    React.useEffect(() => {
      return function cleanup() {
        dispatch([EditorActions.clearTransientProps()], 'canvas')
      }
      /** deps is explicitly empty */
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    const {
      selectedOptions: selectedValues,
      elementPaths,
      isSettable,
    } = useGetSelectedTailwindOptions()
    const elementPath = elementPaths[0]
    const isMenuEnabled = isSettable && elementPaths.length === 1

    const ariaOnFocus = React.useCallback(
      ({ focused, context }: { focused: TailWindOption; context: 'menu' | 'value' }) => {
        if (context === 'menu') {
          if (isMenuOpenRef.current) {
            if (targets.length === 1) {
              const newClassNameString =
                selectedValues?.map((v) => v.label).join(' ') + ' ' + focused.label
              if (queuedDispatchTimeout != null) {
                window.clearTimeout(queuedDispatchTimeout)
              }
              queuedDispatchTimeout = window.setTimeout(() => {
                dispatch(
                  [
                    EditorActions.setPropTransient(
                      targets[0],
                      PP.create(['className']),
                      jsxAttributeValue(newClassNameString, emptyComments),
                    ),
                  ],
                  'canvas',
                )
              }, 10)
            }
            updateFocusedOption(focused)
          }
        } else if (context === 'value') {
          focusedValueRef.current = focused.value
        }
      },
      [updateFocusedOption, dispatch, targets, selectedValues],
    )
    const ariaLiveMessages = React.useMemo(() => ({ onFocus: ariaOnFocus }), [ariaOnFocus])

    const onChange = React.useCallback(
      (newValue: Array<{ label: string; value: string }>) => {
        if (elementPath != null) {
          if (queuedDispatchTimeout != null) {
            window.clearTimeout(queuedDispatchTimeout)
            queuedDispatchTimeout = undefined
          }

          dispatch(
            [
              EditorActions.setProp_UNSAFE(
                elementPath,
                PP.create(['className']),
                jsxAttributeValue(newValue.map((value) => value.value).join(' '), emptyComments),
              ),
              EditorActions.clearTransientProps(),
            ],
            'everyone',
          )
        }
      },
      [dispatch, elementPath],
    )

    const colourStyles: StylesConfig = React.useMemo(
      () => ({
        container: (styles: React.CSSProperties) => ({
          // the outermost element. It contains the popup menu, so don't set a height on it!
          // shouldn't contain any sizing
          ...styles,
          width: '100%',
        }),
        control: (styles) => ({
          // need to remove styles here, since that implicitly sets a height of 38
          // ...styles,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'space-between',
          background: 'transparent',
          outline: 'none',
          ':focus-within': {
            outline: 'none',
            border: 'none',
          },
        }),
        valueContainer: () => ({
          // the container for added options (tags) and input
          // sibling to indicatorsContainer
          // default styles mess with layout, so ignore them
          display: 'flex',
          alignItems: 'center',
          gap: 4,
          maxWidth: 0,
        }),
        multiValue: (style, state) => {
          return {
            cursor: 'pointer',
            display: 'flex',
            alignItems: 'center',
            height: 18,
            backgroundColor: state.isFocused
              ? theme.inverted.primary.value
              : theme.inverted.bg1.value,
          }
        },
        multiValueLabel: () => ({
          fontSize: 10,
          padding: '2px 4px',
          color: theme.inverted.textColor.value,
        }),
        multiValueRemove: (styles: React.CSSProperties, { data }) => ({
          width: 11,
          display: 'flex',
          paddingTop: 2,
          opacity: 0.4,
          color: data.color,
          ':hover': {
            opacity: 1,
            backgroundColor: data.color,
            color: theme.inverted.textColor.value,
          },
          '& > svg': {
            overflow: 'hidden',
          },
        }),
        input: () => {
          return {
            fontSize: 11,
            color: theme.inverted.textColor.value,
            letterSpacing: 0.3,
            background: 'transparent',
            display: 'flex',
            alignItems: 'center',
          }
        },
        indicatorsContainer: (styles) => ({
          ...styles,
          height: 20,
        }),
        menu: (styles) => ({
          ...styles,
          backgroundColor: theme.inverted.bg1.value,
        }),
        option: (styles: React.CSSProperties, { data, isDisabled, isFocused, isSelected }) => {
          // a single entry in the options list
          const optionColors = getOptionColors(theme, isFocused, isSelected, isDisabled, data)
          return {
            minHeight: 27,
            display: 'flex',
            alignItems: 'center',
            paddingLeft: 8,
            paddingRight: 8,
            backgroundColor: optionColors.backgroundColor,
            color: optionColors.color,
            cursor: isDisabled ? 'not-allowed' : 'default',

            ':active': {
              ...(styles as any)[':active'],
              backgroundColor: optionColors.activeBackgroundColor,
            },
          }
        },
      }),
      [theme],
    )

    const onInputChange = React.useCallback(
      (newInput, actionMeta: InputActionMeta) => {
        if (newInput === '') {
          dispatch([EditorActions.clearTransientProps()], 'canvas')
        }
        setInput(newInput)
        focusedValueRef.current = null
      },
      [dispatch, setInput],
    )

    const handleKeyDown = React.useCallback(
      (event: React.KeyboardEvent<HTMLDivElement>) => {
        if (event.key === 'Backspace') {
          if (focusedValueRef.current != null) {
            setInput(focusedValueRef.current)
            focusedValueRef.current = null
            if (ref != null) {
              ;(ref as any).current.focus()
            }
          }
        }
      },
      [setInput, ref],
    )

    return (
      <div
        css={{
          height: 22,
          borderRadius: 3,
          position: 'relative',
          padding: 4,
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
          '&:focus-within': { boxShadow: `0px 0px 0px 1px ${theme.primary.value}` },
        }}
        onKeyDown={handleKeyDown}
      >
        <WindowedSelect
          ref={ref}
          ariaLiveMessages={ariaLiveMessages}
          filterOption={filterOption}
          formatOptionLabel={formatOptionLabel}
          options={filteredOptions}
          onChange={onChange}
          onInputChange={onInputChange}
          inputValue={input}
          onMenuClose={onMenuClose}
          onMenuOpen={onMenuOpen}
          value={selectedValues}
          isMulti={true}
          isDisabled={!isMenuEnabled}
          maxMenuHeight={138}
          styles={colourStyles}
          components={{
            DropdownIndicator,
            ClearIndicator,
            IndicatorSeparator,
            NoOptionsMessage,
            Menu,
            MultiValueContainer,
            ValueContainer,
            Input,
          }}
        />
      </div>
    )
  }),
)
