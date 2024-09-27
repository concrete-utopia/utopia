/** @jsxRuntime classic */
/** @jsx jsx */

import { jsx } from '@emotion/react'
import React from 'react'
import type { StylesConfig } from 'react-select'
import type {
  FormatOptionLabelMeta,
  IndicatorProps,
  InputActionMeta,
  InputProps,
  MenuProps,
  ValueContainerProps,
} from 'react-windowed-select'
import WindowedSelect, { components } from 'react-windowed-select'
import {
  AlwaysTrue,
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../core/shared/atom-with-pub-sub'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'
import * as PP from '../../../core/shared/property-path'
import type { TailWindOption } from '../../../core/tailwind/tailwind-options'
import {
  getTailwindOptionForClassName,
  LabelWithStripes,
  MatchHighlighter,
  useFilteredOptions,
  useGetSelectedClasses,
} from '../../../core/tailwind/tailwind-options'
import {
  AlternateColorThemeComponent,
  colorTheme,
  FlexColumn,
  FlexRow,
  UtopiaTheme,
} from '../../../uuiui'
import * as EditorActions from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'

const DropdownIndicator = React.memo((props: IndicatorProps<TailWindOption, true>) => (
  <components.DropdownIndicator {...props}>
    <span style={{ lineHeight: '20px', opacity: props.isDisabled ? 0 : 1 }}> ↓ </span>
  </components.DropdownIndicator>
))

const ClearIndicator = () => null
const IndicatorSeparator = () => null

const NoOptionsMessage = React.memo((props: any) => <span {...props}>No results found</span>)

const getOptionColors = (
  isFocused: boolean,
  isSelected: boolean,
  isDisabled: boolean,
  data: any,
) => {
  let color: string | undefined = colorTheme.bg0.value
  let selectedColor = colorTheme.primary.value
  let backgroundColor: string | undefined = colorTheme.fg1.value
  let activeBackgroundColor: string | undefined = colorTheme.primary.value
  if (isFocused) {
    backgroundColor = colorTheme.primary.value
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
  { label, categories }: TailWindOption,
  { context, inputValue }: FormatOptionLabelMeta<TailWindOption, true>,
) {
  return context === 'menu' ? (
    <MatchHighlighter text={label} searchString={inputValue} />
  ) : (
    <LabelWithStripes label={label} categories={categories ?? []} />
  )
}

const Menu = React.memo((props: MenuProps<TailWindOption, true>) => {
  const focusedOption = usePubSubAtomReadOnly(focusedOptionAtom, AlwaysTrue)
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
              boxShadow: `inset 0px 1px 0px 0px ${colorTheme.fg0Opacity10.value}`,
              padding: '8px 8px',
              fontSize: '10px',
              pointerEvents: 'none',
              color: colorTheme.bg0.value,
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

const ValueContainer = React.memo((props: ValueContainerProps<TailWindOption, true>) => {
  return (
    <div style={{ overflowX: 'scroll', flex: 1 }}>
      <components.ValueContainer {...props} />
    </div>
  )
})

const filterOption = () => true
const MaxResults = 500

const Input = (props: InputProps) => {
  const value = (props as any).value
  const isHidden = value.length !== 0 ? false : props.isHidden
  return <components.Input {...props} isHidden={isHidden} />
}
let queuedDispatchTimeout: number | undefined = undefined

export const ClassNameSelect = React.memo(
  React.forwardRef<HTMLInputElement>((_, ref) => {
    const targets = useEditorState(
      Substores.selectedViews,
      (store) => store.editor.selectedViews,
      'ClassNameSelect targets',
    )
    const dispatch = useDispatch()
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
        setTimeout(() => {
          // wrapping in a setTimeout so we don't dispatch from inside React lifecycle
          dispatch([EditorActions.clearTransientProps()], 'canvas')
        }, 0)
      }
      /** deps is explicitly empty */
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    const { selectedClasses, elementPaths, isSettable } = useGetSelectedClasses()
    const selectedValues = selectedClasses.map(getTailwindOptionForClassName)
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
                      PP.create('className'),
                      jsExpressionValue(newClassNameString, emptyComments),
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
                PP.create('className'),
                jsExpressionValue(newValue.map((value) => value.value).join(' '), emptyComments),
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
            border: `1px solid ${colorTheme.primary.value}`,
            borderRadius: UtopiaTheme.inputBorderRadius,
            backgroundColor: (state.isFocused as boolean)
              ? colorTheme.primary.value
              : colorTheme.fg1.value,
          }
        },
        multiValueLabel: () => ({
          fontSize: 10,
          padding: '2px 4px',
          color: colorTheme.textColor.value,
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
            color: colorTheme.bg0.value,
          },
          '& > svg': {
            overflow: 'hidden',
          },
        }),
        input: () => {
          return {
            fontSize: 11,
            color: colorTheme.bg0.value,
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
          backgroundColor: colorTheme.fg1.value,
          zIndex: 100,
        }),
        option: (styles: React.CSSProperties, { data, isDisabled, isFocused, isSelected }) => {
          // a single entry in the options list
          const optionColors = getOptionColors(isFocused, isSelected, isDisabled, data)
          return {
            minHeight: 27,
            display: 'flex',
            alignItems: 'center',
            paddingLeft: 8,
            paddingRight: 8,
            backgroundColor: optionColors.backgroundColor,
            color: optionColors.color,
            cursor: (isDisabled as boolean) ? 'not-allowed' : 'default',

            ':active': {
              ...(styles as any)[':active'],
              backgroundColor: optionColors.activeBackgroundColor,
            },
          }
        },
      }),
      [],
    )

    const onInputChange = React.useCallback(
      (newInput: string, actionMeta: InputActionMeta) => {
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
          '&:focus-within': { boxShadow: `0px 0px 0px 1px ${colorTheme.primary.value}` },
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
            ValueContainer,
            Input,
          }}
        />
      </div>
    )
  }),
)
