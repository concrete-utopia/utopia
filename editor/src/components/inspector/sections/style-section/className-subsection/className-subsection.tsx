/** @jsxRuntime classic */
/** @jsx jsx */

import { jsx } from '@emotion/react'
import React from 'react'
import type {
  FormatOptionLabelMeta,
  InputProps,
  OptionTypeBase,
  OptionsType,
  ValueType,
} from 'react-select'
import { components } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import type { IndicatorContainerProps } from 'react-select/src/components/containers'
import type { MultiValueRemoveProps } from 'react-select/src/components/MultiValue'
import type { styleFn } from 'react-select/src/styles'
import { last } from '../../../../../core/shared/array-utils'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../../../core/shared/atom-with-pub-sub'
import { emptyComments, jsExpressionValue } from '../../../../../core/shared/element-template'
import * as PP from '../../../../../core/shared/property-path'
import type { TailWindOption } from '../../../../../core/tailwind/tailwind-options'
import {
  getTailwindOptionForClassName,
  LabelWithStripes,
  MatchHighlighter,
  useFilteredOptions,
  useGetSelectedClasses,
} from '../../../../../core/tailwind/tailwind-options'
import { when } from '../../../../../utils/react-conditionals'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  InspectorSubsectionHeader,
  SquareButton,
  UNSAFE_getIconURL,
  useColorTheme,
  UtopiaTheme,
} from '../../../../../uuiui'
import * as EditorActions from '../../../../editor/actions/action-creators'
import { useInputFocusOnCountIncrease } from '../../../../editor/hook-utils'
import {
  applyShortcutConfigurationToDefaults,
  handleShortcuts,
  REDO_CHANGES_SHORTCUT,
  UNDO_CHANGES_SHORTCUT,
} from '../../../../editor/shortcut-definitions'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import { ExpandableIndicator } from '../../../../navigator/navigator-item/expandable-indicator'
import { UIGridRow } from '../../../widgets/ui-grid-row'

const IndicatorsContainer: React.FunctionComponent<
  React.PropsWithChildren<IndicatorContainerProps<TailWindOption>>
> = () => null

const MultiValueRemove: React.FunctionComponent<
  React.PropsWithChildren<MultiValueRemoveProps<TailWindOption>>
> = (props) => <div {...props.innerProps} />

const valueContainer: styleFn = (base) => ({
  ...base,
  padding: '2px 4px',
  height: '100%',
  width: '100%',
})

const container: styleFn = (base) => ({
  ...base,
  minHeight: UtopiaTheme.layout.inputHeight.default,
  paddingTop: 2,
  paddingBottom: 2,
})

const control: styleFn = () => ({
  label: 'control',
  alignItems: 'center',
  backgroundColor: colorTheme.bg5.value,
  color: colorTheme.fg2.value,
  boxSizing: 'border-box',
  cursor: 'default',
  display: 'flex',
  flexWrap: 'wrap',
  justifyContent: 'space-between',
  position: 'relative',
  transition: 'all 100ms',
  minHeight: UtopiaTheme.layout.inputHeight.default,
})

const placeholder: styleFn = (base) => ({
  ...base,
  paddingTop: 2,
  paddingBottom: 2,
  paddingLeft: 6,
  paddingRight: 6,
})

const menu: styleFn = (base) => ({
  ...base,
  position: 'relative',
  boxShadow: 'none',
  borderRadius: 0,
  background: 'transparent',
})

const AlwaysTrue = () => true
let queuedDispatchTimeout: number | undefined = undefined
let queuedFocusTimeout: number | undefined = undefined

const focusedOptionAtom = atomWithPubSub<string | null>({
  key: 'classNameSubsectionFocusedOption',
  defaultValue: null,
})

function formatOptionLabel(
  { label, categories }: TailWindOption,
  { context, inputValue }: FormatOptionLabelMeta<TailWindOption>,
) {
  return context === 'menu' ? (
    <MatchHighlighter text={label} searchString={inputValue} />
  ) : (
    <LabelWithStripes label={label} categories={categories ?? []} />
  )
}

function isOptionsType<T extends OptionTypeBase>(
  valueType: T | OptionsType<T>,
): valueType is OptionsType<T> {
  return Array.isArray(valueType)
}

function valueTypeAsArray<T extends OptionTypeBase>(valueType: ValueType<T>): ReadonlyArray<T> {
  if (valueType == null) {
    return []
  } else if (isOptionsType(valueType)) {
    return valueType
  } else {
    return [valueType]
  }
}

const FooterSection = React.memo((props: { filter: string; options: Array<TailWindOption> }) => {
  const theme = useColorTheme()
  const focusedOptionValue = usePubSubAtomReadOnly(focusedOptionAtom, AlwaysTrue)
  const focusedOption =
    focusedOptionValue == null ? null : props.options.find((o) => o.value === focusedOptionValue)
  const joinedAttributes = focusedOption?.attributes?.join(', ')
  const attributesText =
    joinedAttributes == null || joinedAttributes === '' ? '\u00a0' : `Sets: ${joinedAttributes}`

  return (
    <div
      css={{
        label: 'focusedElementMetadata',
        overflow: 'hidden',
        boxShadow: `inset 0px 1px 1px 0px ${theme.neutralInvertedBackground10.value}`,
        padding: '8px 8px',
        fontSize: '10px',
        pointerEvents: 'none',
        color: theme.textColor.value,
      }}
    >
      <FlexColumn>
        <FlexRow>
          <span>
            <MatchHighlighter text={attributesText} searchString={props.filter} />
          </span>
        </FlexRow>
      </FlexColumn>
    </div>
  )
})

const Input = (props: InputProps) => {
  const value = (props as any).value
  const isHidden = value.length !== 0 ? false : props.isHidden
  return <components.Input {...props} isHidden={isHidden} />
}

const ClassNameControl = React.memo(() => {
  const editorStoreRef = useRefEditorState((store) => store)
  const theme = useColorTheme()
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'ClassNameSubsection targets',
  )
  const dispatch = useDispatch()

  const [filter, setFilter] = React.useState('')
  const isFocusedRef = React.useRef(false)
  const shouldPreviewOnFocusRef = React.useRef(false)
  const updateFocusedOption = usePubSubAtomWriteOnly(focusedOptionAtom)
  const focusedValueRef = React.useRef<string | null>(null)

  const focusTriggerCount = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.inspector.classnameFocusCounter,
    'ClassNameSubsection classnameFocusCounter',
  )
  const inputRef = useInputFocusOnCountIncrease<CreatableSelect<TailWindOption>>(focusTriggerCount)

  const clearFocusedOption = React.useCallback(() => {
    shouldPreviewOnFocusRef.current = false
    updateFocusedOption(null)
    dispatch([EditorActions.clearTransientProps()], 'canvas')
  }, [updateFocusedOption, dispatch])

  const onBlur = React.useCallback(() => {
    isFocusedRef.current = false
    shouldPreviewOnFocusRef.current = false
    clearFocusedOption()
  }, [clearFocusedOption])

  const onFocus = React.useCallback(() => {
    isFocusedRef.current = true
  }, [])

  const options = useFilteredOptions()

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
  const selectedOptions = selectedClasses.map(getTailwindOptionForClassName)
  const elementPath = elementPaths[0]
  const isMenuEnabled = isSettable && elementPaths.length === 1
  const selectedOptionsLength = selectedOptions?.length ?? 0
  const [isExpanded, setIsExpanded] = React.useState(selectedOptionsLength > 0)

  const expandSection = React.useCallback(() => {
    setIsExpanded(true)
    queuedFocusTimeout = window.setTimeout(() => inputRef.current?.focus(), 0)
  }, [inputRef])
  const contractSection = React.useCallback(() => {
    setIsExpanded(false)
    if (queuedFocusTimeout != null) {
      window.clearTimeout(queuedFocusTimeout)
    }
  }, [])

  const toggleIsExpanded = React.useCallback(() => {
    if (isExpanded) {
      contractSection()
    } else {
      expandSection()
    }
  }, [isExpanded, expandSection, contractSection])

  const triggerCountRef = React.useRef(focusTriggerCount)

  React.useEffect(() => {
    if (!isExpanded && focusTriggerCount > triggerCountRef.current) {
      triggerCountRef.current = focusTriggerCount
      expandSection()
    }
  }, [focusTriggerCount, isExpanded, expandSection])

  const onChange = React.useCallback(
    (newValueType: ValueType<TailWindOption>) => {
      // As the value of the dropdown is changing, hide the selection
      // controls so they can see the results of what they're doing.
      EditorActions.hideAndShowSelectionControls(dispatch)

      const newValue = valueTypeAsArray(newValueType)
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

  const onInputChange = React.useCallback(
    (newInput: string) => {
      if (newInput === '') {
        clearFocusedOption()
      }
      focusedValueRef.current = null
      setFilter(newInput)
    },
    [clearFocusedOption, setFilter],
  )

  const handleKeyDown = React.useCallback(
    (event: React.KeyboardEvent<HTMLElement>) => {
      // As someone is typing, hide the selection
      // controls so they can see the results of what they're doing.
      EditorActions.hideAndShowSelectionControls(dispatch)

      const shouldStopPreviewing =
        filter === '' && (event.key === 'ArrowLeft' || event.key === 'ArrowRight')

      if (shouldStopPreviewing) {
        clearFocusedOption()
      } else {
        shouldPreviewOnFocusRef.current = true
      }

      if (
        event.key === 'ArrowUp' ||
        event.key === 'ArrowDown' ||
        event.key === 'PageUp' ||
        event.key === 'PageDown' ||
        event.key === 'Home' ||
        event.key === 'End'
      ) {
        // Any of these keys will jump the focus to the menu
        focusedValueRef.current = null
      }

      if (
        filter === '' &&
        selectedOptions != null &&
        (event.key === 'Backspace' || event.key === 'Delete')
      ) {
        if (event.key === 'Delete' && focusedValueRef.current == null) {
          // prevent the default react-select behaviour here, as it will delete the last value
          // if nothing is focused, which feels wrong
          event.preventDefault()
        } else {
          const updatedFilterText = focusedValueRef.current ?? last(selectedOptions)?.label
          if (updatedFilterText != null) {
            setFilter(updatedFilterText)
          }
        }
      }

      if (event.key === 'Escape') {
        inputRef.current?.blur()
      }

      const namesByKey = applyShortcutConfigurationToDefaults(
        editorStoreRef.current.userState.shortcutConfig,
      )
      handleShortcuts(namesByKey, event.nativeEvent, null, {
        [UNDO_CHANGES_SHORTCUT]: () => {
          return dispatch([EditorActions.undo()])
        },
        [REDO_CHANGES_SHORTCUT]: () => {
          return dispatch([EditorActions.redo()])
        },
      })
    },
    [clearFocusedOption, dispatch, editorStoreRef, filter, inputRef, selectedOptions],
  )

  const multiValueLabel: styleFn = React.useCallback(
    (base, { isFocused }) => {
      const enabledColor = (isFocused as boolean) ? theme.bg0.value : theme.primary.value
      const color = isMenuEnabled ? enabledColor : theme.fg8.value
      const backgroundColor = (isFocused as boolean) ? theme.primary.value : theme.bg1.value
      return {
        ...base,
        label: 'multiValueLabel',
        display: 'flex',
        alignItems: 'center',
        paddingTop: 2,
        paddingBottom: 2,
        paddingLeft: 6,
        paddingRight: 0,
        fontSize: 9,
        borderRadius: 3,
        color: color,
        backgroundColor: backgroundColor,
      }
    },
    [isMenuEnabled, theme],
  )

  const multiValueRemove: styleFn = React.useCallback(
    (base, state) => ({
      label: 'multiValueRemove',
      width: isMenuEnabled ? 16 : 0,
      height: UtopiaTheme.layout.inputHeight.small,
      display: 'flex',
      alignItems: 'center',
      padding: 0,
      overflow: 'hidden',
      marginRight: 2,
      backgroundImage: `url(${
        (state.isFocused as boolean)
          ? UNSAFE_getIconURL('cross-in-translucent-circle', 'blue')
          : UNSAFE_getIconURL('cross-small')
      })`,
      backgroundSize: 16,
      backgroundPosition: 'center center',
      ':hover': {
        backgroundImage: `url(${UNSAFE_getIconURL('cross-in-translucent-circle', 'blue')})`,
      },
    }),
    [isMenuEnabled],
  )

  const multiValue: styleFn = React.useCallback(
    (base, { isFocused, data }) => {
      const backgroundColor = (isFocused as boolean) ? theme.primary.value : theme.bg1.value
      if (isFocused as boolean) {
        focusedValueRef.current = data.label
      }

      return {
        label: 'multiValue',
        fontWeight: 600,
        color: theme.emphasizedForeground.value,
        borderRadius: UtopiaTheme.inputBorderRadius,
        display: 'flex',
        marginRight: 4,
        marginTop: 2,
        marginBottom: 2,
        minWidth: 0,
        height: UtopiaTheme.layout.inputHeight.small,
        boxShadow: `inset 0 0 0 1px ${
          (isFocused as boolean) ? theme.inspectorFocusedColor.value : 'transparent'
        }`,
        overflow: 'hidden',
        backgroundColor: backgroundColor,
      }
    },
    [theme],
  )

  const option: styleFn = React.useCallback(
    (base, { isFocused, isDisabled, value }) => {
      if (
        isFocusedRef.current &&
        shouldPreviewOnFocusRef.current &&
        (isFocused as boolean) &&
        targets.length === 1
      ) {
        const oldClassNameString =
          selectedOptions == null ? '' : selectedOptions.map((v) => v.value).join(' ') + ' '
        const newClassNameString = oldClassNameString + value
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

        updateFocusedOption(value)
      }

      const color = (isFocused as boolean) ? theme.bg0.value : theme.textColor.value
      const backgroundColor = (isFocused as boolean) ? theme.primary.value : theme.bg1.value
      const borderRadius = (isFocused as boolean) ? 3 : 0

      return {
        minHeight: 27,
        display: 'flex',
        alignItems: 'center',
        paddingLeft: 8,
        paddingRight: 8,
        backgroundColor: backgroundColor,
        color: color,
        cursor: (isDisabled as boolean) ? 'not-allowed' : 'default',
        borderRadius: borderRadius,
      }

      // return base
    },
    [dispatch, targets, selectedOptions, updateFocusedOption, theme],
  )

  return (
    <div>
      <InspectorSubsectionHeader
        style={{
          color: theme.dynamicBlue.value,
          border: 'none',
        }}
      >
        <span style={{ flexGrow: 1, cursor: 'pointer' }} onClick={toggleIsExpanded}>
          CSS
        </span>
        <SquareButton highlight onClick={toggleIsExpanded}>
          <ExpandableIndicator visible collapsed={!isExpanded} selected={false} />
        </SquareButton>
      </InspectorSubsectionHeader>
      {when(
        isExpanded,
        <React.Fragment>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <CreatableSelect
              ref={inputRef}
              autoFocus={false}
              placeholder={isMenuEnabled ? 'Add class…' : ''}
              isMulti
              value={selectedOptions}
              isDisabled={!isMenuEnabled}
              onChange={onChange}
              onInputChange={onInputChange}
              components={{
                IndicatorsContainer,
                Input,
                MultiValueRemove,
              }}
              className='className-inspector-control'
              styles={{
                container,
                control,
                valueContainer,
                multiValue,
                multiValueLabel,
                multiValueRemove,
                placeholder,
                menu,
                option,
              }}
              filterOption={AlwaysTrue}
              options={options}
              menuIsOpen={isMenuEnabled}
              onBlur={onBlur}
              onFocus={onFocus}
              escapeClearsValue={true}
              formatOptionLabel={formatOptionLabel}
              onKeyDown={handleKeyDown}
              maxMenuHeight={199}
              inputValue={filter}
            />
          </UIGridRow>
          {when(isMenuEnabled, <FooterSection options={options} filter={filter} />)}
        </React.Fragment>,
      )}
    </div>
  )
})

export const ClassNameSubsection = React.memo(() => {
  return <ClassNameControl />
})
