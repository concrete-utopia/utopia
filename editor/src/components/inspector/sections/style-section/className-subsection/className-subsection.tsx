/** @jsx jsx */

import { jsx } from '@emotion/react'
import * as React from 'react'
import { FormatOptionLabelMeta, MenuProps, ValueType, components } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import { IndicatorContainerProps } from 'react-select/src/components/containers'
import { MultiValueRemoveProps } from 'react-select/src/components/MultiValue'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useInspectorElementInfo } from '../../../common/property-path-hooks'
import { styleFn } from 'react-select/src/styles'
import { CustomReactSelectInput, SelectOption } from '../../../controls/select-control'
import {
  UtopiaTheme,
  UNSAFE_getIconURL,
  InspectorSectionHeader,
  useColorTheme,
  FlexColumn,
  FlexRow,
} from '../../../../../uuiui'
import { ControlStyles, betterReactMemo, Utils } from '../../../../../uuiui-deps'
import {
  useFilteredOptions,
  TailWindOption,
  MatchHighlighter,
} from '../../../../../core/tailwind/tailwind-options'
import { useEditorState } from '../../../../editor/store/store-hook'
import * as EditorActions from '../../../../editor/actions/action-creators'
import * as PP from '../../../../../core/shared/property-path'
import { jsxAttributeValue } from '../../../../../core/shared/element-template'
import { emptyComments } from '../../../../../core/workers/parser-printer/parser-printer-comments'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
} from '../../../../../core/shared/atom-with-pub-sub'

const IndicatorsContainer: React.FunctionComponent<IndicatorContainerProps<TailWindOption>> = () =>
  null

const MultiValueRemove: React.FunctionComponent<MultiValueRemoveProps<TailWindOption>> = (
  props,
) => <div {...props.innerProps} />

interface ClassNameControlProps<T extends string> {
  controlStyles: ControlStyles
  values: ReadonlyArray<TailWindOption>
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
    (state.isFocused as boolean)
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

const menu: styleFn = (base) => ({
  ...base,
  position: 'relative',
})

const AlwaysTrue = () => true
let queuedDispatchTimeout: number | undefined = undefined

const focusedOptionAtom = atomWithPubSub<string | null>({
  key: 'classNameSubsectionFocusedOption',
  defaultValue: null,
})

function formatOptionLabel(
  { label }: TailWindOption,
  { context, inputValue }: FormatOptionLabelMeta<TailWindOption>,
) {
  return context === 'menu' ? <MatchHighlighter text={label} searchString={inputValue} /> : label
}

const Menu = betterReactMemo('Menu', (props: MenuProps<TailWindOption>) => {
  const theme = useColorTheme()
  const focusedOptionValue = usePubSubAtomReadOnly(focusedOptionAtom)
  const focusedOption =
    focusedOptionValue == null ? null : props.options.find((o) => o.value === focusedOptionValue)
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
              color: theme.textColor.value,
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

const ClassNameControl = betterReactMemo(
  'ClassNameControl',
  <T extends string>({
    controlStyles,
    values,
    onSubmitValue,
    onUnsetValues,
  }: ClassNameControlProps<T>) => {
    const targets = useEditorState(
      (store) => store.editor.selectedViews,
      'ClassNameSubsection targets',
    )
    const dispatch = useEditorState((store) => store.dispatch, 'ClassNameSubsection dispatch')

    const [filter, setFilter] = React.useState('')

    const options = useFilteredOptions(filter, 100)
    const isFocusedRef = React.useRef(false)
    const shouldPreviewOnFocusRef = React.useRef(false)
    const updateFocusedOption = usePubSubAtomWriteOnly(focusedOptionAtom)

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

    const onChange = React.useCallback(
      (newValues: ValueType<TailWindOption>) => {
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

          clearFocusedOption()
        } else if (newValues === null) {
          onUnsetValues()

          clearFocusedOption()
        }
      },
      [clearFocusedOption, onSubmitValue, onUnsetValues],
    )

    const onInputChange = React.useCallback(
      (newInput) => {
        if (newInput === '') {
          clearFocusedOption()
        }
        setFilter(newInput)
      },
      [clearFocusedOption, setFilter],
    )

    const handleKeyDown = React.useCallback((event: React.KeyboardEvent<HTMLElement>) => {
      shouldPreviewOnFocusRef.current = true
    }, [])

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
        minHeight: UtopiaTheme.layout.inputHeight.default,
        paddingTop: 2,
        paddingBottom: 2,
      }),
      [],
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
          (state.isFocused as boolean)
            ? UtopiaTheme.color.inspectorFocusedColor.value
            : controlStyles.borderColor
        }`,
        overflow: 'hidden',
      }),
      [controlStyles],
    )
    const option: styleFn = React.useCallback(
      (base, state) => {
        if (
          isFocusedRef.current &&
          shouldPreviewOnFocusRef.current &&
          state.isFocused &&
          targets.length === 1
        ) {
          const newClassNameString = values.map((v) => v.value).join(' ') + ' ' + state.value
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

          updateFocusedOption(state.value)
        }

        return base
      },
      [dispatch, targets, values, updateFocusedOption],
    )

    return (
      <CreatableSelect
        autoFocus={false}
        placeholder='Add class…'
        isMulti
        value={
          controlStyles.mixed
            ? [
                {
                  value: '',
                  label: 'mixed',
                },
              ]
            : values
        }
        isDisabled={!controlStyles.interactive}
        onChange={onChange}
        onInputChange={onInputChange}
        components={{
          IndicatorsContainer,
          MultiValueRemove,
          Input: CustomReactSelectInput,
          Menu,
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
          option,
        }}
        filterOption={AlwaysTrue}
        options={options}
        menuIsOpen={true}
        onBlur={onBlur}
        onFocus={onFocus}
        escapeClearsValue={true}
        formatOptionLabel={formatOptionLabel}
        onKeyDown={handleKeyDown}
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
  const values: ReadonlyArray<TailWindOption> =
    value === '' ? [] : value.split(' ').map((v) => ({ value: v, label: `.${v}` }))

  return (
    <React.Fragment>
      <InspectorSectionHeader>Class names</InspectorSectionHeader>
      <UIGridRow padded variant='<-------------1fr------------->'>
        <ClassNameControl
          values={values}
          controlStyles={controlStyles}
          onSubmitValue={onSubmitValue}
          onUnsetValues={onUnsetValues}
        />
      </UIGridRow>
    </React.Fragment>
  )
})
