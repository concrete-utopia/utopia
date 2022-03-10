/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../editor/actions/action-creators'
import { useColorTheme, SimpleFlexRow, UtopiaTheme, HeadlessStringInput } from '../../../uuiui'
import { useEditorDispatch, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
} from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import { ModeToggleButton } from './mode-toggle-button'
import { ClassNameSelect } from './classname-select'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import {
  applyShortcutConfigurationToDefaults,
  handleShortcuts,
  TOGGLE_FOCUSED_OMNIBOX_TAB,
} from '../../editor/shortcut-definitions'
import { useInputFocusOnCountIncrease } from '../../editor/hook-utils'
import { EditorStorePatched } from '../../editor/store/editor-state'
import { identity } from '../../../core/shared/utils'

interface FormulaBarProps {
  style: React.CSSProperties
}

const formulaBarModeSelector = (store: EditorStorePatched) => store.editor.topmenu.formulaBarMode
const selectedElementSelector = (store: EditorStorePatched) => {
  const metadata = store.editor.jsxMetadata
  if (store.editor.selectedViews.length === 1) {
    return MetadataUtils.findElementByElementPath(metadata, store.editor.selectedViews[0])
  } else {
    return null
  }
}
const formulaBarFocusCounterSelector = (store: EditorStorePatched) =>
  store.editor.topmenu.formulaBarFocusCounter

export const FormulaBar = React.memo<FormulaBarProps>((props) => {
  const saveTimerRef = React.useRef<any>(null)
  const dispatch = useEditorDispatch('FormulaBar dispatch')

  const selectedMode = useEditorState(formulaBarModeSelector, 'FormulaBar selectedMode')

  const selectedElement = useEditorState(selectedElementSelector, 'FormulaBar selectedElement')

  const focusTriggerCount = useEditorState(
    formulaBarFocusCounterSelector,
    'FormulaBar formulaBarFocusCounter',
  )

  const inputRef = useInputFocusOnCountIncrease<HTMLInputElement>(focusTriggerCount)

  const colorTheme = useColorTheme()
  const [simpleText, setSimpleText] = React.useState('')
  const [disabled, setDisabled] = React.useState(false)

  React.useEffect(() => {
    if (document.activeElement === inputRef.current) {
      return
    }
    const foundText = optionalMap(MetadataUtils.getTextContentOfElement, selectedElement)
    const isDisabled = foundText == null
    setSimpleText(foundText?.trim() ?? '')
    setDisabled(isDisabled)
  }, [selectedElement, inputRef])

  const dispatchUpdate = React.useCallback(
    ({ path, text }) => {
      dispatch([EditorActions.updateChildText(path, text.trim())], 'canvas')
      clearTimeout(saveTimerRef.current)
      saveTimerRef.current = null
    },
    [dispatch],
  )

  const onInputChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElement != null) {
        clearTimeout(saveTimerRef.current)
        saveTimerRef.current = setTimeout(dispatchUpdate, 300, {
          path: selectedElement.elementPath,
          text: event.target.value,
        })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElement, dispatchUpdate],
  )

  const onBlur = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElement != null) {
        clearTimeout(saveTimerRef.current)
        dispatchUpdate({ path: selectedElement.elementPath, text: event.target.value.trim() })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElement, dispatchUpdate],
  )

  const buttonsVisible = selectedElement != null
  const classNameFieldVisible = selectedElement != null && selectedMode === 'css'
  const inputFieldVisible = !classNameFieldVisible

  const editorStoreRef = useRefEditorState(identity)
  const onKeyDown = React.useCallback(
    (event: React.KeyboardEvent<HTMLElement>) => {
      const namesByKey = applyShortcutConfigurationToDefaults(
        editorStoreRef.current.userState.shortcutConfig,
      )
      handleShortcuts(namesByKey, event.nativeEvent, null, {
        [TOGGLE_FOCUSED_OMNIBOX_TAB]: () => {
          dispatch([EditorActions.toggleFocusedOmniboxTab()])

          // We need this to happen in the next frame to ensure the field we want to focus exists
          setTimeout(() => dispatch([EditorActions.focusFormulaBar()]), 0)
        },
      })
    },
    [editorStoreRef, dispatch],
  )

  return (
    <SimpleFlexRow
      css={{
        height: 24,
        flexGrow: 1,
        paddingLeft: 4,
        paddingRight: 4,
        gap: 4,
        borderRadius: 4,
        backgroundColor: colorTheme.inverted.bg2.value,
        color: colorTheme.inverted.fg1.value,
        cursor: 'pointer',
        border: '1px solid transparent',
        '&:hover': {
          outline: 'none',
        },
        '&:focus-within': {
          background: colorTheme.inverted.bg1.value,
        },
      }}
      onKeyDown={onKeyDown}
    >
      {inputFieldVisible ? (
        <HeadlessStringInput
          ref={inputRef}
          type='text'
          placeholder={disabled ? '(Unavailable)' : '(empty)'}
          css={{
            paddingLeft: 4,
            paddingRight: 4,
            width: '100%',
            height: '100%',
            fontSize: 12,
            letterSpacing: 0.2,
            border: '1px solid transparent',
            borderRadius: 3,
            backgroundColor: 'transparent',
            color: colorTheme.inverted.fg1.value,
            transition: 'background-color .1s ease-in-out',
            '&:hover': {
              outline: 'none',
            },
            '&:focus': {
              outline: 'none',
            },
          }}
          onChange={onInputChange}
          onBlur={onBlur}
          value={simpleText}
          disabled={disabled}
        />
      ) : (
        <span style={{ fontSize: 13, letterSpacing: 0.2, opacity: 0.4 }}>(Unavailable)</span>
      )}
    </SimpleFlexRow>
  )
})
