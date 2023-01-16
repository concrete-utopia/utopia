/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../editor/actions/action-creators'
import { SimpleFlexRow, HeadlessStringInput, colorTheme } from '../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

import {
  applyShortcutConfigurationToDefaults,
  handleShortcuts,
  TOGGLE_FOCUSED_OMNIBOX_TAB,
} from '../../editor/shortcut-definitions'
import { useInputFocusOnCountIncrease } from '../../editor/hook-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'

interface FormulaBarProps {
  style: React.CSSProperties
}

export const FormulaBar = React.memo<FormulaBarProps>((props) => {
  const saveTimerRef = React.useRef<any>(null)
  const dispatch = useDispatch()

  const selectedMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.topmenu.formulaBarMode,
    'FormulaBar selectedMode',
  )

  const selectedElementPath = useEditorState(
    Substores.selectedViews,
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        return store.editor.selectedViews[0]
      } else {
        return null
      }
    },
    'FormulaBar selectedElementPath',
  )

  const selectedElementTextContent = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        const metadata = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )
        if (metadata == null) {
          return null
        } else {
          return MetadataUtils.getTextContentOfElement(metadata)
        }
      } else {
        return null
      }
    },
    'FormulaBar selectedElementTextContent',
  )

  const focusTriggerCount = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.topmenu.formulaBarFocusCounter,
    'FormulaBar formulaBarFocusCounter',
  )

  const inputRef = useInputFocusOnCountIncrease<HTMLInputElement>(focusTriggerCount)

  const [simpleText, setSimpleText] = React.useState('')
  const [disabled, setDisabled] = React.useState(false)

  React.useEffect(() => {
    if (document.activeElement === inputRef.current) {
      return
    }
    const isDisabled = selectedElementTextContent == null
    setSimpleText(selectedElementTextContent?.trim() ?? '')
    setDisabled(isDisabled)
  }, [selectedElementTextContent, inputRef])

  const dispatchUpdate = React.useCallback(
    ({ path, text }: { path: ElementPath; text: string }) => {
      dispatch([EditorActions.updateChildText(path, text.trim())], 'canvas')
      clearTimeout(saveTimerRef.current)
      saveTimerRef.current = null
    },
    [dispatch],
  )

  const onInputChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElementPath != null) {
        clearTimeout(saveTimerRef.current)
        saveTimerRef.current = setTimeout(dispatchUpdate, 300, {
          path: selectedElementPath,
          text: event.target.value,
        })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElementPath, dispatchUpdate],
  )

  const onBlur = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElementPath != null) {
        clearTimeout(saveTimerRef.current)
        dispatchUpdate({ path: selectedElementPath, text: event.target.value.trim() })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElementPath, dispatchUpdate],
  )

  const classNameFieldVisible = selectedElementPath != null && selectedMode === 'css'
  const inputFieldVisible = !classNameFieldVisible

  const editorStoreRef = useRefEditorState((store) => store)
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
        backgroundColor: colorTheme.bg1.value,
        color: colorTheme.border1.value,
        border: '1px solid transparent',
        '&:hover': {
          outline: 'none',
        },
        '&:focus-within': {
          background: colorTheme.bg1.value,
        },
      }}
      onKeyDown={onKeyDown}
    >
      {inputFieldVisible ? (
        <HeadlessStringInput
          ref={inputRef}
          type='text'
          placeholder=''
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
            color: colorTheme.bg0.value,
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
