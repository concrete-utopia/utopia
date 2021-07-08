/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme, SimpleFlexRow, UtopiaTheme, HeadlessStringInput } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
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

function useFocusOnCountIncrease(triggerCount: number): React.RefObject<HTMLInputElement> {
  const ref = React.useRef<HTMLInputElement>(null)
  const previousTriggerCountRef = React.useRef(triggerCount)
  if (previousTriggerCountRef.current !== triggerCount) {
    previousTriggerCountRef.current = triggerCount
    // eslint-disable-next-line no-unused-expressions
    ref.current?.focus()
  }
  return ref
}

export const FormulaBar = betterReactMemo('FormulaBar', () => {
  const saveTimerRef = React.useRef<any>(null)
  const dispatch = useEditorState((store) => store.dispatch, 'FormulaBar dispatch')

  const selectedMode = useEditorState(
    (store) => store.editor.topmenu.formulaBarMode,
    'FormulaBar selectedMode',
  )

  const selectedElement = useEditorState((store) => {
    const metadata = store.editor.jsxMetadata
    if (store.editor.selectedViews.length === 1) {
      return MetadataUtils.findElementByElementPath(metadata, store.editor.selectedViews[0])
    } else {
      return null
    }
  }, 'FormulaBar selectedElement')

  const focusTriggerCount = useEditorState(
    (store) => store.editor.topmenu.formulaBarFocusCounter,
    'FormulaBar formulaBarFocusCounter',
  )

  const inputRef = useFocusOnCountIncrease(focusTriggerCount)

  const colorTheme = useColorTheme()
  const [simpleText, setSimpleText] = React.useState('')
  const [disabled, setDisabled] = React.useState(false)

  React.useEffect(() => {
    if (saveTimerRef.current != null) {
      return
    }
    const foundText = optionalMap(MetadataUtils.getTextContentOfElement, selectedElement)
    const isDisabled = foundText == null
    setSimpleText(foundText ?? '')
    setDisabled(isDisabled)
  }, [selectedElement])

  const dispatchUpdate = React.useCallback(
    ({ path, text }) => {
      dispatch([EditorActions.updateChildText(path, text)], 'canvas')
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
        dispatchUpdate({ path: selectedElement.elementPath, text: event.target.value })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElement, dispatchUpdate],
  )

  const buttonsVisible = isFeatureEnabled('TopMenu ClassNames') && selectedElement != null
  const classNameFieldVisible =
    isFeatureEnabled('TopMenu ClassNames') && selectedElement != null && selectedMode === 'css'
  const inputFieldVisible = !classNameFieldVisible

  return (
    <SimpleFlexRow
      style={{
        flexGrow: 1,
        height: UtopiaTheme.layout.inputHeight.default,
      }}
    >
      {buttonsVisible ? <ModeToggleButton /> : null}
      {classNameFieldVisible ? <ClassNameSelect /> : null}
      {inputFieldVisible ? (
        <HeadlessStringInput
          ref={inputRef}
          type='text'
          css={{
            paddingLeft: 4,
            paddingRight: 4,
            border: '0px',
            width: '100%',
            height: '100%',
            backgroundColor: colorTheme.canvasBackground.value,
            borderRadius: 5,
            transition: 'background-color .1s ease-in-out',
            '&:hover': {
              '&:not(:disabled)': {
                boxShadow: 'inset 0px 0px 0px 1px lightgrey',
              },
            },
            '&:focus': {
              backgroundColor: colorTheme.bg0.value,
              boxShadow: 'inset 0px 0px 0px 1px lightgrey',
            },
          }}
          onChange={onInputChange}
          onBlur={onBlur}
          value={simpleText}
          disabled={disabled}
        />
      ) : null}
    </SimpleFlexRow>
  )
})
