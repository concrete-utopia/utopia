/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme, SimpleFlexRow, UtopiaTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
} from '../../../core/shared/element-template'
import { ModeToggleButton } from './mode-toggle-button'
import { ClassNameSelect } from './classname-select'
import { isFeatureEnabled } from '../../../utils/feature-switches'

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

  const colorTheme = useColorTheme()
  const [simpleText, setSimpleText] = React.useState('')
  const [disabled, setDisabled] = React.useState(false)

  React.useEffect(() => {
    if (saveTimerRef.current != null) {
      return
    }
    let foundText = ''
    let isDisabled = true
    if (
      selectedElement != null &&
      isRight(selectedElement.element) &&
      isJSXElement(selectedElement.element.value)
    ) {
      if (selectedElement.element.value.children.length === 1) {
        const childElement = selectedElement.element.value.children[0]
        if (isJSXTextBlock(childElement)) {
          foundText = childElement.text
          isDisabled = false
        } else if (isJSXArbitraryBlock(childElement)) {
          foundText = `{${childElement.originalJavascript}}`
          isDisabled = false
        }
      } else if (selectedElement.element.value.children.length === 0) {
        isDisabled = false
      }
    }
    setSimpleText(foundText)
    setDisabled(isDisabled)
  }, [selectedElement])

  const dispatchUpdate = React.useCallback(
    ({ path, text }) => {
      dispatch([EditorActions.updateChildText(path, text)], 'canvas')
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
      {buttonsVisible && <ModeToggleButton />}
      {classNameFieldVisible && selectedMode === 'css' && (
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            height: UtopiaTheme.layout.rowHeight.normal,
            gap: 4,
            flexGrow: 1,
          }}
        >
          <ClassNameSelect />
        </div>
      )}
      {inputFieldVisible && (
        <input
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
          value={simpleText}
          disabled={disabled}
        />
      )}
    </SimpleFlexRow>
  )
})
