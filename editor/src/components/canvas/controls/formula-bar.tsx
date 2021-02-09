import * as React from 'react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { colorTheme, SimpleFlexRow } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import * as TP from '../../../core/shared/template-path'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
} from '../../../core/shared/element-template'

export const FormulaBar = betterReactMemo('FormulaBar', () => {
  const saveTimerRef = React.useRef<any>(null)
  const dispatch = useEditorState((store) => store.dispatch, 'FormulaBar dispatch')

  const selectedElement = useEditorState((store) => {
    const metadata = store.editor.jsxMetadataKILLME
    if (
      store.editor.selectedViews.length === 1 &&
      TP.isInstancePath(store.editor.selectedViews[0])
    ) {
      return MetadataUtils.getElementByInstancePathMaybe(
        metadata.elements,
        store.editor.selectedViews[0],
      )
    } else {
      return null
    }
  }, 'FormulaBar selectedElement')

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
      if (selectedElement != null && TP.isInstancePath(selectedElement.templatePath)) {
        clearTimeout(saveTimerRef.current)
        saveTimerRef.current = setTimeout(dispatchUpdate, 300, {
          path: selectedElement.templatePath,
          text: event.target.value,
        })
        setSimpleText(event.target.value)
      }
    },
    [saveTimerRef, selectedElement, dispatchUpdate],
  )

  return (
    <SimpleFlexRow
      style={{
        flexGrow: 1,
        paddingRight: 10,
        height: 21,
      }}
    >
      <input
        type='text'
        style={{
          padding: '0px',
          border: '0px',
          width: '100%',
          height: '100%',
          backgroundColor: colorTheme.canvasBackground.value,
          borderRadius: 5,
        }}
        onChange={onInputChange}
        value={simpleText}
        disabled={disabled}
      />
    </SimpleFlexRow>
  )
})
