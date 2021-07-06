/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { useColorTheme, SimpleFlexRow, UtopiaTheme, HeadlessStringInput } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXTextBlock,
} from '../../../core/shared/element-template'

export const FormulaBar = betterReactMemo('FormulaBar', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'FormulaBar dispatch')

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

  const onInputChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElement != null) {
        setSimpleText(event.target.value)
      }
    },
    [selectedElement],
  )

  const onSubmitValue = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (selectedElement != null) {
        setSimpleText(event.target.value)
        dispatch(
          [EditorActions.updateChildText(selectedElement.elementPath, event.target.value)],
          'canvas',
        )
      }
    },
    [dispatch, selectedElement],
  )

  return (
    <SimpleFlexRow
      style={{
        flexGrow: 1,
        height: UtopiaTheme.layout.inputHeight.default,
      }}
    >
      <HeadlessStringInput
        data-testid='formula-bar-input'
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
        onBlur={onSubmitValue}
        value={simpleText}
        disabled={disabled}
      />
    </SimpleFlexRow>
  )
})
