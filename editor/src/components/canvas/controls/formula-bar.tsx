import * as React from 'react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { colorTheme, SimpleFlexRow } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import * as TP from '../../../core/shared/template-path'
import { isJSXElement, JSXTextBlock } from '../../../core/shared/element-template'
import { TemplatePath } from '../../../core/shared/project-file-types'

export const FormulaBar = betterReactMemo('FormulaBar', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenu dispatch')
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'TopMenu selectedViews',
  )
  const componentMetadata = useEditorState(
    (store) => store.editor.jsxMetadataKILLME,
    'TopMenu componentMetadata',
  )

  let textBlock: JSXTextBlock | null = null
  let target: TemplatePath | null = null
  let disabled = false
  if (selectedViews.length === 1 && TP.isInstancePath(selectedViews[0])) {
    target = selectedViews[0]
    const element = MetadataUtils.getElementByInstancePathMaybe(componentMetadata.elements, target)
    if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
      if (element.element.value.children.length === 1) {
        if (element.element.value.children[0].type === 'JSX_TEXT_BLOCK') {
          textBlock = element.element.value.children[0]
        }
      }
      if (element.element.value.children.length > 1) {
        target = null
        disabled = true
      }
    }
  } else {
    disabled = true
  }

  const onChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (target != null && TP.isInstancePath(target)) {
        dispatch([EditorActions.updateChildText(target, event.target.value)], 'canvas')
      }
    },
    [dispatch, target],
  )

  const simpleText = (textBlock as any)?.text ?? ''

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
        onChange={onChange}
        value={simpleText}
        disabled={disabled}
      />
    </SimpleFlexRow>
  )
})
