import * as React from 'react'
import { colorTheme } from '../../../uuiui'
import { EditorDispatch } from '../../editor/action-types'
import { useEditorState } from '../../editor/store/store-hook'
import * as TP from '../../../core/shared/template-path'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isRight } from '../../../core/shared/either'
import { isJSXElement, JSXTextBlock } from '../../../core/shared/element-template'
import { updateSimpleTextChild } from '../../editor/actions/actions'

interface TextInputProps {
  dispatch: EditorDispatch
  simpleText: string
  disabled: boolean
  target: TemplatePath | null
}

export const TextEditingArea = () => {
  const { selectedViews, componentMetadata } = useEditorState(
    (store) => ({
      selectedViews: store.editor.selectedViews,
      componentMetadata: store.editor.jsxMetadataKILLME,
    }),
    'text-editing-selectedviews-metadata',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'text-editing-dispatch')
  let textBlock: JSXTextBlock | null = null
  let target: TemplatePath | null = null
  let disabled = false
  if (selectedViews.length === 1) {
    target = selectedViews[0]
    const element = MetadataUtils.getElementByTemplatePathMaybe(componentMetadata.elements, target)
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
  const simpleText = (textBlock as any)?.text ?? ''
  return (
    <TextEditingInputArea
      key={(target && TP.toString(target)) ?? 'simpletext'}
      target={target}
      simpleText={simpleText}
      disabled={disabled}
      dispatch={dispatch}
    />
  )
}

const TextEditingInputArea = (props: TextInputProps) => {
  const { dispatch, target } = props
  const onChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (target != null && TP.isInstancePath(target)) {
        dispatch([updateSimpleTextChild(target, event.target.value)], 'canvas')
      }
    },
    [dispatch, target],
  )

  return (
    <div
      style={{
        position: 'absolute',
        width: '100%',
        height: 20,
        backgroundColor: colorTheme.inspectorBackground.value,
      }}
    >
      <input
        type='text'
        style={{
          padding: '0px',
          border: '0px',
          width: '100%',
          height: '100%',
          backgroundColor: 'white',
        }}
        onChange={onChange}
        value={props.simpleText}
        disabled={props.disabled}
      />
    </div>
  )
}
