import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { useEditorState } from '../editor/store/store-hook'
import { updateChildText } from '../editor/actions/action-creators'

interface TextEditorProps {
  elementPath: ElementPath
  text: string
}

export const TextEditor: React.FC<TextEditorProps> = ({ elementPath, text }: TextEditorProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'useEditorState dispatch')
  const [firstTextProp] = React.useState(text)

  const onKeyDown = React.useCallback(
    (event: React.KeyboardEvent) => {
      if (event.key === 'Escape') {
        blurTextEditor(elementPath)
      } else {
        event.stopPropagation()
      }
    },
    [elementPath],
  )

  const contentRef = React.useRef(text)

  const onChange = React.useCallback(() => {
    const content = myElement.current?.textContent
    if (content == null) {
      return
    }
    if (content !== contentRef.current) {
      contentRef.current = content
      dispatch([updateChildText(elementPath, content)])
    }
  }, [elementPath, dispatch])
  const myElement = React.useRef<HTMLSpanElement>(null)

  return (
    <span
      ref={myElement}
      id={getSlateEditorId(elementPath)}
      onKeyDown={onKeyDown}
      onKeyUp={onKeyDown}
      onKeyPress={onKeyDown}
      contentEditable={true}
      onInput={onChange}
      suppressContentEditableWarning={true}
    >
      {firstTextProp}
    </span>
  )
}

export function focusTextEditor(elementPath: ElementPath): void {
  const activeTextEditorElement = document.getElementById(getSlateEditorId(elementPath))
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.focus()
}

export function blurTextEditor(elementPath: ElementPath): void {
  const activeTextEditorElement = document.getElementById(getSlateEditorId(elementPath))
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.blur()
}

export function getSlateEditorId(elementPath: ElementPath): string {
  return `slate-editor-${EP.toString(elementPath)}`
}
