import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { useEditorState } from '../editor/store/store-hook'
import { updateChildText, updateEditorMode } from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'

interface TextEditorProps {
  elementPath: ElementPath
  text: string
}

export const TextEditor: React.FC<TextEditorProps> = ({ elementPath, text }: TextEditorProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'useEditorState dispatch')
  const [firstTextProp] = React.useState(text)

  const myElement = React.useRef<HTMLSpanElement>(null)

  React.useEffect(() => {
    if (myElement.current == null) {
      return
    }

    const range = document.createRange()
    range.selectNodeContents(myElement.current)
    range.collapse(false)

    const selection = window.getSelection()
    if (selection != null) {
      selection.removeAllRanges()
      selection.addRange(range)
    }

    myElement.current.focus()

    myElement.current.addEventListener('blur', () => {
      dispatch([updateEditorMode(EditorModes.selectMode())])
    })
  }, [dispatch])

  const onKeyDown = React.useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Escape') {
      // eslint-disable-next-line no-unused-expressions
      myElement.current?.blur()
    } else {
      event.stopPropagation()
    }
  }, [])

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

export function getSlateEditorId(elementPath: ElementPath): string {
  return `slate-editor-${EP.toString(elementPath)}`
}
