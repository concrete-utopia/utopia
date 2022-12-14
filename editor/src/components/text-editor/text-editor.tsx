import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import { useEditorState } from '../editor/store/store-hook'
import {
  clearSelection,
  updateChildText,
  updateEditorMode,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'
import { escape, unescape } from 'he'

interface TextEditorProps {
  elementPath: ElementPath
  text: string
}

export function escapeHTML(s: string): string {
  return escape(s)
}

export function unescapeHTML(s: string): string {
  return unescape(s)
}

export const TextEditor: React.FC<TextEditorProps> = ({ elementPath, text }: TextEditorProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'TextEditor dispatch')
  const [firstTextProp] = React.useState(text)

  const myElement = React.useRef<HTMLSpanElement>(null)

  React.useEffect(() => {
    const currentElement = myElement.current
    if (currentElement == null) {
      return
    }

    setSelectionToEnd(currentElement)

    currentElement.focus()

    return () => {
      const content = currentElement.textContent
      if (content != null) {
        dispatch([updateChildText(elementPath, escapeHTML(content))])
      }
    }
  }, [dispatch, elementPath])

  const onKeyDown = React.useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Escape') {
      // eslint-disable-next-line no-unused-expressions
      myElement.current?.blur()
    } else {
      event.stopPropagation()
    }
  }, [])

  const onBlur = React.useCallback(() => {
    dispatch([updateEditorMode(EditorModes.selectMode()), clearSelection()])
  }, [dispatch])

  return (
    <span
      ref={myElement}
      onKeyDown={onKeyDown}
      onKeyUp={stopPropagation}
      onKeyPress={stopPropagation}
      onBlur={onBlur}
      contentEditable={'plaintext-only' as any} // note: not supported on firefox
      suppressContentEditableWarning={true}
    >
      {firstTextProp}
    </span>
  )
}

function setSelectionToEnd(element: HTMLSpanElement) {
  const range = document.createRange()
  range.selectNodeContents(element)
  range.collapse(false)

  const selection = window.getSelection()
  if (selection != null) {
    selection.removeAllRanges()
    selection.addRange(range)
  }
}

function stopPropagation(e: React.KeyboardEvent) {
  e.stopPropagation()
}
