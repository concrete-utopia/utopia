import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { useEditorState } from '../editor/store/store-hook'
import {
  clearSelection,
  updateChildText,
  updateEditorMode,
} from '../editor/actions/action-creators'
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
    const currentElement = myElement.current
    if (currentElement == null) {
      return
    }

    const range = document.createRange()
    range.selectNodeContents(currentElement)
    range.collapse(false)

    const selection = window.getSelection()
    if (selection != null) {
      selection.removeAllRanges()
      selection.addRange(range)
    }

    currentElement.focus()

    currentElement.addEventListener('blur', () => {
      dispatch([updateEditorMode(EditorModes.selectMode()), clearSelection()])
    })

    return () => {
      const content = currentElement.textContent
      if (content != null) {
        dispatch([updateChildText(elementPath, content)])
      }
    }
  }, [dispatch, elementPath])

  const onKeyEvent = React.useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Escape') {
      // eslint-disable-next-line no-unused-expressions
      myElement.current?.blur()
    } else {
      event.stopPropagation()
    }
  }, [])

  return (
    <span
      ref={myElement}
      onKeyDown={onKeyEvent}
      onKeyUp={onKeyEvent}
      onKeyPress={onKeyEvent}
      contentEditable={'plaintext-only' as any} // note: not supported on firefox
      suppressContentEditableWarning={true}
    >
      {firstTextProp}
    </span>
  )
}
