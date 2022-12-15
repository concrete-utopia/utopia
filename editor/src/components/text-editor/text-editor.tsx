import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import { useEditorState } from '../editor/store/store-hook'
import {
  applyCommandsAction,
  clearSelection,
  updateChildText,
  updateEditorMode,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'
import { escape, unescape } from 'he'
import { AllElementProps } from '../editor/store/editor-state'
import { Modifier } from '../../utils/modifiers'
import { setProperty } from '../canvas/commands/set-property-command'
import * as EP from '../../core/shared/element-path'
import * as PP from '../../core/shared/property-path'
import { ApplyCommandsAction } from '../editor/action-types'

export const TextEditorSpanId = 'text-editor'

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

const handleShortcut = (
  cond: boolean,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  prop: string,
  value: string,
  defaultValue: string,
): Array<ApplyCommandsAction> => {
  if (!cond) {
    return []
  }
  const { style } = allElementProps[EP.toString(elementPath)]
  const newValue = style != null && style[prop] === value ? defaultValue : value
  return [
    applyCommandsAction([setProperty('always', elementPath, PP.create(['style', prop]), newValue)]),
  ]
}

export const TextEditor = React.memo(({ elementPath, text }: TextEditorProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'TextEditor dispatch')
  const allElementProps = useEditorState((store) => store.editor.allElementProps, 'Editor')
  const [firstTextProp] = React.useState(text)
  const [loaded, setLoaded] = React.useState(false)

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

  React.useEffect(() => {
    if (loaded || myElement.current?.firstChild == null) {
      return
    }
    setLoaded(true)
  }, [loaded, myElement.current?.firstChild])

  React.useEffect(() => {
    if (myElement.current == null) {
      return
    }
    myElement.current.textContent = firstTextProp
  }, [firstTextProp])

  const onKeyDown = React.useCallback(
    (event: React.KeyboardEvent) => {
      const modifiers = Modifier.modifiersForEvent(event)
      const meta = modifiers.cmd || modifiers.ctrl
      const shortcuts = [
        ...handleShortcut(
          meta && event.key === 'b', // Meta+b = bold
          allElementProps,
          elementPath,
          'fontWeight',
          'bold',
          'normal',
        ),
        ...handleShortcut(
          meta && event.key === 'i', // Meta+i = italic
          allElementProps,
          elementPath,
          'fontStyle',
          'italic',
          'normal',
        ),
        ...handleShortcut(
          meta && event.key === 'u', // Meta+u = underline
          allElementProps,
          elementPath,
          'textDecoration',
          'underline',
          'none',
        ),
        ...handleShortcut(
          meta && modifiers.shift && event.key === 'x', // Meta+shift+x = strikethrough
          allElementProps,
          elementPath,
          'textDecoration',
          'line-through',
          'none',
        ),
      ]
      if (shortcuts.length > 0) {
        event.stopPropagation()
        dispatch(shortcuts)
      }

      if (event.key === 'Escape') {
        // eslint-disable-next-line no-unused-expressions
        myElement.current?.blur()
      } else {
        event.stopPropagation()
      }
    },
    [dispatch, elementPath, allElementProps],
  )

  const onBlur = React.useCallback(() => {
    dispatch([updateEditorMode(EditorModes.selectMode()), clearSelection()])
  }, [dispatch])

  return (
    <span
      ref={myElement}
      id={TextEditorSpanId}
      onPaste={stopPropagation}
      onKeyDown={onKeyDown}
      onKeyUp={stopPropagation}
      onKeyPress={stopPropagation}
      onBlur={onBlur}
      contentEditable={'plaintext-only' as any} // note: not supported on firefox
      suppressContentEditableWarning={true}
    />
  )
})

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

function stopPropagation(e: React.KeyboardEvent | React.ClipboardEvent) {
  e.stopPropagation()
}
