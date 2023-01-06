import { escape, unescape } from 'he'
import React from 'react'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { Modifier } from '../../utils/modifiers'
import { setProperty } from '../canvas/commands/set-property-command'
import { ApplyCommandsAction } from '../editor/action-types'
import {
  applyCommandsAction,
  updateChildText,
  updateEditorMode,
} from '../editor/actions/action-creators'
import { Coordinates, EditorModes } from '../editor/editor-modes'
import { useEditorState } from '../editor/store/store-hook'

export const TextEditorSpanId = 'text-editor'

interface TextEditorProps {
  elementPath: ElementPath
  text: string
  component: React.ComponentType<React.PropsWithChildren<any>>
  passthroughProps: Record<string, any>
}

export function escapeHTML(s: string): string {
  return escape(s).replace(/\n/g, '<br />')
}

export function unescapeHTML(s: string): string {
  return unescape(s).replace(/<br \/>/g, '\n')
}

const handleShortcut = (
  cond: boolean,
  style: { [key: string]: unknown } | null,
  elementPath: ElementPath,
  prop: string,
  value: string,
  defaultValue: string,
): Array<ApplyCommandsAction> => {
  if (!cond) {
    return []
  }
  const newValue = style != null && style[prop] === value ? defaultValue : value
  return [
    applyCommandsAction([setProperty('always', elementPath, PP.create(['style', prop]), newValue)]),
  ]
}

export const TextEditorWrapper = React.memo((props: TextEditorProps) => {
  const { elementPath, text, component, passthroughProps } = props
  const dispatch = useEditorState((store) => store.dispatch, 'TextEditor dispatch')
  const cursorPosition = useEditorState(
    (store) => (store.editor.mode.type === 'textEdit' ? store.editor.mode.cursorPosition : null),
    'TextEditor cursor position',
  )
  const scale = useEditorState((store) => store.editor.canvas.scale, 'TextEditor scale')
  const [firstTextProp] = React.useState(text)

  const myElement = React.useRef<HTMLSpanElement>(null)

  React.useEffect(() => {
    const currentElement = myElement.current
    if (currentElement == null) {
      return
    }

    currentElement.focus()

    return () => {
      const content = currentElement.textContent
      if (content != null) {
        dispatch([updateChildText(elementPath, escapeHTML(content).replace(/\n/g, '<br />'))])
      }
    }
  }, [dispatch, elementPath])

  React.useEffect(() => {
    if (myElement.current == null) {
      return
    }
    myElement.current.textContent = firstTextProp
  }, [firstTextProp])

  React.useEffect(() => {
    if (myElement.current == null) {
      return
    }
    void setSelectionToOffset(myElement.current, scale, cursorPosition)
  }, [scale, cursorPosition])

  const onKeyDown = React.useCallback(
    (event: React.KeyboardEvent) => {
      const modifiers = Modifier.modifiersForEvent(event)
      const meta = modifiers.cmd || modifiers.ctrl
      const style = passthroughProps.style ?? {}
      const shortcuts = [
        ...handleShortcut(
          meta && event.key === 'b', // Meta+b = bold
          style,
          elementPath,
          'fontWeight',
          'bold',
          'normal',
        ),
        ...handleShortcut(
          meta && event.key === 'i', // Meta+i = italic
          style,
          elementPath,
          'fontStyle',
          'italic',
          'normal',
        ),
        ...handleShortcut(
          meta && event.key === 'u', // Meta+u = underline
          style,
          elementPath,
          'textDecoration',
          'underline',
          'none',
        ),
        ...handleShortcut(
          meta && modifiers.shift && event.key === 'x', // Meta+shift+x = strikethrough
          style,
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
    [dispatch, elementPath, passthroughProps],
  )

  const onBlur = React.useCallback(() => {
    dispatch([updateEditorMode(EditorModes.selectMode())])
  }, [dispatch])

  const editorProps = {
    ref: myElement,
    id: TextEditorSpanId,
    onPaste: stopPropagation,
    onKeyDown: onKeyDown,
    onKeyUp: stopPropagation,
    onKeyPress: stopPropagation,
    onBlur: onBlur,
    contentEditable: 'plaintext-only' as any, // note: not supported on firefox,
    suppressContentEditableWarning: true,
  }

  // When the component to render is a simple html element we should make that contenteditable
  if (typeof component === 'string') {
    return React.createElement(component, {
      ...passthroughProps,
      ...editorProps,
    })
  }
  return React.createElement(component, passthroughProps, <span {...editorProps} />)
})

async function setSelectionToOffset(
  element: HTMLSpanElement,
  scale: number,
  cursorPosition: Coordinates | null,
) {
  const sel = window.getSelection()
  if (sel == null) {
    return
  }
  if (element.childNodes.length != 1) {
    return
  }
  const textNode = element.childNodes[0]
  if (textNode.nodeType !== element.TEXT_NODE) {
    return
  }

  const setRange = (start: number | null) => {
    sel.removeAllRanges()
    const range = document.createRange()
    range.selectNodeContents(textNode)
    range.collapse(start != null)
    if (start != null) {
      range.setStart(textNode, start)
    }
    sel.addRange(range)
    return range
  }

  const maxLength = setRange(null).endOffset

  if (cursorPosition != null) {
    // to find the right target offset:
    // 1. find the valid X points
    // 2. find the valid Y points
    // 3. either use the intersection of the two arrays, or the minimum possible
    //    location if the intersection is empty
    let validX: number[] = []
    let validY: number[] = []
    // linear search is a tad slow, but it should be fine
    // and it's a lot easier when dealing with the scaling of the editor
    const targetX = cursorPosition.x / scale
    for (let i = 0; i <= maxLength; i++) {
      const range = setRange(i)
      const rect = range.getBoundingClientRect()
      if (i > 0 && rect.x > targetX) {
        validX.push(i > 0 ? i - 1 : 0)
      }
    }
    const targetY = cursorPosition.y / scale
    for (let i = 0; i <= maxLength; i++) {
      const range = setRange(i)
      const rect = range.getBoundingClientRect()
      if (rect.y <= targetY && targetY <= rect.y + rect.height) {
        validY.push(i)
      }
    }
    const intersection = validX.filter((xx) => validY.includes(xx))
    if (intersection.length > 0) {
      setRange(intersection[0])
    } else {
      setRange(validY.length > 0 ? validY[validY.length - 1] : maxLength)
    }
  }
}

function stopPropagation(e: React.KeyboardEvent | React.ClipboardEvent) {
  e.stopPropagation()
}
