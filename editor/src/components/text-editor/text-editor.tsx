import { escape, unescape } from 'he'
import React from 'react'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { keyCharacterFromCode } from '../../utils/keyboard'
import { Modifier } from '../../utils/modifiers'
import {
  adjustFontSize,
  getFontSize,
  isAdjustFontSizeShortcut,
} from '../canvas/canvas-strategies/strategies/keyboard-set-font-size-strategy'
import {
  adjustFontWeight,
  getFontWeightFromComputedStyle,
  isAdjustFontWeightShortcut,
} from '../canvas/canvas-strategies/strategies/keyboard-set-font-weight-strategy'
import { setProperty } from '../canvas/commands/set-property-command'
import { ApplyCommandsAction } from '../editor/action-types'
import {
  applyCommandsAction,
  deleteView,
  reparseProjectFile,
  updateChildText,
  updateEditorMode,
} from '../editor/actions/action-creators'
import { Coordinates, EditorModes } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import { useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { printCSSNumber } from '../inspector/common/css-utils'

export const TextEditorSpanId = 'text-editor'

interface TextEditorProps {
  elementPath: ElementPath
  text: string
  component: React.ComponentType<React.PropsWithChildren<any>>
  passthroughProps: Record<string, any>
  filePath: string
}

const htmlEntities = {
  curlyBraceLeft: '&#123;',
  curlyBraceRight: '&#125;',
}

const deferredReparseTimeoutMS = 250

export function escapeHTML(s: string): string {
  return (
    escape(s)
      // restore br tags
      .replace(/\n/g, '<br />')
      // clean up curly braces
      .replace(/\{/g, htmlEntities.curlyBraceLeft)
      .replace(/\}/g, htmlEntities.curlyBraceRight)
      // restore the ones that wrap valid jsx expressions
      .replace(
        new RegExp(`${htmlEntities.curlyBraceLeft}([^&}]+)${htmlEntities.curlyBraceRight}`, 'g'),
        '{$1}',
      )
  )
}

export function unescapeHTML(s: string): string {
  return unescape(s)
    .replace(/<br \/>/g, '\n')
    .replace(new RegExp(htmlEntities.curlyBraceLeft, 'g'), '{')
    .replace(new RegExp(htmlEntities.curlyBraceRight, 'g'), '}')
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

const handleSetFontSizeShortcut = (
  event: React.KeyboardEvent<Element>,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<ApplyCommandsAction> => {
  const modifiers = Modifier.modifiersForEvent(event)
  const character = keyCharacterFromCode(event.keyCode)
  const matches = isAdjustFontSizeShortcut(modifiers, character)

  if (matches == null) {
    return []
  }

  const delta = character === 'period' ? 1 : character === 'comma' ? -1 : 0
  const fontSize = getFontSize(metadata, elementPath)
  if (fontSize == null) {
    return []
  }

  return [
    applyCommandsAction([
      setProperty(
        'always',
        elementPath,
        PP.create(['style', 'fontSize']),
        printCSSNumber(adjustFontSize(fontSize[0], delta), null),
      ),
    ]),
  ]
}

const handleSetFontWeightShortcut = (
  event: React.KeyboardEvent<Element>,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<ApplyCommandsAction> => {
  const modifiers = Modifier.modifiersForEvent(event)
  const character = keyCharacterFromCode(event.keyCode)
  const matches = isAdjustFontWeightShortcut(modifiers, character)

  if (matches == null) {
    return []
  }

  const delta = character === 'period' ? 1 : character === 'comma' ? -1 : 0

  const fontWeight = getFontWeightFromComputedStyle(metadata, elementPath)
  if (fontWeight == null) {
    return []
  }

  return [
    applyCommandsAction([
      setProperty(
        'always',
        elementPath,
        PP.create(['style', 'fontWeight']),
        adjustFontWeight(fontWeight, delta),
      ),
    ]),
  ]
}

export const TextEditorWrapper = React.memo((props: TextEditorProps) => {
  const { elementPath, text, component, passthroughProps, filePath: filename } = props
  const dispatch = useDispatch()
  const cursorPosition = useEditorState(
    (store) => (store.editor.mode.type === 'textEdit' ? store.editor.mode.cursorPosition : null),
    'TextEditor cursor position',
  )
  const elementState = useEditorState(
    (store) => (store.editor.mode.type === 'textEdit' ? store.editor.mode.elementState : null),
    'TextEditor element state',
  )
  const shouldSelectOnFocus = useEditorState(
    (store) =>
      store.editor.mode.type === 'textEdit' ? store.editor.mode.selectOnFocus : 'no-text-selection',
    'TextEditor shouldSelectOnFocus',
  )

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

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
        if (elementState === 'new' && content === '') {
          dispatch([deleteView(elementPath)])
        } else {
          dispatch([updateChildText(elementPath, escapeHTML(content).replace(/\n/g, '<br />'))])

          // defer reparsing the open project file to give it time to process the
          // updateChildText action
          setTimeout(() => dispatch([reparseProjectFile(filename)]), deferredReparseTimeoutMS)
        }
      }
    }
  }, [dispatch, elementPath, elementState, filename])

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

  React.useEffect(() => {
    if (myElement.current == null || shouldSelectOnFocus === 'no-text-selection') {
      return
    }

    const range = document.createRange()
    range.selectNodeContents(myElement.current)
    const selection = window.getSelection()
    if (selection != null) {
      selection.removeAllRanges()
      selection.addRange(range)
    }
  }, [shouldSelectOnFocus])

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
        ...handleSetFontSizeShortcut(event, metadataRef.current, elementPath),
        ...handleSetFontWeightShortcut(event, metadataRef.current, elementPath),
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
    [dispatch, elementPath, metadataRef, passthroughProps],
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

  const filteredPassthroughProps = filterMouseHandlerProps(passthroughProps)

  // When the component to render is a simple html element we should make that contenteditable
  if (typeof component === 'string') {
    return React.createElement(component, {
      ...filteredPassthroughProps,
      ...editorProps,
    })
  }
  return React.createElement(component, filteredPassthroughProps, <span {...editorProps} />)
})

async function setSelectionToOffset(
  element: HTMLSpanElement,
  scale: number,
  cursorPosition: Coordinates | null,
) {
  const selection = window.getSelection()
  if (selection == null) {
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
    selection.removeAllRanges()
    const range = document.createRange()
    range.selectNodeContents(textNode)
    range.collapse(start != null)
    if (start != null) {
      range.setStart(textNode, start)
    }
    selection.addRange(range)
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
    const targetY = cursorPosition.y / scale
    for (let i = 0; i <= maxLength; i++) {
      const range = setRange(i)
      const rect = range.getBoundingClientRect()
      if (i > 0 && rect.x > targetX) {
        validX.push(i > 0 ? i - 1 : 0)
      }
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

function filterMouseHandlerProps(props: Record<string, any>) {
  const {
    onClick,
    onContextMenu,
    onDblClick,
    onMouseDown,
    onMouseEnter,
    onMouseLeave,
    onMouseMove,
    onMouseOut,
    onMouseOver,
    onMouseUp,
    ...filteredProps
  } = props
  return filteredProps
}
