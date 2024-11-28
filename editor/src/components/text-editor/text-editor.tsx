import { unescape } from 'he'
import type { CSSProperties } from 'react'
import React from 'react'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { keyCharacterFromCode } from '../../utils/keyboard'
import { Modifier } from '../../utils/modifiers'
import {
  adjustFontSize,
  getFontSize,
  isAdjustFontSizeShortcut,
  adjustFontWeight,
  getFontWeightFromMetadata,
  isAdjustFontWeightShortcut,
} from '../canvas/canvas-strategies/strategies/text-editor-utils'
import { setProperty } from '../canvas/commands/set-property-command'
import type { ApplyCommandsAction, EditorAction, EditorDispatch } from '../editor/action-types'
import {
  applyCommandsAction,
  deleteView,
  updateText,
  updateEditorMode,
  showToast,
} from '../editor/actions/action-creators'
import type { Coordinates } from '../editor/editor-modes'
import { EditorModes } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import { MainEditorStoreProvider } from '../editor/store/store-context-providers'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { DOMEventHandlerNames, printCSSNumber } from '../inspector/common/css-utils'
import {
  toggleTextBold,
  toggleTextItalic,
  toggleTextStrikeThrough,
  toggleTextUnderline,
} from './text-editor-shortcut-helpers'
import { useColorTheme } from '../../uuiui'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
import type { TextRelatedCssProperties } from '../../core/properties/css-properties'
import { TextRelatedProperties } from '../../core/properties/css-properties'
import { assertNever } from '../../core/shared/utils'
import { notice } from '../common/notice'
import type { AllElementProps } from '../editor/store/editor-state'
import { toString } from '../../core/shared/element-path'
import type { SteganoTextData } from '../../core/shared/stegano-text'
import { cleanSteganoTextData, decodeSteganoData } from '../../core/shared/stegano-text'
import { useUpdateStringRun } from '../../core/model/project-file-helper-hooks'
import { STEGANOGRAPHY_ENABLED } from '../../utils/feature-switches'

export const TextEditorSpanId = 'text-editor'

export type TextProp = 'itself' | 'child' | 'whenTrue' | 'whenFalse' | 'fullConditional'

export interface TextEditorProps {
  elementPath: ElementPath
  text: string
  originalText: string | null
  component: React.ComponentType<React.PropsWithChildren<any>>
  passthroughProps: Record<string, any>
  filePath: string
  textProp: TextProp
}

const entities = {
  lesserThan: '&lt;',
  greaterThan: '&gt;',
  curlyBraceLeft: '&#123;',
  curlyBraceRight: '&#125;',
}

// canvas → editor
export function escapeHTML(s: string, textProp: TextProp): string {
  const withoutNewLines = s
    // a trailing newline is added by contenteditable for multiline strings, so get rid of it
    .replace(/\n$/, '')

  //encode < and > when necessary
  const encoded = encodeHTMLWhenNotInJsCode(withoutNewLines)

  switch (textProp) {
    case 'child':
      // restore br tags
      return encoded.replace(/\n/g, '\n<br />')
    case 'itself':
    case 'fullConditional':
    case 'whenTrue':
    case 'whenFalse':
      return encoded
    default:
      assertNever(textProp)
  }
}

// This is a very basic function to separate the real text content and the JS content in curly brackets
// We only want to html encode in text content, but not in js content
// E.g. we don't want to encode the < in `{ 2>3 ? "foo" : "bar" }
function encodeHTMLWhenNotInJsCode(s: string): string {
  let result = ''
  let parenCounter = 0
  let inSingleQuotation = false
  let inDoubleQuotation = false

  const isInJSExpression = () => parenCounter > 0

  for (let ch of s) {
    let characterToPrint = ch
    switch (ch) {
      case '{':
        if (!inSingleQuotation && !inDoubleQuotation) {
          parenCounter++
        }
        break
      case '}':
        if (!inSingleQuotation && !inDoubleQuotation) {
          parenCounter--
        }
        break
      case '<':
        if (!isInJSExpression()) {
          characterToPrint = entities.lesserThan
        }
        break
      case '>':
        if (!isInJSExpression()) {
          characterToPrint = entities.greaterThan
        }
        break
      case '"':
        if (!inSingleQuotation) {
          inDoubleQuotation = !inDoubleQuotation
        }
        break
      case "'":
        if (!inDoubleQuotation) {
          inSingleQuotation = !inSingleQuotation
        }
        break
    }
    result += characterToPrint
  }

  return result
}

// editor → canvas
export function unescapeHTML(s: string): string {
  if (s.length === 0) {
    return ''
  }

  const unescaped = unescape(s)

  // We need to add a trailing newline so that the contenteditable can render and reach the last newline
  // if the string _ends_ with a newline.
  return unescaped + '\n'
}

const handleToggleShortcuts = (
  event: React.KeyboardEvent<Element>,
  metadataRef: {
    readonly current: ElementInstanceMetadataMap
  },
  target: ElementPath,
  dispatch: EditorDispatch,
): Array<EditorAction> => {
  const modifiers = Modifier.modifiersForEvent(event)
  const meta = modifiers.cmd || modifiers.ctrl
  const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
    metadataRef.current,
    target,
  )?.specialSizeMeasurements

  // Meta+b = bold
  if (meta && event.key === 'b') {
    toggleTextBold(
      target,
      specialSizeMeasurements?.fontWeight ?? null,
      dispatch,
      metadataRef,
      'separate-undo-step',
    )
    return []
  }
  // Meta+i = italic
  if (meta && event.key === 'i') {
    toggleTextItalic(
      target,
      specialSizeMeasurements?.fontStyle ?? null,
      dispatch,
      metadataRef,
      'separate-undo-step',
    )
    return []
  }
  // Meta+u = underline
  if (meta && event.key === 'u') {
    toggleTextUnderline(
      target,
      specialSizeMeasurements?.textDecorationLine ?? null,
      dispatch,
      metadataRef,
      'separate-undo-step',
    )
    return []
  }
  // Meta+shift+x = strikethrough
  if (meta && modifiers.shift && event.key === 'x') {
    toggleTextStrikeThrough(
      target,
      specialSizeMeasurements?.textDecorationLine ?? null,
      dispatch,
      metadataRef,
      'separate-undo-step',
    )
    return []
  }
  return []
}

const handleSetFontSizeShortcut = (
  event: React.KeyboardEvent<Element>,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<ApplyCommandsAction> => {
  const modifiers = Modifier.modifiersForEvent(event)
  const character = keyCharacterFromCode(event.keyCode)
  const matches = isAdjustFontSizeShortcut(modifiers, character)

  if (!matches) {
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
        PP.create('style', 'fontSize'),
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

  if (!matches) {
    return []
  }

  const delta = character === 'period' ? 1 : character === 'comma' ? -1 : 0

  const fontWeight = getFontWeightFromMetadata(metadata, elementPath)
  if (fontWeight == null) {
    return []
  }

  return [
    applyCommandsAction([
      setProperty(
        'always',
        elementPath,
        PP.create('style', 'fontWeight'),
        adjustFontWeight(fontWeight, delta),
      ),
    ]),
  ]
}

export const TextEditorWrapper = React.memo((props: TextEditorProps) => {
  return (
    <MainEditorStoreProvider>
      <TextEditor {...props} />
    </MainEditorStoreProvider>
  )
})

type TextEditedText =
  | { type: 'untracked'; contents: string }
  | { type: 'tracked'; contents: string; data: SteganoTextData; quote: string }

function getTextToUse({
  text,
  originalText,
}: {
  text: string
  originalText: string | null
}): TextEditedText {
  if (STEGANOGRAPHY_ENABLED && originalText != null) {
    const data = decodeSteganoData(originalText)
    if (data != null) {
      const { cleaned } = cleanSteganoTextData(originalText)
      return {
        type: 'tracked',
        contents: cleaned,
        data: data,
        quote: data.originalString[0],
      }
    }
  }
  return { type: 'untracked', contents: text }
}

const TextEditor = React.memo((props: TextEditorProps) => {
  const { elementPath, component, passthroughProps, textProp } = props

  const textToUse = React.useMemo(
    () => getTextToUse({ text: props.text, originalText: props.originalText }),
    [props.text, props.originalText],
  )

  const dispatch = useDispatch()
  const cursorPosition = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.mode.type === 'textEdit' ? store.editor.mode.cursorPosition : null),
    'TextEditor cursor position',
  )
  const elementState = useEditorState(
    Substores.restOfEditor,
    (store) => (store.editor.mode.type === 'textEdit' ? store.editor.mode.elementState : null),
    'TextEditor element state',
  )
  const shouldSelectOnFocus = useEditorState(
    Substores.restOfEditor,
    (store) =>
      store.editor.mode.type === 'textEdit' ? store.editor.mode.selectOnFocus : 'no-text-selection',
    'TextEditor shouldSelectOnFocus',
  )

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const savedContentRef = React.useRef<string | null>(null)

  const scale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'TextEditor scale',
  )

  const colorTheme = useColorTheme()
  const outlineWidth = 1 / scale
  const outlineColor = colorTheme.textEditableOutline.value

  const [firstTextProp] = React.useState(textToUse.contents)

  const myElement = React.useRef<HTMLSpanElement>(null)

  const updateStringRunCommands = useUpdateStringRun()

  React.useEffect(() => {
    const currentElement = myElement.current
    if (currentElement == null) {
      return
    }

    currentElement.focus()
    savedContentRef.current = currentElement.textContent
    const initialText = currentElement.textContent

    const elementCanvasFrame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
      elementPath,
      metadataRef.current,
    )
    if (elementCanvasFrame.width === 0) {
      currentElement.style.minWidth = '0.5px'
    }

    const canDeleteWhenEmpty = canDeleteElementWhenEmpty(
      metadataRef.current,
      elementPath,
      allElementPropsRef.current,
    )

    return () => {
      const content = currentElement.textContent
      if (content == null) {
        return
      }
      if (elementState === 'new' && content.replace(/\n/g, '') === '') {
        requestAnimationFrame(() => dispatch([deleteView(elementPath)]))
        return
      }
      if (elementState != null && savedContentRef.current !== content) {
        savedContentRef.current = content
        if (textToUse.type === 'tracked') {
          requestAnimationFrame(() => dispatch(updateStringRunCommands(textToUse.data, content)))
        } else {
          requestAnimationFrame(() => dispatch([getSaveAction(elementPath, content, textProp)]))
        }
      }

      if (
        content != null &&
        initialText !== content &&
        content.replace(/^\n/, '').length === 0 &&
        canDeleteWhenEmpty
      ) {
        requestAnimationFrame(() => dispatch([deleteView(elementPath)]))
      }
    }
  }, [
    dispatch,
    elementPath,
    elementState,
    textProp,
    metadataRef,
    allElementPropsRef,
    updateStringRunCommands,
    textToUse,
  ])

  React.useLayoutEffect(() => {
    if (myElement.current == null) {
      return
    }
    myElement.current.textContent = firstTextProp
  }, [firstTextProp])

  React.useEffect(() => {
    if (myElement.current == null) {
      setTimeout(() => {
        dispatch([
          updateEditorMode(EditorModes.selectMode(null, false, 'none')),
          showToast(
            notice(
              "This element doesn't support children, so it cannot be text edited",
              'WARNING',
              false,
              'text-editor-does-not-support-children',
            ),
          ),
        ])
      }, 0)
    }
  }, [dispatch])

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
      const shortcuts = [
        ...handleToggleShortcuts(event, metadataRef, elementPath, dispatch),
        ...handleSetFontSizeShortcut(event, metadataRef.current, elementPath),
        ...handleSetFontWeightShortcut(event, metadataRef.current, elementPath),
      ]
      if (shortcuts.length > 0) {
        event.stopPropagation()
        dispatch(shortcuts)
      }

      if (event.key === 'Tab') {
        event.preventDefault()
      }

      if (event.key === 'Escape') {
        // eslint-disable-next-line no-unused-expressions
        myElement.current?.blur()
      }

      event.stopPropagation()
    },
    [dispatch, elementPath, metadataRef],
  )

  const onBlur = React.useCallback(() => {
    const content = myElement.current?.textContent
    if (content != null && elementState != null && savedContentRef.current !== content) {
      savedContentRef.current = content
      if (textToUse.type === 'tracked') {
        requestAnimationFrame(() => {
          dispatch([
            ...updateStringRunCommands(textToUse.data, content),
            updateEditorMode(EditorModes.selectMode(null, false, 'none')),
          ])
        })
      } else {
        dispatch([
          getSaveAction(elementPath, content, textProp),
          updateEditorMode(EditorModes.selectMode(null, false, 'none')),
        ])
      }
    } else {
      dispatch([updateEditorMode(EditorModes.selectMode(null, false, 'none'))])
    }
  }, [dispatch, elementPath, elementState, textProp, textToUse, updateStringRunCommands])

  const editorProps: React.DetailedHTMLProps<
    React.HTMLAttributes<HTMLSpanElement>,
    HTMLSpanElement
  > = {
    ref: myElement,
    id: TextEditorSpanId,
    style: {
      // Ensure that font and text settings are inherited from
      // the containing element:
      ...mapArrayToDictionary<
        keyof TextRelatedCssProperties,
        'inherit',
        keyof TextRelatedCssProperties
      >(
        TextRelatedProperties,
        (key) => key,
        () => 'inherit',
      ),
      // These properties need to be set to get the positioning that
      // is required of the text editor element itself:
      display: 'inline-block',
      width: '100%',
      height: '100%',
      // text editor outline
      // Prevent double applying these properties:
      opacity: 1,
      // this is to be able to add trailing spaces (as part of the writing flow)
      // to elements where `text-wrap` doesn't allow them
      whiteSpace: 'normal',
    },
    onPaste: stopPropagation,
    onKeyDown: onKeyDown,
    onKeyUp: stopPropagation,
    onKeyPress: stopPropagation,
    // preventDefault is necessary to make sure we don't navigate away when click on anchor elements
    // It seems click events are not necessary for other built-in text editing functionality
    // (e.g. setting cursor position, drag to select, etc.), if it turns out to be a problem later,
    // we may revisit this decision
    onClick: stopPropagationAndPreventDefault,
    onContextMenu: stopPropagation,
    onMouseDown: stopPropagation,
    onMouseEnter: stopPropagation,
    onMouseLeave: stopPropagation,
    onMouseMove: stopPropagation,
    onMouseOut: stopPropagation,
    onMouseOver: stopPropagation,
    onMouseUp: stopPropagation,
    onBlur: onBlur,
    contentEditable: 'plaintext-only' as any, // note: not supported on firefox,
    suppressContentEditableWarning: true,
  }

  const filteredPassthroughProps = addBoxShadowToProps(filterEventHandlerProps(passthroughProps), {
    width: outlineWidth,
    color: outlineColor,
  })

  return React.createElement(
    component,
    filteredPassthroughProps,
    <span data-testid={TextEditorSpanId} {...editorProps} />,
  )
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

function stopPropagation(e: React.UIEvent | React.ClipboardEvent) {
  e.stopPropagation()
}

function stopPropagationAndPreventDefault(e: React.UIEvent | React.ClipboardEvent) {
  e.preventDefault()
  e.stopPropagation()
}

function filterEventHandlerProps(props: Record<string, any>) {
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
    onPaste,
    onKeyDown,
    onKeyUp,
    onKeyPress,
    ...filteredProps
  } = props
  return filteredProps
}

function addBoxShadowToProps(
  props: Record<string, any>,
  outline: { width: number; color: string },
) {
  return {
    ...props,
    style: {
      ...(props.style ?? {}),
      boxShadow: `0px 0px 0px ${outline.width}px ${outline.color}`,
    },
  }
}

function getSaveAction(
  elementPath: ElementPath,
  content: string,
  textProp: TextProp,
): EditorAction {
  return updateText(elementPath, escapeHTML(content, textProp), textProp)
}

function canDeleteElementWhenEmpty(
  jsxMetadata: ElementInstanceMetadataMap,
  path: ElementPath,
  allElementProps: AllElementProps,
): boolean {
  const element = MetadataUtils.findElementByElementPath(jsxMetadata, path)
  if (element == null) {
    return false
  }
  if (!MetadataUtils.isSpan(element)) {
    return false
  }

  const elementProps = allElementProps[toString(path)]
  if (elementProps == null) {
    return false
  }

  // it must not have defined event handlers
  if (Object.keys(elementProps).some((prop) => DOMEventHandlerNames.includes(prop as any))) {
    return false
  }

  return true
}
