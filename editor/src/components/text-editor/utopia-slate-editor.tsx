import React from 'react'
import { createEditor } from 'slate'
import {
  DefaultElement,
  DefaultLeaf,
  Editable,
  RenderElementProps,
  RenderLeafProps,
  Slate,
  withReact,
} from 'slate-react'
import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'

interface UtopiaSlateEditorProps {
  elementPath: ElementPath
  text: string
}

export const UtopiaSlateEditor: React.FC<UtopiaSlateEditorProps> = (
  props: UtopiaSlateEditorProps,
) => {
  const renderElement = React.useCallback(
    (innerProps: RenderElementProps) => <DefaultElement {...innerProps} />,
    [],
  )
  const renderLeaf = React.useCallback(
    (innerProps: RenderLeafProps) => <DefaultLeaf {...innerProps} />,
    [],
  )
  const onKeyDown = React.useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Escape') {
      releaseTextEditorFocus()
    } else {
      event.stopPropagation()
    }
  }, [])
  const editor = withReact(createEditor())
  const initialValue = [
    {
      type: 'paragraph',
      children: [{ text: props.text }],
    },
  ]

  return (
    <Slate editor={editor} value={initialValue}>
      <Editable
        id={getSlateEditorId(props.elementPath)}
        renderElement={renderElement}
        renderLeaf={renderLeaf}
        spellCheck
        autoFocus
        onKeyDown={onKeyDown}
      />
    </Slate>
  )
}

let activeTextEditorElement: HTMLElement | null = null
function forceFocusCallback() {
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.focus()
}

export function forceTextEditorFocus(elementPath: ElementPath): void {
  releaseTextEditorFocus()
  activeTextEditorElement = document.getElementById(getSlateEditorId(elementPath))
  if (activeTextEditorElement != null) {
    activeTextEditorElement.addEventListener('blur', () => activeTextEditorElement?.focus())
    activeTextEditorElement.focus()
  }
}

export function releaseTextEditorFocus(): void {
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.removeEventListener('blur', forceFocusCallback)
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.blur()
}

function getSlateEditorId(elementPath: ElementPath) {
  return `slate-editor-${EP.toString(elementPath)}`
}
