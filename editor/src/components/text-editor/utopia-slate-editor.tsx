import React from 'react'
import { createEditor, Descendant, Node } from 'slate'
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
import { useEditorState } from '../editor/store/store-hook'
import { updateChildText } from '../editor/actions/action-creators'

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
  const dispatch = useEditorState((store) => store.dispatch, 'useEditorState dispatch')
  const onKeyDown = React.useCallback((event: React.KeyboardEvent) => {
    if (event.key === 'Escape') {
      releaseTextEditorFocus()
    } else {
      event.stopPropagation()
    }
  }, [])
  const contentRef = React.useRef(props.text)
  const onChange = React.useCallback(
    (value: Array<Descendant>) => {
      if (value.length !== 1) {
        return
      }
      const content = Node.string(value[0])
      if (content !== contentRef.current) {
        contentRef.current = content
        dispatch([updateChildText(props.elementPath, content)])
      }
    },
    [props.elementPath, dispatch],
  )

  const editor = withReact(createEditor())
  const nodes = [
    {
      type: 'paragraph',
      children: [{ text: contentRef.current }],
    },
  ]

  return (
    <Slate editor={editor} value={nodes} onChange={onChange}>
      <Editable
        id={getSlateEditorId(props.elementPath)}
        renderElement={renderElement}
        renderLeaf={renderLeaf}
        spellCheck
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
    activeTextEditorElement.addEventListener('blur', forceFocusCallback)
    activeTextEditorElement.focus()
  }
}

export function releaseTextEditorFocus(): void {
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.removeEventListener('blur', forceFocusCallback)
  // eslint-disable-next-line no-unused-expressions
  activeTextEditorElement?.blur()
}

export function getSlateEditorId(elementPath: ElementPath): string {
  return `slate-editor-${EP.toString(elementPath)}`
}
