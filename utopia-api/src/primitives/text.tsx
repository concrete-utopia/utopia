import * as React from 'react'
import { defaultIfNull } from '../utils'
import {
  Editor as DraftEditor,
  EditorState as DraftEditorState,
  RawDraftContentState,
} from 'draft-js'
import { createContentState, createDraftStyles, getNewSizeFromContent } from '../draft-utils'
import { TextAlignProperty } from 'csstype'
import { UtopiaComponentProps } from './common'

export type TextSizing = 'auto' | 'fixed'

export interface TextProps extends React.DOMAttributes<Text>, UtopiaComponentProps {
  text: string | RawDraftContentState
  css?: any
  actionHandlers?: any
  textSizing?: TextSizing
  scale?: number
  componentSizeResult?: ((width: number | null, height: number | null) => void) | null
}

export interface TextState {
  editorState: DraftEditorState
}

// eslint-disable-next-line @typescript-eslint/no-empty-function
const NoOp = () => {}

export const Text: React.FunctionComponent<TextProps> = (props: TextProps) => {
  const defaultedStyle = defaultIfNull<React.CSSProperties>({}, props.style)
  const defaultedTextAlign: TextAlignProperty = defaultIfNull<TextAlignProperty>(
    'left',
    defaultedStyle.textAlign,
  )
  const draftEditorRef = React.createRef<DraftEditor>()
  const [draftState, setDraftState] = React.useState<TextState>(() => {
    const contentState = createContentState(props.text)
    const draftEditorState = DraftEditorState.createWithContent(contentState)
    return {
      editorState: draftEditorState,
    }
  })

  const { text } = props
  React.useEffect(() => {
    const nextContentState = createContentState(text)
    const nextEditorState = DraftEditorState.push(
      draftState.editorState,
      nextContentState,
      'insert-characters',
    )
    setDraftState({ editorState: nextEditorState })
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [text])

  // Forgive me. This is because we're using a fork of draft.js and haven't updated the types
  const DraftEditorAny = DraftEditor as any

  const isAutoSizing = props.textSizing === 'auto'
  const { customStyleFn } = createDraftStyles()

  const scaleProp = props.scale
  const componentSizeResult = props.componentSizeResult

  React.useEffect(() => {
    if (componentSizeResult != null && draftEditorRef.current != null && scaleProp != null) {
      // if componentSizeResult callback is not null then this component is just for text measurement
      // for autosizing and it has to use the callback to report the size
      const size = getNewSizeFromContent(
        draftState.editorState.getCurrentContent(),
        scaleProp,
        draftEditorRef.current,
      )
      componentSizeResult(size.width, size.height)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  const scale = props.scale != null ? props.scale : 1
  // TODO memoize me again
  // TODO text is shit!
  return (
    <div
      data-uid={props['data-uid']}
      data-label={props['data-label']}
      data-utopia-do-not-traverse={true}
      className={'canvas-text canvas-text-editable'}
      css={props.css}
      style={{
        ...props.style,
        scale: 1,
        zoom: `${(1 / scale) * 100}%`,
        transformOrigin: `top left`,
        transform: `scale(${scale})`,
      }}
      {...props.actionHandlers}
    >
      <DraftEditorAny
        key={'drafteditor'}
        ref={draftEditorRef}
        editorState={draftState.editorState}
        onChange={NoOp}
        customStyleFn={customStyleFn}
        textAlignment={defaultedTextAlign}
        whiteSpaceStyle={isAutoSizing ? 'nowrap' : 'pre-wrap'}
      />
    </div>
  )
}
Text.displayName = 'Text'
