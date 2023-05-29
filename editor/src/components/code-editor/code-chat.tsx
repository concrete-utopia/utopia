import React from 'react'
import { FlexColumn, FlexRow, SimpleFlexColumn, StringInput, useColorTheme } from '../../uuiui'
import { atom, useAtom } from 'jotai'
import { useRefEditorState } from '../editor/store/store-hook'
import { getContentsTreeFileFromElements, getProjectContentKeyPathElements } from '../assets'
import { useDispatch } from '../editor/store/dispatch-context'
import { updateFileContents } from '../../core/model/project-file-utils'
import { updateFile } from '../editor/actions/action-creators'
import {
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../core/shared/project-file-types'
import { NO_OP } from '../../core/shared/utils'
import { StoryboardFilePath } from '../editor/store/editor-state'

const ChatMessagesAtom = atom<string[]>([])

interface ChatTabProps {
  height: number
}

export const ChatTab = React.memo((props: ChatTabProps) => {
  const [chatMessages, setChatMessages] = useAtom(ChatMessagesAtom)
  const [composedMessage, setComposedMessage] = React.useState<string>('')
  const { height } = props

  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const endMarkerRef = React.useRef<HTMLDivElement | null>(null)
  const inputRef = React.useRef<HTMLInputElement | null>(null)

  const containerStyle: React.CSSProperties = React.useMemo(
    () => ({ height: height - 32, width: '100%', border: '1px solid red' }),
    [height],
  )

  const messagesContainerStyle: React.CSSProperties = React.useMemo(
    () => ({ overflow: 'scroll', flexShrink: 1, flexGrow: 1 }),
    [],
  )

  const inputRowContainerStyle: React.CSSProperties = React.useMemo(() => ({ flex: 0, gap: 5 }), [])

  const sendButtonStyles = React.useMemo(
    () => ({
      backgroundColor: colorTheme.buttonBackground.value,
      color: colorTheme.fg0.value,
      cursor: 'pointer',
      padding: '5px 5px 5px 5px',
      width: 'max-content',
      height: 'max-content',
      borderRadius: 6,
    }),
    [colorTheme.buttonBackground.value, colorTheme.fg0.value],
  )

  const openFilePathRef = useRefEditorState((store) => store.editor.canvas.openFile?.filename)

  const openFileContentsRef = useRefEditorState((store) => {
    const openFilePath = store.editor.canvas.openFile?.filename
    if (openFilePath == null) {
      return null
    }

    const fileContents = getContentsTreeFileFromElements(
      store.editor.projectContents,
      getProjectContentKeyPathElements(openFilePath),
    )

    if (fileContents == null || fileContents.type != 'TEXT_FILE') {
      return null
    }

    return fileContents.fileContents.code
  })

  const updateComposedMessage = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    setComposedMessage(event.currentTarget.value)
  }, [])

  const reset = React.useCallback(() => setChatMessages([]), [setChatMessages])

  const sendMessage = React.useCallback(() => {
    const openFilePath = openFilePathRef.current
    if (composedMessage.length < 1 || openFilePath == null) {
      return
    }

    const newMessages = [...chatMessages, composedMessage]
    setChatMessages(newMessages)
    setComposedMessage('')
    void promptGPT(
      PROMPT(openFileContentsRef.current ?? '', newMessages, openFilePath === StoryboardFilePath),
    )
      .then(
        (result) =>
          result != null &&
          dispatch([
            updateFile(
              openFilePath,
              textFile(
                textFileContents(result, unparsed, RevisionsState.CodeAhead),
                null,
                null,
                Date.now(),
              ),
              true,
            ),
          ]),
      )
      .catch(NO_OP) // console.log
  }, [
    chatMessages,
    composedMessage,
    dispatch,
    openFileContentsRef,
    openFilePathRef,
    setChatMessages,
  ])

  const onInputKeyDown = React.useCallback(
    (event: React.KeyboardEvent) => {
      if (event.key !== 'Enter') {
        return
      }

      sendMessage()
    },
    [sendMessage],
  )

  React.useEffect(() => {
    inputRef.current?.focus()
    endMarkerRef.current?.scrollIntoView({ behavior: 'auto' })
  }, [])

  return (
    <FlexColumn style={containerStyle}>
      <FlexColumn style={messagesContainerStyle}>
        {chatMessages.map((m, idx) => (
          <div key={idx}>{m}</div>
        ))}
        <div ref={endMarkerRef} />
      </FlexColumn>
      <FlexRow style={inputRowContainerStyle}>
        <div style={sendButtonStyles} onClick={reset}>
          Reset
        </div>
        <StringInput // TODO: maybe Textarea?
          ref={inputRef}
          testId='chat-input'
          placeholder='Type prompt...'
          value={composedMessage}
          onChange={updateComposedMessage}
          onKeyDown={onInputKeyDown}
          style={{ flexGrow: 1 }}
        />
        <div style={sendButtonStyles} onClick={sendMessage}>
          Send
        </div>
      </FlexRow>
    </FlexColumn>
  )
})

const PROMPT = (
  openFileContents: string,
  messages: string[],
  isStoryboardPath: boolean,
) => `Instructions:
- only output code, no prose is needed
- use React and Javascript in the generated code
- only use inline styles in the generated code
- the generated code should use \`flex\` or \`position: absolute\` when possible
${
  isStoryboardPath
    ? `- the generated code should look like the following:
\`\`\`
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 478,
        height: 777,
        position: 'absolute',
        left: 20,
        top: 128,
      }}
      data-label='Playground'
    >
       <<<GENERATED CODE GOES HERE>>>
    </Scene>
  </Storyboard>
)
\`\`\``
    : ''
}
- Some code already exists that you can use to create your response:
\`\`\`
${openFileContents}
\`\`\`
- output all the code necessary to make your suggestion work
- follow the instructions below:
${messages.join('\n')}
`

async function promptGPT(prompt: string): Promise<string | null> {
  const key = 'REDACTED'
  const requestOptions = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: 'Bearer ' + key,
    },
    body: JSON.stringify({
      model: 'gpt-3.5-turbo',
      messages: [{ role: 'user', content: prompt }],
    }),
  }
  const response = await fetch('https://api.openai.com/v1/chat/completions', requestOptions)
  const data = await response.json()

  if (!response.ok) {
    return null
  }

  return data.choices[0]?.message?.content ?? null // TODO
}
