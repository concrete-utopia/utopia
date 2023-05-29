import React from 'react'
import { FlexColumn, FlexRow, SimpleFlexColumn, StringInput, useColorTheme } from '../../uuiui'
import { atom, useAtom } from 'jotai'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { ProjectContentTreeRoot, getContentsTreeFileFromElements } from '../assets'

const ChatMessagesAtom = atom<string[]>([])

interface ChatTabProps {
  height: number
}

export const ChatTab = React.memo((props: ChatTabProps) => {
  const [chatMessages, setChatMessages] = useAtom(ChatMessagesAtom)
  const [composedMessage, setComposedMessage] = React.useState<string>('')
  const { height } = props

  const colorTheme = useColorTheme()

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

  const updateComposedMessage = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    setComposedMessage(event.currentTarget.value)
  }, [])

  const sendMessage = React.useCallback(() => {
    const newMessages = [...chatMessages, composedMessage]
    setChatMessages(newMessages)
    setComposedMessage('')
  }, [chatMessages, composedMessage, setChatMessages])

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
