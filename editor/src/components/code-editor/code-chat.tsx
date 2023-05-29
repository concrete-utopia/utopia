import React from 'react'
import { SimpleFlexColumn } from '../../uuiui'

interface ChatTabProps {
  height: number
}

export const ChatTab = React.memo((props: ChatTabProps) => {
  const { height } = props

  const style = React.useMemo(() => ({ height: height, width: '100%' }), [height])

  return <SimpleFlexColumn style={style}>Hello</SimpleFlexColumn>
})
