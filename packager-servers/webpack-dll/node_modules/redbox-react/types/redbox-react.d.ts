import * as React from 'react'

export interface RedBoxProps extends React.Props<RedBoxError> {
  error: Error

  filename?: string
  editorScheme?: string
  useLines?: boolean
  useColumns?: boolean
  style?: React.CSSProperties
  className?: string
}

export class RedBoxError extends React.Component<RedBoxProps, {}> {}
export default class RedBox extends React.Component<RedBoxProps, {}> {}
