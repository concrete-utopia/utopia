import React from 'react'
import { EditorDispatch } from '../action-types'

export const DispatchContext = React.createContext<EditorDispatch | null>(null)
DispatchContext.displayName = 'DispatchContext'

export function useDispatch(): EditorDispatch {
  return React.useContext(DispatchContext)!
}
