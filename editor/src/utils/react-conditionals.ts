import React from 'react'
import type { ReactNode } from 'react'

function elementFromElementOrFunction(element: ReactNode | (() => ReactNode)): ReactNode {
  if (React.isValidElement(element)) {
    return element
  } else if (typeof element === 'function') {
    return element()
  } else {
    return element
  }
}

export function when(condition: boolean, element: ReactNode | (() => ReactNode)): ReactNode {
  if (condition) {
    return elementFromElementOrFunction(element)
  } else {
    return null
  }
}

export function unless(condition: boolean, element: ReactNode | (() => ReactNode)): ReactNode {
  if (condition) {
    return null
  } else {
    return elementFromElementOrFunction(element)
  }
}
