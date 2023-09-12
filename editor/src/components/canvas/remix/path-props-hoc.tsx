import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'

export function PathPropHOC<P>(Wrapped: React.ComponentType<P>, path: string) {
  return (props: P): JSX.Element => {
    const propsWithPath = {
      [UTOPIA_PATH_KEY]: path,
      ...props,
    }
    return <Wrapped {...propsWithPath} />
  }
}
