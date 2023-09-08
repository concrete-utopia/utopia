import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'

export const PathPropHOC = (Wrapped: any, path: string) => (props: any) => {
  const propsWithPath = {
    [UTOPIA_PATH_KEY]: path,
    ...props,
  }
  return <Wrapped {...propsWithPath} />
}
