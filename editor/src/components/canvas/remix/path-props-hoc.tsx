import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import type { ElementPath } from '../../../core/shared/project-file-types'

export function* getPaths(paths: ElementPath[]): Generator<ElementPath, void, unknown> {
  for (const path of paths) {
    yield path
  }
}

export function PathPropHOC<P>(Wrapped: React.ComponentType<P>, path: () => string) {
  return (props: P): JSX.Element => {
    const propsWithPath = {
      [UTOPIA_PATH_KEY]: path(),
      ...props,
    }
    return <Wrapped {...propsWithPath} />
  }
}
