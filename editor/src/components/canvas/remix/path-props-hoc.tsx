import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { OutletPathContext } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'

type WrapperProps<P> = P & {
  Wrapped: React.ComponentType<P>
  possiblePaths: Array<ElementPath>
}

function Wrapper<P>({ Wrapped, possiblePaths, ...props }: WrapperProps<P>) {
  const outletContext = React.useContext(OutletPathContext)
  if (outletContext == null) {
    throw new Error('Route module should be rendered in an OutletPathContext')
  }

  const path = possiblePaths.find((p) => EP.toUid(p) === EP.toUid(outletContext))
  if (path == null) {
    throw new Error('no relative path found for route module')
  }

  const propsWithPath = {
    [UTOPIA_PATH_KEY]: EP.toString(path),
    ...props,
  }
  return <Wrapped {...propsWithPath} />
}

export function PathPropHOC<P>(Wrapped: React.ComponentType<P>, possiblePaths: Array<ElementPath>) {
  return (props: P): JSX.Element => {
    return <Wrapper Wrapped={Wrapped} possiblePaths={possiblePaths} {...props} />
  }
}
