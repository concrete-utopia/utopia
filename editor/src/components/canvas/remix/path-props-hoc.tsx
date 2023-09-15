import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { OutletPathContext } from './remix-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'

type PropsWithPath<OriginalProps> = OriginalProps & { [UTOPIA_PATH_KEY]: string }

type WrapperProps<P> = {
  Wrapped: React.ComponentType<PropsWithPath<P>>
  originalProps: P
  possiblePaths: Array<ElementPath>
}

function Wrapper<P>({ Wrapped, possiblePaths, originalProps }: WrapperProps<P>): JSX.Element {
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
    ...originalProps,
  }
  return <Wrapped {...propsWithPath} />
}

export function PathPropHOC<P>(
  Wrapped: React.ComponentType<PropsWithPath<P>>,
  possiblePaths: Array<ElementPath>,
) {
  return (props: P): JSX.Element => {
    return <Wrapper Wrapped={Wrapped} possiblePaths={possiblePaths} originalProps={props} />
  }
}
