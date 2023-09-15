import React from 'react'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { OutletPathContext } from './remix-utils'
import * as EP from '../../../core/shared/element-path'

type PropsWithPath<OriginalProps> = OriginalProps & { [UTOPIA_PATH_KEY]: string }

type WrapperProps<P> = {
  Wrapped: React.ComponentType<PropsWithPath<P>>
  originalProps: P
}

function Wrapper<P>({ Wrapped, originalProps }: WrapperProps<P>): JSX.Element {
  const outletContext = React.useContext(OutletPathContext)
  if (outletContext == null) {
    throw new Error('Route module should be rendered in an OutletPathContext')
  }

  const propsWithPath = {
    [UTOPIA_PATH_KEY]: EP.toString(outletContext),
    ...originalProps,
  }
  return <Wrapped {...propsWithPath} />
}

export function PathPropHOC<P>(Wrapped: React.ComponentType<PropsWithPath<P>>) {
  return (props: P): JSX.Element => {
    return <Wrapper Wrapped={Wrapped} originalProps={props} />
  }
}
