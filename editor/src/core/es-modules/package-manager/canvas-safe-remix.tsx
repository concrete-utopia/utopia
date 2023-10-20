import * as React from 'react'
import { Link, Outlet } from '@remix-run/react'
import type { LinkProps } from '@remix-run/react'
import { useInRouterContext, Router } from 'react-router'
import type { Navigator } from 'react-router'
import { OutletPathContext } from '../../../components/canvas/remix/remix-utils'
import { UTOPIA_PATH_KEY } from '../../model/utopia-constants'
import * as EP from '../../shared/element-path'

const dummyNavigator: Navigator = {
  createHref: () => '',
  go: (n) => {
    /* Do Nothing */
  },
  push: (to, state, opts) => {
    /* Do Nothing */
  },
  replace: (to, state, opts) => {
    /* Do Nothing */
  },
}

export const SafeLink: typeof Link = React.forwardRef<HTMLAnchorElement, LinkProps>(
  (props, forwardRef) => {
    const inRouterContext = useInRouterContext()

    if (inRouterContext) {
      return <Link {...props} />
    } else {
      return (
        <Router location='/' navigator={dummyNavigator}>
          <Link {...props} ref={forwardRef} />
        </Router>
      )
    }
  },
)

type SafeOutletProps = typeof Outlet & {
  [UTOPIA_PATH_KEY]?: string
}

export const SafeOutlet = (props: SafeOutletProps): JSX.Element => {
  const inRouterContext = useInRouterContext()
  const pathString = props[UTOPIA_PATH_KEY]
  if (pathString == null) {
    throw new Error('Outlets rendered on the canvas should have a data-path prop')
  }

  if (inRouterContext) {
    return (
      <OutletPathContext.Provider value={EP.fromString(pathString)}>
        <Outlet {...props} />
      </OutletPathContext.Provider>
    )
  } else {
    return (
      <OutletPathContext.Provider value={EP.fromString(pathString)}>
        <Router location='/' navigator={dummyNavigator}>
          <Outlet {...props} />
        </Router>
      </OutletPathContext.Provider>
    )
  }
}

// Why are these causing issues?
export const PrefetchPageLinks = () => null
export const Links = () => null
export const Meta = () => null
export const Scripts = () => null
