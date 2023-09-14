import * as React from 'react'
import { Link, Outlet } from '@remix-run/react'
import type { LinkProps } from '@remix-run/react'
import { useInRouterContext, Router } from 'react-router'
import type { Navigator } from 'react-router'

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

export const SafeOutlet: typeof Outlet = (props) => {
  const inRouterContext = useInRouterContext()

  if (inRouterContext) {
    return <Outlet {...props} />
  } else {
    return (
      <Router location='/' navigator={dummyNavigator}>
        <Outlet {...props} />
      </Router>
    )
  }
}
