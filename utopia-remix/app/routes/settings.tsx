import { LoaderFunctionArgs } from '@remix-run/node'
import React from 'react'
import { requireUser } from '../util/api.server'
import { Link } from '@remix-run/react'
import urlJoin from 'url-join'
import { useBrowserEnv } from '../util/use-env'
import { auth0LoginURL } from '../util/auth0.server'

export async function loader(args: LoaderFunctionArgs) {
  await requireUser(args.request, { redirect: auth0LoginURL() })
  return {}
}

const SettingsPage = React.memo(() => {
  const env = useBrowserEnv()
  return (
    <div>
      <Link to={urlJoin(env?.EDITOR_URL ?? '', '/logout')}>Log out</Link>
    </div>
  )
})

export default SettingsPage
