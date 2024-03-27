import type { LoaderFunctionArgs } from '@remix-run/node'
import { Link } from '@remix-run/react'
import React from 'react'
import urlJoin from 'url-join'
import { useAppStore } from '../stores/appStore'
import { requireUser } from '../util/api.server'
import { auth0LoginURL } from '../util/auth0.server'

export async function loader(args: LoaderFunctionArgs) {
  await requireUser(args.request, { redirect: auth0LoginURL() })
  return {}
}

const SettingsPage = React.memo(() => {
  const env = useAppStore((store) => store.env)
  return (
    <div>
      <Link to={urlJoin(env?.EDITOR_URL ?? '', '/logout')}>Log out</Link>
    </div>
  )
})

export default SettingsPage
