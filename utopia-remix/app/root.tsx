/* eslint-disable react/react-in-jsx-scope */
// FIXME Move the eslint and prettier config files and scripts to the root level

import { Theme } from '@radix-ui/themes'
import radixStyle from '@radix-ui/themes/styles.css'
import { cssBundleHref } from '@remix-run/css-bundle'
import type { HeadersFunction, LinksFunction } from '@remix-run/node'
import {
  Links,
  LiveReload,
  Meta,
  Outlet,
  Scripts,
  ScrollRestoration,
  json,
  useLoaderData,
  useRouteError,
} from '@remix-run/react'
import React from 'react'
import { BrowserEnvironment } from './env.server'
import { useIsDarkMode } from './hooks/useIsDarkMode'
import './normalize.css'
import { AppContext, createAppStore } from './stores/appStore'
import { styles } from './styles/styles.css'
import type { ErrorWithStatus } from './util/errors'
import { isErrorWithStatus } from './util/errors'
import { Status, getStatusName } from './util/statusCodes'
import { useIsClientReady } from './hooks/use-client-ready'

export const links: LinksFunction = () => [
  ...(cssBundleHref ? [{ rel: 'stylesheet', href: cssBundleHref }] : []),
  { rel: 'stylesheet', href: radixStyle },
]

export async function loader() {
  return json({
    env: BrowserEnvironment,
  })
}

export const rootLoader = loader

export const headers: HeadersFunction = () => ({
  'cache-control': 'no-cache',
  'cross-origin-embedder-policy': 'require-corp',
})

export default function App() {
  const data = useLoaderData<typeof loader>()
  const isDarkMode = useIsDarkMode()

  const store = React.useRef(createAppStore({ env: data.env })).current

  const theme = isDarkMode ? 'dark' : 'light'

  return (
    <html lang='en'>
      <head>
        <meta charSet='utf-8' />
        <meta name='viewport' content='width=device-width, initial-scale=1' />
        <title>Utopia</title>
        <meta property='og:title' content='Utopia' />
        <Meta />
        <Links />
      </head>
      <body className={styles.root}>
        <AppContext.Provider value={store}>
          <Theme appearance={theme} accentColor='blue' panelBackground='solid'>
            <OutletWrapper />
          </Theme>
        </AppContext.Provider>
        <ScrollRestoration />
        <Scripts />
        <LiveReload />
      </body>
    </html>
  )
}

const OutletWrapper = React.memo(() => {
  // Only render the outlet if the client is ready.
  const ready = useIsClientReady()
  if (!ready) {
    return null
  }

  return <Outlet />
})

export function ErrorBoundary() {
  const routeError = useRouteError()
  const error: ErrorWithStatus = isErrorWithStatus(routeError)
    ? routeError
    : routeError instanceof Error
    ? {
        status: Status.INTERNAL_ERROR,
        statusText: getStatusName(Status.INTERNAL_ERROR),
        data: routeError.message,
      }
    : {
        status: Status.INTERNAL_ERROR,
        statusText: getStatusName(Status.INTERNAL_ERROR),
        data: JSON.stringify(routeError),
      }

  return (
    <html lang='en'>
      <head>
        <meta charSet='utf-8' />
        <meta name='viewport' content='width=device-width, initial-scale=1' />
        <title>Utopia</title>
        <meta property='og:title' content='Utopia' />
      </head>
      <body
        style={{
          fontFamily: 'Inter, sans-serif',
          height: '100vh',
          width: '100vw',
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          textAlign: 'center',
          padding: 0,
          margin: 0,
          gap: 10,
        }}
      >
        <h1>
          {error.status} – {error.statusText}
        </h1>
        <div>{error.data}</div>
      </body>
    </html>
  )
}
