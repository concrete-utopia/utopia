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
import { BrowserEnvironment } from './env.server'
import { styles } from './styles/styles.css'
import { ErrorWithStatus, isErrorWithStatus } from './util/errors'
import { Status, getStatusName } from './util/statusCodes'

import './normalize.css'

declare global {
  interface Window {
    ENV: BrowserEnvironment
  }
}

export const links: LinksFunction = () => [
  ...(cssBundleHref ? [{ rel: 'stylesheet', href: cssBundleHref }] : []),
]

export async function loader() {
  return json({
    ENV: BrowserEnvironment,
  })
}

export const headers: HeadersFunction = () => ({
  'cache-control': 'no-cache',
})

export default function App() {
  const data = useLoaderData<typeof loader>()

  return (
    <html lang='en'>
      <head>
        <meta charSet='utf-8' />
        <meta name='viewport' content='width=device-width, initial-scale=1' />
        <Meta />
        <Links />
      </head>
      <body className={styles.root}>
        <Outlet />
        <script
          // https://remix.run/docs/en/1.19.3/guides/envvars#browser-environment-variables
          dangerouslySetInnerHTML={{
            __html: `window.ENV = ${JSON.stringify(data.ENV)}`,
          }}
        />
        <ScrollRestoration />
        <Scripts />
        <LiveReload />
      </body>
    </html>
  )
}

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
      </head>
      <body style={{ fontFamily: 'Inter, sans-serif' }}>
        <div>
          <h1>
            {error.status} – {error.statusText}
          </h1>
          <div>{error.data}</div>
        </div>
      </body>
    </html>
  )
}
