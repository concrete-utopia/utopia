import { cssBundleHref } from '@remix-run/css-bundle'
import type { LinksFunction } from '@remix-run/node'
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
import type { HeadersFunction } from '@remix-run/node'

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
  const error = useRouteError()
  if (error instanceof Error) {
    return `${error.name} – ${error.message}`
  }
  throw error
}
