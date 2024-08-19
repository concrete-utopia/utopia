import { createRequestHandler } from '@remix-run/express'
import { broadcastDevReady, installGlobals } from '@remix-run/node'
import cors from 'cors'
import express from 'express'
import * as fs from 'node:fs'
import * as http from 'node:http'
import * as path from 'node:path'
import * as url from 'node:url'
import sourceMapSupport from 'source-map-support'

// To make sure everything keeps working and the server doesn't crash, if
// there are any uncaught errors, log them out gracefully
process.on('uncaughtException', (err) => {
  console.error('server uncaught exception', err)
})
process.on('unhandledRejection', (err) => {
  console.error('server unhandled rejection', err)
})

// figlet -f isometric3 'utopia'
const asciiBanner = `
      ___                       ___           ___                     ___
     /__/\\          ___        /  /\\         /  /\\      ___          /  /\\
     \\  \\:\\        /  /\\      /  /::\\       /  /::\\    /  /\\        /  /::\\
      \\  \\:\\      /  /:/     /  /:/\\:\\     /  /:/\\:\\  /  /:/       /  /:/\\:\\
  ___  \\  \\:\\    /  /:/     /  /:/  \\:\\   /  /:/~/:/ /__/::\\      /  /:/~/::\\
 /__/\\  \\__\\:\\  /  /::\\    /__/:/ \\__\\:\\ /__/:/ /:/  \\__\\/\\:\\__  /__/:/ /:/\\:\\
 \\  \\:\\ /  /:/ /__/:/\\:\\   \\  \\:\\ /  /:/ \\  \\:\\/:/      \\  \\:\\/\\ \\  \\:\\/:/__\\/
  \\  \\:\\  /:/  \\__\\/  \\:\\   \\  \\:\\  /:/   \\  \\::/        \\__\\::/  \\  \\::/
   \\  \\:\\/:/        \\  \\:\\   \\  \\:\\/:/     \\  \\:\\        /__/:/    \\  \\:\\
    \\  \\::/          \\__\\/    \\  \\::/       \\  \\:\\       \\__\\/      \\  \\:\\
     \\__\\/                     \\__\\/         \\__\\/                   \\__\\/
`

// -----------------------------------------------------------------------------
// From: https://github.com/remix-run/remix/tree/main/templates/express
// -----------------------------------------------------------------------------
const BUILD_PATH = path.resolve('build/index.js')
const VERSION_PATH = path.resolve('build/version.txt')

const nodeEnv = process.env.NODE_ENV
console.info(`Server environment: ${nodeEnv}`)

const environment = {
  NODE_ENV: nodeEnv,
  BACKEND_URL: new URL(mustEnvOrLocalFallback('BACKEND_URL', 'http://localhost:8002')),
  EDITOR_URL: mustEnvOrLocalFallback('EDITOR_URL', 'http://localhost:8000'),
  PORT: Number.parseInt(mustEnvOrLocalFallback('PORT', '8000')),
}

// counterpart of `env.server.ts`'s mustEnvOrLocalFallback
function mustEnvOrLocalFallback(key, localFallback) {
  const value = process.env[key] ?? ''
  if (value !== '') {
    return value
  }
  if (nodeEnv === 'development' || nodeEnv === 'test') {
    return localFallback
  }
  throw new Error(`missing required environment variable ${key}`)
}

sourceMapSupport.install({
  retrieveSourceMap: function (source) {
    const match = source.startsWith('file://')
    if (match) {
      const filePath = url.fileURLToPath(source)
      const sourceMapPath = `${filePath}.map`
      if (fs.existsSync(sourceMapPath)) {
        return {
          url: source,
          map: fs.readFileSync(sourceMapPath, 'utf8'),
        }
      }
    }
    return null
  },
})
installGlobals()

async function createDevRequestHandler(initialBuild) {
  let build = initialBuild
  async function handleServerUpdate() {
    // 0. give the rebuild time to breathe
    await new Promise((res) => setTimeout(res, 1000))

    // 1. re-import the server build
    build = await reimportServer()

    // 2. tell Remix that this app server is now up-to-date and ready
    broadcastDevReady(build)
  }
  const chokidar = await import('chokidar')
  chokidar
    .watch(VERSION_PATH, { ignoreInitial: true })
    .on('add', handleServerUpdate)
    .on('change', handleServerUpdate)

  // wrap request handler to make sure its recreated with the latest build for every request
  return async (req, res, next) => {
    try {
      return createRequestHandler({
        build,
        mode: 'development',
      })(req, res, next)
    } catch (error) {
      next(error)
    }
  }
}

async function reimportServer() {
  const stat = fs.statSync(BUILD_PATH)

  // convert build path to URL for Windows compatibility with dynamic `import`
  const BUILD_URL = url.pathToFileURL(BUILD_PATH).href

  // use a timestamp query parameter to bust the import cache
  return import(BUILD_URL + '?t=' + stat.mtimeMs)
}

const initialBuild = await reimportServer()

const remixHandler =
  environment.NODE_ENV === 'development'
    ? await createDevRequestHandler(initialBuild)
    : createRequestHandler({ build: initialBuild })
// -----------------------------------------------------------------------------

function proxy(originalRequest, originalResponse) {
  let headers = new Headers()

  for (const [key, value] of Object.entries(originalRequest.headers)) {
    // add headers to ignore here, with simple comparisons to make it faster than i.e. an array lookup
    // NOTE! this should match the check in `proxy.server.ts`!
    if (key !== 'host') {
      headers[key] = value
    }
  }

  const proxyRequest = http.request(
    {
      // target the right server
      protocol: originalRequest.protocol + ':',
      host: environment.BACKEND_URL.hostname,
      port: environment.BACKEND_URL.port,

      // proxy everything
      path: originalRequest.originalUrl,
      method: originalRequest.method,
      headers: headers,
    },
    (proxyResponse) => {
      originalResponse.writeHead(proxyResponse.statusCode, proxyResponse.headers)
      proxyResponse.pipe(originalResponse)
    },
  )

  originalRequest
    .pipe(proxyRequest)
    // if the request fails for any non-explicit reason (e.g. ERRCONNREFUSED),
    // handle the error gracefully and return a common status code back to the client
    .on('error', (err) => {
      console.error('failed proxy request', err)
      originalResponse.status(502).json({ error: 'Service temporarily unavailable' })
    })
}

const corsMiddleware = cors({
  origin: environment.EDITOR_URL,
  credentials: true,
})

const app = express()

app.disable('x-powered-by')

// proxy middleware hooks
app.use('/editor', proxy)
app.use('/hashed-assets.json', proxy)
app.use('/logout', proxy)
app.use('/share', proxy)
app.use('/sockjs-node', proxy)
app.use('/v1/javascript/packager', proxy)
app.use('/vscode', proxy)

// other middlewares
app.use((_req, res, next) => {
  res.setHeader('cross-origin-resource-policy', 'cross-origin')
  next()
}, corsMiddleware)

// static files
app.use('/build', express.static('public/build', { immutable: true, maxAge: '1y' }))
app.use(express.static('public'))

// remix handler
app.all('*', remixHandler)

function listenCallback(portNumber) {
  return () => {
    console.log(asciiBanner)
    console.log(`Express listening on http://localhost:${portNumber}`)
    if (environment.NODE_ENV === 'development') {
      broadcastDevReady(initialBuild)
    }
  }
}

app.listen(environment.PORT, listenCallback(environment.PORT))
if (environment.NODE_ENV === 'development') {
  app.listen(environment.PORT + 1, listenCallback(environment.PORT + 1))
}
