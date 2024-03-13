import { createRequestHandler } from '@remix-run/express'
import { broadcastDevReady, installGlobals } from '@remix-run/node'
import cors from 'cors'
import express from 'express'
import * as fs from 'node:fs'
import * as http from 'node:http'
import * as path from 'node:path'
import * as url from 'node:url'
import sourceMapSupport from 'source-map-support'

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
  process.env.NODE_ENV === 'development'
    ? await createDevRequestHandler(initialBuild)
    : createRequestHandler({ build: initialBuild })
// -----------------------------------------------------------------------------

function proxy(originalRequest, originalResponse) {
  let headers = new Headers()

  for (const [key, value] of Object.entries(originalRequest.headers)) {
    // add headers to ignore here, with simple comparisons to make it faster than i.e. an array lookup
    if (key !== 'host') {
      headers[key] = value
    }
  }

  const proxyRequest = http.request(
    {
      // target the right server
      protocol: originalRequest.protocol + ':',
      host: process.env.EXPRESS_PROXY_TARGET_HOST,
      port: process.env.EXPRESS_PROXY_TARGET_PORT,

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

  originalRequest.pipe(proxyRequest)
}

const corsMiddleware = cors({
  origin: process.env.CORS_ORIGIN,
  credentials: true,
})

const app = express()

app.disable('x-powered-by')

// proxy middleware hooks
app.use('/authenticate', proxy)
app.use('/editor', proxy)
app.use('/hashed-assets.json', proxy)
app.use('/logout', proxy)
app.use('/p', proxy)
app.use('/project', proxy)
app.use('/share', proxy)
app.use('/sockjs-node', proxy)
app.use('/v1/javascript/packager', proxy)
app.use('/vscode', proxy)

// other middlewares
app.use(corsMiddleware)

// static files
app.use('/build', express.static('public/build', { immutable: true, maxAge: '1y' }))
app.use(express.static('public'))

// remix handler
app.all('*', remixHandler)

const portFromEnv = Number.parseInt(process.env.PORT)
const port = isNaN(portFromEnv) ? 8000 : portFromEnv

function listenCallback(portNumber) {
  return () => {
    console.log(asciiBanner)
    console.log(`Express listening on http://localhost:${portNumber}`)
    if (process.env.NODE_ENV === 'development') {
      broadcastDevReady(initialBuild)
    }
  }
}

app.listen(port, listenCallback(port))
if (process.env.NODE_ENV === 'development') {
  app.listen(port + 1, listenCallback(port + 1))
}
