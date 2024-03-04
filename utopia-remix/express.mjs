import { createRequestHandler } from '@remix-run/express'
import cors from 'cors'
import express from 'express'
import * as http from 'node:http'

// Remix bundle
import * as build from './build/index.js'

function proxy(originalRequest, originalResponse) {
  const proxyRequest = http.request(
    {
      // target the right server
      protocol: originalRequest.protocol + ':',
      host: process.env.EXPRESS_PROXY_TARGET_HOST,
      port: process.env.EXPRESS_PROXY_TARGET_PORT,

      // proxy everything
      path: originalRequest.originalUrl,
      method: originalRequest.method,
      headers: originalRequest.headers,
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

app.use('/editor', proxy)
app.use('/hashed-assets.json', proxy)
app.use('/p', proxy)
app.use('/v1', proxy)
app.use('/vscode', proxy)

app.use(corsMiddleware)
app.use(express.static('public'))
app.all('*', createRequestHandler({ build }))

app.listen(process.env.PORT, () => {
  console.log(`Express listening on :${process.env.PORT}`)
})
