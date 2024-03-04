import { createRequestHandler } from '@remix-run/express'
import express from 'express'
import cors from 'cors'
import { createProxyMiddleware } from 'http-proxy-middleware'

import * as build from './build/index.js'

const proxyMiddleware = createProxyMiddleware({
  target: 'http://localhost:8001',
  changeOrigin: true,
  ws: true,
})

const app = express()
app.use(
  cors({
    origin: 'http://localhost:8000',
    credentials: true,
  }),
)

app.use('/v1', proxyMiddleware)
app.use('/p', proxyMiddleware)
app.use('/editor', proxyMiddleware)
app.use('/hashed-assets.json', proxyMiddleware)
app.use('/vscode', proxyMiddleware)

app.all('*', createRequestHandler({ build }))

app.use(express.static('public'))

app.listen(8002)
