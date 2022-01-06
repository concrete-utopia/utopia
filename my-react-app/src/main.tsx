import React from 'react'
import ReactDOM from 'react-dom'
import './index.css'
import App from './App'

import { createWorker } from './worker-util'

const worker = createWorker()

ReactDOM.render(
  <React.StrictMode>
    <App worker={worker} />
  </React.StrictMode>,
  document.getElementById('root'),
)
