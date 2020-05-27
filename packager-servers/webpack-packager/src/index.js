const cluster = require('express-cluster')
const os = require('os')
const app = require('./app')

const PORT = process.env.NODE_ENV === 'production' ? 80 : 5500

cluster(function(worker) {
  console.log('Hello from worker ' + worker.id)
  return app.listen(PORT)
})
