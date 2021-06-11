const cdnUrl = process.env.UTOPIA_CDN_URL

module.exports = {
  trailingSlash: true,
  assetPrefix: cdnUrl ?? '', // Use the CDN in production and localhost for development.
}

console.log('Running Next with config:', JSON.stringify(module.exports))
