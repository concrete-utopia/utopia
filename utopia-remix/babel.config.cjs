// We only use this for Jest for now

module.exports = (api) => {
  const isTest = api.env('test') // we can use this to determine if we are in a Jest env

  return {
    presets: ['@babel/preset-typescript', '@babel/preset-env'],
  }
}
