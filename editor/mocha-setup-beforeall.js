beforeEach(function () {
  viewport.set(2200, 1000)
  if ('resetGlobalPositions' in globalThis) {
    globalThis.resetGlobalPositions()
  }
})
