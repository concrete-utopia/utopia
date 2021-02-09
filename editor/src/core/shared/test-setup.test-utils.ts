export function setElectronWindow(done: jest.DoneCallback): void {
  // we need to set the Electron window to a larger size so document.elementsUnderPoint works correctly!
  const currentWindow = require('electron').remote.getCurrentWindow()
  const [width, height] = currentWindow.getSize()
  if (width < 2200 || height < 1000) {
    currentWindow.once('resize', () => {
      done()
    })
    currentWindow.setSize(2200, 1000)
  } else {
    done()
  }
}
