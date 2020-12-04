const puppeteer = require('puppeteer')
const fs = require('fs')
const path = require('path')

// this is the same as utils.ts@defer
function defer() {
  var res, rej
  var promise = new Promise((resolve, reject) => {
    res = resolve
    rej = reject
  })
  Object.defineProperty(promise, 'resolve', { value: res })
  Object.defineProperty(promise, 'reject', { value: rej })

  return promise
}

/**
 *
 * @param {puppeteer.Page} page
 */

function consoleDoneMessage(page) {
  return new Promise((resolve, reject) => {
    page.on('console', (message) => {
      if (message.text().includes('SCROLL_TEST_FINISHED')) {
        resolve()
      }
    })
  })
}

puppeteerStart = async function () {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--enable-thread-instruction-count'],
    headless: true,
  })
  const page = await browser.newPage()
  // page.on('console', (message) =>
  //   console.log(`${message.type().substr(0, 3).toUpperCase()} ${message.text()}`),
  // )
  await page.goto(`https://utopia.pizza/project/5596ecdd/?branch_name=feature/perf-test-button`)
  await page.waitForXPath("//a[contains(., 'P S')]")
  const [button] = await page.$x("//a[contains(., 'P S')]")
  await button.click()
  await consoleDoneMessage(page)
  await page.tracing.start({ path: 'trace.json' })
  await button.click()
  await consoleDoneMessage(page)
  await page.tracing.stop()
  await browser.close()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)

  const frameTimeEvents = traceJson.traceEvents.filter((e) => e.name.startsWith('scroll_step_'))
  let frameTimes = []
  let lastFrameTimestamp = null
  let totalFrameTimes = 0
  frameTimeEvents.forEach((fte) => {
    const frameID = fte.name.split('scroll_step_')[1] - 1
    const frameTimestamp = fte.ts
    if (lastFrameTimestamp != null) {
      const frameDelta = (frameTimestamp - lastFrameTimestamp) / 1000
      frameTimes[frameID] = frameDelta
      totalFrameTimes += frameDelta
    }
    lastFrameTimestamp = frameTimestamp
  })

  const frameAvg = totalFrameTimes / frameTimes.length

  console.info(
    `::set-output name=perf-result::"${totalFrameTimes}ms – average frame length: ${frameAvg} – frame times: [${frameTimes.join(
      ',',
    )}]"`,
  )
}

try {
  puppeteerStart()
} catch (e) {
  console.info(
    `::set-output name=perf-result::"There was an error with Puppeteer: ${e.name} – ${e.message}"`,
  )
  return
}
