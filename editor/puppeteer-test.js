const puppeteer = require('puppeteer')
const fs = require('fs')
const path = require('path')

puppeteerStart = async function () {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--enable-thread-instruction-count'],
  })
  const page = await browser.newPage()
  await page.goto(`file:${path.join(__dirname, 'perftest.html')}`)
  const [button] = await page.$x("//div[contains(., 'expensive computation')]")
  if (button) {
    await page.tracing.start({ path: 'trace.json' })
    await button.click()
    await page.waitForSelector('#done', { visible: true })
    await page.tracing.stop()
  }
  await browser.close()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)
  const performanceMeasureEvents = traceJson.traceEvents.filter((e) => e.name === 'SLOW_THING')
  const beginEvent = performanceMeasureEvents.find((e) => e.ph === 'b')
  const endEvent = performanceMeasureEvents.find((e) => e.ph === 'e')
  const time = (endEvent.ts - beginEvent.ts) / 1000

  const frameTimeEvents = traceJson.traceEvents.filter((e) => e.name.startsWith('animation_frame_'))
  const frameStarts = new Map()
  const frameEnds = new Map()

  frameTimeEvents.forEach((fte) => {
    const frameID = fte.name.split('animation_frame_')[1]
    if (fte.ph === 'b') {
      frameStarts.set(frameID, fte.ts)
    }
    if (fte.ph === 'e') {
      frameEnds.set(frameID, fte.ts)
    }
  })

  let frameTimes = []
  let totalFrameTimes = 0
  frameEnds.forEach((_, i) => {
    const frameTime = (frameEnds.get(i) - frameStarts.get(i)) / 1000
    frameTimes.push(frameTime)
    totalFrameTimes += frameTime
  })

  const frameAvg = totalFrameTimes / frameTimes.length

  console.info(
    `::set-output name=perf-result::"${time}ms – average frame length: ${frameAvg} – frame times: [${frameTimes.join(
      ',',
    )}]"`,
  )
}

puppeteerStart()
