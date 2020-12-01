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
    await page.tracing.stop()
  }
  await browser.close()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)
  const performanceMeasureEvents = traceJson.traceEvents.filter((e) => e.name === 'SLOW_THING')
  const beginEvent = performanceMeasureEvents.find((e) => e.ph === 'b')
  const endEvent = performanceMeasureEvents.find((e) => e.ph === 'e')
  const time = endEvent.ts - beginEvent.ts
  console.info('time!', time)
  console.info(`::set-output name=perf-result::${time}ms`)
}

puppeteerStart()
