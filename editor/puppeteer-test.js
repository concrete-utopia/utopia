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
  // console.log(traceData);
  if (traceData.includes(',"ticount":')) {
    console.info('ticount support is enabled!')
  } else {
    console.info('ticount support is disabled!')
    process.exitCode = 1
  }
}

puppeteerStart()
