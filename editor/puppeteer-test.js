const puppeteer = require('puppeteer')
const fs = require('fs')

puppeteerStart = async function () {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--enable-thread-instruction-count'],
  })
  const page = await browser.newPage()
  await page.tracing.start({ path: 'trace.json' })
  await page.goto('https://google.com/')
  await page.tracing.stop()
  await browser.close()
  let traceData = fs.readFileSync('trace.json').toString()
  // console.log(traceData);
  if (traceData.includes(',"ticount":')) {
    console.info('ticount support is enabled!')
  } else {
    console.info('ticount support is disabled!')
  }
}

puppeteerStart()
