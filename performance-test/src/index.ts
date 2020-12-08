import { puppeteerStart } from './puppeteer-test'

puppeteerStart().catch((e) => {
  console.info(
    `::set-output name=perf-result::"There was an error with Puppeteer: ${e.name} – ${e.message}"`,
  )
  return
})
