import { testScrollingPerformance } from './puppeteer-test'

// Execute the one (and only so far) performance test we have.
testScrollingPerformance().catch((e) => {
  console.info(
    `::set-output name=perf-result::"There was an error with Puppeteer: ${e.name} – ${e.message}"`,
  )
  return
})
