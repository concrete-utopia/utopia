import { testPerformance } from './puppeteer-test'

// Execute the one (and only so far) performance test we have.
testPerformance().catch((e) => {
  const errorMessage = `"There was an error with Puppeteer: ${e.name} – ${e.message}"`
  console.info(`::set-output name=perf-result::${errorMessage}`)

  // Output the individual parts for building a discord message
  console.info(`::set-output name=perf-message-staging:: ${errorMessage}`)
  console.info(`::set-output name=perf-chart-staging:: ""`)
  console.info(`::set-output name=perf-message-master:: ""`)
  console.info(`::set-output name=perf-chart-master:: ""`)
  return
})
