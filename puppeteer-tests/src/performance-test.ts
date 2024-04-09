/* eslint-disable no-console */
require('dotenv').config({ path: 'src/.env' })
import type * as puppeteer from 'puppeteer'
import { v4 } from 'uuid'
import { consoleDoneMessage, setupBrowser, uploadPNGtoAWS } from './utils'
import * as JSONStream from 'JSONStream'
const fs = require('fs')
const path = require('path')
const moveFile = require('move-file')

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const TARGET_BRANCH_NAME = process.env.TARGET_BRANCH_NAME
  ? `&branch_name=${process.env.TARGET_BRANCH_NAME}`
  : ''
const STAGING_EDITOR_URL =
  process.env.EDITOR_URL ??
  `https://utopia.fish/p?fakeUser=alice&code_editor_disabled=true${BRANCH_NAME}`
const MASTER_EDITOR_URL =
  process.env.MASTER_EDITOR_URL ??
  `https://utopia.fish/p?fakeUser=alice&code_editor_disabled=true${TARGET_BRANCH_NAME}`

interface FrameResult {
  title: string
  timeSeries: Array<number>
  analytics: {
    frameMin: number
    frameMax: number
    frameAvg: number
    percentile25: number | undefined
    percentile50: number | undefined
    percentile75: number | undefined
  }
  succeeded: boolean
}

const EmptyResult: FrameResult = {
  title: '',
  timeSeries: [],
  analytics: {
    frameMin: 0,
    frameMax: 0,
    frameAvg: 0,
    percentile25: undefined,
    percentile50: undefined,
    percentile75: undefined,
  },
  succeeded: true,
}

function loadTraceEventsJSON(): Promise<any> {
  const fileReader = fs.createReadStream('trace.json')
  const parser = JSONStream.parse('traceEvents.*')
  const composed = fileReader.pipe(parser)
  let result: Array<any> = []
  composed.on('data', (data: any) => {
    result.push(data)
  })
  return new Promise((resolve, reject) => {
    composed.on('end', () => {
      resolve(result)
    })
    composed.on('error', (err: any) => {
      reject(err)
    })
  })
}

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

function consoleMessageForResult(result: FrameResult, beforeOrAfter: 'Before' | 'After'): string {
  return `${beforeOrAfter}: ${result.analytics.percentile50}ms (${result.analytics.frameMin}-${result.analytics.frameMax}ms)`
}

type ResultsGroup = { [key: string]: FrameResult }
type PerformanceResults = {
  frameTests: ResultsGroup
  interactionTests: ResultsGroup
}

export const testPerformance = async function () {
  const stagingResult = await testPerformanceInner(STAGING_EDITOR_URL)
  const masterResult = await testPerformanceInner(MASTER_EDITOR_URL)

  const framesSummaryImage = await uploadSummaryImage(
    stagingResult.frameTests,
    masterResult.frameTests,
    100,
  )
  const interactionsSummaryImage = await uploadSummaryImage(
    stagingResult.interactionTests,
    masterResult.interactionTests,
    200,
  )

  const combinedStagingResult = { ...stagingResult.frameTests, ...stagingResult.interactionTests }
  const combinedMasterResult = { ...masterResult.frameTests, ...masterResult.interactionTests }

  // Explicitly flag any serious regressions found
  const seriousRegressionThresholdPercent = 20
  let seriousRegressionFound = false

  const messageParts = Object.entries(combinedStagingResult).flatMap(([k, result]) => {
    const targetResult = combinedMasterResult[k]
    const beforeMedian = targetResult.analytics.percentile50 ?? 1
    const afterMedian = result.analytics.percentile50 ?? 1
    const change = ((afterMedian - beforeMedian) / beforeMedian) * 100

    if (change > seriousRegressionThresholdPercent) {
      seriousRegressionFound = true
    }

    const titleLine = `**${result.title} (${Math.round(change)}%)**`
    const spacerLine = ''
    if (Math.abs(change) > 5) {
      return [
        titleLine,
        consoleMessageForResult(targetResult, 'Before'),
        consoleMessageForResult(result, 'After'),
        spacerLine,
      ]
    } else {
      return [titleLine, spacerLine]
    }
  })

  const message = seriousRegressionFound ? `${messageParts.join('<br />')} <br />` : ''
  const discordMessage = messageParts.join('\\n')

  console.info(
    `::set-output name=perf-result:: ${message} ![(Chart1)](${framesSummaryImage}) <br /> ![(Chart2)](${interactionsSummaryImage})`,
  )

  // Output the individual parts for building a discord message
  console.info(`::set-output name=perf-discord-message:: ${discordMessage}`)
  console.info(`::set-output name=perf-frames-chart:: ${framesSummaryImage}`)
  console.info(`::set-output name=perf-interactions-chart:: ${interactionsSummaryImage}`)
  console.info(`::set-output name=perf-serious-regression-found:: ${seriousRegressionFound}`)
}

type PageToPromiseResult<T> = (page: puppeteer.Page) => Promise<T>

async function retryPageCalls<T>(
  url: string,
  call: PageToPromiseResult<T>,
  checkSucceeded: (value: T) => boolean,
  defaultResult: T,
): Promise<T> {
  for (let retryCount: number = 1; retryCount <= 3; retryCount++) {
    const { page, browser } = await setupBrowser(url, 120000)
    await page.waitForSelector(`xpath/.//div[contains(@id, "canvas-container")]`)
    await page.waitForSelector('xpath/.//div[contains(@class, "item-label-container")]')
    try {
      const iterations = await page.evaluate(() => {
        try {
          // @ts-expect-error – page.evaluate remotely calls this function in the running editor!!
          SetPerformanceScriptNumberOfIterations(100)
        } catch (e) {
          // SetPerformanceScriptNumberOfIterations was not on window
        }
      })

      const result = await call(page)
      // Check the result.
      const success: boolean = checkSucceeded(result)
      if (success || retryCount === 3) {
        return result
      }
    } catch (e) {
      if (retryCount >= 3) {
        console.error(e)
        throw new Error(`Error during measurements ${e}`)
      }
    } finally {
      await page.close()
      await browser.close()
    }
  }
  return defaultResult
}

function frameResultSuccess(frameResult: FrameResult): boolean {
  return frameResult.succeeded
}

function frameObjectSuccess(frameObject: { [key: string]: FrameResult }): boolean {
  return Object.values(frameObject).every(frameResultSuccess)
}

export const testPerformanceInner = async function (url: string): Promise<PerformanceResults> {
  const highlightRegularResult = await retryPageCalls(
    url,
    testHighlightRegularPerformance,
    frameResultSuccess,
    EmptyResult,
  )
  const highlightAllElementsResult = await retryPageCalls(
    url,
    testHighlightAllElementsPerformance,
    frameResultSuccess,
    EmptyResult,
  )
  const selectionChangeResult = await retryPageCalls(
    url,
    testSelectionChangePerformance,
    frameResultSuccess,
    EmptyResult,
  )
  const selectionResult = await retryPageCalls(url, testSelectionPerformance, frameObjectSuccess, {
    selection: EmptyResult,
    deselection: EmptyResult,
  })
  // const resizeResult = await retryPageCalls(
  //   url,
  //   testResizePerformance,
  //   frameResultSuccess,
  //   EmptyResult,
  // )
  const scrollResult = await retryPageCalls(
    url,
    testScrollingPerformance,
    frameResultSuccess,
    EmptyResult,
  )
  // const absoluteMoveLargeResult = await retryPageCalls(
  //   url,
  //   testAbsoluteMovePerformanceLarge,
  //   frameObjectSuccess,
  //   { interaction: EmptyResult, move: EmptyResult },
  // )
  // const absoluteMoveSmallResult = await retryPageCalls(
  //   url,
  //   testAbsoluteMovePerformanceSmall,
  //   frameObjectSuccess,
  //   { interaction: EmptyResult, move: EmptyResult },
  // )

  return {
    frameTests: {
      highlightRegularResult: highlightRegularResult,
      highlightAllElementsResult: highlightAllElementsResult,
      selectionResult: selectionResult.selection,
      deselectionResult: selectionResult.deselection,
      selectionChangeResult: selectionChangeResult,
      scrollResult: scrollResult,
      // resizeResult: resizeResult,
      // absoluteMoveLargeMoveResult: absoluteMoveLargeResult.move,
      // absoluteMoveSmallMoveResult: absoluteMoveSmallResult.move,
    },
    interactionTests: {
      // absoluteMoveLargeInteractionResult: absoluteMoveLargeResult.interaction,
      // absoluteMoveSmallInteractionResult: absoluteMoveSmallResult.interaction,
    },
  }
}

async function clickOnce(
  page: puppeteer.Page,
  xpath: string,
  expectedConsoleMessage: string,
  errorMessage?: string,
): Promise<boolean> {
  await page.waitForSelector(`xpath/.${xpath}`)
  const [button] = await page.$$(`xpath/.${xpath}`)
  await (button as puppeteer.ElementHandle<HTMLButtonElement>)!.click()
  return consoleDoneMessage(page, expectedConsoleMessage, errorMessage)
}

export const testScrollingPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log('Test Scrolling Performance')
  // we run it twice without measurements to warm up the environment
  await clickOnce(page, "//a[contains(., 'P S')]", 'SCROLL_TEST_FINISHED', 'SCROLL_TEST_ERROR')
  await clickOnce(page, "//a[contains(., 'P S')]", 'SCROLL_TEST_FINISHED', 'SCROLL_TEST_ERROR')

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'P S')]",
    'SCROLL_TEST_FINISHED',
    'SCROLL_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return getFrameData(traceJson, 'scroll', 'Scroll Canvas', succeeded)
}

// export const testResizePerformance = async function (page: puppeteer.Page): Promise<FrameResult> {
//   console.log('Test Resize Performance')
//   await page.waitForXPath(ResizeButtonXPath)

//   // select element using the navigator
//   const navigatorElement = await page.waitForXPath(
//     '//div[contains(@class, "item-label-container")]',
//   )
//   await navigatorElement!.click()

//   // we run it twice without measurements to warm up the environment
//   await clickOnce(page, ResizeButtonXPath, 'RESIZE_TEST_FINISHED', 'RESIZE_TEST_ERROR')
//   await clickOnce(page, ResizeButtonXPath, 'RESIZE_TEST_FINISHED', 'RESIZE_TEST_ERROR')

//   // and then we run the test for a third time, this time running tracing
//   await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
//   const succeeded = await clickOnce(
//     page,
//     ResizeButtonXPath,
//     'RESIZE_TEST_FINISHED',
//     'RESIZE_TEST_ERROR',
//   )
//   await page.tracing.stop()
//   const traceJson = await loadTraceEventsJSON()
//   return getFrameData(traceJson, 'resize', 'Resize', succeeded)
// }

export const testHighlightRegularPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log(`Test Regular Highlight Performance`)
  await page.waitForSelector("xpath/.//a[contains(., 'PRH')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    "//a[contains(., 'PRH')]",
    'HIGHLIGHT_REGULAR_TEST_FINISHED',
    'HIGHLIGHT_REGULAR_TEST_ERROR',
  )
  await clickOnce(
    page,
    "//a[contains(., 'PRH')]",
    'HIGHLIGHT_REGULAR_TEST_FINISHED',
    'HIGHLIGHT_REGULAR_TEST_ERROR',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'PRH')]",
    'HIGHLIGHT_REGULAR_TEST_FINISHED',
    'HIGHLIGHT_REGULAR_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return getFrameData(traceJson, 'highlight_regular', 'Highlight Regular', succeeded)
}

export const testHighlightAllElementsPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log(`Test All Elements Highlight Performance`)
  await page.waitForSelector("xpath/.//a[contains(., 'PAH')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    "//a[contains(., 'PAH')]",
    'HIGHLIGHT_ALL-ELEMENTS_TEST_FINISHED',
    'HIGHLIGHT_ALL-ELEMENTS_TEST_ERROR',
  )
  await clickOnce(
    page,
    "//a[contains(., 'PAH')]",
    'HIGHLIGHT_ALL-ELEMENTS_TEST_FINISHED',
    'HIGHLIGHT_ALL-ELEMENTS_TEST_ERROR',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'PAH')]",
    'HIGHLIGHT_ALL-ELEMENTS_TEST_FINISHED',
    'HIGHLIGHT_ALL-ELEMENTS_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return getFrameData(traceJson, 'highlight_all-elements', 'Highlight All Elements', succeeded)
}

export const testSelectionPerformance = async function (page: puppeteer.Page): Promise<{
  selection: FrameResult
  deselection: FrameResult
}> {
  console.log('Test Selection Performance')
  await page.waitForSelector("xpath/.//a[contains(., 'P E')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(page, "//a[contains(., 'P E')]", 'SELECT_TEST_FINISHED', 'SELECT_TEST_ERROR')
  await clickOnce(page, "//a[contains(., 'P E')]", 'SELECT_TEST_FINISHED', 'SELECT_TEST_ERROR')

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'P E')]",
    'SELECT_TEST_FINISHED',
    'SELECT_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return {
    selection: getFrameData(traceJson, 'select', 'Selection', succeeded),
    deselection: getFrameData(traceJson, 'select_deselect', 'De-Selection', succeeded),
  }
}

export const testSelectionChangePerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log('Test Selection Change Performance')
  await page.waitForSelector("xpath/.//a[contains(., 'PSC')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    "//a[contains(., 'PSC')]",
    'SELECTION_CHANGE_TEST_FINISHED',
    'SELECTION_CHANGE_TEST_ERROR',
  )
  await clickOnce(
    page,
    "//a[contains(., 'PSC')]",
    'SELECTION_CHANGE_TEST_FINISHED',
    'SELECTION_CHANGE_TEST_ERROR',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'PSC')]",
    'SELECTION_CHANGE_TEST_FINISHED',
    'SELECTION_CHANGE_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return getFrameData(traceJson, 'selection_change', 'Selection Change', succeeded)
}

export const testAbsoluteMovePerformanceLarge = async function (page: puppeteer.Page): Promise<{
  interaction: FrameResult
  move: FrameResult
}> {
  console.log('Test Absolute Move Performance (Large)')
  await page.waitForSelector("xpath/.//a[contains(., 'PAML')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    "//a[contains(., 'PAML')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )
  await clickOnce(
    page,
    "//a[contains(., 'PAML')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'PAML')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return {
    interaction: getFrameData(
      traceJson,
      'absolute_move_interaction',
      'Absolute Move (Interaction, Large)',
      succeeded,
    ),
    move: getFrameData(
      traceJson,
      'absolute_move_move',
      'Absolute Move (Just Move, Large)',
      succeeded,
    ),
  }
}

export const testAbsoluteMovePerformanceSmall = async function (
  page: puppeteer.Page,
): Promise<{ interaction: FrameResult; move: FrameResult }> {
  console.log('Test Absolute Move Performance (Small)')
  await page.waitForSelector("xpath/.//a[contains(., 'PAMS')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    "//a[contains(., 'PAMS')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )
  await clickOnce(
    page,
    "//a[contains(., 'PAMS')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    "//a[contains(., 'PAMS')]",
    'ABSOLUTE_MOVE_TEST_FINISHED',
    'ABSOLUTE_MOVE_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return {
    interaction: getFrameData(
      traceJson,
      'absolute_move_interaction',
      'Absolute Move (Interaction, Small)',
      succeeded,
    ),
    move: getFrameData(
      traceJson,
      'absolute_move_move',
      'Absolute Move (Just Move, Small)',
      succeeded,
    ),
  }
}

const getFrameData = (
  traceEventsJson: any,
  markNamePrefix: string,
  title: string,
  succeeded: boolean,
): FrameResult => {
  const relevantEvents: any[] = traceEventsJson.filter((e: any) => {
    return e.cat === 'blink.user_timing' && e.name.startsWith(`${markNamePrefix}_`)
  })

  function findStartEvent(frameCounter: number): any | null {
    return relevantEvents.find((e) => {
      return e.cat === 'blink.user_timing' && e.name === `${markNamePrefix}_start_${frameCounter}`
    })
  }

  function findEndEvent(frameCounter: number): any | null {
    return relevantEvents.find((e) => {
      return e.cat === 'blink.user_timing' && e.name === `${markNamePrefix}_end_${frameCounter}`
    })
  }

  let frameTimes: Array<number> = []
  let totalFrameTimes = 0
  for (let frameCounter: number = 0; frameCounter < 100000; frameCounter++) {
    const startEvent = findStartEvent(frameCounter)
    const endEvent = findEndEvent(frameCounter)
    if (startEvent == null || endEvent == null) {
      break
    } else {
      const frameDelta = (endEvent.ts - startEvent.ts) / 1000
      frameTimes[frameCounter] = frameDelta
      totalFrameTimes += frameDelta
    }
  }

  let frameTimesFixed = frameTimes.map((x) => Number(x.toFixed(1)))
  const sortedFrameTimes = frameTimesFixed.sort((a, b) => a - b)

  const analytics = {
    frameMin: sortedFrameTimes[0]!,
    frameMax: sortedFrameTimes[sortedFrameTimes.length - 1]!,
    frameAvg: Number((totalFrameTimes / sortedFrameTimes.length).toFixed()),
    percentile25: sortedFrameTimes[Math.floor(sortedFrameTimes.length * 0.25)],
    percentile50: sortedFrameTimes[Math.floor(sortedFrameTimes.length * 0.5)],
    percentile75: sortedFrameTimes[Math.floor(sortedFrameTimes.length * 0.75)],
  }
  return {
    title: title,
    analytics: analytics,
    timeSeries: sortedFrameTimes,
    succeeded: succeeded,
  }
}

async function uploadSummaryImage(
  stagingResult: ResultsGroup,
  masterResult: ResultsGroup,
  maxXValue: number,
): Promise<string> {
  const imageFileName = v4() + '.png'
  const fileURI = await createSummaryPng(stagingResult, masterResult, imageFileName, maxXValue)

  if (fileURI != null) {
    const s3FileUrl = await uploadPNGtoAWS(fileURI)
    return s3FileUrl ?? ''
  } else {
    return ''
  }
}

async function createSummaryPng(
  stagingResult: ResultsGroup,
  masterResult: ResultsGroup,
  testFileName: string,
  maxXValue: number,
): Promise<string | null> {
  if (
    process.env.PERFORMANCE_GRAPHS_PLOTLY_USERNAME == null ||
    process.env.PERFORMANCE_GRAPHS_PLOTLY_API_KEY == null
  ) {
    console.info('Plotly summary generation skipped because of missing username or API key')
    return null
  }

  const plotly = require('plotly')(
    process.env.PERFORMANCE_GRAPHS_PLOTLY_USERNAME,
    process.env.PERFORMANCE_GRAPHS_PLOTLY_API_KEY,
  )

  const boxPlotConfig = (label: string, data: Array<number>) => {
    return {
      x: data,
      y: label,
      name: label,
      type: 'box',
      boxpoints: 'all',
      whiskerwidth: 0.5,
      fillcolor: 'cls',
      marker: {
        size: 1,
      },
      line: {
        width: 1,
      },
    }
  }

  const numberOfTests = Object.keys(stagingResult).length * 2

  let processedData = Object.entries(stagingResult).flatMap(([k, result]) => {
    const targetResult = masterResult[k]
    return [
      boxPlotConfig(`${targetResult.title} (before)`, targetResult.timeSeries),
      boxPlotConfig(`${result.title} (after)`, result.timeSeries),
    ]
  })

  processedData.reverse() // Plotly will produce the box plot in the reverse order

  const chartHeight = 30 * numberOfTests
  const chartWidth = 720
  const imagePadding = 80

  const layout = {
    margin: {
      l: 50,
      r: 50,
      b: 60,
      t: 10,
      pad: 4,
    },
    showlegend: false,
    height: chartHeight,
    width: chartWidth,
    yaxis: {
      automargin: true,
      zeroline: true,
    },
    shapes: [
      {
        type: 'rectangle',
        xref: 'x',
        yref: 'y',
        x0: 0.6,
        x1: 16.6,
        y0: 0,
        y1: 3,
        fillcolor: '#d3d3d3',
        opacity: 0.1,
        line: {
          width: 0,
        },
      },
    ],
    xaxis: {
      title: 'lower is better, ms / frame (16.67 = 60fps), many runs, cutoff 200ms',
      range: [0, maxXValue],
      showgrid: true,
      zeroline: true,
      dtick: 16.67,
      gridcolor: 'rgba(0,0,0,.1)',
      gridwidth: 1,
      zerolinecolor: 'rgba(0,0,0,.1)',
      zerolinewidth: 1,
      color: '#999',
    },
  }

  const imgOpts = {
    format: 'png',
    width: chartWidth + imagePadding,
    height: chartHeight + imagePadding,
  }
  const figure = { data: processedData, layout: layout }

  return new Promise<string>((resolve, reject) => {
    plotly.getImage(figure, imgOpts, async function (error: any, imageStream: any) {
      if (error) return console.log(error)

      var fileStream = await fs.createWriteStream(testFileName)

      const writeStreamPromise = new Promise<void>((streamResolve, streamReject) => {
        imageStream
          .pipe(fileStream)
          .on('finish', () => streamResolve())
          .on('error', (streamError: any) => streamReject(streamError))
      })

      await writeStreamPromise
      const path1 = path.resolve(testFileName)
      const path2 = path.resolve('frameimages')
      await moveFile(path1, path2 + '/' + testFileName)
      resolve(path2 + '/' + testFileName)
    })
  })
}

testPerformance().catch((e) => {
  console.error(e)
  console.error(`"There was an error with Puppeteer: ${e.name} – ${e.message}"`)
  process.exit(1)
})
