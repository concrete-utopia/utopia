/* eslint-disable no-console */
require('dotenv').config({ path: 'src/.env' })
import * as puppeteer from 'puppeteer'
import { v4 } from 'uuid'
import { consoleDoneMessage, setupBrowser, uploadPNGtoAWS } from './utils'
import * as JSONStream from 'JSONStream'
const fs = require('fs')
const path = require('path')
const moveFile = require('move-file')

const BRANCH_NAME = process.env.BRANCH_NAME ? `?branch_name=${process.env.BRANCH_NAME}` : ''
const STAGING_EDITOR_URL = process.env.EDITOR_URL ?? `https://utopia.pizza/p${BRANCH_NAME}`
const MASTER_EDITOR_URL =
  process.env.MASTER_EDITOR_URL ?? `https://utopia.pizza/p?branch_name=performance-test-target`

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

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

const ResizeButtonXPath = "//a[contains(., 'P R')]"

function consoleMessageForResult(result: FrameResult): string {
  return `${result.title}: ${result.analytics.percentile50}ms (${result.analytics.frameMin}-${result.analytics.frameMax}ms)`
}

interface PerformanceResult {
  message: string
  summaryImageUrl: string
}

export const testPerformance = async function () {
  const stagingResult = await testPerformanceInner(STAGING_EDITOR_URL)
  const masterResult = await testPerformanceInner(MASTER_EDITOR_URL)

  const stagingCombined = `${stagingResult.message} | ![(Chart)](${stagingResult.summaryImageUrl})`
  const masterCombined = `${masterResult.message} | ![(Chart)](${masterResult.summaryImageUrl})`

  console.info(
    `::set-output name=perf-result:: This PR: <br /> ${stagingCombined} <br /> Compare with last deployed Master: <br /> ${masterCombined}`,
  )

  // Output the individual parts for building a discord message
  console.info(`::set-output name=perf-message-staging:: ${stagingResult.message}`)
  console.info(`::set-output name=perf-chart-staging:: ${stagingResult.summaryImageUrl}`)
  console.info(`::set-output name=perf-message-master:: ${masterResult.message}`)
  console.info(`::set-output name=perf-chart-master:: ${masterResult.summaryImageUrl}`)
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
    await page.waitForXPath(`//div[contains(@id, "canvas-container")]`)
    await page.waitForXPath('//div[contains(@class, "item-label-container")]')
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

function frameArraySuccess(frameArray: Array<FrameResult>): boolean {
  return frameArray.every(frameResultSuccess)
}

export const testPerformanceInner = async function (url: string): Promise<PerformanceResult> {
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
  const selectionResult = await retryPageCalls(url, testSelectionPerformance, frameArraySuccess, [])
  const resizeResult = await retryPageCalls(
    url,
    testResizePerformance,
    frameResultSuccess,
    EmptyResult,
  )
  const scrollResult = await retryPageCalls(
    url,
    testScrollingPerformance,
    frameResultSuccess,
    EmptyResult,
  )
  const absoluteMoveLargeResult = await retryPageCalls(
    url,
    testAbsoluteMovePerformanceLarge,
    frameArraySuccess,
    [],
  )
  const absoluteMoveSmallResult = await retryPageCalls(
    url,
    testAbsoluteMovePerformanceSmall,
    frameArraySuccess,
    [],
  )
  const messageParts = [
    highlightRegularResult,
    highlightAllElementsResult,
    ...selectionResult,
    scrollResult,
    resizeResult,
    ...absoluteMoveLargeResult,
    ...absoluteMoveSmallResult,
  ]
  const summaryImage = await uploadSummaryImage(messageParts)

  const message = messageParts.map(consoleMessageForResult).join(` | `)

  return {
    message: message,
    summaryImageUrl: summaryImage,
  }
}

async function clickOnce(
  page: puppeteer.Page,
  xpath: string,
  expectedConsoleMessage: string,
  errorMessage?: string,
): Promise<boolean> {
  await page.waitForXPath(xpath)
  const [button] = await page.$x(xpath)
  await button!.click()
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

export const testResizePerformance = async function (page: puppeteer.Page): Promise<FrameResult> {
  console.log('Test Resize Performance')
  await page.waitForXPath(ResizeButtonXPath)

  // select element using the navigator
  const navigatorElement = await page.waitForXPath(
    '//div[contains(@class, "item-label-container")]',
  )
  await navigatorElement!.click()

  // we run it twice without measurements to warm up the environment
  await clickOnce(page, ResizeButtonXPath, 'RESIZE_TEST_FINISHED', 'RESIZE_TEST_ERROR')
  await clickOnce(page, ResizeButtonXPath, 'RESIZE_TEST_FINISHED', 'RESIZE_TEST_ERROR')

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ categories: ['blink.user_timing'], path: 'trace.json' })
  const succeeded = await clickOnce(
    page,
    ResizeButtonXPath,
    'RESIZE_TEST_FINISHED',
    'RESIZE_TEST_ERROR',
  )
  await page.tracing.stop()
  const traceJson = await loadTraceEventsJSON()
  return getFrameData(traceJson, 'resize', 'Resize', succeeded)
}

export const testHighlightRegularPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log(`Test Regular Highlight Performance`)
  await page.waitForXPath("//a[contains(., 'PRH')]")
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
  await page.waitForXPath("//a[contains(., 'PAH')]")
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

export const testSelectionPerformance = async function (
  page: puppeteer.Page,
): Promise<Array<FrameResult>> {
  console.log('Test Selection Performance')
  await page.waitForXPath("//a[contains(., 'P E')]")
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
  return [
    getFrameData(traceJson, 'select', 'Selection', succeeded),
    getFrameData(traceJson, 'select_deselect', 'De-Selection', succeeded),
  ]
}

export const testAbsoluteMovePerformanceLarge = async function (
  page: puppeteer.Page,
): Promise<Array<FrameResult>> {
  console.log('Test Absolute Move Performance (Large)')
  await page.waitForXPath("//a[contains(., 'PAML')]")
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
  return [
    getFrameData(
      traceJson,
      'absolute_move_interaction',
      'Absolute Move (Interaction, Large)',
      succeeded,
    ),
    getFrameData(traceJson, 'absolute_move_move', 'Absolute Move (Just Move, Large)', succeeded),
  ]
}

export const testAbsoluteMovePerformanceSmall = async function (
  page: puppeteer.Page,
): Promise<Array<FrameResult>> {
  console.log('Test Absolute Move Performance (Small)')
  await page.waitForXPath("//a[contains(., 'PAMS')]")
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
  return [
    getFrameData(
      traceJson,
      'absolute_move_interaction',
      'Absolute Move (Interaction, Small)',
      succeeded,
    ),
    getFrameData(traceJson, 'absolute_move_move', 'Absolute Move (Just Move, Small)', succeeded),
  ]
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

async function uploadSummaryImage(results: Array<FrameResult>): Promise<string> {
  const imageFileName = v4() + '.png'
  const fileURI = await createSummaryPng(results, imageFileName, results.length)

  if (fileURI != null) {
    const s3FileUrl = await uploadPNGtoAWS(fileURI)
    return s3FileUrl ?? ''
  } else {
    return ''
  }
}

async function createSummaryPng(
  results: Array<FrameResult>,
  testFileName: string,
  numberOfTests: number,
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

  const processedData = results.map((result) => boxPlotConfig(result.title, result.timeSeries))

  const layout = {
    margin: {
      l: 50,
      r: 50,
      b: 60,
      t: 10,
      pad: 4,
    },
    showlegend: false,
    height: 50 * numberOfTests,
    width: 720,
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
      range: [0, 251],
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
    width: 800,
    height: 220,
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
  const errorMessage = `"There was an error with Puppeteer: ${e.name} – ${e.message}"`
  console.info(`::set-output name=perf-result::${errorMessage}`)

  // Output the individual parts for building a discord message
  console.info(`::set-output name=perf-message-staging:: ${errorMessage}`)
  console.info(`::set-output name=perf-chart-staging:: ""`)
  console.info(`::set-output name=perf-message-master:: ""`)
  console.info(`::set-output name=perf-chart-master:: ""`)
  return
})
