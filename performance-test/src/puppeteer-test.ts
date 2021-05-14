require('dotenv').config({ path: 'src/.env' })
import puppeteer from 'puppeteer'
import { v4 } from 'uuid'
import { timeLimitPromise } from './utils'
const fs = require('fs')
const path = require('path')
const AWS = require('aws-sdk')
const moveFile = require('move-file')
const yn = require('yn')

const BRANCH_NAME = process.env.BRANCH_NAME ? `?branch_name=${process.env.BRANCH_NAME}` : ''
const PROJECT_ID = '5596ecdd'
const EDITOR_URL =
  process.env.EDITOR_URL ?? `https://utopia.pizza/project/${PROJECT_ID}/${BRANCH_NAME}`

type FrameResult = {
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

function consoleDoneMessage(
  page: puppeteer.Page,
  expectedConsoleMessage: string,
  errorMessage?: string,
): Promise<void> {
  const consoleDonePromise = new Promise<void>((resolve, reject) => {
    page.on('console', (message) => {
      if (
        message.text().includes(expectedConsoleMessage) ||
        (errorMessage != null && message.text().includes(errorMessage))
      ) {
        // the editor will console.info('SCROLL_TEST_FINISHED') when the scrolling test is complete.
        // we wait until we see this console log and then we resolve the Promise
        resolve()
      }
    })
  })
  return timeLimitPromise(
    consoleDonePromise,
    120000,
    `Missing console message ${expectedConsoleMessage} in test browser.`,
  )
}

export const setupBrowser = async (): Promise<{
  page: puppeteer.Page
  browser: puppeteer.Browser
}> => {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--enable-thread-instruction-count'],
    headless: yn(process.env.HEADLESS),
    executablePath: process.env.BROWSER,
  })
  const page = await browser.newPage()
  await page.setDefaultNavigationTimeout(120000)
  await page.setDefaultTimeout(120000)
  await page.setViewport({ width: 1500, height: 768 })
  // page.on('console', (message) =>
  //   console.log(`${message.type().substr(0, 3).toUpperCase()} ${message.text()}`),
  // )
  console.info('loading editor at URL:', EDITOR_URL)
  await page.goto(EDITOR_URL)
  return {
    browser: browser,
    page: page,
  }
}

const ResizeButtonXPath = "//a[contains(., 'P R')]"

async function initialiseProject(page: puppeteer.Page): Promise<void> {
  console.log('Initialising the project')
  await page.waitForXPath('//div[contains(@class, "item-label-container")]')

  // Select something a resize it to trigger a fork
  const navigatorElement = await page.$('[class^="item-label-container"]')
  await navigatorElement!.click()

  // First selection will open the file in VS Code, triggering a bunch of downloads, so we pause briefly
  await page.waitForTimeout(15000)

  const [button] = await page.$x(ResizeButtonXPath)
  await button!.click()
  await consoleDoneMessage(page, 'RESIZE_TEST_FINISHED', 'RESIZE_TEST_MISSING_SELECTEDVIEW')

  // This change should have triggered a fork, so pause again
  await page.waitForTimeout(15000)

  console.log('Finished initialising')
}

function consoleMessageForResult(result: FrameResult): string {
  return `${result.title}: ${result.analytics.percentile50}ms (${result.analytics.frameMin}-${result.analytics.frameMax}ms)`
}

export const testPerformance = async function () {
  let scrollResult = EmptyResult
  let resizeResult = EmptyResult
  let selectionResult = EmptyResult
  const { page, browser } = await setupBrowser()
  try {
    await initialiseProject(page)
    selectionResult = await testSelectionPerformance(page)
    resizeResult = await testResizePerformance(page)
    scrollResult = await testScrollingPerformance(page)
  } catch (e) {
    throw new Error(`Error during measurements ${e}`)
  } finally {
    browser.close()
  }
  const summaryImage = await uploadSummaryImage([selectionResult, scrollResult, resizeResult])

  console.info(
    `::set-output name=perf-result:: ${consoleMessageForResult(
      scrollResult,
    )} | ${consoleMessageForResult(resizeResult)} | ${consoleMessageForResult(
      selectionResult,
    )} ![(Chart)](${summaryImage})`,
  )
}

async function clickOnce(
  page: puppeteer.Page,
  xpath: string,
  expectedConsoleMessage: string,
  errorMessage?: string,
): Promise<void> {
  const [button] = await page.$x(xpath)
  await button!.click()
  await consoleDoneMessage(page, expectedConsoleMessage, errorMessage)
}

export const testScrollingPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log('Test Scrolling Performance')
  await page.waitForXPath("//a[contains(., 'P S')]") // the button with the text 'P S' is the "secret" trigger to start the scrolling performance test
  // we run it twice without measurements to warm up the environment
  await clickOnce(page, "//a[contains(., 'P S')]", 'SCROLL_TEST_FINISHED')
  await clickOnce(page, "//a[contains(., 'P S')]", 'SCROLL_TEST_FINISHED')

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ path: 'trace.json' })
  await clickOnce(page, "//a[contains(., 'P S')]", 'SCROLL_TEST_FINISHED')
  await page.tracing.stop()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)
  return getFrameData(traceJson, 'scroll_step_', 'Scroll Canvas')
}

export const testResizePerformance = async function (page: puppeteer.Page): Promise<FrameResult> {
  console.log('Test Resize Performance')
  await page.waitForXPath(ResizeButtonXPath)

  // select element using the navigator
  const navigatorElement = await page.$('[class^="item-label-container"]')
  await navigatorElement!.click()

  // we run it twice without measurements to warm up the environment
  await clickOnce(
    page,
    ResizeButtonXPath,
    'RESIZE_TEST_FINISHED',
    'RESIZE_TEST_MISSING_SELECTEDVIEW',
  )
  await clickOnce(
    page,
    ResizeButtonXPath,
    'RESIZE_TEST_FINISHED',
    'RESIZE_TEST_MISSING_SELECTEDVIEW',
  )

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ path: 'trace.json' })
  await clickOnce(
    page,
    ResizeButtonXPath,
    'RESIZE_TEST_FINISHED',
    'RESIZE_TEST_MISSING_SELECTEDVIEW',
  )
  await page.tracing.stop()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)
  return getFrameData(traceJson, 'resize_step_', 'Resize')
}

export const testSelectionPerformance = async function (
  page: puppeteer.Page,
): Promise<FrameResult> {
  console.log('Test Selection Performance')
  await page.waitForTimeout(20000)
  await page.waitForXPath("//a[contains(., 'P E')]")
  // we run it twice without measurements to warm up the environment
  await clickOnce(page, "//a[contains(., 'P E')]", 'SELECT_TEST_FINISHED', 'SELECT_TEST_ERROR')
  await clickOnce(page, "//a[contains(., 'P E')]", 'SELECT_TEST_FINISHED', 'SELECT_TEST_ERROR')

  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ path: 'trace.json' })
  await clickOnce(page, "//a[contains(., 'P E')]", 'SELECT_TEST_FINISHED', 'SELECT_TEST_ERROR')
  await page.tracing.stop()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)
  return getFrameData(traceJson, 'select_step_', 'Selection')
}

const getFrameData = (traceJson: any, markNamePrefix: string, title: string): FrameResult => {
  const frameTimeEvents: any[] = traceJson.traceEvents.filter((e: any) =>
    e.name.startsWith(markNamePrefix),
  )
  let frameTimes: Array<number> = []
  let lastFrameTimestamp: number | null = null
  let totalFrameTimes = 0
  frameTimeEvents.forEach((fte) => {
    const frameID = fte.name.split(markNamePrefix)[1] - 1
    const frameTimestamp = fte.ts
    if (lastFrameTimestamp != null) {
      const frameDelta = (frameTimestamp - lastFrameTimestamp) / 1000
      frameTimes[frameID] = frameDelta
      totalFrameTimes += frameDelta
    }
    lastFrameTimestamp = frameTimestamp
  })

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
  }
}

async function uploadSummaryImage(results: Array<FrameResult>): Promise<string> {
  const imageFileName = v4() + '.png'
  const fileURI = await createSummaryPng(results, imageFileName, results.length)

  if (fileURI != null) {
    const s3FileUrl = await uploadPNGtoAWS(fileURI)
    return s3FileUrl
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
      title: 'lower is better, ms / frame (16.67 = 60fps), 100 runs, cutoff 200ms',
      range: [0, 200],
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

      const writeStreamPromise = new Promise<void>((resolve, reject) => {
        imageStream
          .pipe(fileStream)
          .on('finish', () => resolve())
          .on('error', (error: any) => reject(error))
      })

      await writeStreamPromise
      const path1 = path.resolve(testFileName)
      const path2 = path.resolve('frameimages')
      await moveFile(path1, path2 + '/' + testFileName)
      resolve(path2 + '/' + testFileName)
    })
  })
}

async function uploadPNGtoAWS(testFile: string): Promise<string> {
  AWS.config.update({
    region: process.env.AWS_REGION,
    AWS_ACCESS_KEY_ID: process.env.AWS_ACCESS_KEY_ID,
    AWS_SECRET_ACCESS_KEY: process.env.AWS_SECRET_ACCESS_KEY,
  })

  let s3 = new AWS.S3({ apiVersion: '2006-03-01' })
  const uploadParams = {
    Bucket: process.env.AWS_S3_BUCKET,
    Key: testFile,
    Body: '',
    ContentType: 'image/png',
    ACL: 'public-read',
  }

  return new Promise<string>((resolve, reject) => {
    const path1 = path.resolve(testFile)
    let filestream = fs.createReadStream(path1)
    filestream.on('error', function (err: any) {
      console.log('File Error', err)
      reject(err)
    })
    uploadParams.Body = filestream
    uploadParams.Key = path.basename(testFile)

    s3.upload(uploadParams, function (err: any, data: any) {
      if (err) {
        console.log('Error', err)
        reject(err)
      }
      if (data) {
        console.log('Upload Success', data.Location)
        resolve(data.Location)
      }
    })
  })
}
