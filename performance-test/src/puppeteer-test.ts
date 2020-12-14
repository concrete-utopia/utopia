import puppeteer from 'puppeteer'
const fs = require('fs')
const path = require('path')

const BRANCH_NAME = process.env.BRANCH_NAME
const PROJECT_ID = '5596ecdd'
const EDITOR_URL = `http://localhost:8000/p/39c427a7-hypnotic-king/` //locally
// const EDITOR_URL = `https://utopia.pizza/project/${PROJECT_ID}/?branch_name=${BRANCH_NAME}` //server, whenever push to server make sure this line is active

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

function consoleDoneMessage(page: puppeteer.Page) {
  return new Promise<void>((resolve, reject) => {
    page.on('console', (message) => {
      if (message.text().includes('SCROLL_TEST_FINISHED')) {
        // the editor will console.info('SCROLL_TEST_FINISHED') when the scrolling test is complete.
        // we wait until we see this console log and then we resolve the Promise
        resolve()
      }
    })
  })
}

export const testScrollingPerformance = async function () {
  
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--enable-thread-instruction-count'],
    headless: false, 
  })
  const page = await browser.newPage()
  await page.setViewport({ width: 1500, height: 768});
  // page.on('console', (message) =>
  //   console.log(`${message.type().substr(0, 3).toUpperCase()} ${message.text()}`),
  // )
  await page.goto(EDITOR_URL)
  await page.waitForXPath("//a[contains(., 'P S')]") // the button with the text 'P S' is the "secret" trigger to start the scrolling performance test
  // we run it twice without measurements to warm up the environment
  const [button] = await page.$x("//a[contains(., 'P S')]")
  await button!.click()
  await consoleDoneMessage(page)
  const [button2] = await page.$x("//a[contains(., 'P S')]")
  await button2!.click()
  await consoleDoneMessage(page)
  // and then we run the test for a third time, this time running tracing
  await page.tracing.start({ path: 'trace.json' })
  const [button3] = await page.$x("//a[contains(., 'P S')]")
  await button3!.click()
  await consoleDoneMessage(page)
  await page.tracing.stop()
  await browser.close()
  let traceData = fs.readFileSync('trace.json').toString()
  const traceJson = JSON.parse(traceData)

  const frameTimeEvents: any[] = traceJson.traceEvents.filter((e: any) =>
    e.name.startsWith('scroll_step_'),
  )
  let frameTimes: Array<number> = []
  let lastFrameTimestamp: number | null = null
  let totalFrameTimes = 0
  frameTimeEvents.forEach((fte) => {
    const frameID = fte.name.split('scroll_step_')[1] - 1
    const frameTimestamp = fte.ts
    if (lastFrameTimestamp != null) {
      const frameDelta = (frameTimestamp - lastFrameTimestamp) / 1000
      frameTimes[frameID] = frameDelta
      totalFrameTimes += frameDelta
    }
    lastFrameTimestamp = frameTimestamp
  })

  var arr1 = frameTimes.sort((a,b) => a - b)
  function returnArr() {
    return arr1;
  }

  const frameAvg = totalFrameTimes / frameTimes.length
  const percentile25 = frameTimes.sort((a, b) => a - b)[Math.floor(frameTimes.length * 0.25)]
  const percentile50 = frameTimes.sort((a, b) => a - b)[Math.floor(frameTimes.length * 0.50)]
  const percentile75 = frameTimes.sort((a,b) => a- b)[Math.floor(frameTimeEvents.length * 0.75)]
  
  
  
  
  
  
  function returnTestPng(testResults = frameTimes) {
    const plotly = require('plotly')("OmarDaSilva", "szS7pGItjmB7z50Ft3e9")
    const ffs = require('fs');

    const trace = {
      x: testResults.sort((a,b) => a-b),
      name: "Frame Times",
      type: "histogram"
    }
    const layout = {
      title: {
        text: 'Frame Time Test',
        font: {
          family:'Courier New, monospace',
          size:16
        },
        xref: 'paper',
        x: 0.05,
      },
      xaxis: {
        //range: [, 16.6],
        title: {
          text: 'Frame Times (ms)',
          font: {
            family: 'Courier New, monospace',
            size: 12,
            color: '#7f7f7f'
          }
        },
      },
      yaxis: {
        title: {
          text: 'Frequency',
          font: {
            family: 'Courier New, monospace',
            size: 12,
            color: '#7f7f7f'
          }
        }
      }
    };
    const imgOpts = {
      format: 'png',
      width: 500,
      height: 300
    };
    const figure = {'data': [trace], layout: layout};
    plotly.getImage(figure, imgOpts, function (error: any, imageStream: any) {
      if (error) return console.log (error);
  
      var fileStream = ffs.createWriteStream('2.png');
      imageStream.pipe(fileStream);
  });
}
  returnTestPng();

  console.info(
    `::set-output name=perf-result:: " ![GitHub Logo](https://imgur.com/a/n5YXrB8.png) ${totalFrameTimes}ms – average frame length: ${frameAvg} – Q1: ${percentile25} – Q2: ${percentile50} – Q3: ${percentile75} – Median: ${percentile50} – frame times: [${frameTimes.join(
      ',',
    )}]"`
  )


}


