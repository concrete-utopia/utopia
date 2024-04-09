/* eslint-disable no-console */
require('dotenv').config({ path: 'src/.env' })
import type { ElementHandle, Page, PageEvents } from 'puppeteer'
import { initialiseTests, ONE_MINUTE_IN_MS, setupBrowser, timeLimitPromise } from './utils'

const PROJECT_ID = process.env.PROJECT_ID ?? ''
const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const STAGING_EDITOR_URL =
  process.env.EDITOR_URL ?? `https://utopia.fish/p/${PROJECT_ID}?fakeUser=alice${BRANCH_NAME}`

async function clickOnce(
  page: Page,
  xpath: string,
  expectedConsoleMessage: string,
  errorMessage?: string,
): Promise<boolean> {
  await page.waitForSelector(`xpath/.${xpath}`)
  const [button] = await page.$$(`xpath/.${xpath}`)

  let handler: (message: PageEvents['console']) => void = () => {
    console.info('Should not fire.')
  }
  const consoleDonePromise = new Promise<boolean>((resolve, reject) => {
    handler = (message: PageEvents['console']) => {
      const messageText = message.text()
      if (messageText.includes(expectedConsoleMessage)) {
        console.info(messageText)
        resolve(true)
      } else if (errorMessage != null && messageText.includes(errorMessage)) {
        console.info(messageText)
        resolve(false)
      }
    }
    page.on('console', handler)
  })

  await (button as ElementHandle<HTMLButtonElement>)!.click()

  return timeLimitPromise(
    consoleDonePromise,
    10 * ONE_MINUTE_IN_MS, // 10 minutes.
    `Missing console message ${expectedConsoleMessage} in test browser.`,
  ).finally(() => {
    // Ensure we remove the handler afterwards.
    page.off('console', handler)
  })
}

export function wait(timeout: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}

async function testCodeEditor() {
  console.info('Checking Code Editor')

  const { page, browser } = await setupBrowser(STAGING_EDITOR_URL, 120000)
  await initialiseTests(page)

  let result: boolean = false
  try {
    result = await checkCodeEditor(page)
  } finally {
    await page.close()
    await browser.close()
  }

  if (result) {
    process.exitCode = 0
  } else {
    process.exitCode = 1
  }
}

async function checkCodeEditor(page: Page): Promise<boolean> {
  const desiredReadyState = {
    vscodeReady: true,
    loadingScreenVisible: false,
  }

  const maxRetries = 15
  for (let retryCount: number = 1; retryCount <= maxRetries; retryCount++) {
    const result = await clickOnce(
      page,
      "//a[contains(., 'VSC')]",
      `VSCode State: ${JSON.stringify(desiredReadyState)}`,
      `VSCode State: `,
    )

    if (result || retryCount === maxRetries) {
      return result
    } else {
      await wait(ONE_MINUTE_IN_MS) // wait 1 minute and try again
    }
  }

  return false
}

testCodeEditor().catch((e) => {
  console.error(e)
  process.exitCode = 1
})
