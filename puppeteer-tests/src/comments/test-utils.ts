import type { BrowserForPuppeteerTest } from '../utils'
import { setupBrowser } from '../utils'

export interface UtopiaPuppeteerBrowserOptions {
  url: string
  timeout: number
}

interface UtopiaPuppeteerBrowser {
  setup: (options: UtopiaPuppeteerBrowserOptions) => Promise<BrowserForPuppeteerTest>
  browserForTest: { current: BrowserForPuppeteerTest | null }
}

export const createUtopiaPuppeteerBrowser = (): UtopiaPuppeteerBrowser => {
  let browserForTest: { current: BrowserForPuppeteerTest | null } = { current: null }

  afterEach(async () => {
    if (browserForTest.current?.browser != null) {
      await browserForTest.current.browser.close()
    }

    browserForTest.current = null
  })

  return {
    browserForTest: browserForTest,
    setup: async (options: UtopiaPuppeteerBrowserOptions) => {
      browserForTest.current = await setupBrowser(options.url, options.timeout)
      return browserForTest.current
    },
  }
}
