import type { BrowserForPuppeteerTest } from '../utils'
import { setupBrowser } from '../utils'

export interface UtopiaPuppeteerBrowserOptions {
  url: string
  timeout: number
}

export interface UtopiaPuppeteerBrowser {
  setup: (options: UtopiaPuppeteerBrowserOptions) => Promise<BrowserForPuppeteerTest>
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
    setup: async (options: UtopiaPuppeteerBrowserOptions) => {
      browserForTest.current = await setupBrowser(options.url, options.timeout)
      return browserForTest.current
    },
  }
}
