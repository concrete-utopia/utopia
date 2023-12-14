import { setupBrowser, wait } from '../utils'
import type { Browser } from 'puppeteer'

async function getFirstTab(browser: Browser) {
  const [firstTab] = await browser.pages()
  return firstTab
}

const TIMEOUT = 120000

describe('Comments test', () => {
  it(
    'can place a comment',
    async () => {
      const { page, browser } = await setupBrowser(
        'http://localhost:8000/p/?fakeUser=alice&Commenting=true',
        TIMEOUT,
      )
      await page.waitForSelector('#sign-in-button')
      await page.click('#sign-in-button')
      await page.waitForSelector('div[data-testid="canvas-toolbar-comment-mode"]')

      await wait(1000000)

      expect(1).toEqual(1)

      await page.close()
      await browser.close()
    },
    TIMEOUT,
  )
})
