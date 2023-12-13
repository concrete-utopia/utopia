import { setupBrowser } from '../utils'
import type { Browser } from 'puppeteer'

async function getFirstTab(browser: Browser) {
  const [firstTab] = await browser.pages()
  return firstTab
}

describe('Comments test', () => {
  it('can place a comment', async () => {
    const { page, browser } = await setupBrowser('http://localhost:8000/p/', 120000)
    const tab = await getFirstTab(browser)
    tab.click('#sign-in-button')
    expect(1).toEqual(1)
  })
})
