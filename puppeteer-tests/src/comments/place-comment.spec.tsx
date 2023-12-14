import { setupBrowser, wait } from '../utils'
import type { Browser, ElementHandle } from 'puppeteer'

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
      const commentModeButton = await page.waitForSelector(
        'div[data-testid="canvas-toolbar-comment-mode"]',
      )
      await wait(5000) // wait for Liveblocks to connect
      await commentModeButton!.click()
      const canvasControlsContainer = await page.waitForSelector('#new-canvas-controls-container')
      await canvasControlsContainer!.click({ offset: { x: 500, y: 500 } })

      const commentBox = await page.waitForSelector('[contenteditable="true"]')
      await commentBox!.focus()
      await commentBox!.type('hello comments')
      await page.keyboard.press('Enter')
      const thread = await page.waitForFunction(
        'document.querySelector("body").innerText.includes("hello comments")',
      )

      expect(thread).not.toBeNull()

      const [resolveButton] = await page.$x("//div[contains(., 'Resolve')]")
      const resolveButtonBounds = await resolveButton.boundingBox()
      await (resolveButton as ElementHandle<Element>).click({
        offset: {
          x: resolveButtonBounds!.x + resolveButtonBounds!.width / 2,
          y: resolveButtonBounds!.y + resolveButtonBounds!.height / 2,
        },
      })

      await wait(10000)

      await page.close()
      await browser.close()
    },
    TIMEOUT,
  )
})
