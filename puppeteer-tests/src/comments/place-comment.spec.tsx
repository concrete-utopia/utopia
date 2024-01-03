import { setupBrowser, wait } from '../utils'
import type { Browser } from 'puppeteer'

const TIMEOUT = 120000

describe('Comments test', () => {
  it(
    'can place a comment',
    async () => {
      const { page, browser } = await setupBrowser(
        'http://localhost:8000/p/?fakeUser=alice&Commenting=true',
        TIMEOUT,
      )

      const signInButton = await page.waitForSelector('div[data-testid="sign-in-button"]')
      await signInButton!.click()
      const commentModeButton = await page.waitForSelector(
        'div[data-testid="canvas-toolbar-comment-mode-connected"]',
      )
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

      const resolveButton = await page.waitForSelector('div[data-testid="resolve-thread-button"]')
      await resolveButton!.click()

      await page.close()
      await browser.close()
    },
    TIMEOUT,
  )
})
