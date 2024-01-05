import { setupBrowser, wait } from '../utils'
import type { Browser } from 'puppeteer'

const TIMEOUT = 120000

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''

describe('Comments test', () => {
  it(
    'can place a comment',
    async () => {
      const { page, browser } = await setupBrowser(
        `https://utopia.pizza/p/?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
        TIMEOUT,
      )

      const signInButton = await page.waitForSelector('div[data-testid="sign-in-button"]')
      await signInButton!.click()
      await page.waitForSelector('#playground-scene') // wait for the scene to render
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
