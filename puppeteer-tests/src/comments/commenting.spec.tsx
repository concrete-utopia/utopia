import { setupBrowser, wait } from '../utils'
import type { Browser } from 'puppeteer'
import {
  enterCommentMode,
  initBrowserTest,
  initSignedInBrowserTest,
  placeCommentOnCanvas,
} from './comment-utils'
import { createUtopiaPuppeteerBrowser } from './test-utils'

const TIMEOUT = 120001

describe('Comments test', () => {
  let utopiaBrowser = createUtopiaPuppeteerBrowser()
  it(
    'Basic comment workflow (place, reply)',
    async () => {
      const page = await initSignedInBrowserTest(utopiaBrowser)

      // Enter comment mode using the toolbar
      const commentModeButton = await page.waitForSelector(
        'div[data-testid="canvas-toolbar-comment-mode-connected"]',
      )
      await commentModeButton!.click()

      // Click on the canvas to open the comment popup
      const canvasControlsContainer = await page.waitForSelector('#new-canvas-controls-container')
      await canvasControlsContainer!.click({ offset: { x: 500, y: 500 } })

      // Write to comment text and submit it
      const commentBox = await page.waitForSelector('[contenteditable="true"]')
      await commentBox!.focus()
      await commentBox!.type('hello comments')
      await page.keyboard.press('Enter')

      // Check if the comment text is on the screen
      const thread = await page.waitForFunction(
        'document.querySelector("body").innerText.includes("hello comments")',
      )
      expect(thread).not.toBeNull()

      // Click away to close the comment popup
      await canvasControlsContainer!.click({ offset: { x: 700, y: 700 } })

      // Check if the comment indicator is still visible
      const commentIndicator = await page.waitForSelector('div[data-testid="comment-indicator"]')

      expect(commentIndicator).not.toBeNull()

      // Check that the comment popup is closed
      const popupAfterClose = await page.$$('div[data-testid="comment-popup"]')
      expect(popupAfterClose).toHaveLength(0)

      // Open the comment popup again
      const comment = await page.waitForSelector('div[data-testid="comment-wrapper"]')
      await comment!.click({ offset: { x: 10, y: 10 } })

      // Check that the comment popup is open
      const popupAfterReopen = await page.$$('div[data-testid="comment-popup"]')
      expect(popupAfterReopen).toHaveLength(1)

      // Submit a reply
      const commentBox2 = await page.waitForSelector('[contenteditable="true"]')
      await commentBox2!.focus()
      await commentBox2!.type('this is a reply')
      await page.keyboard.press('Enter')

      // Check if the original comment and the reply comment are both on the screen
      const originalComment = await page.waitForFunction(
        'document.querySelector("body").innerText.includes("hello comments")',
      )
      expect(originalComment).not.toBeNull()

      const replyComment = await page.waitForFunction(
        'document.querySelector("body").innerText.includes("this is a reply")',
      )
      expect(replyComment).not.toBeNull()

      // Leave comment mode by pressing ESC
      await page.keyboard.press('Escape')

      // Check that the comment popup is closed
      const popupAfterEsc = await page.$$('div[data-testid="comment-popup"]')
      expect(popupAfterEsc).toHaveLength(0)
    },
    TIMEOUT,
  )
  it(
    'Placing a comment without submitting does not create a comment',
    async () => {
      const page = await initSignedInBrowserTest(utopiaBrowser)

      await enterCommentMode(page)

      // Clicking to add a comment
      const canvasControlsContainer = await page.waitForSelector('#new-canvas-controls-container')
      await canvasControlsContainer!.click({ offset: { x: 500, y: 500 } })

      // Pressing escape without submitting
      await page.keyboard.press('Escape')

      // There are no comment indicators on the canvas
      const commentIndicators = await page.$$('div[data-testid="comment-indicator"]')
      expect(commentIndicators).toHaveLength(0)
    },
    TIMEOUT,
  )
  it(
    'Resolve comment',
    async () => {
      const page = await initSignedInBrowserTest(utopiaBrowser)

      await enterCommentMode(page)
      await placeCommentOnCanvas(page, 'hello comments', 500, 500)

      // Resolve the comment

      const resolveButton = await page.waitForSelector('div[data-testid="resolve-thread-button"]')
      await resolveButton!.click()

      // Check that the comment indicator is gone
      const commentIndicators = await page.$$('div[data-testid="comment-indicator"]')
      expect(commentIndicators).toHaveLength(0)
    },
    TIMEOUT,
  )
  it(
    'Close comment popup with the mouse',
    async () => {
      const page = await initSignedInBrowserTest(utopiaBrowser)

      await enterCommentMode(page)
      await placeCommentOnCanvas(page, 'hello comments', 500, 500)

      const closeCommentButton = await page.waitForSelector('div[data-testid="close-comment"]')
      await closeCommentButton!.click()

      // Check that the comment popup is closed but the indicator is still there
      const commentPopups = await page.$$('div[data-testid="comment-popup"]')
      expect(commentPopups).toHaveLength(0)

      const commentIndicators = await page.$$('div[data-testid="comment-indicator"]')
      expect(commentIndicators).toHaveLength(1)
    },
    TIMEOUT,
  ),
    it(
      'There is a comment tab when logged in',
      async () => {
        const page = await initSignedInBrowserTest(utopiaBrowser)

        const commentTabs = await page.waitForSelector('div[data-testid="comments-tab"]')
        expect(commentTabs).not.toBeNull()
      },
      TIMEOUT,
    ),
    it(
      'There is no comment tab when logged out',
      async () => {
        const page = await initBrowserTest(utopiaBrowser)

        const commentsTabs = await page.$$('div[data-testid="comments-tab"]')
        expect(commentsTabs).toHaveLength(0)
      },
      TIMEOUT,
    )
})
