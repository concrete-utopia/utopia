import type { Page } from 'puppeteer'
import type { UtopiaPuppeteerBrowser } from './test-utils'

export const TIMEOUT = 120000

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const BASE_URL = process.env.BASE_URL ?? 'http://localhost:8000'

export async function initBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
    timeout: TIMEOUT,
  })
  await page.waitForSelector('#playground-scene') // wait for the scene to render

  return page
}

export async function initSignedInBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
    timeout: TIMEOUT,
  })

  const signInButton = await page.waitForSelector('div[data-testid="sign-in-button"]')
  await signInButton!.click()

  await page.waitForSelector('#playground-scene') // wait for the scene to render

  return page
}

export async function enterCommentMode(page: Page) {
  const commentModeButton = await page.waitForSelector(
    'div[data-testid="canvas-toolbar-comment-mode-connected"]',
  )
  await commentModeButton!.click()
}

export async function placeCommentOnCanvas(page: Page, text: string, x: number, y: number) {
  const canvasControlsContainer = await page.waitForSelector('#new-canvas-controls-container')
  await canvasControlsContainer!.click({ offset: { x, y } })

  const commentBox = await page.waitForSelector('[contenteditable="true"]')
  await commentBox!.focus()
  await commentBox!.type(text)
  await page.keyboard.press('Enter')
}
