import type { ElementHandle, Page } from 'puppeteer'
import type { UtopiaPuppeteerBrowser } from './test-utils'

export const TIMEOUT = 120000
export const PUPPETEER_TIMEOUT = 30000

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const BASE_URL = process.env.BASE_URL ?? 'http://localhost:8000'

export async function getElementWithSelector(
  page: Page,
  selector: string,
): Promise<ElementHandle<Element>> {
  console.info('\x1b[93m  waiting for element with selector \x1b[0m', selector)
  try {
    const element = await page.waitForSelector(selector)
    console.info('\x1b[92m element found \x1b[0m', selector)
    return element!
  } catch (e) {
    console.error('\x1b[31m ERROR did not find element, failing test \x1b[0m', selector)
    throw e
  }
}

export async function initBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice${BRANCH_NAME}`,
    timeout: PUPPETEER_TIMEOUT,
  })
  await getElementWithSelector(page, '#playground-scene') // wait for the scene to render

  return page
}

export async function initSignedInBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice${BRANCH_NAME}`,
    timeout: PUPPETEER_TIMEOUT,
  })

  const signInButton = await getElementWithSelector(page, 'div[data-testid="sign-in-button"]')
  await signInButton!.click()

  await getElementWithSelector(page, '#playground-scene') // wait for the scene to render

  return page
}

export async function enterCommentMode(page: Page) {
  const commentModeButton = await getElementWithSelector(
    page,
    'div[data-testid="canvas-toolbar-comment-mode-connected"]',
  )
  await commentModeButton!.click()
}

export async function placeCommentOnCanvas(page: Page, text: string, x: number, y: number) {
  const canvasControlsContainer = await getElementWithSelector(
    page,
    '#new-canvas-controls-container',
  )
  await canvasControlsContainer!.click({ offset: { x, y } })

  const commentBox = await getElementWithSelector(page, '[contenteditable="true"]')
  await commentBox!.focus()
  await commentBox!.type(text)
  await page.keyboard.press('Enter')
}
