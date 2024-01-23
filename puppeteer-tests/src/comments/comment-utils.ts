import type { ElementHandle, Page } from 'puppeteer'
import type { UtopiaPuppeteerBrowser } from './test-utils'

export const TIMEOUT = 120000

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const BASE_URL = process.env.BASE_URL ?? 'http://localhost:8000'

export async function getElementWithSelector(
  page: Page,
  selector: string,
): Promise<ElementHandle<Element> | null> {
  // eslint-disable-next-line no-console
  console.log('waiting for element with selector', selector)
  const element = await page.waitForSelector(selector)
  return element!
}

export async function initBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
    timeout: TIMEOUT,
  })
  await getElementWithSelector(page, '#playground-scene') // wait for the scene to render

  return page
}

export async function initSignedInBrowserTest(utopiaBrowser: UtopiaPuppeteerBrowser) {
  const { page } = await utopiaBrowser.setup({
    url: `${BASE_URL}/p/?fakeUser=alice&Multiplayer=true${BRANCH_NAME}`,
    timeout: TIMEOUT,
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
