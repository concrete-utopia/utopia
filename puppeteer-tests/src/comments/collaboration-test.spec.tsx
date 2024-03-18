import type { ElementHandle, Page } from 'puppeteer'
import { setupBrowser, wait } from '../utils'
import * as url from 'url'
import { createUtopiaPuppeteerBrowser } from './test-utils'
import { PUPPETEER_TIMEOUT, TIMEOUT, getElementWithSelector } from './comment-utils'

const BRANCH_NAME = process.env.BRANCH_NAME ? `&branch_name=${process.env.BRANCH_NAME}` : ''
const BASE_URL = process.env.BASE_URL ?? 'http://localhost:8000'

async function signIn(page: Page) {
  const signInButton = await getElementWithSelector(page, 'div[data-testid="sign-in-button"]')
  await signInButton!.click()
  await getElementWithSelector(page, '#playground-scene') // wait for the scene to render
}

async function clickCanvasContainer(page: Page, { x, y }: { x: number; y: number }) {
  const canvasControlsContainer = await getElementWithSelector(
    page,
    '#new-canvas-controls-container',
  )
  await canvasControlsContainer!.click({ offset: { x, y } })
}

xdescribe('Collaboration test', () => {
  let utopiaBrowser1 = createUtopiaPuppeteerBrowser()
  let utopiaBrowser2 = createUtopiaPuppeteerBrowser()
  it(
    'can collaboratively add an element',
    async () => {
      const { page: page1 } = await utopiaBrowser1.setup({
        url: `${BASE_URL}/p/?fakeUser=alice${BRANCH_NAME}`,
        timeout: PUPPETEER_TIMEOUT,
      })

      console.info('waiting for page to navigate to the new project')
      await page1.waitForNavigation()
      await signIn(page1)
      // wait for project to be saved
      console.info(
        'waiting for element with function',
        'document.querySelector("body").innerText.includes("Project successfully uploaded!")',
      )
      await page1.waitForFunction(
        'document.querySelector("body").innerText.includes("Project successfully uploaded!")',
        { polling: 'mutation' },
      )

      const newProjectUrl = new url.URL(page1.url()).pathname

      const { page: page2 } = await utopiaBrowser2.setup({
        url: `${BASE_URL}${newProjectUrl}?fakeUser=bob${BRANCH_NAME}`,
        timeout: PUPPETEER_TIMEOUT,
      })

      await signIn(page2)

      await Promise.all([signIn(page1), signIn(page2)])
      await Promise.all([
        clickCanvasContainer(page1, { x: 500, y: 500 }),
        clickCanvasContainer(page2, { x: 500, y: 500 }),
      ])

      const insertTab = (await page1.$$(
        "xpath/.//div[contains(text(), 'Insert')]",
      )) as ElementHandle<Element>[]
      await insertTab!.at(0)!.click()

      const sampleTextOptions = (await page1.$$(
        "xpath/.//span[contains(text(), 'Sample text')]",
      )) as ElementHandle<Element>[]
      await sampleTextOptions!.at(0)!.click()
      await clickCanvasContainer(page1, { x: 500, y: 500 })

      await clickCanvasContainer(page1, { x: 500, y: 500 })

      console.info(
        'waiting for element with function',
        'document.querySelector("body").innerText.includes("Sample text")',
      )
      const sampleText = await page2.waitForFunction(
        'document.querySelector("body").innerText.includes("Sample text")',
      )

      expect(sampleText).not.toBeNull()

      await page1.keyboard.down('MetaLeft')
      await page1.keyboard.press('z', {})
      await page1.keyboard.up('MetaLeft')
    },
    TIMEOUT,
  )
})
