/* eslint-disable no-console */
require('dotenv').config({ path: 'src/.env' })
import { v4 as UUID } from 'uuid'
import { addFakeUserParam, initialiseTests, setupBrowser, uploadPNGtoAWS } from './utils'

const PROJECT_ID = process.env.PROJECT_ID ?? ''
const BASE_EDITOR_URL = process.env.BASE_EDITOR_URL ?? `https://utopia.pizza`
const EDITOR_URL = process.env.EDITOR_URL ?? `${BASE_EDITOR_URL}/project/${PROJECT_ID}`
const EDITOR_URL_WITH_FAKE_USER = addFakeUserParam(EDITOR_URL)

async function takeScreenshot() {
  const { page, browser } = await setupBrowser(EDITOR_URL_WITH_FAKE_USER, 120000)
  try {
    try {
      await initialiseTests(page)
    } catch (e: any) {
      console.error(
        `"There was an error initialising the tests, but continuing anyway: ${e.name} - ${e.message}"`,
      )
    }

    const imageFileName = `./app-screenshot-${UUID()}.png`
    console.info(`Capturing screenshot with file name ${imageFileName}`)
    await page.screenshot({
      path: imageFileName,
    })

    const uploadResult = (await uploadPNGtoAWS(imageFileName)) ?? ''

    console.info(`Screenshot captured`)
    console.info(`::set-output name=screenshot:: ${uploadResult}`)
  } catch (e: any) {
    await browser.close()
    console.error(`"There was an error with Puppeteer: ${e.name} - ${e.message}"`)
    process.exit(1)
  } finally {
    await browser.close()
  }
}

takeScreenshot()
