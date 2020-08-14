import * as core from '@actions/core'
import fetch from 'node-fetch'
interface GoogleFontsFontMetadata {
  family: string
  variants: Array<string>
}
type GoogleFontsList = Array<GoogleFontsFontMetadata>

const GOOGLE_WEB_FONTS_KEY = 'AIzaSyBffJtCo2vL68hdQKH3IYjo0ELFAAGYNW4'
const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${GOOGLE_WEB_FONTS_KEY}`

async function run(): Promise<void> {
  fetch(GoogleWebFontsURL)
    .then(response => {
      response
        .json()
        .then((responseData: {items: GoogleFontsList}) => {
          const data = responseData.items.map(datum => ({
            type: 'google-fonts-typeface',
            family: datum.family,
            variants: datum.variants
          }))
          if (!(data.length > 0)) {
            core.setFailed(`Data: ${JSON.stringify(data)} is empty`)
          } else {
            core.setOutput('google-fonts-list-length', data.length)

            const dataJSONString = JSON.stringify(data)
            const file = `import { GoogleFontsTypefaceMetadata } from "../src/components/navigator/external-resources/google-fonts-utils"

/** This is auto-generated using this workflow action: https://github.com/concrete-utopia/get-google-fonts-list-file */
export const googleFontsList: Array<GoogleFontsTypefaceMetadata> = ${dataJSONString}`
            core.setOutput('google-fonts-file', file)
          }
        })
        .catch(error => {
          core.setFailed(error.message)
        })
    })
    .catch(error => {
      core.setFailed(error.message)
    })
}

run()
