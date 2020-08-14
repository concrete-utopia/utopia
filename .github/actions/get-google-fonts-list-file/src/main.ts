import * as core from '@actions/core'
import fetch from 'node-fetch'
import * as parserTypescript from 'prettier/parser-typescript'
import * as prettier from 'prettier/standalone'

interface GoogleFontsFontMetadata {
  family: string
  variants: Array<string>
}
type GoogleFontsList = Array<GoogleFontsFontMetadata>

const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${process.env.GOOGLE_WEB_FONTS_KEY}`

async function run(): Promise<void> {
  if (process.env.GOOGLE_WEB_FONTS_KEY === '') {
    core.setFailed(`Env variable 'process.env.GOOGLE_WEB_FONTS_KEY' is empty`)
  }
  fetch(GoogleWebFontsURL)
    .then(response => {
      response
        .json()
        .then((responseData: {items: GoogleFontsList; error?: any}) => {
          if (responseData?.error?.message != null) {
            core.setFailed(
              `${responseData.error.message}, current API key: ${process.env.GOOGLE_WEB_FONTS_KEY}`
            )
          } else {
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
              const uglyFile = `import { GoogleFontsTypefaceMetadata } from '../src/components/navigator/external-resources/google-fonts-utils'

/** This is auto-generated using this workflow action: https://github.com/concrete-utopia/get-google-fonts-list-file */
export const googleFontsList: Array<GoogleFontsTypefaceMetadata> = ${dataJSONString}`
              const prettyFile = prettier.format(uglyFile, {
                parser: 'typescript',
                plugins: [parserTypescript],
                printWidth: 100,
                trailingComma: 'all',
                tabWidth: 2,
                semi: false,
                singleQuote: true,
                quoteProps: 'as-needed',
                bracketSpacing: true,
                jsxSingleQuote: true,
                jsxBracketSameLine: false,
                arrowParens: 'always'
              })
              core.setOutput('google-fonts-file', prettyFile)
            }
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
