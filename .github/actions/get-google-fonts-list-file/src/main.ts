import * as core from '@actions/core'
import fetch from 'node-fetch'
import * as parserTypescript from 'prettier/parser-typescript'
import * as prettier from 'prettier/standalone'

interface GoogleFontsTypeface {
  type: 'google-fonts-typeface'
  name: string
  variants: Array<WebFontVariant>
}

function googleFontsTypeface(
  name: string,
  variants: Array<WebFontVariant>
): GoogleFontsTypeface {
  return {
    type: 'google-fonts-typeface',
    name,
    variants
  }
}

const GoogleWebFontsURL = `https://www.googleapis.com/webfonts/v1/webfonts?key=${process.env.GOOGLE_WEB_FONTS_KEY}`

type WebFontWeight = 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900
type WebFontStyle = 'normal' | 'italic'

interface WebFontVariant {
  type: 'web-font-variant'
  webFontWeight: WebFontWeight
  webFontStyle: WebFontStyle
}

function webFontVariant(
  webFontWeight: WebFontWeight,
  webFontStyle: WebFontStyle
): WebFontVariant {
  return {
    type: 'web-font-variant',
    webFontWeight,
    webFontStyle
  }
}

type GoogleFontVariantIdentifier =
  | '100'
  | '200'
  | '300'
  | 'regular'
  | '500'
  | '600'
  | '700'
  | '800'
  | '900'
  | '100italic'
  | '200italic'
  | '300italic'
  | 'italic'
  | '500italic'
  | '600italic'
  | '700italic'
  | '800italic'
  | '900italic'

const fontVariantMap: {[key in GoogleFontVariantIdentifier]: WebFontVariant} = {
  '100': webFontVariant(100, 'normal'),
  '200': webFontVariant(200, 'normal'),
  '300': webFontVariant(300, 'normal'),
  regular: webFontVariant(400, 'normal'),
  '500': webFontVariant(500, 'normal'),
  '600': webFontVariant(600, 'normal'),
  '700': webFontVariant(700, 'normal'),
  '800': webFontVariant(800, 'normal'),
  '900': webFontVariant(900, 'normal'),
  '100italic': webFontVariant(100, 'italic'),
  '200italic': webFontVariant(200, 'italic'),
  '300italic': webFontVariant(300, 'italic'),
  italic: webFontVariant(400, 'italic'),
  '500italic': webFontVariant(500, 'italic'),
  '600italic': webFontVariant(600, 'italic'),
  '700italic': webFontVariant(700, 'italic'),
  '800italic': webFontVariant(800, 'italic'),
  '900italic': webFontVariant(900, 'italic')
}

function googleVariantToFontVariant(
  googleVariant: string
): WebFontVariant | null {
  if (fontVariantMap.hasOwnProperty(googleVariant)) {
    return fontVariantMap[googleVariant as GoogleFontVariantIdentifier]
  } else {
    // Only numeric font-weight keyword values are currently supported.
    return null
  }
}

/** Order the variants from lowest to highest weights, with regular styles
 *  coming before italic ones.
 */
function googleVariantStringsIntoWebFontVariants(
  variants: Array<GoogleFontVariantIdentifier>
): Array<WebFontVariant> {
  return variants
    .map(googleVariantToFontVariant)
    .filter((v): v is WebFontVariant => v != null)
    .sort((a, b) => {
      const aFontStyle = a.webFontStyle ? 1 : 0
      const bFontStyle = b.webFontStyle ? 1 : 0
      return a.webFontWeight + aFontStyle - b.webFontWeight + bFontStyle
    })
}

async function run(): Promise<void> {
  if (
    process.env.GOOGLE_WEB_FONTS_KEY === '' ||
    process.env.GOOGLE_WEB_FONTS_KEY == null
  ) {
    core.setFailed(`Env variable 'process.env.GOOGLE_WEB_FONTS_KEY' is empty`)
  }
  fetch(GoogleWebFontsURL)
    .then((response: any) => {
      response
        .json()
        .then(
          (responseData: {
            items: Array<{
              family: string
              variants: Array<GoogleFontVariantIdentifier>
            }>
            error?: any
          }) => {
            if (
              responseData.error != null &&
              responseData.error.message != null
            ) {
              core.setFailed(
                `${responseData.error.message} current API key: ${process.env.GOOGLE_WEB_FONTS_KEY}`
              )
            } else {
              const data = responseData.items.map(datum =>
                googleFontsTypeface(
                  datum.family,
                  googleVariantStringsIntoWebFontVariants(datum.variants)
                )
              )
              if (!(data.length > 0)) {
                core.setFailed(`Data: ${JSON.stringify(data)} is empty`)
              } else {
                core.setOutput('google-fonts-list-length', data.length)

                const dataJSONString = JSON.stringify(data)
                const uglyFile = `import { GoogleFontsTypeface } from '../src/components/navigator/external-resources/google-fonts-utils'

/** This is auto-generated using this workflow action: https://github.com/concrete-utopia/get-google-fonts-list-file */
export const googleFontsList: Array<GoogleFontsTypeface> = ${dataJSONString}`
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
          }
        )
        .catch((error: any) => {
          core.setFailed(error.message)
        })
    })
    .catch((error: any) => {
      core.setFailed(error.message)
    })
}

run()
