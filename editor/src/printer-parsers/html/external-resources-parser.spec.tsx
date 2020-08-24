import { webFontVariant } from '../../components/navigator/external-resources/google-fonts-utils'
import { previewHtml } from '../../core/model/new-project-files'
import { isRight } from '../../core/shared/either'
import {
  externalResources,
  genericExternalResource,
  getGeneratedExternalLinkText,
  googleFontsResource,
  parseLinkTags,
  printExternalResources,
  updateHTMLExternalResourcesLinks,
} from './external-resources-parser'

const testFile = `
<!DOCTYPE html>
<html lang="en">
  <head>
  <!-- Begin Generated Utopia External Links -->
  
  <!-- Valid Google Fonts -->
  <link href="https://fonts.googleapis.com/css2?family=Red+Rose:ital,wght@0,400" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Crimson+Pro:ital,wght@0,400;1,400;0,600;1,600" rel="stylesheet">

  <!-- User-generated Google Fonts links parsed with extra query params parsed -->
  <link href="https://fonts.googleapis.com/css2?family=Red+Rose" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Comfortaa&text=Hello%20World" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!" rel="stylesheet">
  
  <!-- Valid External Resources -->
  <link href="https://utopia.com/styles.css" rel="stylesheet">
  <link href="http://utopia.com/styles.css" rel="stylesheet">
  <link href="//utopia.com/styles.css" rel="stylesheet">


  <!-- User-generated links that are partially parsed and destructively printed back -->
  <link href="apple-icon-114.png" rel="apple-touch-icon-precomposed" sizes="114x114" type="image/png">
  <link rel="preload" href="myFont.woff2" as="font" type="font/woff2" crossorigin="anonymous">
  
  <!-- End Generated Utopia External Links -->
    <meta charset="utf-8">
    <title>Utopia React App</title>
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>
`

describe('external-resources-parser', () => {
  it('parses an html file', () => {
    const parsedLinkTagsText = getGeneratedExternalLinkText(testFile)
    expect(parsedLinkTagsText).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": "<!-- Valid Google Fonts -->
        <link href=\\"https://fonts.googleapis.com/css2?family=Red+Rose:ital,wght@0,400\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Crimson+Pro:ital,wght@0,400;1,400;0,600;1,600\\" rel=\\"stylesheet\\">

        <!-- User-generated Google Fonts links parsed with extra query params parsed -->
        <link href=\\"https://fonts.googleapis.com/css2?family=Red+Rose\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=Hello%20World\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!\\" rel=\\"stylesheet\\">
        
        <!-- Valid External Resources -->
        <link href=\\"https://utopia.com/styles.css\\" rel=\\"stylesheet\\">
        <link href=\\"http://utopia.com/styles.css\\" rel=\\"stylesheet\\">
        <link href=\\"//utopia.com/styles.css\\" rel=\\"stylesheet\\">


        <!-- User-generated links that are partially parsed and destructively printed back -->
        <link href=\\"apple-icon-114.png\\" rel=\\"apple-touch-icon-precomposed\\" sizes=\\"114x114\\" type=\\"image/png\\">
        <link rel=\\"preload\\" href=\\"myFont.woff2\\" as=\\"font\\" type=\\"font/woff2\\" crossorigin=\\"anonymous\\">",
      }
    `)

    if (isRight(parsedLinkTagsText)) {
      const parsedLinkTags = parseLinkTags(parsedLinkTagsText.value)
      expect(parsedLinkTags).toMatchInlineSnapshot(`
        Object {
          "type": "RIGHT",
          "value": Object {
            "genericExternalResources": Array [
              Object {
                "href": "https://utopia.com/styles.css",
                "rel": "stylesheet",
                "type": "generic-external-resource",
              },
              Object {
                "href": "http://utopia.com/styles.css",
                "rel": "stylesheet",
                "type": "generic-external-resource",
              },
              Object {
                "href": "//utopia.com/styles.css",
                "rel": "stylesheet",
                "type": "generic-external-resource",
              },
              Object {
                "href": "apple-icon-114.png",
                "rel": "apple-touch-icon-precomposed",
                "type": "generic-external-resource",
              },
              Object {
                "href": "myFont.woff2",
                "rel": "preload",
                "type": "generic-external-resource",
              },
            ],
            "googleFontsResources": Array [
              Object {
                "fontFamily": "Comfortaa",
                "otherQueryStringParams": "text=Hello+World",
                "type": "google-fonts-resource",
                "variants": Array [
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 400,
                  },
                ],
              },
              Object {
                "fontFamily": "Comfortaa",
                "otherQueryStringParams": "text=%C2%A1Hola%21",
                "type": "google-fonts-resource",
                "variants": Array [
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 400,
                  },
                ],
              },
              Object {
                "fontFamily": "Crimson Pro",
                "otherQueryStringParams": "",
                "type": "google-fonts-resource",
                "variants": Array [
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 400,
                  },
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "italic",
                    "webFontWeight": 400,
                  },
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 600,
                  },
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "italic",
                    "webFontWeight": 600,
                  },
                ],
              },
              Object {
                "fontFamily": "Red Rose",
                "otherQueryStringParams": "",
                "type": "google-fonts-resource",
                "variants": Array [
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 400,
                  },
                ],
              },
              Object {
                "fontFamily": "Red Rose",
                "otherQueryStringParams": "",
                "type": "google-fonts-resource",
                "variants": Array [
                  Object {
                    "type": "web-font-variant",
                    "webFontStyle": "normal",
                    "webFontWeight": 400,
                  },
                ],
              },
            ],
            "type": "external-resources",
          },
        }
      `)
    }
  })
})

describe('updates external resources', () => {
  it('adds new resources', () => {
    const updated = updateHTMLExternalResourcesLinks(
      previewHtml,
      printExternalResources(
        externalResources(
          [genericExternalResource('https://utopia.com/stylesheet.css', 'stylesheet')],
          [googleFontsResource('Roboto', [webFontVariant(400, 'normal')])],
        ),
      ),
    )
    expect(updated).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": "<!DOCTYPE html>
      <html lang=\\"en\\">
        <head>
          <meta charset=\\"utf-8\\">
          <title>Utopia React App</title>
          <!-- Begin Generated Utopia External Links -->
          <link href=\\"https://utopia.com/stylesheet.css\\" rel=\\"stylesheet\\">
          <link href=\\"https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,400\\" rel=\\"stylesheet\\">
          <!-- End Generated Utopia External Links -->
        </head>
        <body>
          <div id=\\"root\\"></div>
        </body>
      </html>",
      }
    `)
  })
})
