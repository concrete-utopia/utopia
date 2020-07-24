import { previewHtml } from '../../core/model/new-project-files'
import { isRight } from '../../core/shared/either'
import {
  externalResources,
  genericExternalResource,
  getGeneratedExternalLinkText,
  googleFontsResource,
  parseLinkTags,
  updateHTMLExternalResourcesLinks,
} from './external-resources-parser'

const testFile = `
<!DOCTYPE html>
<html lang="en">
  <head>
  <!-- Begin Generated Utopia External Links -->
  <link href="https://fonts.googleapis.com/css2?family=Red+Rose" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Red+Rose:wght@400&display=swap" rel="stylesheet">
  <link href="https://utopia.com/styles.css" rel="stylesheet">
  <link href="http://utopia.com/styles.css" rel="stylesheet">
  <!-- Random extra comment that doesn't belong -->
  <link href="//utopia.com/styles.css" rel="stylesheet">
  <link href="/favicon.ico" rel="icon">
  <link href="apple-icon-114.png" rel="apple-touch-icon-precomposed" sizes="114x114" type="image/png">
  <link rel="preload" href="myFont.woff2" as="font" type="font/woff2" crossorigin="anonymous">
  <link href="https://fonts.googleapis.com/css2?family=Comfortaa&text=Hello%20World" rel="stylesheet">
  <link href="https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!" rel="stylesheet">
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
        "value": "<link href=\\"https://fonts.googleapis.com/css2?family=Red+Rose\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Red+Rose:wght@400&display=swap\\" rel=\\"stylesheet\\">
        <link href=\\"https://utopia.com/styles.css\\" rel=\\"stylesheet\\">
        <link href=\\"http://utopia.com/styles.css\\" rel=\\"stylesheet\\">
        <!-- Random extra comment that doesn't belong -->
        <link href=\\"//utopia.com/styles.css\\" rel=\\"stylesheet\\">
        <link href=\\"/favicon.ico\\" rel=\\"icon\\">
        <link href=\\"apple-icon-114.png\\" rel=\\"apple-touch-icon-precomposed\\" sizes=\\"114x114\\" type=\\"image/png\\">
        <link rel=\\"preload\\" href=\\"myFont.woff2\\" as=\\"font\\" type=\\"font/woff2\\" crossorigin=\\"anonymous\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=Hello%20World\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!\\" rel=\\"stylesheet\\">",
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
                "href": "/favicon.ico",
                "rel": "icon",
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
                "fontFamily": "Red Rose",
                "fontStyleParams": undefined,
                "type": "google-fonts-resource",
              },
              Object {
                "fontFamily": "Red Rose",
                "fontStyleParams": ":wght@400",
                "type": "google-fonts-resource",
              },
              Object {
                "fontFamily": "Comfortaa",
                "fontStyleParams": undefined,
                "type": "google-fonts-resource",
              },
              Object {
                "fontFamily": "Comfortaa",
                "fontStyleParams": undefined,
                "type": "google-fonts-resource",
              },
            ],
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
      externalResources(
        [genericExternalResource('https://utopia.com/stylesheet.css', 'stylesheet')],
        [googleFontsResource('Roboto')],
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
          <link href=\\"https://fonts.googleapis.com/css2?family=Roboto\\" rel=\\"stylesheet\\">
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

describe('updates external resources', () => {
  it('adds new resources', () => {
    const updated = updateHTMLExternalResourcesLinks(
      previewHtml,
      externalResources(
        [genericExternalResource('https://utopia.com/stylesheet.css', 'stylesheet')],
        [googleFontsResource('Roboto')],
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
          <link href=\\"https://fonts.googleapis.com/css2?family=Roboto\\" rel=\\"stylesheet\\">
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
