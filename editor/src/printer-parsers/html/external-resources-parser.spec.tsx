import { parseLinkTags, getGeneratedExternalLinkText } from './external-resources-parser'

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
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=Hello%20World\\" rel=\\"stylesheet\\">
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!\\" rel=\\"stylesheet\\">",
      }
    `)

    const parsedLinkTags = parseLinkTags(parsedLinkTagsText.value)
    expect(parsedLinkTags).toMatchInlineSnapshot(`
      Object {
        "type": "RIGHT",
        "value": Object {
          "genericExternalResources": Array [
            Object {
              "href": "https://utopia.com/styles.css",
              "type": "generic-external-resource",
            },
            Object {
              "href": "http://utopia.com/styles.css",
              "type": "generic-external-resource",
            },
            Object {
              "href": "//utopia.com/styles.css",
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
  })
})
