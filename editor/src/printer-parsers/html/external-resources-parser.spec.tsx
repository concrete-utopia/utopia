import { getStoreHook } from '../../components/inspector/common/inspector-test-utils'
import { NO_OP } from '../../core/shared/utils'
import {
  getExternalResourcesInfo,
  getGeneratedExternalLinkText,
  parseLinkTags,
  googleFontsResource,
  ExternalResources,
} from './external-resources-parser'
import { isRight } from '../../core/shared/either'

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
        <link href=\\"https://fonts.googleapis.com/css2?family=Comfortaa&text=%c2%a1Hola!\\" rel=\\"stylesheet\\">
        <!-- End Generated Utopia External Links -->",
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
  })

  // not really sure how this should
  it('parses and updates editor model', () => {
    const storeHookForTest = getStoreHook(NO_OP)
    let originalResources: ExternalResources | null = null
    storeHookForTest.updateStoreWithImmer((store) => {
      const parsed = getExternalResourcesInfo(store.editor, store.dispatch)
      if (isRight(parsed)) {
        originalResources = { ...parsed.value.externalResources }
        const { externalResources, onSubmitValue } = parsed.value
        onSubmitValue({
          ...externalResources,
          googleFontsResources: [
            ...externalResources.googleFontsResources,
            googleFontsResource('Roboto'),
          ],
        })
      }
    })
    expect(originalResources).toMatchInlineSnapshot(`null`) // hmmmm

    const updatedResources: ExternalResources | null = (() => {
      let updated: ExternalResources | null = null
      // hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      storeHookForTest.useStore((store) => {
        const parsed = getExternalResourcesInfo(store.editor, store.dispatch)
        if (isRight(parsed)) {
          updated = parsed.value.externalResources
        }
      })
      return updated
    })()
    expect(updatedResources).toMatchInlineSnapshot(`null`) // hmmmmmmmmmmm
  })
})
