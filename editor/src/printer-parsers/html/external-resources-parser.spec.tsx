import { act, renderHook } from '@testing-library/react-hooks'
import * as React from 'react'
import { EditorStateContext } from '../../components/editor/store/store-hook'
import { getStoreHook } from '../../components/inspector/common/inspector-test-utils'
import { isRight } from '../../core/shared/either'
import { NO_OP } from '../../core/shared/utils'
import {
  externalResources,
  genericExternalResource,
  getGeneratedExternalLinkText,
  googleFontsResource,
  parseLinkTags,
  useExternalResources,
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

  it('parses and updates editor model', () => {
    // the dispatch isn't doing anything here. how do i make this work?
    const storeHookForTest = getStoreHook(NO_OP)
    const wrapper: React.FunctionComponent = ({ children }) => (
      <EditorStateContext.Provider value={storeHookForTest}>{children}</EditorStateContext.Provider>
    )
    const { result, rerender } = renderHook(() => useExternalResources(), { wrapper })
    expect(isRight(result.current)).toBeTruthy()
    expect(result.current).toMatchInlineSnapshot(`
              Object {
                "type": "RIGHT",
                "value": Object {
                  "externalResources": Object {
                    "genericExternalResources": Array [],
                    "googleFontsResources": Array [],
                  },
                  "onSubmitValue": [Function],
                },
              }
          `)

    act(() => {
      expect(isRight(result.current)).toBeTruthy()
      if (isRight(result.current)) {
        result.current.value.onSubmitValue(
          externalResources(
            [genericExternalResource('https://utopia.app/stylesheet.css', 'stylesheet')],
            [googleFontsResource('Roboto')],
          ),
        )
      }
    })

    expect(result.current).toMatchInlineSnapshot()
  })
})
