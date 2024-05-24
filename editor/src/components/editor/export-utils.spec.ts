import { isParseSuccess, parseFailure, unparsed } from '../../core/shared/project-file-types'
import { emptySet } from '../../core/shared/set-utils'
import { parseCode } from '../../core/workers/parser-printer/parser-printer'
import { getExportedComponentImports } from './export-utils'

describe('getExportedComponentImports', () => {
  it('returns null for an unparsed value', () => {
    const actualResult = getExportedComponentImports('/src/app.js', '/src/index.js', unparsed, {})
    expect(actualResult).toMatchInlineSnapshot(`null`)
  })
  it('returns null for a parse failure', () => {
    const actualResult = getExportedComponentImports(
      '/src/app.js',
      '/src/index.js',
      parseFailure(null, null, 'Parse test failure.', []),
      {},
    )
    expect(actualResult).toMatchInlineSnapshot(`null`)
  })
  it('returns the expected components for a parse success', () => {
    const codeForFile = `import React from "react";
export var Whatever = (props) => {
  return (
    <div />
  )
}`
    const parseResult = parseCode(
      '/src/index.js',
      codeForFile,
      null,
      emptySet(),
      'do-not-apply-steganography',
    )
    expect(isParseSuccess(parseResult)).toEqual(true)

    const actualResult = getExportedComponentImports(
      '/src/app.js',
      '/src/index.js',
      parseResult,
      {},
    )
    expect(actualResult).toMatchInlineSnapshot(`
      Array [
        Object {
          "importsToAdd": Object {
            "/src/index.js": Object {
              "importedAs": null,
              "importedFromWithin": Array [
                Object {
                  "alias": "Whatever",
                  "name": "Whatever",
                },
              ],
              "importedWithName": null,
            },
          },
          "listingName": "Whatever",
        },
      ]
    `)
  })
})
