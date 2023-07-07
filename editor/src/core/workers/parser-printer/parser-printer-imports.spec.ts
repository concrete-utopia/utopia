import { applyPrettier } from 'utopia-vscode-common'
import type { Imports } from '../../shared/project-file-types'
import { exportFunction, importAlias, isParseSuccess } from '../../shared/project-file-types'
import { printCode, printCodeOptions } from './parser-printer'
import { testParseCode } from './parser-printer.test-utils'

describe('Inserting imports into the parsed model', () => {
  it('Inserts all import styles', () => {
    const codeBefore = applyPrettier(
      `
      import * as React from 'react'
      import { Card } from '/src/card'
      import Thing from '/src/thing'
      import { Original as Renamed } from '/src/original'
      `,
      false,
    ).formatted

    // Due to code preservation, this isn't merging the new imports, as otherwise we can't guarantee
    // that we'll preserve required import ordering, comments, or whitespace
    const expectedCodeAfter = applyPrettier(
      `
      import * as React from 'react'
      import { Card } from '/src/card'
      import Thing from '/src/thing'
      import { Original as Renamed } from '/src/original'
      import { OtherCard } from '/src/card'
      import { OtherThing } from '/src/thing'
      import { OtherOriginal as OtherRenamed } from '/src/original'
      import * as Star from '/src/star'
      import '/src/styles.css'
      `,
      false,
    ).formatted

    const expectedImports: Imports = {
      react: {
        importedWithName: null,
        importedFromWithin: [],
        importedAs: 'React',
      },
      '/src/card': {
        importedWithName: null,
        importedFromWithin: [importAlias('Card'), importAlias('OtherCard')],
        importedAs: null,
      },
      '/src/thing': {
        importedWithName: 'Thing',
        importedFromWithin: [importAlias('OtherThing')],
        importedAs: null,
      },
      '/src/original': {
        importedWithName: null,
        importedFromWithin: [
          importAlias('Original', 'Renamed'),
          importAlias('OtherOriginal', 'OtherRenamed'),
        ],
        importedAs: null,
      },
      '/src/star': {
        importedWithName: null,
        importedFromWithin: [],
        importedAs: 'Star',
      },
      '/src/styles.css': {
        importedWithName: null,
        importedFromWithin: [],
        importedAs: null,
      },
    }

    const parsedCode = testParseCode(codeBefore)
    if (isParseSuccess(parsedCode)) {
      const printedCode = printCode(
        '/index.js',
        printCodeOptions(false, true, true),
        expectedImports,
        parsedCode.topLevelElements,
        null,
        [],
      )
      expect(printedCode).toEqual(expectedCodeAfter)
    } else {
      throw new Error('Parse result is not a success.')
    }
  })
})
