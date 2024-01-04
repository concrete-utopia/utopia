import type { ImportDetails } from '../../../core/shared/project-file-types'
import { importAlias, importDetails } from '../../../core/shared/project-file-types'
import { renameDuplicateImports } from './import-worker-utils'

describe('renameDuplicateImports', () => {
  const tests: {
    name: string
    existingImports: { [key: string]: ImportDetails }
    toAdd: { [key: string]: ImportDetails }
    expected: { [key: string]: ImportDetails }
    expectedMap: Map<string, string>
  }[] = [
    {
      name: 'can rename a duplicate import',
      existingImports: { 'fileA.js': importDetails(null, [importAlias('Card')], null) },
      toAdd: { 'fileB.js': importDetails(null, [importAlias('Card')], null) },
      expected: { 'fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null) },
      expectedMap: new Map([['Card', 'Card_2']]),
    },
    {
      name: 'can rename a named import',
      existingImports: {
        'fileA.js': importDetails('Card', [], null),
      },
      toAdd: { 'fileB.js': importDetails('Card', [], null) },
      expected: { 'fileB.js': importDetails('Card_2', [], null) },
      expectedMap: new Map([['Card', 'Card_2']]),
    },
    {
      name: 'can rename an as import',
      existingImports: {
        'fileA.js': importDetails(null, [], 'Card'),
      },
      toAdd: { 'fileB.js': importDetails(null, [], 'Card') },
      expected: { 'fileB.js': importDetails(null, [], 'Card_2') },
      expectedMap: new Map([['Card', 'Card_2']]),
    },
    {
      name: 'can rename in an order',
      existingImports: {
        'fileA.js': importDetails(null, [], 'Card'),
        'fileB.js': importDetails(null, [], 'Card_2'),
        'fileC.js': importDetails(null, [], 'Card_4'),
      },
      toAdd: { 'fileD.js': importDetails(null, [], 'Card') },
      expected: { 'fileD.js': importDetails(null, [], 'Card_3') },
      expectedMap: new Map([['Card', 'Card_3']]),
    },
    {
      name: 'can honor an existing rename',
      existingImports: {
        'fileA.js': importDetails(null, [importAlias('Card')], null),
        'fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null),
      },
      toAdd: { 'fileB.js': importDetails(null, [importAlias('Card')], null) },
      expected: { 'fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null) },
      expectedMap: new Map([['Card', 'Card_2']]),
    },
  ]

  tests.forEach((test) => {
    it(`${test.name}`, () => {
      const result = renameDuplicateImports(test.existingImports, test.toAdd)
      expect(result.imports).toEqual(test.expected)
      expect(result.duplicateNameMapping).toEqual(test.expectedMap)
    })
  })
})
