import { importAlias, importDetails } from '../../shared/project-file-types'
import { addImport, mergeImports } from './project-file-utils'

describe('mergeImports', () => {
  it('can merge an empty imports', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      {},
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
    })
  })

  it('combines two separate imports', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      { '/src/fileB.js': importDetails(null, [importAlias('FlexRow')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
      '/src/fileB.js': importDetails(null, [importAlias('FlexRow')], null),
    })
  })

  it('combines two imports pointing to the same file', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      { '/src/fileA.js': importDetails(null, [importAlias('FlexRow')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card'), importAlias('FlexRow')], null),
    })
  })

  it('combines two imports pointing to the same file, even if the relative path are written differently', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      { './fileA': importDetails(null, [importAlias('FlexRow')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card'), importAlias('FlexRow')], null),
    })
  })

  it('combines the same thing imported smartly', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
    })
  })

  it('combines the same thing imported smartly, even if the relative path are written differently', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      {
        './fileA.js': importDetails(null, [importAlias('Card')], null),
        '../src/fileA.js': importDetails(null, [importAlias('FlexRow')], null),
      },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card'), importAlias('FlexRow')], null),
    })
  })

  it('does not duplicate same import from aliased sources', () => {
    const result = mergeImports(
      '/src/code.js',
      [[/@h2\/([^/]*)/y, ['/app/components/hydrogen/$1']]],
      { '@h2/ProductCard': importDetails(null, [importAlias('Card')], null) },
      {
        '/app/components/hydrogen/ProductCard.jsx': importDetails(
          null,
          [importAlias('Card')],
          null,
        ),
      },
    )
    expect(result.imports).toEqual({
      '@h2/ProductCard': importDetails(null, [importAlias('Card')], null),
    })
    expect(result.duplicateNameMapping).toEqual(new Map())
  })

  it('handles duplicate imports', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      {
        '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
        '/src/fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null),
      },
      { '/src/fileC.js': importDetails(null, [importAlias('Card')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
      '/src/fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null),
      '/src/fileC.js': importDetails(null, [importAlias('Card', 'Card_3')], null),
    })

    expect(result.duplicateNameMapping).toEqual(new Map([['Card', 'Card_3']]))
  })

  it('handles existing duplicate imports', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      {
        '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
        '/src/fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null),
      },
      { '/src/fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card')], null),
      '/src/fileB.js': importDetails(null, [importAlias('Card', 'Card_2')], null),
    })

    expect(result.duplicateNameMapping).toEqual(new Map())
  })

  it('combines the same thing imported smartly, even if the relative path are written differently, with omitted file extension', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails(null, [importAlias('Card')], null) },
      {
        '../src/fileA': importDetails(null, [importAlias('FlexRow')], null),
      },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails(null, [importAlias('Card'), importAlias('FlexRow')], null),
    })
  })

  it('default import doesnt override existing default import', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails('Card', [], null) },
      { '/src/fileA.js': importDetails('Flexrow', [], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails('Card', [], null),
    })
  })

  it('adds non-relative import with ease', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { '/src/fileA.js': importDetails('Card', [], null) },
      { 'component-library': importDetails('Flexrow', [], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA.js': importDetails('Card', [], null),
      'component-library': importDetails('Flexrow', [], null),
    })
  })

  it('combines non-relative import with ease', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      { 'component-library': importDetails(null, [importAlias('Card')], null) },
      { 'component-library': importDetails(null, [importAlias('FlexRow')], null) },
    )

    expect(result.imports).toEqual({
      'component-library': importDetails(null, [importAlias('Card'), importAlias('FlexRow')], null),
    })
  })

  it('handles multiple imports for the same file with different paths', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      {
        '/src/fileA': importDetails(null, [importAlias('Card')], null),
        './fileA': importDetails(null, [importAlias('OtherCard')], null),
      },
      { '/src/fileB': importDetails(null, [importAlias('FlexRow')], null) },
    )

    expect(result.imports).toEqual({
      '/src/fileA': importDetails(null, [importAlias('Card'), importAlias('OtherCard')], null),
      '/src/fileB': importDetails(null, [importAlias('FlexRow')], null),
    })
  })

  it('does not add an import for the current file', () => {
    const result = mergeImports(
      '/src/code.js',
      [],
      {
        '/src/fileA': importDetails(null, [importAlias('Card')], null),
        './fileA': importDetails(null, [importAlias('OtherCard')], null),
      },
      {
        '/src/code.js': importDetails(null, [importAlias('FlexRow')], null),
        './code.js': importDetails(null, [importAlias('FlexCol')], null),
        '/src/code': importDetails(null, [importAlias('OtherFlexRow')], null),
        './code': importDetails(null, [importAlias('OtherFlexCol')], null),
      },
    )

    expect(result.imports).toEqual({
      '/src/fileA': importDetails(null, [importAlias('Card'), importAlias('OtherCard')], null),
    })
  })

  it('merges namespaced import and default import without named imports', () => {
    {
      const result = mergeImports(
        '/src/code.js',
        [],
        { library: importDetails('Library', [], null) },
        { library: importDetails(null, [], 'Library') },
      )

      /**
       * import Library from 'library'
       * import * as Library from 'library'
       *
       * becomes
       *
       * import Library from 'library'
       */
      expect(result.imports).toEqual({ library: importDetails('Library', [], null) })
    }
    {
      const result = mergeImports(
        '/src/code.js',
        [],
        { LibraryToo: importDetails(null, [], 'library-too') },
        { LibraryToo: importDetails('library-too', [], null) },
      )

      /**
       * import * as Library from 'library'
       * import Library from 'library'
       * (same as the previous test but the other way around)
       */
      expect(result.imports).toEqual({ LibraryToo: importDetails('library-too', [], null) })
    }
    {
      const result = mergeImports(
        '/src/code.js',
        [],
        {
          'death-star': importDetails(
            'DeathStar',
            [importAlias('hello', 'helloFn'), importAlias('General', 'GeneralComponent')],
            null,
          ),
        },
        {
          'death-star': importDetails(
            null,
            [importAlias('there', 'thereFn'), importAlias('Kenobi', 'KenobiComponent')],
            'DeathStar',
          ),
        },
      )

      /**
       * import DeathStar, { hello as helloFn, General as GeneralComponent } from 'death-star'
       * import * as DeathStar, { there as thereFn, Kenobi as KenobiComponent } from 'death-star'
       *
       * becomes
       *
       * import DeathStar, { hello as helloFn, General as GeneralComponent, there as thereFn, Kenobi as KenobiComponent } from 'death-star'
       */

      expect(result.imports).toEqual({
        'death-star': importDetails(
          'DeathStar',
          [
            importAlias('hello', 'helloFn'),
            importAlias('General', 'GeneralComponent'),
            importAlias('there', 'thereFn'),
            importAlias('Kenobi', 'KenobiComponent'),
          ],
          null,
        ),
      })
    }
  })
})

describe('addImport', () => {
  it('does not add an import for the current file', () => {
    const result = addImport(
      '/src/code.js',
      [],
      '/src/code.js',
      null,
      [importAlias('FlexRow'), importAlias('FlexCol')],
      null,
      {
        '/src/fileA': importDetails(null, [importAlias('Card')], null),
        './fileA': importDetails(null, [importAlias('OtherCard')], null),
      },
    )

    expect(result.imports).toEqual({
      '/src/fileA': importDetails(null, [importAlias('Card'), importAlias('OtherCard')], null),
    })
  })
})
