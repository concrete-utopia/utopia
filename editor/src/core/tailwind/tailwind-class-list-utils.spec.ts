import type { TailwindClassParserResult } from './tailwind-class-list-utils'
import {
  addNewClasses,
  getClassListFromParsedClassList,
  getParsedClassList,
  removeClasses,
  updateExistingClasses,
} from './tailwind-class-list-utils'

describe('tailwind class list utils', () => {
  describe('class list parsing / writing', () => {
    function classNameAstSlice(ast: TailwindClassParserResult) {
      switch (ast.type) {
        case 'unparsed':
          return ast
        case 'parsed':
          const slice: Record<string, unknown> = {
            type: ast.type,
            property: ast.ast.property,
            value: ast.ast.value,
          }
          if (ast.ast.negative) {
            slice.negative = true
          }
          return slice
      }
    }
    it('can parse the class list', () => {
      const classList = getParsedClassList(
        'p-4 m-4 lg:w-4 hover:text-red-100 -top-4 flex flex-row fancyButton',
        null,
      )
      expect(classList.map(classNameAstSlice)).toMatchInlineSnapshot(`
        Array [
          Object {
            "property": "padding",
            "type": "parsed",
            "value": "1rem",
          },
          Object {
            "property": "margin",
            "type": "parsed",
            "value": "1rem",
          },
          Object {
            "property": "width",
            "type": "parsed",
            "value": "1rem",
          },
          Object {
            "property": "textColor",
            "type": "parsed",
            "value": "#fee2e2",
          },
          Object {
            "negative": true,
            "property": "positionTop",
            "type": "parsed",
            "value": "1rem",
          },
          Object {
            "property": "display",
            "type": "parsed",
            "value": "flex",
          },
          Object {
            "property": "flexDirection",
            "type": "parsed",
            "value": "row",
          },
          Object {
            "className": "fancyButton",
            "type": "unparsed",
          },
        ]
      `)
    })

    it('respects the tailwind config when parsing the class list', () => {
      const classList = getParsedClassList('flex gap-huge', {
        content: [],
        theme: { extend: { gap: { huge: '123px' } } },
      })
      expect(classList.map(classNameAstSlice)).toMatchInlineSnapshot(`
        Array [
          Object {
            "property": "display",
            "type": "parsed",
            "value": "flex",
          },
          Object {
            "property": "gap",
            "type": "parsed",
            "value": "123px",
          },
        ]
      `)
    })

    it('can stringify class list', () => {
      const classList = getClassListFromParsedClassList(
        [
          {
            type: 'parsed',
            ast: { property: 'padding', value: '2rem', variants: [], negative: false },
          },
          {
            type: 'parsed',
            ast: { property: 'positionTop', value: '-14px', variants: [], negative: false },
          },
          {
            type: 'parsed',
            ast: { property: 'display', value: 'flex', variants: [], negative: false },
          },
          {
            type: 'parsed',
            ast: { property: 'gap', value: '123px', variants: [], negative: false },
          },
          { type: 'unparsed', className: 'highlight-button' },
        ],
        {
          content: [],
          theme: { extend: { gap: { huge: '123px' } } },
        },
      )

      expect(classList).toMatchInlineSnapshot(`"p-8 -top-[14px] flex gap-huge highlight-button"`)
    })

    it('stringifying a parsed class list yields the same class list string', () => {
      const startingClassList =
        'p-4 m-2 w-4 lg:w-8 text-black hover:text-red-200 flex flex-row fancy-button'
      const classList = getClassListFromParsedClassList(
        getParsedClassList(startingClassList, null),
        null,
      )
      expect(classList).toEqual(startingClassList)
    })
  })

  describe('removing classes', () => {
    it('can remove property', () => {
      const classList = getParsedClassList('p-4 m-2 text-white w-4 flex flex-row', null)
      const updatedClassList = removeClasses(['padding', 'textColor'])(classList)
      expect(getClassListFromParsedClassList(updatedClassList, null)).toMatchInlineSnapshot(
        `"m-2 w-4 flex flex-row"`,
      )
    })
    it('does not remove property with selector', () => {
      const classList = getParsedClassList(
        'p-4 m-2 text-white hover:text-red-100 w-4 flex flex-row',
        null,
      )
      const updatedClassList = removeClasses(['padding', 'textColor'])(classList)
      expect(getClassListFromParsedClassList(updatedClassList, null)).toMatchInlineSnapshot(
        `"m-2 hover:text-red-100 w-4 flex flex-row"`,
      )
    })
  })

  describe('updating classes', () => {
    it('can update class in class list', () => {
      const classList = getParsedClassList('p-4 m-2 text-white w-4 flex flex-row', null)
      const updatedClassList = updateExistingClasses({
        flexDirection: 'column',
        width: '23px',
      })(classList)
      expect(getClassListFromParsedClassList(updatedClassList, null)).toMatchInlineSnapshot(
        `"p-4 m-2 text-white w-[23px] flex flex-col"`,
      )
    })
    it('does not remove property with selector', () => {
      const classList = getParsedClassList('p-4 hover:p-6 m-2 text-white w-4 flex flex-row', null)
      const updatedClassList = updateExistingClasses({ padding: '8rem' })(classList)
      expect(getClassListFromParsedClassList(updatedClassList, null)).toMatchInlineSnapshot(
        `"p-32 hover:p-6 m-2 text-white w-4 flex flex-row"`,
      )
    })
  })

  describe('adding new classes', () => {
    it('can add new class to class list', () => {
      const classList = getParsedClassList('p-4 m-2 text-white w-4 flex flex-row', null)
      const updatedClassList = addNewClasses({
        backgroundColor: 'white',
        justifyContent: 'space-between',
        positionLeft: '-20px',
      })(classList)
      expect(getClassListFromParsedClassList(updatedClassList, null)).toMatchInlineSnapshot(
        `"p-4 m-2 text-white w-4 flex flex-row bg-white justify-between -left-[20px]"`,
      )
    })
  })
})
