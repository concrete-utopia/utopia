import * as PP from '../../shared/property-path'
import {
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isJSXArbitraryBlock,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxArbitraryBlock,
  jsxAttributeOtherJavaScript,
  jsxAttributeValue,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
} from '../../shared/element-template'
import { setJSXValueAtPath } from '../../shared/jsx-attributes'
import { forEachRight } from '../../shared/either'
import {
  addModifierExportToDetail,
  addNamedExportToDetail,
  EmptyExportsDetail,
  foldParsedTextFile,
  ParseSuccess,
} from '../../shared/project-file-types'
import { parseSuccess } from '../common/project-file-utils'
import { applyPrettier } from './prettier-utils'
import {
  clearParseResultUniqueIDs,
  JustImportViewAndReact,
  testParseCode,
  testParseModifyPrint,
} from './parser-printer.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'
import { emptyComments } from './parser-printer-comments'

describe('JSX parser', () => {
  it('should add in uid attributes for elements', () => {
    const code = applyPrettier(
      `import * as React from "react";
import { View, Storyboard, Scene } from 'utopia-api';
export var App = props => {
  return (
    <View>
      {<div />}
    </View>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
        data-uid='scene-0'
      />
    </Storyboard>
  )
}`,
      false,
    ).formatted
    const parseResult = testParseCode(code)
    foldParsedTextFile(
      (failure) => fail(failure),
      (success) => {
        const firstComponent = success.topLevelElements[0]
        if (isUtopiaJSXComponent(firstComponent)) {
          const view = firstComponent.rootElement
          if (isJSXElement(view)) {
            expect(view.props['data-uid']).not.toBeNull()
            const firstChild = view.children[0]
            if (isJSXArbitraryBlock(firstChild)) {
              const elementWithin =
                firstChild.elementsWithin[Object.keys(firstChild.elementsWithin)[0]]
              expect(elementWithin.props['data-uid']).not.toBeNull()
            } else {
              fail('First child is not an arbitrary block of code.')
            }
          } else {
            fail('Root element not a JSX element.')
          }
        } else {
          fail('Not a component at the root.')
        }
      },
      (unparsed) => fail(unparsed),
      parseResult,
    )
  })
  it('should write updated arbitrary elements back into code', () => {
    const code = applyPrettier(
      `import * as React from "react";
import { View, Storyboard, Scene } from 'utopia-api';
export var App = props => {
  return (
    <View data-uid='aaa'>
      {<div data-uid='bbb' />}
    </View>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
        data-uid='scene-0'
      />
    </Storyboard>
  )
}
`,
      false,
    ).formatted

    const expectedCode = applyPrettier(
      `import * as React from "react";
import { View, Storyboard, Scene } from 'utopia-api';
export var App = props => {
  return (
    <View data-uid="aaa">
      {<div data-uid="bbb" style={{ left: 20, top: 300 }} />}
    </View>
  );
};
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
        data-uid='scene-0'
      />
    </Storyboard>
  )
}
`,
      false,
    ).formatted

    testParseModifyPrint(code, expectedCode, (success: ParseSuccess) => {
      const firstComponent = success.topLevelElements[0]
      if (isUtopiaJSXComponent(firstComponent)) {
        const view = firstComponent.rootElement
        if (isJSXElement(view)) {
          const firstChild = view.children[0]
          if (isJSXArbitraryBlock(firstChild)) {
            const elementWithin = firstChild.elementsWithin['bbb']
            const newAttributes = setJSXValueAtPath(
              elementWithin.props,
              PP.create(['style']),
              jsxAttributeValue({ left: 20, top: 300 }),
            )
            forEachRight(newAttributes, (updated) => {
              elementWithin.props = updated
            })
          }
        }
      }
      return success
    })
  })

  it('Supports using top level components inside an arbitrary block', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
var MyComp = (props) => <div data-uid='abc'/>
export var whatever = props => (
<View data-uid='aaa'>
  {<MyComp data-uid='aab'/>}
</View>
)
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))

    const myComp = utopiaJSXComponent(
      'MyComp',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      jsxElement('div', { 'data-uid': jsxAttributeValue('abc') }, []),
      null,
      false,
      emptyComments,
      emptyComments,
    )

    const codeBlock = jsxArbitraryBlock(
      `<MyComp data-uid='aab'/>`,
      `<MyComp data-uid='aab' />;`,
      `return utopiaCanvasJSXLookup("aab", {});`,
      ['React', 'MyComp', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      { aab: jsxElement('MyComp', { 'data-uid': jsxAttributeValue('aab') }, []) },
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [codeBlock])
    const whatever = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'parenthesized-expression',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [myComp, whatever].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('supports object destructuring in a function param', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const arr = [ { n: 1 } ]
  return (
    <View data-uid='aaa'>
      { arr.map(({ n }) => <View data-uid='aab' thing={n} /> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          `arr.map(({ n }) => <View data-uid='aab' thing={n} /> )`,
          `arr.map(({ n }) => <View data-uid='aab' thing={n} />);`,
          `return arr.map(function (_ref) {
  var n = _ref.n;
  return utopiaCanvasJSXLookup("aab", {
    n: n
  });
});`,
          ['arr', 'React', 'View', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aab'),
                thing: jsxAttributeOtherJavaScript(
                  'n',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                ),
              },
              [],
            ),
          },
        ),
      ],
    )
    const jsCode = `const arr = [{ n: 1 }];`
    const transpiledJsCode = `var arr = [{
  n: 1
}];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['arr'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      emptyComments,
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('supports nested object destructuring in a function param', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const arr = [ { a: { n: 1 } } ]
  return (
    <View data-uid='aaa'>
      { arr.map(({ a: { n } }) => <View data-uid='aab' thing={n} /> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          `arr.map(({ a: { n } }) => <View data-uid='aab' thing={n} /> )`,
          `arr.map(({ a: { n } }) => <View data-uid='aab' thing={n} />);`,
          `return arr.map(function (_ref) {
  var n = _ref.a.n;
  return utopiaCanvasJSXLookup("aab", {
    n: n
  });
});`,
          ['arr', 'React', 'View', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aab'),
                thing: jsxAttributeOtherJavaScript(
                  'n',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                ),
              },
              [],
            ),
          },
        ),
      ],
    )
    const jsCode = `const arr = [{ a: { n: 1 } }];`
    const transpiledJsCode = `var arr = [{
  a: {
    n: 1
  }
}];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['arr'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      emptyComments,
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('supports array destructuring in a function param', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const arr = [ [ 1 ] ]
  return (
    <View data-uid='aaa'>
      { arr.map(([ n ]) => <View data-uid='aab' thing={n} /> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const originalMapJsCode = `arr.map(([ n ]) => <View data-uid='aab' thing={n} /> )`
    const mapJsCode = `arr.map(([n]) => <View data-uid='aab' thing={n} />);`
    const transpiledMapJsCode = `return arr.map(function (_ref) {
  var _ref2 = babelHelpers.slicedToArray(_ref, 1),
      n = _ref2[0];

  return utopiaCanvasJSXLookup(\"aab\", {
    n: n
  });
});`
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          originalMapJsCode,
          mapJsCode,
          transpiledMapJsCode,
          ['arr', 'React', 'View', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aab'),
                thing: jsxAttributeOtherJavaScript(
                  'n',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                ),
              },
              [],
            ),
          },
        ),
      ],
    )
    const jsCode = `const arr = [[1]];`
    const transpiledJsCode = `var arr = [[1]];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['arr'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      emptyComments,
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('supports passing down the scope to children of components', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  return (
    <View data-uid='aaa'>
      { [1].map((n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          `[1].map((n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div> )`,
          `[1].map(n => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>);`,
          `return [1].map(function (n) {
  return utopiaCanvasJSXLookup("aab", {
    n: n
  });
});`,
          ['React', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'div',
              {
                'data-uid': jsxAttributeValue('aab'),
              },
              [
                jsxElement(
                  'div',
                  {
                    'data-uid': jsxAttributeValue('aac'),
                  },
                  [
                    jsxArbitraryBlock(
                      `n`,
                      `n;`,
                      `return n;`,
                      ['n'],
                      expect.objectContaining({
                        sources: ['code.tsx'],
                        version: 3,
                        file: 'code.tsx',
                      }),
                      {},
                    ),
                  ],
                ),
              ],
            ),
          },
        ),
      ],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  xit('supports nested array destructuring in a function param', () => {
    // FIXME Nested array destructuring doesn't work
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const arr = [ [ [ 1 ] ] ]
  return (
    <View data-uid='aaa'>
      { arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const mapJsCode = `arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> )`
    const transpiledMapJsCode = `return arr.map(function (_ref) {
  var _ref2 = babelHelpers.slicedToArray(_ref, 1),
      _ref2$ = babelHelpers.slicedToArray(_ref2[0], 1),
      n = _ref2$[0];

  return utopiaCanvasJSXLookup(\"aab\", {
    n: n
  });
});`
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          mapJsCode,
          mapJsCode,
          transpiledMapJsCode,
          ['arr', 'React', 'View', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aab'),
                thing: jsxAttributeOtherJavaScript(
                  'n',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                ),
              },
              [],
            ),
          },
        ),
      ],
    )
    const jsCode = `const arr = [ [ [ 1 ] ] ]`
    const transpiledJsCode = `var arr = [[[1]]];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['arr'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      emptyComments,
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('supports passing down the scope to children of components', () => {
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  return (
    <View data-uid='aaa'>
      { [1].map((n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          `[1].map((n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div> )`,
          `[1].map(n => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>);`,
          `return [1].map(function (n) {
  return utopiaCanvasJSXLookup("aab", {
    n: n
  });
});`,
          ['React', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'div',
              {
                'data-uid': jsxAttributeValue('aab'),
              },
              [
                jsxElement(
                  'div',
                  {
                    'data-uid': jsxAttributeValue('aac'),
                  },
                  [
                    jsxArbitraryBlock(
                      `n`,
                      `n;`,
                      `return n;`,
                      ['n'],
                      expect.objectContaining({
                        sources: ['code.tsx'],
                        version: 3,
                        file: 'code.tsx',
                      }),
                      {},
                    ),
                  ],
                ),
              ],
            ),
          },
        ),
      ],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  xit('supports nested array destructuring in a function param', () => {
    // FIXME Nested array destructuring doesn't work
    const code = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const arr = [ [ [ 1 ] ] ]
  return (
    <View data-uid='aaa'>
      { arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> ) }
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const mapJsCode = `arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> )`
    const transpiledMapJsCode = `return arr.map(function (_ref) {
  var _ref2 = babelHelpers.slicedToArray(_ref, 1),
      _ref2$ = babelHelpers.slicedToArray(_ref2[0], 1),
      n = _ref2$[0];

  return utopiaCanvasJSXLookup(\"aab\", {
    n: n
  });
});`
    const view = jsxElement(
      'View',
      {
        'data-uid': jsxAttributeValue('aaa'),
      },
      [
        jsxArbitraryBlock(
          mapJsCode,
          mapJsCode,
          transpiledMapJsCode,
          ['arr', 'React', 'View', 'utopiaCanvasJSXLookup'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
          {
            aab: jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aab'),
                thing: jsxAttributeOtherJavaScript(
                  'n',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                ),
              },
              [],
            ),
          },
        ),
      ],
    )
    const jsCode = `const arr = [ [ [ 1 ] ] ]`
    const transpiledJsCode = `var arr = [[[1]]];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['arr'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      emptyComments,
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
      false,
      emptyComments,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('circularly referenced arbitrary blocks parse and produce a combined block', () => {
    const code = `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'

function a(n) {
  if (n <= 0) {
    return 0
  } else {
    return b(n - 1)
  }
}

export var App = (props) => {
  return (
    <div
      data-uid='aaa'
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
    >{b(5)} - {a(5)}</div>
  )
}

function b(n) {
  if (n <= 0) {
    return 0
  } else {
    return a(n - 1)
  }
}

export var storyboard = (
  <Storyboard data-uid='bbb' layout={{ layoutSystem: 'pinSystem' }}>
    <Scene
      data-uid='ccc'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(actualResult).toMatchSnapshot()
  })
})
