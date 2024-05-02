/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testParseModifyPrint"] }] */
import * as PP from '../../shared/property-path'
import type { ArbitraryJSBlock, JSXAttributesEntry } from '../../shared/element-template'
import {
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isJSExpressionMapOrOtherJavaScript,
  isJSXElement,
  isUtopiaJSXComponent,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  getJSXAttribute,
  jsxAttributesFromMap,
  isArbitraryJSBlock,
  jsxElementName,
  emptyComments,
  jsxMapExpression,
  jsIdentifier,
  isJSXAttributesEntry,
  jsOpaqueArbitraryStatement,
  jsAssignmentStatement,
  jsAssignment,
  jsExpressionNestedArray,
  jsxArrayValue,
  jsExpressionNestedObject,
  jsxPropertyAssignment,
  isJSExpressionOtherJavaScript,
  functionParam,
  regularParam,
  jsxAttributeNestedArraySimple,
  clearExpressionUniqueIDs,
  destructuredArray,
  destructuredObject,
  destructuredParamPart,
} from '../../shared/element-template'
import { setJSXValueAtPath } from '../../shared/jsx-attribute-utils'
import { forEachRight } from '../../shared/either'
import type { ParseSuccess } from '../../shared/project-file-types'
import {
  EmptyExportsDetail,
  exportFunction,
  exportVariable,
  exportVariables,
  foldParsedTextFile,
  isParseFailure,
  isParseSuccess,
  parseSuccess,
} from '../../shared/project-file-types'
import {
  clearParseResultUniqueIDsAndEmptyBlocks,
  JustImportViewAndReact,
  simplifyParsedTextFileAttributes,
  testParseCode,
  testParseModifyPrint,
} from './parser-printer.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'
import { TestAppUID, TestSceneUID } from '../../../components/canvas/ui-jsx.test-utils'
import { applyPrettier } from 'utopia-vscode-common'
import {
  BLOCK_RAN_TO_END_FUNCTION_NAME,
  EARLY_RETURN_RESULT_FUNCTION_NAME,
  EARLY_RETURN_VOID_FUNCTION_NAME,
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
} from '../../shared/dom-utils'
import { styleStringInArray } from '../../../utils/common-constants'

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
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
    </Storyboard>
  )
}`,
      false,
    ).formatted
    const parseResult = testParseCode(code)
    foldParsedTextFile(
      (failure) => {
        throw new Error(JSON.stringify(failure))
      },
      (success) => {
        const firstComponent = success.topLevelElements.find(isUtopiaJSXComponent)
        if (firstComponent != null) {
          const view = firstComponent.rootElement
          if (isJSXElement(view)) {
            expect(getJSXAttribute(view.props, 'data-uid')).not.toBeNull()
            const firstChild = view.children[0]
            if (isJSExpressionOtherJavaScript(firstChild)) {
              const elementWithin =
                firstChild.elementsWithin[Object.keys(firstChild.elementsWithin)[0]]
              expect(getJSXAttribute(elementWithin.props, 'data-uid')).not.toBeNull()
            } else {
              throw new Error('First child is not an arbitrary block of code.')
            }
          } else {
            throw new Error('Root element not a JSX element.')
          }
        } else {
          throw new Error('Not a component at the root.')
        }
      },
      (unparsed) => {
        throw new Error(JSON.stringify(unparsed))
      },
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
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
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
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
    </Storyboard>
  )
}
`,
      false,
    ).formatted

    testParseModifyPrint('/index.js', code, expectedCode, (success: ParseSuccess) => {
      const firstComponent = success.topLevelElements.find(isUtopiaJSXComponent)
      if (firstComponent != null) {
        const view = firstComponent.rootElement
        if (isJSXElement(view)) {
          const firstChild = view.children[0]
          if (isJSExpressionOtherJavaScript(firstChild)) {
            const elementWithin = firstChild.elementsWithin['bbb']
            const newAttributes = setJSXValueAtPath(
              elementWithin.props,
              PP.create('style'),
              jsExpressionValue({ left: 20, top: 300 }, emptyComments),
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

  it('should write updated JSX inside a prop back into code', () => {
    const code = applyPrettier(
      `import * as React from "react";
import { View, Storyboard, Scene } from 'utopia-api';

export var App = props => {
  return (
      <View thing={true ? <div data-uid='bbb' /> : null} data-uid='aaa' />
    )
  }

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
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
    <View thing={true ? <div data-uid="bbb" style={{ left: 20, top: 300 }} /> : null} data-uid="aaa" />
  );
};

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        data-uid='${TestSceneUID}'
      >
        <App
          data-uid='${TestAppUID}'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
    </Storyboard>
  )
}
`,
      false,
    ).formatted

    testParseModifyPrint('/index.js', code, expectedCode, (success: ParseSuccess) => {
      const firstComponent = success.topLevelElements.find(isUtopiaJSXComponent)
      if (firstComponent != null) {
        const view = firstComponent.rootElement
        if (isJSXElement(view)) {
          const jsxProp: JSXAttributesEntry | undefined = view.props
            .filter(isJSXAttributesEntry)
            .find((prop) => prop.key === 'thing')
          if (jsxProp != null && isJSExpressionOtherJavaScript(jsxProp.value)) {
            const elementWithin = jsxProp.value.elementsWithin['bbb']
            const newAttributes = setJSXValueAtPath(
              elementWithin.props,
              PP.create('style'),
              jsExpressionValue({ left: 20, top: 300 }, emptyComments),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))

    const myComp = utopiaJSXComponent(
      'MyComp',
      true,
      'var',
      'expression',
      defaultPropsParam,
      [],
      jsxElement(
        'div',
        'abc',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('abc', emptyComments) }),
        [],
      ),
      null,
      false,
      emptyComments,
    )

    const codeBlock = jsExpressionOtherJavaScript(
      [],
      `<MyComp data-uid='aab'/>`,
      `<MyComp data-uid='aab' />;`,
      `return utopiaCanvasJSXLookup("aab", {
  callerThis: this
});`,
      ['React', 'MyComp', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {
        aab: jsxElement(
          'MyComp',
          'aab',
          jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aab', emptyComments) }),
          [],
        ),
      },
      emptyComments,
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments) }),
      [codeBlock],
    )
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
    )
    const topLevelElements = [myComp, whatever].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsIdentifier('arr', '', expect.objectContaining({}), emptyComments),
          jsExpressionOtherJavaScript(
            [
              functionParam(
                false,
                destructuredObject([
                  destructuredParamPart(
                    undefined,
                    functionParam(false, regularParam('n', null)),
                    null,
                  ),
                ]),
              ),
            ],
            `({ n }) => <View data-uid='aab' thing={n} />`,
            `({ n }) => <View data-uid='aab' thing={n} />;`,
            `return ({
  n
}) => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
});`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              aab: jsxElement(
                'View',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                  thing: jsIdentifier('n', '', expect.objectContaining({}), emptyComments),
                }),
                [],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
        ),
      ],
    )
    const jsCode = `const arr = [ { n: 1 } ]`
    const transpiledJsCode = `return (() => {
  const arr = [{
    n: 1
  }];
  return utopiaCanvasBlockRanToEnd({
    arr: arr
  });
})();`
    const arrValue = jsExpressionNestedArray(
      [
        jsxArrayValue(
          jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                'n',
                jsExpressionValue(1, emptyComments, ''),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          emptyComments,
        ),
      ],
      emptyComments,
    )
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['arr'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsAssignmentStatement('const', [jsAssignment(regularParam('arr', arrValue), arrValue)], '')],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsIdentifier('arr', '', expect.objectContaining({}), emptyComments),
          jsExpressionOtherJavaScript(
            [
              functionParam(
                false,
                destructuredObject([
                  destructuredParamPart(
                    'a',
                    functionParam(
                      false,
                      destructuredObject([
                        destructuredParamPart(
                          undefined,
                          functionParam(false, regularParam('n', null)),
                          null,
                        ),
                      ]),
                    ),
                    null,
                  ),
                ]),
              ),
            ],
            `({ a: { n } }) => <View data-uid='aab' thing={n} />`,
            `({ a: { n } }) => <View data-uid='aab' thing={n} />;`,
            `return ({
  a: {
    n
  }
}) => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
});`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              aab: jsxElement(
                'View',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                  thing: jsIdentifier('n', '', expect.objectContaining({}), emptyComments),
                }),
                [],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
        ),
      ],
    )
    const jsCode = `const arr = [ { a: { n: 1 } } ]`
    const transpiledJsCode = `return (() => {
  const arr = [{
    a: {
      n: 1
    }
  }];
  return utopiaCanvasBlockRanToEnd({
    arr: arr
  });
})();`
    const arrValue = jsExpressionNestedArray(
      [
        jsxArrayValue(
          jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                'a',
                jsExpressionNestedObject(
                  [
                    jsxPropertyAssignment(
                      'n',
                      jsExpressionValue(1, emptyComments, ''),
                      emptyComments,
                      emptyComments,
                    ),
                  ],
                  emptyComments,
                ),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          emptyComments,
        ),
      ],
      emptyComments,
    )
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['arr'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsAssignmentStatement('const', [jsAssignment(regularParam('arr', arrValue), arrValue)], '')],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsIdentifier('arr', '', expect.objectContaining({}), emptyComments),
          jsExpressionOtherJavaScript(
            [
              functionParam(
                false,
                destructuredArray([functionParam(false, regularParam('n', null))]),
              ),
            ],
            `([ n ]) => <View data-uid='aab' thing={n} />`,
            `([n]) => <View data-uid='aab' thing={n} />;`,
            `return ([n]) => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
});`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              aab: jsxElement(
                'View',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                  thing: jsIdentifier('n', '', expect.objectContaining({}), emptyComments),
                }),
                [],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
        ),
      ],
    )
    const jsCode = `const arr = [ [ 1 ] ]`
    const transpiledJsCode = `return (() => {
  const arr = [[1]];
  return utopiaCanvasBlockRanToEnd({
    arr: arr
  });
})();`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['arr'],
      [
        JSX_CANVAS_LOOKUP_FUNCTION_NAME,
        BLOCK_RAN_TO_END_FUNCTION_NAME,
        EARLY_RETURN_RESULT_FUNCTION_NAME,
        EARLY_RETURN_VOID_FUNCTION_NAME,
      ],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [
        jsAssignmentStatement(
          'const',
          [
            jsAssignment(
              regularParam(
                'arr',
                jsExpressionNestedArray(
                  [
                    jsxArrayValue(
                      jsExpressionNestedArray(
                        [jsxArrayValue(jsExpressionValue(1, emptyComments), emptyComments)],
                        emptyComments,
                      ),
                      emptyComments,
                    ),
                  ],
                  emptyComments,
                ),
              ),
              jsExpressionNestedArray(
                [
                  jsxArrayValue(
                    jsExpressionNestedArray(
                      [jsxArrayValue(jsExpressionValue(1, emptyComments), emptyComments)],
                      emptyComments,
                    ),
                    emptyComments,
                  ),
                ],
                emptyComments,
              ),
            ),
          ],
          '',
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
      arbitraryBlock,
      false,
      emptyComments,
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsxAttributeNestedArraySimple([jsExpressionValue(1, emptyComments)]),
          jsExpressionOtherJavaScript(
            [functionParam(false, regularParam('n', null))],
            `(n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>`,
            `(n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>;`,
            `return n => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
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
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                }),
                [
                  jsxElement(
                    'div',
                    'aac',
                    jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aac', emptyComments) }),
                    [jsIdentifier('n', '', expect.objectContaining({}), emptyComments)],
                  ),
                ],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const mapJsCode = `arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> )`
    const transpiledMapJsCode = `return arr.map(function (_ref) {
  var _ref2 = babelHelpers.slicedToArray(_ref, 1),
      _ref2$ = babelHelpers.slicedToArray(_ref2[0], 1),
      n = _ref2$[0];

  return utopiaCanvasJSXLookup(\"aab\", {
    n: n,
    callerThis: this
  });
});`
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsExpressionOtherJavaScript(
          [],
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
              'aab',
              jsxAttributesFromMap({
                'data-uid': jsExpressionValue('aab', emptyComments),
                thing: jsExpressionOtherJavaScript(
                  [],
                  'n',
                  'n;',
                  'return n;',
                  ['n'],
                  expect.objectContaining({
                    sources: ['code.tsx'],
                    version: 3,
                    file: 'code.tsx',
                  }),
                  {},
                  emptyComments,
                ),
              }),
              [],
            ),
          },
          emptyComments,
        ),
      ],
    )
    const jsCode = `const arr = [ [ [ 1 ] ] ]`
    const transpiledJsCode = `var arr = [[[1]]];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['arr'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode, ['arr'], [], '')],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('supports passing down the scope to children of components 2', () => {
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsxAttributeNestedArraySimple([jsExpressionValue(1, emptyComments)]),
          jsExpressionOtherJavaScript(
            [functionParam(false, regularParam('n', null))],
            `(n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>`,
            `(n) => <div data-uid='aab'><div data-uid='aac'>{n}</div></div>;`,
            `return n => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
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
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                }),
                [
                  jsxElement(
                    'div',
                    'aac',
                    jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aac', emptyComments) }),
                    [jsIdentifier('n', '', expect.objectContaining({}), emptyComments)],
                  ),
                ],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  xit('supports nested array destructuring in a function param 2', () => {
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
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    const mapJsCode = `arr.map(([[ n ]]) => <View data-uid='aab' thing={n} /> )`
    const transpiledMapJsCode = `return arr.map(function (_ref) {
  var _ref2 = babelHelpers.slicedToArray(_ref, 1),
      _ref2$ = babelHelpers.slicedToArray(_ref2[0], 1),
      n = _ref2$[0];

  return utopiaCanvasJSXLookup(\"aab\", {
    n: n,
    callerThis: this
  });
});`
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [
        jsxMapExpression(
          jsIdentifier('arr', '', null, emptyComments),
          jsExpressionOtherJavaScript(
            [functionParam(false, regularParam('n', null))],
            `({ n }) => <View data-uid='aab' thing={n} />`,
            `({ n }) => <View data-uid='aab' thing={n} />`,
            `return ({
  n
}) => utopiaCanvasJSXLookup("aab", {
  n: n,
  callerThis: this
})`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              aab: jsxElement(
                'View',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments),
                  thing: jsIdentifier('n', '', expect.objectContaining({}), emptyComments),
                }),
                [],
              ),
            },
            emptyComments,
            '',
          ),
          emptyComments,
          ['n'],
        ),
      ],
    )
    const jsCode = `const arr = [ [ [ 1 ] ] ]`
    const transpiledJsCode = `var arr = [[[1]]];
return { arr: arr };`
    const arbitraryBlock = arbitraryJSBlock(
      [],
      jsCode,
      transpiledJsCode,
      ['arr'],
      [JSX_CANVAS_LOOKUP_FUNCTION_NAME],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
      [jsOpaqueArbitraryStatement(jsCode, ['arr'], [], '')],
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
    )
    const topLevelElements = [exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      null,
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('circularly referenced arbitrary blocks parse and produce a combined block', () => {
    const code = `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

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
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)`
    const actualResult = simplifyParsedTextFileAttributes(
      clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code)),
    )
    expect(actualResult).toMatchSnapshot()
  })

  it('Correctly maps elements within arbitrary blocks including combined blocks', () => {
    const code = `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'

export class RenderPropsFunctionChild extends React.Component {
  render() {
    return this.props.children('huha')
  }
}

export const ParsedComponentToBreakUpArbitraryBlocks = (props) => {
  return <div />
}

export function getPicker() {
  class Picker extends React.Component {
    renderPicker(locale) {
      return (
        <RenderPropsFunctionChild>
          {(size) => {
            return (
              <div id='nasty-div'>
                {locale} {size}
              </div>
            )
          }}
        </RenderPropsFunctionChild>
      )
    }

    render() {
      return <RenderPropsFunctionChild>{this.renderPicker}</RenderPropsFunctionChild>
    }
  }

  return Picker
}

const Thing = getPicker()

export var App = (props) => {
  return (
    <Thing data-uid={'aaa'} />
  )
}
export var storyboard = (
  <Storyboard>
    <Scene style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}>
      <App />
    </Scene>
  </Storyboard>
)
`
    const parsedResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))
    if (isParseSuccess(parsedResult)) {
      const { combinedTopLevelArbitraryBlock, topLevelElements } = parsedResult
      const getPickerBlock = topLevelElements.find(
        (e) => isArbitraryJSBlock(e) && e.definedWithin.includes('getPicker'),
      ) as ArbitraryJSBlock
      const results = {
        alone: {
          elements: Object.keys(getPickerBlock.elementsWithin),
          js: getPickerBlock?.transpiledJavascript,
        },
        combined: {
          elements: Object.keys(combinedTopLevelArbitraryBlock!.elementsWithin),
          js: combinedTopLevelArbitraryBlock!.transpiledJavascript,
        },
      }

      // The first lookup call should match the first element, the second lookup call should match the second
      expect(results.alone).toMatchInlineSnapshot(`
        Object {
          "elements": Array [
            "219",
            "971",
          ],
          "js": "return (() => {
          function getPicker() {
            class Picker extends React.Component {
              renderPicker(locale) {
                return React.createElement(RenderPropsFunctionChild, null, size => {
                  return React.createElement(\\"div\\", {
                    id: \\"nasty-div\\"
                  }, locale, \\" \\", size);
                });
              }

              render() {
                return React.createElement(RenderPropsFunctionChild, null, this.renderPicker);
              }

            }

            return Picker;
          }

          return utopiaCanvasBlockRanToEnd({
            getPicker: getPicker
          });
        })();",
        }
      `)

      // The first lookup call in getPicker should match the first element, the second lookup call should match the second
      expect(results.combined).toMatchInlineSnapshot(`
        Object {
          "elements": Array [
            "d1b",
            "064",
          ],
          "js": "return (() => {
          class RenderPropsFunctionChild extends React.Component {
            render() {
              return this.props.children('huha');
            }

          }

          function getPicker() {
            class Picker extends React.Component {
              renderPicker(locale) {
                return React.createElement(RenderPropsFunctionChild, null, size => {
                  return React.createElement(\\"div\\", {
                    id: \\"nasty-div\\"
                  }, locale, \\" \\", size);
                });
              }

              render() {
                return React.createElement(RenderPropsFunctionChild, null, this.renderPicker);
              }

            }

            return Picker;
          }

          const Thing = getPicker();
          return utopiaCanvasBlockRanToEnd({
            RenderPropsFunctionChild: RenderPropsFunctionChild,
            getPicker: getPicker,
            Thing: Thing
          });
        })();",
        }
      `)
    } else {
      throw new Error(`Failed to parse code`)
    }
  })

  it('should not add intrinsic elements to the defined elsewhere of an arbitrary block', () => {
    const code = `import React from "react";
      export var App = (props) => {
        return (
          <div>
            {[1].map(i => <someIntrinsicElement/>)}
          </div>
        )
      }
    `

    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))

    const rootElement = expect.objectContaining({
      name: jsxElementName('div', []),
      children: [
        expect.objectContaining({
          type: 'JSX_MAP_EXPRESSION',
          valueToMap: clearExpressionUniqueIDs(
            jsxAttributeNestedArraySimple([jsExpressionValue(1, emptyComments)]),
          ),
          mapFunction: expect.objectContaining({
            originalJavascript: `i => <someIntrinsicElement/>`,
          }),
        }),
      ],
    })

    const exported = utopiaJSXComponent(
      'App',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      rootElement,
      null,
      false,
      emptyComments,
    )

    const expectedResult = parseSuccess(
      expect.objectContaining({}),
      expect.arrayContaining([exported]),
      expect.objectContaining({}),
      null,
      null,
      expect.objectContaining({}),
      expect.objectContaining({}),
    )

    expect(actualResult).toEqual(expectedResult)
  })
})
