/* eslint-disable jest/expect-expect */
import {
  clearParseResultUniqueIDsAndEmptyBlocks,
  testParseCode,
  JustImportViewAndReact,
} from './parser-printer.test-utils'
import {
  jsxElement,
  jsExpressionValue,
  utopiaJSXComponent,
  defaultPropsParam,
  clearTopLevelElementUniqueIDs,
  functionParam,
  regularParam,
  destructuredObject,
  destructuredParamPart,
  destructuredArray,
  omittedParam,
  jsExpressionOtherJavaScript,
  jsxAttributesFromMap,
  emptyComments,
  clearJSXElementChildUniqueIDs,
  jsOpaqueArbitraryStatement,
} from '../../shared/element-template'
import { printCode, printCodeOptions } from './parser-printer'
import {
  exportFunction,
  exportVariable,
  exportVariables,
  isParseSuccess,
  parseSuccess,
} from '../../shared/project-file-types'

const codeWithBasicPropsObject = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithBasicPropsObjectWithDefault = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props = {thing: true}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithRenamedBasicPropsObject = `import React from "react";
import { View } from "utopia-api";
export var whatever = (myProps) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredPropsObject = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({prop}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredPropsObjectWithDefault = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({prop = 5}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredPropsObjectWithRenamedParam = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({prop: renamedProp}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredPropsObjectWithRenamedParamAndDefault = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({prop: renamedProp = 5}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredPropsObjectWithRestParam = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({prop, ...otherProps}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredArray = `import React from "react";
import { View } from "utopia-api";
export var whatever = ([prop]) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithDestructuredArrayWithDefault = `import React from "react";
import { View } from "utopia-api";
export var whatever = ([prop = 5]) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`
const codeWithDestructuredArrayWithOmittedParam = `import React from "react";
import { View } from "utopia-api";
export var whatever = ([prop1, ,prop2]) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithNestedDestructuredPropsMess = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({arrayPart: [prop1, ,{ prop2: renamedProp2, ...otherObjectProps }, ...otherArrayProps]}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithNestedDestructuredPropsMessWithDefaults = `import React from "react";
import { View } from "utopia-api";
export var whatever = ({arrayPart: [prop1 = 5, ,{ prop2: renamedProp2 = {thing: true}, ...otherObjectProps }, ...otherArrayProps]}) => {
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithPropsDestructuredInTheFunction = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const { a } = props;
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithARenamedFunction = `import React from "react";
import { View } from "utopia-api";
function originalFn() {}
export var whatever = (props) => {
  const renamedFn = originalFn;
  return (
    <View data-uid={'aaa'} />
  )
}
`

const codeWithComponentWithTopLevelIf = `import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  if (props.showA) {
    return <View data-uid={'aaa'} />
  } else {
    return <View data-uid={'bbb'} />
  }
}`

describe('Parsing a function component with props', () => {
  it('Correctly parses a basic props object', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithBasicPropsObject),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
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

  it('Correctly parses a basic props object with default', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithBasicPropsObjectWithDefault),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const propsParam = functionParam(
      false,
      regularParam(
        'props',
        jsExpressionOtherJavaScript(
          [],
          '{thing: true}',
          '({ thing: true });',
          `return ({
  thing: true
});`,
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
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

  it('Correctly parses a basic props object with a different name', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithRenamedBasicPropsObject),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const propsParam = functionParam(false, regularParam('myProps', null))
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
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

  it('Correctly parses a destructured props object', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredPropsObject),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(false, regularParam('prop', null))
    const propsParam = functionParam(
      false,
      destructuredObject([destructuredParamPart(undefined, destructuredParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
      ['prop'],
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

  it('Correctly parses a destructured props object with a default', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredPropsObjectWithDefault),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(
      false,
      regularParam(
        'prop',
        jsExpressionOtherJavaScript(
          [],
          '5',
          '5;',
          'return 5;',
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const propsParam = functionParam(
      false,
      destructuredObject([destructuredParamPart(undefined, destructuredParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
      ['prop'],
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

  it('Correctly parses a destructured props object that renames the param', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredPropsObjectWithRenamedParam),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(false, regularParam('renamedProp', null))
    const propsParam = functionParam(
      false,
      destructuredObject([destructuredParamPart('prop', destructuredParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
      ['prop'],
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

  it('Correctly parses a destructured props object that renames the param with a default', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredPropsObjectWithRenamedParamAndDefault),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(
      false,
      regularParam(
        'renamedProp',
        jsExpressionOtherJavaScript(
          [],
          '5',
          '5;',
          'return 5;',
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const propsParam = functionParam(
      false,
      destructuredObject([destructuredParamPart('prop', destructuredParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
      ['prop'],
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

  it('Correctly parses a destructured props object that uses a rest param', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredPropsObjectWithRestParam),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam1 = functionParam(false, regularParam('prop', null))
    const destructuredRestParam = functionParam(true, regularParam('otherProps', null))
    const destructuredParams = [
      destructuredParamPart(undefined, destructuredParam1, null),
      destructuredParamPart(undefined, destructuredRestParam, null),
    ]
    const propsParam = functionParam(false, destructuredObject(destructuredParams))
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
      ['prop'],
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

  it('Correctly parses a destructured props array', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredArray),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(false, regularParam('prop', null))
    const propsParam = functionParam(false, destructuredArray([destructuredParam]))
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
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

  it('Correctly parses a destructured props array with a default', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredArrayWithDefault),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam = functionParam(
      false,
      regularParam(
        'prop',
        jsExpressionOtherJavaScript(
          [],
          '5',
          '5;',
          'return 5;',
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const propsParam = functionParam(false, destructuredArray([destructuredParam]))
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
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

  it('Correctly parses a destructured props array with an omitted param', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithDestructuredArrayWithOmittedParam),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const destructuredParam1 = functionParam(false, regularParam('prop1', null))
    const destructuredParam2 = functionParam(false, regularParam('prop2', null))
    const propsParam = functionParam(
      false,
      destructuredArray([destructuredParam1, omittedParam(), destructuredParam2]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      propsParam,
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

  it('Correctly parses a nested destructured props mess', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithNestedDestructuredPropsMess),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const otherArrayProps = functionParam(true, regularParam('otherArrayProps', null))
    const renamedProp2 = functionParam(false, regularParam('renamedProp2', null))
    const otherObjectProps = functionParam(true, regularParam('otherObjectProps', null))
    const innerDestructuredObject = functionParam(
      false,
      destructuredObject([
        destructuredParamPart('prop2', renamedProp2, null),
        destructuredParamPart(undefined, otherObjectProps, null),
      ]),
    )
    const prop1 = functionParam(false, regularParam('prop1', null))
    const destructuredArrayParam = functionParam(
      false,
      destructuredArray([prop1, omittedParam(), innerDestructuredObject, otherArrayProps]),
    )
    const outerDestructuredObject = functionParam(
      false,
      destructuredObject([destructuredParamPart('arrayPart', destructuredArrayParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      outerDestructuredObject,
      ['arrayPart'],
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

  it('Correctly parses a nested destructured props mess with defaults', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithNestedDestructuredPropsMessWithDefaults),
    )
    const view = jsxElement(
      'View',
      'aaa',
      jsxAttributesFromMap({
        'data-uid': jsExpressionValue('aaa', emptyComments),
      }),
      [],
    )
    const otherArrayProps = functionParam(true, regularParam('otherArrayProps', null))
    const renamedProp2 = functionParam(
      false,
      regularParam(
        'renamedProp2',
        jsExpressionOtherJavaScript(
          [],
          '{thing: true}',
          '({ thing: true });',
          `return ({
  thing: true
});`,
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const otherObjectProps = functionParam(true, regularParam('otherObjectProps', null))
    const innerDestructuredObject = functionParam(
      false,
      destructuredObject([
        destructuredParamPart('prop2', renamedProp2, null),
        destructuredParamPart(undefined, otherObjectProps, null),
      ]),
    )
    const prop1 = functionParam(
      false,
      regularParam(
        'prop1',
        jsExpressionOtherJavaScript(
          [],
          '5',
          '5;',
          'return 5;',
          [],
          expect.objectContaining({}),
          {},
          emptyComments,
        ),
      ),
    )
    const destructuredArrayParam = functionParam(
      false,
      destructuredArray([prop1, omittedParam(), innerDestructuredObject, otherArrayProps]),
    )
    const outerDestructuredObject = functionParam(
      false,
      destructuredObject([destructuredParamPart('arrayPart', destructuredArrayParam, null)]),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      outerDestructuredObject,
      ['arrayPart'],
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

  it('Correctly parses a basic props object destructured inside the function', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithPropsDestructuredInTheFunction),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [],
      ),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      defaultPropsParam,
      [],
      view,
      expect.objectContaining({
        javascript: `const { a } = props;`,
        definedElsewhere: expect.arrayContaining(['props']),
        definedWithin: ['a'],
      }),
      false,
      emptyComments,
    )
    const topLevelElements = [exported]
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

  it('Correctly parses a component with a renamed function', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithARenamedFunction),
    )
    const view = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [],
      ),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      defaultPropsParam,
      [],
      view,
      expect.objectContaining({
        javascript: `const renamedFn = originalFn;`,
        definedElsewhere: expect.arrayContaining(['originalFn']),
        definedWithin: ['renamedFn'],
      }),
      false,
      emptyComments,
    )
    const topLevelElements = [exported]
    const expectedResult = parseSuccess(
      JustImportViewAndReact,
      expect.arrayContaining(topLevelElements),
      expect.objectContaining({}),
      null,
      expect.objectContaining({
        javascript: `function originalFn() {}`,
        definedWithin: ['originalFn'],
      }),
      [exportFunction('whatever')],
      expect.objectContaining({}),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('Correctly parses a component with a top level if statement', () => {
    const actualResult = clearParseResultUniqueIDsAndEmptyBlocks(
      testParseCode(codeWithComponentWithTopLevelIf),
    )
    const viewA = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
        }),
        [],
      ),
    )
    const viewB = clearJSXElementChildUniqueIDs(
      jsxElement(
        'View',
        'bbb',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('bbb', emptyComments),
        }),
        [],
      ),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      'var',
      'block',
      [],
      defaultPropsParam,
      ['showA'],
      expect.objectContaining({
        javascriptWithUIDs: `(() => {if (props.showA) {\n    return <View data-uid={'aaa'} />;\n  } else {\n    return <View data-uid={'bbb'} />;\n  }})();`,
        definedElsewhere: expect.arrayContaining(['props']),
        elementsWithin: {
          aaa: viewA,
          bbb: viewB,
        },
      }),
      expect.objectContaining({}),
      false,
      emptyComments,
    )

    const topLevelElements = [exported]
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
})

describe('Parsing, printing, reparsing a function component with props', () => {
  // The below tests are all based on the assumption that we know the parsed models are correct
  // due to the above tests

  function testParsePrintParse(code: string) {
    const firstParse = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(code))

    if (!isParseSuccess(firstParse)) {
      throw new Error(JSON.stringify(firstParse))
    }

    const firstAsParseSuccess = firstParse

    const printed = printCode(
      '/index.js',
      printCodeOptions(false, true, true),
      firstAsParseSuccess.imports,
      firstAsParseSuccess.topLevelElements,
      firstAsParseSuccess.jsxFactoryFunction,
      firstAsParseSuccess.exportsDetail,
    )

    const secondParse = clearParseResultUniqueIDsAndEmptyBlocks(testParseCode(printed))

    if (!isParseSuccess(secondParse)) {
      throw new Error(JSON.stringify(secondParse))
    }

    const secondAsParseSuccess = firstParse
    expect(secondAsParseSuccess.topLevelElements).toEqual(firstAsParseSuccess.topLevelElements)
  }

  it('Correctly parses back and forth a basic props object', () => {
    testParsePrintParse(codeWithBasicPropsObject)
  })

  it('Correctly parses back and forth a basic props object with a default', () => {
    testParsePrintParse(codeWithBasicPropsObjectWithDefault)
  })

  it('Correctly parses back and forth a basic props object with a different name', () => {
    testParsePrintParse(codeWithRenamedBasicPropsObject)
  })

  it('Correctly parses back and forth a destructured props object', () => {
    testParsePrintParse(codeWithDestructuredPropsObject)
  })

  it('Correctly parses back and forth a destructured props object with a default', () => {
    testParsePrintParse(codeWithDestructuredPropsObjectWithDefault)
  })

  it('Correctly parses back and forth a destructured props object that renames the param', () => {
    testParsePrintParse(codeWithDestructuredPropsObjectWithRenamedParam)
  })

  it('Correctly parses back and forth a destructured props object that renames the param with a default', () => {
    testParsePrintParse(codeWithDestructuredPropsObjectWithRenamedParamAndDefault)
  })

  it('Correctly parses back and forth a destructured props object that uses a rest param', () => {
    testParsePrintParse(codeWithDestructuredPropsObjectWithRestParam)
  })

  it('Correctly parses back and forth a destructured props array', () => {
    testParsePrintParse(codeWithDestructuredArray)
  })

  it('Correctly parses back and forth a destructured props array with a default', () => {
    testParsePrintParse(codeWithDestructuredArrayWithDefault)
  })

  it('Correctly parses back and forth a destructured props array with an omitted param', () => {
    testParsePrintParse(codeWithDestructuredArrayWithOmittedParam)
  })

  it('Correctly parses back and forth a nested destructured props mess', () => {
    testParsePrintParse(codeWithNestedDestructuredPropsMess)
  })

  it('Correctly parses back and forth a nested destructured props mess with defaults', () => {
    testParsePrintParse(codeWithNestedDestructuredPropsMessWithDefaults)
  })

  it('Correctly parses back and forth a basic props object destructured inside the function', () => {
    testParsePrintParse(codeWithPropsDestructuredInTheFunction)
  })

  it('Correctly parses back and forth a component with a renamed function', () => {
    testParsePrintParse(codeWithARenamedFunction)
  })

  it('Correctly parses back and forth a component with a top level if statement', () => {
    testParsePrintParse(codeWithComponentWithTopLevelIf)
  })
})
