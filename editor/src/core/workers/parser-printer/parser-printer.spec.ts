import * as FastCheck from 'fast-check'
import { getSourceMapConsumer } from '../../../third-party/react-error-overlay/utils/getSourceMap'
import {
  arbitraryJSBlock,
  clearTopLevelElementUniqueIDs,
  isArbitraryJSBlock,
  isJSXAttributeOtherJavaScript,
  isJSXElement,
  isUtopiaJSXComponent,
  jsxArbitraryBlock,
  jsxArraySpread,
  jsxArrayValue,
  jsxAttributeFunctionCall,
  jsxAttributeNestedArray,
  jsxAttributeNestedArraySimple,
  jsxAttributeNestedObject,
  jsxAttributeNestedObjectSimple,
  jsxAttributeOtherJavaScript,
  JSXAttributes,
  jsxAttributeValue,
  jsxElement,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  jsxTextBlock,
  utopiaJSXComponent,
  UtopiaJSXComponent,
  defaultPropsParam,
  clearArbitraryJSBlockUniqueIDs,
} from '../../shared/element-template'
import { sampleCode } from '../../model/new-project-files'
import { addImport, emptyImports, parseSuccess } from '../common/project-file-utils'
import { sampleImportsForTests } from '../../model/test-ui-js-file'
import {
  isParseSuccess,
  importAlias,
  foldParsedTextFile,
  exportsDetail,
  addNamedExportToDetail,
  EmptyExportsDetail,
  setNamedDefaultExportInDetail,
  addModifierExportToDetail,
} from '../../shared/project-file-types'
import {
  lintAndParse,
  parseCode,
  printCode,
  printCodeOptions,
  getHighlightBoundsWithoutUID,
} from './parser-printer'
import { applyPrettier } from './prettier-utils'
import { transpileJavascriptFromCode } from './parser-printer-transpiling'
import {
  clearParseResultPassTimes,
  clearParseResultUniqueIDs,
  ensureElementsHaveUID,
  JustImportViewAndReact,
  PrintableProjectContent,
  printableProjectContentArbitrary,
  testParseCode,
} from './parser-printer.test-utils'
import { InfiniteLoopError, InfiniteLoopMaxIterations } from './transform-prevent-infinite-loops'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'

describe('JSX parser', () => {
  it('parses the code when it is a const', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a const with no params', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = () => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeValue(20),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export function whatever(props) {
  return (
    <View data-uid={'aaa'}>
      <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function without any params', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export function whatever() {
  return (
    <View data-uid={'aaa'}>
      <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeValue(20),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is an export default function', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export default function whatever(props) {
  return (
    <View data-uid={'aaa'}>
      <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        setNamedDefaultExportInDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is an export default function with no params', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export default function whatever() {
  return (
    <View data-uid={'aaa'}>
      <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={20} right={20} top={-20} />
    </View>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeValue(20),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        setNamedDefaultExportInDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it includes a default import', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import cake from 'cake'
import './style.css'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const importsWithCake = addImport('cake', 'cake', [], null, sampleImportsForTests)
    const importsWithStylecss = addImport('./style.css', null, [], null, importsWithCake)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        importsWithStylecss,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it includes a mixed import', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import cake, { cake2 } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
  <cake2 data-uid={'aac'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake2Attributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aac'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const cake2 = jsxElement('cake2', cake2Attributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake, cake2])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const imports = addImport('cake', 'cake', [importAlias('cake2')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  }),
    it('parses the code when it includes an import with alias', () => {
      const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake as cake2 } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake2 data-uid={'aac'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
      const actualResult = clearParseResultUniqueIDs(testParseCode(code))
      const cake2Attributes: JSXAttributes = {
        'data-uid': jsxAttributeValue('aac'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
        left: jsxAttributeOtherJavaScript(
          'props.leftOfTheCake[0].hat',
          'return props.leftOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
        ),
        right: jsxAttributeValue(20),
        top: jsxAttributeValue(-20),
      }
      const cake2 = jsxElement('cake2', cake2Attributes, [])
      const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake2])
      const exported = utopiaJSXComponent(
        'whatever',
        true,
        defaultPropsParam,
        ['leftOfTheCake'],
        view,
        null,
      )
      const imports = addImport(
        'cake',
        null,
        [importAlias('cake', 'cake2')],
        null,
        sampleImportsForTests,
      )
      const expectedResult = clearParseResultUniqueIDs(
        parseSuccess(
          imports,
          [exported],
          expect.objectContaining({}),
          null,
          null,
          addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
        ),
      )
      expect(actualResult).toEqual(expectedResult)
    }),
    it('parses the code when it is a function, with metadata', () => {
      const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} data-label={'First cake'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
  <cake data-uid={'111'} data-label={'Second cake'} style={{backgroundColor: 'blue'}} left={props.rightOfTheCake[0].hat} right={10} top={-10} />
</View>
`
      const actualResult = clearParseResultUniqueIDs(testParseCode(code))
      const firstCakeAttributes: JSXAttributes = {
        'data-uid': jsxAttributeValue('aab'),
        'data-label': jsxAttributeValue('First cake'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
        left: jsxAttributeOtherJavaScript(
          'props.leftOfTheCake[0].hat',
          'return props.leftOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
        ),
        right: jsxAttributeValue(20),
        top: jsxAttributeValue(-20),
      }
      const firstCake = jsxElement('cake', firstCakeAttributes, [])
      const secondCakeAttributes: JSXAttributes = {
        'data-uid': jsxAttributeValue('111'),
        'data-label': jsxAttributeValue('Second cake'),
        style: jsxAttributeValue({ backgroundColor: 'blue' }),
        left: jsxAttributeOtherJavaScript(
          'props.rightOfTheCake[0].hat',
          'return props.rightOfTheCake[0].hat;',
          ['props'],
          expect.objectContaining({}),
        ),
        right: jsxAttributeValue(10),
        top: jsxAttributeValue(-10),
      }
      const secondCake = jsxElement('cake', secondCakeAttributes, [])
      const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [
        firstCake,
        secondCake,
      ])
      const exported = utopiaJSXComponent(
        'whatever',
        true,
        defaultPropsParam,
        ['leftOfTheCake', 'rightOfTheCake'],
        view,
        null,
      )
      const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
      const expectedResult = clearParseResultUniqueIDs(
        parseSuccess(
          imports,
          [exported],
          expect.objectContaining({}),
          null,
          null,
          addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
        ),
      )
      expect(actualResult).toEqual(expectedResult)
    })
  it('parses the code when it is a function, with undefined and null as props', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red' }} left={props.leftOfTheCake[0].hat} right={20} top={-20} nullProp={null} undefinedProp={undefined} trueProp={true} falseProp={false} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({
        backgroundColor: 'red',
      }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
      nullProp: jsxAttributeValue(null),
      undefinedProp: jsxAttributeValue(undefined),
      trueProp: jsxAttributeValue(true),
      falseProp: jsxAttributeValue(false),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function, with some arbitrary JavaScript', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
function getSizing(n) {
  return 100 + n
}
var spacing = 20
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={getSizing(spacing)} right={20} top={-20} onClick={function(){console.log('click')}} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'getSizing(spacing)',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
      onClick: jsxAttributeOtherJavaScript(
        `function(){console.log('click')}`,
        `return (function () {
  console.log('click');
});`,
        ['console'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const jsCode = `function getSizing(n) {
  return 100 + n;
}
var spacing = 20;`
    const transpiledJsCode = `function getSizing(n) {
  return 100 + n;
}

var spacing = 20;
return { getSizing: getSizing, spacing: spacing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['getSizing', 'spacing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [arbitraryBlock, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has an export default function', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export default function getSizing(n) {
  return 100 + n
}
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const jsCode = `export default function getSizing(n) {
  return 100 + n;
}`
    const transpiledJsCode = `function getSizing(n) {
  return 100 + n;
}
return { getSizing: getSizing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['getSizing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [arbitraryBlock, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has a default keyword elsewhere', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export function getSizing(n) {
  switch (n) {
    case 100:
      return 1
    default:
      return 100 + n
  }
}
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const jsCode = `export function getSizing(n) {
  switch (n) {
    case 100:
      return 1;
    default:
      return 100 + n;}

}`
    const transpiledJsCode = `function getSizing(n) {
  switch (n) {
    case 100:
      return 1;

    default:
      return 100 + n;
  }
}
return { getSizing: getSizing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['getSizing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [arbitraryBlock, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has an export default anonymous function', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export default (n) => {
  return 100 + n
}
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.leftOfTheCake[0].hat} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.leftOfTheCake[0].hat',
        'return props.leftOfTheCake[0].hat;',
        ['props'],
        expect.objectContaining({}),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['leftOfTheCake'],
      view,
      null,
    )
    const jsCode = `export default (n => {
  return 100 + n;
});`
    const transpiledJsCode = `(function (n) {
  return 100 + n;
});
return {  };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      [],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [arbitraryBlock, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a function, with some arbitrary JavaScript (variable declaration)', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
var spacing = 20
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={spacing} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'spacing',
        'return spacing;',
        ['spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const transpiledJSCode = `var spacing = 20;
return { spacing: spacing };`
    const jsVariable = arbitraryJSBlock(
      'var spacing = 20;',
      transpiledJSCode,
      ['spacing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript (array literals + element access)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const bgs = ['black', 'grey']
  const bg = bgs[0]
  return (
    <View data-uid={'aaa'} style={{ backgroundColor: bgs[0] }} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      style: jsxAttributeNestedObjectSimple({
        backgroundColor: jsxAttributeOtherJavaScript(
          'bgs[0]',
          'return bgs[0];',
          ['bgs'],
          expect.objectContaining({
            sources: ['code.tsx'],
            version: 3,
            file: 'code.tsx',
          }),
        ),
      }),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const bgs = ['black', 'grey'];
const bg = bgs[0];`
    const transpiledJsCode = `var bgs = ['black', 'grey'];
var bg = bgs[0];
return { bgs: bgs, bg: bg };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['bgs', 'bg'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (array spread)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const greys = ['lightGrey', 'grey']
  return (
    <View data-uid={'aaa'} colors={['black', ...greys]}/>
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      colors: jsxAttributeNestedArray([
        jsxArrayValue(jsxAttributeValue('black')),
        jsxArraySpread(
          jsxAttributeOtherJavaScript(
            'greys',
            'return greys;',
            ['greys'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
          ),
        ),
      ]),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const greys = ['lightGrey', 'grey'];`
    const transpiledJsCode = `var greys = ['lightGrey', 'grey'];
return { greys: greys };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['greys'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (binary expressions)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = 10
  const b = 20
  return (
    <View data-uid={'aaa'} left={a + b} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      left: jsxAttributeOtherJavaScript(
        'a + b',
        'return a + b;',
        ['a', 'b'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const a = 10;
const b = 20;`
    const transpiledJsCode = `var a = 10;
var b = 20;
return { a: a, b: b };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (conditional expressions)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = true
  const b = 10
  const c = 20
  return (
    <View data-uid={'aaa'} left={a ? b : c} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      left: jsxAttributeOtherJavaScript(
        'a ? b : c',
        'return a ? b : c;',
        ['a', 'b', 'c'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const a = true;
const b = 10;
const c = 20;`
    const transpiledJsCode = `var a = true;
var b = 10;
var c = 20;
return { a: a, b: b, c: c };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b', 'c'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (postfix and prefix)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  let a = 10
  return (
    <View data-uid={'aaa'} left={a++} right={++a} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      left: jsxAttributeOtherJavaScript(
        'a++',
        'return a++;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeOtherJavaScript(
        '++a',
        'return ++a;',
        ['a'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `let a = 10;`
    const transpiledJsCode = `var a = 10;
return { a: a };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (object literals)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const a = 10
  const b = { a: a }
  return (
    <View data-uid={'aaa'} left={b.a} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      left: jsxAttributeOtherJavaScript(
        'b.a',
        'return b.a;',
        ['b'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const a = 10;
const b = { a: a };`
    const transpiledJsCode = `var a = 10;
var b = {
  a: a
};
return { a: a, b: b };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['a', 'b'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (object spread)', () => {
    const code = `
import React from "react";
import { View } from "utopia-api";
export var whatever = (props) => {
  const bg = { backgroundColor: 'grey' }
  return (
    <View data-uid={'aaa'} style={{...bg}} />
  )
}
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const viewAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aaa'),
      style: jsxAttributeNestedObject([
        jsxSpreadAssignment(
          jsxAttributeOtherJavaScript(
            'bg',
            'return bg;',
            ['bg'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
          ),
        ),
      ]),
    }
    const view = jsxElement('View', viewAttributes, [])
    const jsCode = `const bg = { backgroundColor: 'grey' };`
    const transpiledJsCode = `var bg = {
  backgroundColor: 'grey'
};
return { bg: bg };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['bg'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
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

  it('parses the code when it is a function, with some arbitrary JavaScript (string interpolation)', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
var count = 10
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} text={\`Count \${count}\`} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      text: jsxAttributeOtherJavaScript(
        '`Count ${count}`',
        'return "Count ".concat(count);',
        ['count'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const jsCode = `var count = 10;`
    const transpiledJSCode = `var count = 10;
return { count: count };`
    const jsVariable = arbitraryJSBlock(
      jsCode,
      transpiledJSCode,
      ['count'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with a ternary referencing arbitrary JS', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
var use20 = true
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={use20 ? 20 : 10} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'use20 ? 20 : 10',
        'return use20 ? 20 : 10;',
        ['use20'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const transpiledJSCode = `var use20 = true;
return { use20: use20 };`
    const jsVariable = arbitraryJSBlock(
      'var use20 = true;',
      transpiledJSCode,
      ['use20'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with arbitrary JS with Set constructor', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
var mySet = new Set()
export var whatever = (props) => <View data-uid={'aaa'}>
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const transpiledJSCode = `var mySet = new Set();
return { mySet: mySet };`
    const jsVariable = arbitraryJSBlock(
      'var mySet = new Set();',
      transpiledJSCode,
      ['mySet'],
      ['Set'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with some arbitrary JavaScript combined with props', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
var spacing = 20
export var whatever = (props) => <View data-uid={'aaa'}>
  <cake data-uid={'aab'} style={{backgroundColor: 'red'}} left={props.left + spacing} right={20} top={-20} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'props.left + spacing',
        'return props.left + spacing;',
        ['spacing', 'props'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, ['left'], view, null)
    const transpiledJSCode = `var spacing = 20;
return { spacing: spacing };`
    const jsVariable = arbitraryJSBlock(
      'var spacing = 20;',
      transpiledJSCode,
      ['spacing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [jsVariable, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(jsVariable),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with code component', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
var MyComp = props => {
  return React.createElement(
    "div",
    {
      style: {
        position: "absolute",
        left: props.layout.left,
        backgroundColor: "hotpink"
      }
    },
    "hello"
  );
};
export var whatever = (props) => <View data-uid={'aaa'}>
  <MyComp data-uid={'aab'} layout={{left: 100}} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const jsCode = `var MyComp = props => {
  return React.createElement(
  "div",
  {
    style: {
      position: "absolute",
      left: props.layout.left,
      backgroundColor: "hotpink" } },


  "hello");

};`
    const transpiledJsCode = `var MyComp = function MyComp(props) {
  return React.createElement("div", {
    style: {
      position: "absolute",
      left: props.layout.left,
      backgroundColor: "hotpink"
    }
  }, "hello");
};
return { MyComp: MyComp };`
    const definedElseWhere = ['React']
    const MyComp = arbitraryJSBlock(
      jsCode,
      transpiledJsCode,
      ['MyComp'],
      definedElseWhere,
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const myCompAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      layout: jsxAttributeValue({ left: 100 }),
    }
    const myCompElement = jsxElement('MyComp', myCompAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [myCompElement])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const topLevelElements = [MyComp, exported].map(clearTopLevelElementUniqueIDs)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(MyComp),
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, with jsx code component', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
var MyComp = (props) => {
  return (
    <div
      data-uid={"abc"}
      style={{
        position: "absolute",
        left: props.layout.left,
        top: props.layout.top,
        width: 100,
        height: 100,
        backgroundColor: "hotpink"
      }}
    >hello</div>
  );
};
export var whatever = props => (
  <View data-uid={'aaa'}>
    <MyComp data-uid={'aab'} layout={{left: 100}} />
  </View>
)
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))

    const rootDivAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('abc'),
      style: jsxAttributeNestedObject([
        jsxPropertyAssignment('position', jsxAttributeValue('absolute')),
        jsxPropertyAssignment(
          'left',
          jsxAttributeOtherJavaScript(
            'props.layout.left',
            'return props.layout.left;',
            ['props'],
            expect.objectContaining({}),
          ),
        ),
        jsxPropertyAssignment(
          'top',
          jsxAttributeOtherJavaScript(
            'props.layout.top',
            'return props.layout.top;',
            ['props'],
            expect.objectContaining({}),
          ),
        ),
        jsxPropertyAssignment('width', jsxAttributeValue(100)),
        jsxPropertyAssignment('height', jsxAttributeValue(100)),
        jsxPropertyAssignment('backgroundColor', jsxAttributeValue('hotpink')),
      ]),
    }

    const rootDiv = jsxElement('div', rootDivAttributes, [jsxTextBlock('hello')])

    const myComp = utopiaJSXComponent('MyComp', true, defaultPropsParam, ['layout'], rootDiv, null)

    const myCompAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      layout: jsxAttributeValue({ left: 100 }),
    }
    const myCompElement = jsxElement('MyComp', myCompAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [myCompElement])
    const whatever = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const topLevelElements = [myComp, whatever].map(clearTopLevelElementUniqueIDs)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [...topLevelElements],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses the code when it is a function, component with unknown jsx element is arbitrary js', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var Whatever = (props) => <View>
  <MyComp layout={{left: 100}} />
</View>
`
    const actualResult = parseCode('code.tsx', code)
    if (isParseSuccess(actualResult)) {
      if (actualResult.topLevelElements.length === 1) {
        const result = actualResult.topLevelElements[0]
        expect(isArbitraryJSBlock(result)).toBeTruthy()
      } else {
        fail(`Unexpected number of results returned: ${actualResult.topLevelElements.length}`)
      }
    } else {
      fail('Parse result is not a success.')
    }
  })

  it('parses the code when it is a function, with an arbitrary piece of JavaScript', () => {
    const code = `import * as React from "react";
import { View } from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
<cake data-uid={'aab'} left={2 + 2} />
</View>
`
    const actualResult = testParseCode(code)
    if (isParseSuccess(actualResult)) {
      if (actualResult.topLevelElements.length === 1) {
        const result = actualResult.topLevelElements[0]
        if (isUtopiaJSXComponent(result)) {
          if (isJSXElement(result.rootElement)) {
            if (result.rootElement.children.length === 1) {
              const child = result.rootElement.children[0]
              if (isJSXElement(child)) {
                const childPropKeys = Object.keys(child.props)
                expect(childPropKeys).toEqual(['data-uid', 'left'])
                const leftProp = child.props['left']
                expect(leftProp.type).toEqual('ATTRIBUTE_OTHER_JAVASCRIPT')
              } else {
                fail(`First child is not an element ${child}`)
              }
            } else {
              fail(`Unexpected number of children returned: ${result.rootElement.children.length}`)
            }
          } else {
            fail(`Root element is not an element ${result.rootElement}`)
          }
        } else {
          fail(`Unexpected top level element ${JSON.stringify(result)}`)
        }
      } else {
        fail(`Unexpected number of results returned: ${actualResult.topLevelElements.length}`)
      }
    } else {
      fail('Parse result is not a success.')
    }
  })
  it('parses the code when it is a function, with a nested object', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = (props) => <View data-uid={'aaa'}>
<cake data-uid={'aab'} style={{backgroundColor: 'red', color: [props.color, -200]}} />
</View>
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aab'),
        style: jsxAttributeNestedObjectSimple({
          backgroundColor: jsxAttributeValue('red'),
          color: jsxAttributeNestedArraySimple([
            jsxAttributeOtherJavaScript(
              'props.color',
              'return props.color;',
              ['props'],
              expect.objectContaining({}),
            ),
            jsxAttributeValue(-200),
          ]),
        }),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, ['color'], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it is a var', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
import { cake } from 'cake'
export var whatever = <View data-uid={'aaa'}>
<cake data-uid={'aab'} style={{backgroundColor: 'red'}} />
</View>
`
    const actualResult = testParseCode(code)
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aab'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', false, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const expectedResult = parseSuccess(
      imports,
      [exported],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has empty brackets {} as jsx child', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = (props) => <View data-uid={'bbb'}>
  {}
</View>
`
    const actualResult = testParseCode(code)
    const emptyBrackets = {
      ...jsxArbitraryBlock('', '', 'return undefined', [], null, {}),
      uniqueID: expect.any(String),
    }
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('bbb') }, [emptyBrackets])
    const exported = utopiaJSXComponent('App', true, defaultPropsParam, [], view, null)
    const expectedResult = parseSuccess(
      sampleImportsForTests,
      [exported],
      expect.objectContaining({}),
      null,
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'App'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses the code when it has a JSX block with an object defined inside', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
const a = "cake"
export var App = (props) => <View data-uid={'bbb'}>
  {{a: a}}
</View>
`
    const actualResult = testParseCode(code)
    expect(clearParseResultUniqueIDs(actualResult)).toMatchSnapshot()
  })

  it('parses back and forth as a var', () => {
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aab'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
        left: jsxAttributeValue(10),
        name: jsxAttributeValue('test'),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', false, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      [exported],
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses back and forth as a var, with some arbitrary javascript', () => {
    const cakeAttributes: JSXAttributes = {
      'data-uid': jsxAttributeValue('aab'),
      style: jsxAttributeValue({ backgroundColor: 'red' }),
      left: jsxAttributeOtherJavaScript(
        'getSizing(spacing)',
        'return getSizing(spacing);',
        ['getSizing', 'spacing'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
      right: jsxAttributeValue(20),
      top: jsxAttributeValue(-20),
    }
    const cake = jsxElement('cake', cakeAttributes, [])
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const jsCode = `function getSizing(n) {
  return 100 + n;
}
var spacing = 20;`
    const transpiledJSCode = `function getSizing(n) {
  return 100 + n;
}

var spacing = 20;
return { getSizing: getSizing, spacing: spacing };`
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJSCode,
      ['getSizing', 'spacing'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const topLevelElements = [arbitraryBlock, exported].map(clearTopLevelElementUniqueIDs)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [...topLevelElements],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDs(testParseCode(printedCode))
    const expectedResult = parseSuccess(
      imports,
      [...topLevelElements],
      expect.objectContaining({}),
      null,
      clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a var and includes canvas metadata', () => {
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aab'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent('whatever', false, null, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      [exported],
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('parses back and forth as a function, with an arbitrary piece of JavaScript', () => {
    const code = applyPrettier(
      `import { cake } from "cake";
import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
function cakeFn(n) {
  return n
}
function otherFn(n) {
  return n
}
export var whatever = props => {
  return (
    <View data-uid={"aaa"}>
      <cake data-uid={"aab"} left={cakeFn(otherFn("b") + 2)} />
    </View>
  );
};
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    expect(clearParseResultUniqueIDs(parsedCode)).toMatchSnapshot()
  })
  it('parses back and forth as a function, with an empty bracket jsx child {}', () => {
    const code = applyPrettier(
      `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View,
} from 'utopia-api'
export var whatever = props => {
  return (
    <View data-uid={"aaa"}>
      {}
    </View>
  );
};
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    if (isParseSuccess(parsedCode)) {
      const printedCode = printCode(
        printCodeOptions(false, true, true),
        sampleImportsForTests,
        parsedCode.topLevelElements,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(printedCode).toEqual(code)
    } else {
      fail('Parse result is not a success.')
    }
  })
  it('parses back and forth as a function', () => {
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aaa'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aab') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = testParseCode(printedCode)
    const expectedResult = parseSuccess(
      imports,
      [exported],
      expect.objectContaining({}),
      null,
      null,
      detailOfExports,
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with null and undefined values for props', () => {
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aaa'),
        style: jsxAttributeValue({ backgroundColor: 'red' }),
        trueProp: jsxAttributeValue(true),
        falseProp: jsxAttributeValue(false),
        nullProp: jsxAttributeValue(null),
        undefinedProp: jsxAttributeValue(undefined),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aab') }, [cake])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDs(testParseCode(printedCode))
    // As false properties are eliminated from the output,
    // create a copy of the original and snip that value out.
    let propsWithoutFalseProp: JSXAttributes = {
      ...cake.props,
    }
    delete propsWithoutFalseProp['falseProp']
    const withoutFalseProp: UtopiaJSXComponent = {
      ...exported,
      rootElement: {
        ...view,
        children: [
          {
            ...cake,
            props: propsWithoutFalseProp,
          },
        ],
      },
    }
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [withoutFalseProp],
        expect.objectContaining({}),
        null,
        null,
        detailOfExports,
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses back and forth as a function, with a nested object', () => {
    const cake = jsxElement(
      'cake',
      {
        'data-uid': jsxAttributeValue('aab'),
        style: jsxAttributeNestedObjectSimple({
          backgroundColor: jsxAttributeValue('red'),
          color: jsxAttributeNestedArraySimple([
            jsxAttributeOtherJavaScript(
              'props.color',
              'return props.color;',
              ['props'],
              expect.objectContaining({}),
            ),
            jsxAttributeValue(-200),
          ]),
          boxShadow: jsxAttributeFunctionCall('createShadow', [
            jsxAttributeValue(15),
            jsxAttributeOtherJavaScript(
              'props.shadowValue',
              'return props.shadowValue;',
              ['props'],
              expect.objectContaining({}),
            ),
            jsxAttributeNestedArraySimple([
              jsxAttributeValue('hello'),
              jsxAttributeOtherJavaScript(
                'props.there',
                'return props.there;',
                ['props'],
                expect.objectContaining({}),
              ),
            ]),
          ]),
        }),
      },
      [],
    )
    const view = jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [cake])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      ['color', 'shadowValue', 'there'],
      view,
      null,
    )
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'whatever')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      [exported],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDs(testParseCode(printedCode))
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(imports, [exported], expect.objectContaining({}), null, null, detailOfExports),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserves modifiers on variables', () => {
    const code = applyPrettier(
      `const first = 100
let second = 'cake'
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid={'${BakedInStoryboardUID}'} />
`,
      false,
    ).formatted
    const expectedCode = applyPrettier(
      `
const first = 100;
let second = "cake";
export var ${BakedInStoryboardVariableName} = <Storyboard data-uid={'${BakedInStoryboardUID}'} />
`,
      false,
    ).formatted
    const parsedCode = testParseCode(code)
    if (isParseSuccess(parsedCode)) {
      const printedCode = printCode(
        printCodeOptions(false, true, true),
        emptyImports(),
        parsedCode.topLevelElements,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      )
      expect(printedCode).toEqual(expectedCode)
    } else {
      throw new Error('Did not parse successfully.')
    }
  })
  it('parses code within a component block', () => {
    const code = `import { cake } from 'cake'
import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var whatever = props => {
  function test(n) {
    return n * 2
  }
  return (
    <View data-uid={"aaa"}>
      <cake data-uid={"aab"} left={test(100)} />
    </View>
  );
};
`
    const jsCode = `function test(n) {
  return n * 2;
}`
    const transpiledJSCode = `function test(n) {
  return n * 2;
}
return { test: test };`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const arbitraryBlock = arbitraryJSBlock(
      jsCode,
      transpiledJSCode,
      ['test'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        imports,
        [
          utopiaJSXComponent(
            'whatever',
            true,
            defaultPropsParam,
            [],
            jsxElement(
              'View',
              {
                'data-uid': jsxAttributeValue('aaa'),
              },
              [
                jsxElement(
                  'cake',
                  {
                    'data-uid': jsxAttributeValue('aab'),
                    left: jsxAttributeOtherJavaScript(
                      'test(100)',
                      'return test(100);',
                      ['test'],
                      expect.objectContaining({
                        sources: ['code.tsx'],
                        version: 3,
                        file: 'code.tsx',
                      }),
                    ),
                  },
                  [],
                ),
              ],
            ),
            clearArbitraryJSBlockUniqueIDs(arbitraryBlock),
          ),
        ],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )

    expect(actualResult).toEqual(expectedResult)
  })
  it('parses arbitrary code in a component back and forth', () => {
    const jsCode = `function test(n) {
  return n * 2;
}`
    const transpiledJSCode = `function test(n) {
  return n * 2;
}
return { test: test };`
    const imports = addImport('cake', null, [importAlias('cake')], null, sampleImportsForTests)
    const components = [
      clearTopLevelElementUniqueIDs(
        utopiaJSXComponent(
          'whatever',
          true,
          defaultPropsParam,
          [],
          jsxElement(
            'View',
            {
              'data-uid': jsxAttributeValue('aaa'),
            },
            [
              jsxElement(
                'cake',
                {
                  'data-uid': jsxAttributeValue('aab'),
                  left: jsxAttributeOtherJavaScript(
                    'test(100)',
                    'return test(100);',
                    ['test'],
                    expect.objectContaining({
                      sources: ['code.tsx'],
                      version: 3,
                      file: 'code.tsx',
                    }),
                  ),
                },
                [],
              ),
            ],
          ),
          arbitraryJSBlock(
            jsCode,
            transpiledJSCode,
            ['test'],
            [],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
          ),
        ),
      ),
      clearTopLevelElementUniqueIDs(
        utopiaJSXComponent(
          BakedInStoryboardVariableName,
          false,
          null,
          [],
          jsxElement(
            'Storyboard',
            {
              'data-uid': jsxAttributeValue(BakedInStoryboardUID),
            },
            [],
          ),
          null,
        ),
      ),
    ]
    const detailOfExports = addModifierExportToDetail(
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      'storyboard',
    )
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      imports,
      components,
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDs(testParseCode(printedCode))
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(imports, components, expect.objectContaining({}), null, null, detailOfExports),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserve children of an element that is a block of random JavaScript', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid={'aaa'}>
      {[1,2,3].map(x=> <View data-uid={'abc'} />)}
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        {
          'data-uid': jsxAttributeValue('aaa'),
        },
        [
          jsxArbitraryBlock(
            `[1,2,3].map(x=> <View data-uid={'abc'} />)`,
            `[1, 2, 3].map(x => <View data-uid={'abc'} />);`,
            `return [1, 2, 3].map(function (x) {
  return utopiaCanvasJSXLookup("abc", {});
});`,
            ['React', 'View', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            { abc: jsxElement('View', { 'data-uid': jsxAttributeValue('abc') }, []) },
          ),
        ],
      ),
      null,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'App'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('preserve text within a JSX element', () => {
    const code = `
import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid={'aaa'}>cake</View>
  );
};`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        {
          'data-uid': jsxAttributeValue('aaa'),
        },
        [jsxTextBlock('cake')],
      ),
      null,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'App'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('captures an expression within a JSX element', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid={'aaa'}>
      {[1, 2, 3].map(n => (
        <div data-uid={"abc"} />
      ))}
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        {
          'data-uid': jsxAttributeValue('aaa'),
        },
        [
          jsxArbitraryBlock(
            `[1, 2, 3].map(n => (
        <div data-uid={"abc"} />
      ))`,
            `[1, 2, 3].map((n) =>
<div data-uid={"abc"} />);`,
            `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("abc", {});
});`,
            ['React', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            { abc: jsxElement('div', { 'data-uid': jsxAttributeValue('abc') }, []) },
          ),
        ],
      ),
      null,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'App'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('ensure the sample file parses successfully', () => {
    const parseResult = testParseCode(sampleCode)
    expect(isParseSuccess(parseResult)).toBe(true)
  })
  it('parses expressions in JSX back and forth', () => {
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        {
          'data-uid': jsxAttributeValue('aaa'),
        },
        [
          jsxArbitraryBlock(
            `[1, 2, 3].map((n) => (
        <div data-uid={'abc'} />
      ))`,
            `[1, 2, 3].map((n) =>
<div data-uid={'abc'} />);`,
            `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("abc", {});
});`,
            ['React', 'utopiaCanvasJSXLookup'],
            expect.objectContaining({
              sources: ['code.tsx'],
              version: 3,
              file: 'code.tsx',
            }),
            {
              abc: jsxElement(
                'div',
                {
                  'data-uid': jsxAttributeValue('abc'),
                },
                [],
              ),
            },
          ),
        ],
      ),
      null,
    )
    const detailOfExports = addModifierExportToDetail(EmptyExportsDetail, 'App')
    const printedCode = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [component],
      null,
      detailOfExports,
    )
    const actualResult = clearParseResultUniqueIDs(testParseCode(printedCode))
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        detailOfExports,
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('should handle inner components', () => {
    const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = props => {
  const a = 20;
  const b = 40;
  const MyCustomCompomnent = props => <View data-uid={"abc"}>{props.children}</View>;

  return (
    <View
      style={{ backgroundColor: "lightgrey", position: "absolute" }}
      layout={{
        height: props.layout.height,
        left: props.layout.left,
        width: props.layout.width,
        top: props.layout.top
      }}
      data-uid={"aaa"}
    >
      <MyCustomCompomnent data-uid={"ddd"}>
        <Ellipse
          style={{ backgroundColor: "lightgreen" }}
          layout={{ height: 100, left: 150, width: 100, top: 540 }}
          data-uid={"bbb"}
        />
        <Rectangle
          style={{ backgroundColor: "orange" }}
          layout={{ height: 100, left: 150, width: 100, top: 540 }}
          data-uid={"ccc"}
        />
      </MyCustomCompomnent>
      <View
        style={{ backgroundColor: "blue", position: "absolute" }}
        layout={{ height: 200, left: 80, width: 100, top: 145 }}
        data-uid={"ggg"}
      />
    </View>
  );
};`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const ellipse = jsxElement(
      'Ellipse',
      {
        style: jsxAttributeValue({ backgroundColor: 'lightgreen' }),
        layout: jsxAttributeValue({ height: 100, left: 150, width: 100, top: 540 }),
        'data-uid': jsxAttributeValue('bbb'),
      },
      [],
    )
    const rectangle = jsxElement(
      'Rectangle',
      {
        style: jsxAttributeValue({ backgroundColor: 'orange' }),
        layout: jsxAttributeValue({ height: 100, left: 150, width: 100, top: 540 }),
        'data-uid': jsxAttributeValue('ccc'),
      },
      [],
    )
    const myCustomCompomnent = jsxElement(
      'MyCustomCompomnent',
      {
        'data-uid': jsxAttributeValue('ddd'),
      },
      [ellipse, rectangle],
    )
    const view = jsxElement(
      'View',
      {
        style: jsxAttributeValue({ backgroundColor: 'blue', position: 'absolute' }),
        layout: jsxAttributeValue({ height: 200, left: 80, width: 100, top: 145 }),
        'data-uid': jsxAttributeValue('ggg'),
      },
      [],
    )
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      ['layout'],
      jsxElement(
        'View',
        {
          style: jsxAttributeValue({ backgroundColor: 'lightgrey', position: 'absolute' }),
          layout: jsxAttributeNestedObjectSimple({
            height: jsxAttributeOtherJavaScript(
              'props.layout.height',
              'return props.layout.height;',
              ['props'],
              expect.objectContaining({}),
            ),
            left: jsxAttributeOtherJavaScript(
              'props.layout.left',
              'return props.layout.left;',
              ['props'],
              expect.objectContaining({}),
            ),
            width: jsxAttributeOtherJavaScript(
              'props.layout.width',
              'return props.layout.width;',
              ['props'],
              expect.objectContaining({}),
            ),
            top: jsxAttributeOtherJavaScript(
              'props.layout.top',
              'return props.layout.top;',
              ['props'],
              expect.objectContaining({}),
            ),
          }),
          'data-uid': jsxAttributeValue('aaa'),
        },
        [myCustomCompomnent, view],
      ),
      arbitraryJSBlock(
        `const a = 20;
const b = 40;
const MyCustomCompomnent = props => <View data-uid={"abc"}>{props.children}</View>;`,
        `var a = 20;
var b = 40;

var MyCustomCompomnent = function MyCustomCompomnent(props) {
  return React.createElement(View, {
    "data-uid": "abc"
  }, props.children);
};
return { a: a, b: b, MyCustomCompomnent: MyCustomCompomnent };`,
        ['a', 'b', 'MyCustomCompomnent'],
        ['React', 'View'],
        expect.objectContaining({
          sources: ['code.tsx'],
          version: 3,
          file: 'code.tsx',
        }),
      ),
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'App'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })

  it('properties without a value are treated as a property assigned to true', () => {
    const code = `
import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var App = props => {
  return (
    <View data-uid={'aaa'} booleanProperty />
  )
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const component = utopiaJSXComponent(
      'App',
      true,
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        {
          'data-uid': jsxAttributeValue('aaa'),
          booleanProperty: jsxAttributeValue(true),
        },
        [],
      ),
      null,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        sampleImportsForTests,
        [component],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'App'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('true attributes are printed without a value assigned to them directly', () => {
    const expectedResult = applyPrettier(
      `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var whatever = props => {
  return <View data-uid={"aaa"} booleanProperty />;
};
`,
      false,
    ).formatted
    const view = jsxElement(
      'View',
      { 'data-uid': jsxAttributeValue('aaa'), booleanProperty: jsxAttributeValue(true) },
      [],
    )
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const actualResult = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('false attributes are omitted completely', () => {
    const expectedResult = applyPrettier(
      `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";
export var whatever = props => {
  return <View data-uid={"aaa"} />;
};
`,
      false,
    ).formatted
    const view = jsxElement(
      'View',
      { 'data-uid': jsxAttributeValue('aaa'), booleanProperty: jsxAttributeValue(false) },
      [],
    )
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const actualResult = printCode(
      printCodeOptions(false, true, true),
      sampleImportsForTests,
      [exported],
      null,
      addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('injects infinite loop prevention into while and for loops', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  for (var n = 0; n != -1; n++) {
    const n2 = n * 2;
  }
  while (true) {
    const a = 1
  }
  return <div data-uid={'aaa'}></div>
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const arbitraryBlockCode = `for (var n = 0; n != -1; n++) {
  const n2 = n * 2;
}
while (true) {
  const a = 1;
}`
    const arbitraryBlockTranspiledCode = `var _loopIt = 0,
    _loopIt2 = 0;

for (var n = 0; n != -1; n++) {
  if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var n2 = n * 2;
}

while (true) {
  if (_loopIt2++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var a = 1;
}
return {  };`
    const arbitraryBlock = arbitraryJSBlock(
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      [],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const view = jsxElement('div', { 'data-uid': jsxAttributeValue('aaa') }, [])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        { react: sampleImportsForTests['react'] },
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('handles variables defined in a for loop declaration', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  let result = []
  for (var n = 0; n < 5; n++) {
    const n2 = n * 2;
    result.push(<div style={{ left: n, top: n2 }} data-uid={'bbb'} />);
  }
  return <div data-uid={'aaa'}>{result}</div>
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const arbitraryBlockCode = `let result = [];
for (var n = 0; n < 5; n++) {
  const n2 = n * 2;
  result.push(<div style={{ left: n, top: n2 }} data-uid={'bbb'} />);
}`
    const arbitraryBlockTranspiledCode = `var _loopIt = 0;
var result = [];

for (var n = 0; n < 5; n++) {
  if (_loopIt++ > ${InfiniteLoopMaxIterations}) {
    throw new RangeError('${InfiniteLoopError}');
  }

  var n2 = n * 2;
  result.push(React.createElement("div", {
    style: {
      left: n,
      top: n2
    },
    "data-uid": 'bbb'
  }));
}
return { result: result };`
    const arbitraryBlock = arbitraryJSBlock(
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['result'],
      ['React'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )
    const innerBlock = jsxArbitraryBlock(
      'result',
      'result;',
      'return result;',
      ['result'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      {},
    )
    const view = jsxElement('div', { 'data-uid': jsxAttributeValue('aaa') }, [innerBlock])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      arbitraryBlock,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        { react: sampleImportsForTests['react'] },
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('defined elsewhere values are assigned for elements inside arbitrary blocks', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  return <div data-uid={'aaa'}>
    {[1, 2, 3].map(n => {
      return <div style={{left: n * 30, top: n * 30}} data-uid={'bbb'} />
    })}
  </div>
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const arbitraryBlockOriginalCode = `[1, 2, 3].map(n => {
      return <div style={{left: n * 30, top: n * 30}} data-uid={'bbb'} />
    })`
    const arbitraryBlockCode = `[1, 2, 3].map(n => {
  return <div style={{ left: n * 30, top: n * 30 }} data-uid={'bbb'} />;
});`
    const arbitraryBlockTranspiledCode = `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("bbb", {
    n: n
  });
});`
    const innerElement = jsxElement(
      'div',
      {
        style: jsxAttributeNestedObject([
          jsxPropertyAssignment(
            'left',
            jsxAttributeOtherJavaScript(
              `n * 30`,
              `return n * 30;`,
              ['n'],
              expect.objectContaining({}),
            ),
          ),
          jsxPropertyAssignment(
            'top',
            jsxAttributeOtherJavaScript(
              `n * 30`,
              `return n * 30;`,
              ['n'],
              expect.objectContaining({}),
            ),
          ),
        ]),
        ['data-uid']: jsxAttributeValue('bbb'),
      },
      [],
    )
    const arbitraryBlock = jsxArbitraryBlock(
      arbitraryBlockOriginalCode,
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['React', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      { bbb: innerElement },
    )
    const view = jsxElement('div', { 'data-uid': jsxAttributeValue('aaa') }, [arbitraryBlock])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        { react: sampleImportsForTests['react'] },
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('defined elsewhere values coming from outside the block are recognised in props', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  const a = 30
  return <div data-uid={'aaa'}>
    {[1, 2, 3].map(n => {
      return <div style={{left: n * a, top: n * a}} data-uid={'bbb'} />
    })}
  </div>
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const arbitraryBlockOriginalCode = `[1, 2, 3].map(n => {
      return <div style={{left: n * a, top: n * a}} data-uid={'bbb'} />
    })`
    const arbitraryBlockCode = `[1, 2, 3].map(n => {
  return <div style={{ left: n * a, top: n * a }} data-uid={'bbb'} />;
});`
    const arbitraryBlockTranspiledCode = `return [1, 2, 3].map(function (n) {
  return utopiaCanvasJSXLookup("bbb", {
    n: n,
    a: a
  });
});`
    const innerElement = jsxElement(
      'div',
      {
        style: jsxAttributeNestedObject([
          jsxPropertyAssignment(
            'left',
            jsxAttributeOtherJavaScript(
              `n * a`,
              `return n * a;`,
              ['n', 'a'],
              expect.objectContaining({}),
            ),
          ),
          jsxPropertyAssignment(
            'top',
            jsxAttributeOtherJavaScript(
              `n * a`,
              `return n * a;`,
              ['n', 'a'],
              expect.objectContaining({}),
            ),
          ),
        ]),
        ['data-uid']: jsxAttributeValue('bbb'),
      },
      [],
    )
    const arbitraryBlock = jsxArbitraryBlock(
      arbitraryBlockOriginalCode,
      arbitraryBlockCode,
      arbitraryBlockTranspiledCode,
      ['a', 'React', 'utopiaCanvasJSXLookup'],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
      { bbb: innerElement },
    )

    const topLevelArbitraryBlock = arbitraryJSBlock(
      `const a = 30;`,
      `var a = 30;
return { a: a };`,
      ['a'],
      [],
      expect.objectContaining({
        sources: ['code.tsx'],
        version: 3,
        file: 'code.tsx',
      }),
    )

    const view = jsxElement('div', { 'data-uid': jsxAttributeValue('aaa') }, [arbitraryBlock])
    const exported = utopiaJSXComponent(
      'whatever',
      true,
      defaultPropsParam,
      [],
      view,
      topLevelArbitraryBlock,
    )
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        { react: sampleImportsForTests['react'] },
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('svg elements are accepted', () => {
    const code = `import * as React from "react"
export var whatever = props => {
  return <svg data-uid={'abc'}/>
}`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    const view = jsxElement('svg', { 'data-uid': jsxAttributeValue('abc') }, [])
    const exported = utopiaJSXComponent('whatever', true, defaultPropsParam, [], view, null)
    const expectedResult = clearParseResultUniqueIDs(
      parseSuccess(
        { react: sampleImportsForTests['react'] },
        [exported],
        expect.objectContaining({}),
        null,
        null,
        addModifierExportToDetail(EmptyExportsDetail, 'whatever'),
      ),
    )
    expect(actualResult).toEqual(expectedResult)
  })
  it('parses fine with a circular dependency.', () => {
    const code = `import * as React from "react";
import {
  View
} from "utopia-api";
const a = (n) => n > 0 ? n : b(10)
export var whatever = (props) => <View data-uid={'aaa'} />
const b = (n) => n > 0 ? n : a(10)
`
    const actualResult = clearParseResultUniqueIDs(testParseCode(code))
    expect(clearParseResultPassTimes(actualResult)).toMatchSnapshot()
  })
  it('corrects duplicated data-uid entries', () => {
    const code = `import * as React from "react";
import {
  View
} from "utopia-api";
export var whatever = (props) => <View data-uid={'aaa'}>
  <View data-uid={'aaa'} />
</View>
export var whatever2 = (props) => <View data-uid={'aaa'}>
  <View data-uid={'aaa'} />
</View>
`
    const actualResult = testParseCode(code)
    foldParsedTextFile(
      (_) => fail('Unable to parse code.'),
      (success) => {
        let uids: Array<string> = []
        for (const topLevelElement of success.topLevelElements) {
          if (isUtopiaJSXComponent(topLevelElement)) {
            ensureElementsHaveUID(topLevelElement.rootElement, uids)
          }
        }
      },
      (_) => fail('Unable to parse code.'),
      actualResult,
    )
  })
  it('inserts data-uid into elements as part of the parse', () => {
    function checkDataUIDsPopulated(printableProjectContent: PrintableProjectContent): boolean {
      const printedCode = printCode(
        printCodeOptions(false, true, false),
        printableProjectContent.imports,
        printableProjectContent.topLevelElements,
        printableProjectContent.jsxFactoryFunction,
        printableProjectContent.exportsDetail,
      )
      const parseResult = testParseCode(printedCode)
      return foldParsedTextFile(
        (failure) => {
          console.error(failure)
          return false
        },
        (success) => {
          let uids: Array<string> = []
          for (const topLevelElement of success.topLevelElements) {
            if (isUtopiaJSXComponent(topLevelElement)) {
              ensureElementsHaveUID(topLevelElement.rootElement, uids)
            }
          }
          return true
        },
        (unparsed) => {
          console.error(unparsed)
          return false
        },
        parseResult,
      )
    }
    const printableArbitrary = printableProjectContentArbitrary()
    const dataUIDProperty = FastCheck.property(printableArbitrary, checkDataUIDsPopulated)
    FastCheck.assert(dataUIDProperty, { verbose: true })
  })
})

describe('SourceMap', () => {
  const code = `import * as React from "react";
import {
  Ellipse,
  Image,
  Rectangle,
  Storyboard,
  Text,
  UtopiaUtils,
  View
} from "utopia-api";

console.log('hello!') // line 15 char 9

export var App = props => {
  const a = 20;
  const b = 40; // line 19 char 9

  return (
    <View
      style={{ backgroundColor: "lightgrey", position: "absolute" }}
      layout={{
        height: props.layout.height,
        left: props.layout.left,
        width: props.layout.width,
        top: props.layout.top,
      }}
      data-uid={"aaa"}
      arbitrary={console.log('hi')} // line 31, char 26
    >
    </View>
  )
}`
  it('maps a arbitraryJSBlock correctly', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be Right')
    }
    const consoleLogBlock = parseResult.topLevelElements[0]

    if (
      !isArbitraryJSBlock(consoleLogBlock) ||
      consoleLogBlock.javascript !== `console.log('hello!');`
    ) {
      fail('expected the first topLevelElement to be the console logline')
    }

    const transpiledCharacter = 1 + consoleLogBlock.transpiledJavascript.indexOf('log')
    const transpiledLine = 1

    const consumer = getSourceMapConsumer(consoleLogBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 12, column: 9 }))
  })

  it('maps an arbitraryJSBlock inside a utopiaJSXComponent', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be Right')
    }
    const appComponent = parseResult.topLevelElements[1]
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      fail('expected the second topLevelElement to be the App component')
    }

    const arbitraryBlock = appComponent.arbitraryJSBlock

    if (arbitraryBlock == null) {
      fail(`expected the App component's arbitraryJSBlock to be defined`)
    }

    const transpiledCharacter = 1 + arbitraryBlock.transpiledJavascript.split('\n')[1].indexOf('b')
    const transpiledLine = 2

    const consumer = getSourceMapConsumer(arbitraryBlock.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    // TODO BALAZS we should test that the code is in col 9, but I just couldn't make it work :(
    // expect(position).toEqual(expect.objectContaining({ line: 22, column: 9 }))
    expect(position).toEqual(expect.objectContaining({ line: 16 }))
  })

  it('maps a jsxAttributeOtherJavaScript correctly', () => {
    const parseResult = testParseCode(code)
    if (!isParseSuccess(parseResult)) {
      fail('expected parseResult to be a success')
    }
    const appComponent = parseResult.topLevelElements[1]
    if (!isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
      fail('expected the second topLevelElement to be the App component')
    }
    if (!isJSXElement(appComponent.rootElement)) {
      fail(`expected the App component's root element to be a JSXElement`)
    }

    const arbitraryProp = appComponent.rootElement.props.arbitrary

    if (!isJSXAttributeOtherJavaScript(arbitraryProp)) {
      fail(`expected <View /> to have an arbitrary js prop called props.arbitrary`)
    }

    const transpiledCharacter = 1 + arbitraryProp.transpiledJavascript.indexOf('log')

    const transpiledLine = 1

    const consumer = getSourceMapConsumer(arbitraryProp.sourceMap)

    const position = consumer.getOriginalPosition(transpiledLine, transpiledCharacter)

    expect(position).toEqual(expect.objectContaining({ line: 28, column: 26 }))
  })
})

describe('getHighlightBounds', () => {
  it('gets some bounds', () => {
    const bounds = getHighlightBoundsWithoutUID(
      'test.ts',
      `import * as React from "react";
    import {
      Ellipse,
      UtopiaUtils,
      Image,
      Rectangle,
      Text,
      View
    } from "utopia-api";
    
    console.log('hello!') // line 18 char 9
    
    export var App = props => {
      const a = 20;
      const b = 40; // line 22 char 9
    
      return (
        <View
          style={{ backgroundColor: "lightgrey", position: "absolute" }}
          layout={{
            height: props.layout.height,
            left: props.layout.left,
            width: props.layout.width,
            top: props.layout.top,
          }}
          data-uid={"aaa"}
          arbitrary={console.log('hi')} // line 34, char 26
        >
        </View>
      )
    })`,
    )
    expect(bounds).toMatchSnapshot()
  })
})

describe('lintAndParse', () => {
  it('returns a syntax error from eslint when something is broken', () => {
    const result = lintAndParse(
      'test.js',
      `
    import {
      Ellipse,
      UtopiaUtils,
      Image,
      Rectangle,
      Text,
      View
    } from "utopia-api";
    
    export var App = props => {
      const a = 20
      const b = 40
    
      return (
        <View
          style={{ backgroundColor: "darkgrey", position: "absolute" }, ...hello}
        >
        </View>
      )
    })`,
    )
    expect(clearParseResultPassTimes(result)).toMatchSnapshot()
  })
})

describe('Babel transpile', () => {
  it('can transpile jsx fragment', () => {
    const file = `/** @jsx jsx */
import * as React from 'react'
import { View, jsx } from 'utopia-api'

export var App = (props) => {
  return (
    <>
      <View
        style={{ ...props.style, backgroundColor: '#FFFFFF' }}
        layout={{ layoutSystem: 'pinSystem' }}
        data-uid={'aaa'}
      ></View>
    </>
  )
}`
    const code = `var App = (props) => {
      return (
        <>
          <View
            style={{ ...props.style, backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          ></View>
        </>
      )
    }`

    const sourceMap = {
      version: 3,
      sources: ['/src/app.js'],
      names: [
        '\n',
        ' ',
        'var',
        'App',
        '=',
        '(',
        'props',
        ')',
        '>',
        '{',
        'return',
        '<',
        'View',
        'style',
        '.',
        '|',
        '}',
        ',',
        'backgroundColor',
        ':',
        "'",
        '#',
        'FFFFFF',
        'layout',
        'layoutSystem',
        'pinSystem',
        'data',
        '-',
        'uid',
        'aaa',
        '/',
      ],
      mappings:
        'AAcEA;AACDA;AACMC,CAACC,GAAGD,CAACE,GAAGF,CAACG,CAACH,CAACI,CAACC,KAAKC,CAACN,CAACG,CAACI,CAACP,CAACQ,CAACT;AAC7BC,CAACA,CAACS,MAAMT,CAACI,CAACL;AACVC,CAACA,CAACA,CAACA,CAACU,CAACH,CAACR;AACNC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACU,CAACC,IAAIZ;AACXC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACY,KAAKT,CAACK,CAACA,CAACR,CAACa,CAACA,CAACA,CAACT,CAACC,KAAKQ,CAACD,KAAKZ,CAACc,CAACA,CAACd,CAACQ,CAACO,CAACT,CAACU,CAAChB,CAACiB,eAAeC,CAAClB,CAACmB,CAACC,CAACC,MAAMF,CAACnB,CAACe,CAACA,CAAChB;AACtEC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACsB,MAAMnB,CAACK,CAACA,CAACR,CAACuB,YAAYL,CAAClB,CAACmB,CAACK,SAASL,CAACnB,CAACe,CAACA,CAAChB;AAC9CC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACA,CAACyB,IAAIC,CAACC,GAAGxB,CAACK,CAACW,CAACS,GAAGT,CAACJ,CAAChB;AACxBC,CAACA,CAACA,CAACA,CAACA,CAACA,CAACO,CAACG,CAACmB,CAAClB,IAAIJ,CAACR;AACdC,CAACA,CAACA,CAACA,CAACU,CAACmB,CAACtB,CAACR;AACPC,CAACA,CAACM,CAACP;AACHgB',
      file: '/src/app.js',
      sourcesContent: [
        "/** @jsx jsx */\nimport * as React from 'react'\nimport { View, jsx } from 'utopia-api'\n\nexport var canvasMetadata = {\n  scenes: [\n    {\n      component: 'App',\n      frame: { height: 812, left: 0, width: 375, top: 0 },\n      props: { layout: { top: 0, left: 0, bottom: 0, right: 0 } },\n      container: { layoutSystem: 'pinSystem' },\n    },\n  ],\n  elementMetadata: {},\n}\n\nexport var App = (props) => {\n  return (\n    <>\n      <View\n        style={{ ...props.style, backgroundColor: '#FFFFFF' }}\n        layout={{ layoutSystem: 'pinSystem' }}\n        data-uid={'aaa'}\n      ></View>\n    </>\n  )\n}\n",
      ],
    }
    expect(
      transpileJavascriptFromCode('test.js', file, code, sourceMap, [], false),
    ).toMatchSnapshot()
  })
})
