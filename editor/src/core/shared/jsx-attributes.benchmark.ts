import * as Benny from 'benny'
import * as PP from './property-path'
import { jsxAttributesToProps } from './jsx-attributes'
import { getJSXAttributesAtPath, setJSXValueAtPath } from './jsx-attribute-utils'
import type { JSXAttributes } from './element-template'
import {
  emptyComments,
  jsOpaqueArbitraryStatement,
  jsExpressionNestedObject,
  jsExpressionOtherJavaScript,
  jsExpressionValue,
  jsxAttributeNestedArraySimple,
  jsxAttributeNestedObjectSimple,
  jsxAttributesFromMap,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
} from './element-template'
import { forEachLeft } from './either'
import { testRenderContext } from '../../utils/utils.test-utils'

function sampleJsxAttributes(): JSXAttributes {
  return jsxAttributesFromMap({
    style: jsExpressionNestedObject(
      [
        jsxSpreadAssignment(
          jsExpressionValue({ first: 1, second: 2 }, emptyComments),
          emptyComments,
        ),
        jsxPropertyAssignment(
          'backgroundColor',
          jsExpressionValue('red', emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'shadow',
          jsExpressionOtherJavaScript(
            [],
            'props.someShadow',
            'props.someShadow',
            'return props.someShadow;',
            ['props'],
            null,
            {},
            emptyComments,
          ),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'border',
          jsExpressionValue('1px solid green', emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'boxShadow',
          jsExpressionValue('0 0 0 1px blue', emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'top',
          jsExpressionValue(0, emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'left',
          jsExpressionValue(50, emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'height',
          jsExpressionValue(150, emptyComments),
          emptyComments,
          emptyComments,
        ),
        jsxPropertyAssignment(
          'width',
          jsExpressionValue(200, emptyComments),
          emptyComments,
          emptyComments,
        ),
      ],
      emptyComments,
    ),
    objectWithArray: jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        array: jsExpressionValue([0, 1, 2], emptyComments),
      }),
      emptyComments,
    ),
    objectWithNestedArray: jsxAttributeNestedObjectSimple(
      jsxAttributesFromMap({
        array: jsxAttributeNestedArraySimple([
          jsExpressionValue(0, emptyComments),
          jsExpressionValue(1, emptyComments),
          jsExpressionValue(2, emptyComments),
        ]),
      }),
      emptyComments,
    ),
    doggo: jsExpressionOtherJavaScript(
      [],
      'props.hello',
      'props.hello',
      'return props.hello;',
      ['props'],
      null,
      {},
      emptyComments,
    ),
    objectValue: jsExpressionValue(
      {
        deep: {
          object: {
            path: 'yes',
          },
        },
      },
      emptyComments,
    ),
    otherJs: jsExpressionOtherJavaScript(
      [],
      'true ? 10 : 5',
      'true ? 10 : 5',
      'return true ? 10 : 5',
      [],
      null,
      {},
      emptyComments,
    ),
    'data-uid': jsExpressionValue('aaa', emptyComments),
  })
}

export async function benchmarkAttributes(): Promise<void> {
  await Benny.suite(
    'get an attribute',
    Benny.add('getJSXAttributesAtPath', () => {
      const propertyPath = PP.create('style', 'width')
      const attributes = sampleJsxAttributes()
      return () => {
        const result = getJSXAttributesAtPath(attributes, propertyPath)
        switch (result.attribute.type) {
          case 'ATTRIBUTE_NOT_FOUND':
            throw new Error(`getJSXAttributeAtPath failed: ATTRIBUTE_NOT_FOUND`)
          case 'PART_OF_ATTRIBUTE_VALUE':
            throw new Error(`getJSXAttributeAtPath failed: PART_OF_ATTRIBUTE_VALUE`)
          default:
          // Do nothing, we got a value.
        }
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'get an attribute', details: true }),
  )
  await Benny.suite(
    'convert attributes into props',
    Benny.add('jsxAttributesToProps', () => {
      const inScope = {
        someValue: 1,
        anotherValue: 'another',
        props: {
          hello: 'Hello World',
        },
      }
      const attributes = sampleJsxAttributes()
      return () => {
        jsxAttributesToProps(inScope, attributes, null, testRenderContext, undefined, null)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'convert attributes to props', details: true }),
  )
  await Benny.suite(
    'setting a value into the attributes',
    Benny.add('setJSXValueAtPath', () => {
      const propertyPath = PP.create('style', 'width')
      const attributes = sampleJsxAttributes()
      const newValue = jsExpressionValue(240, emptyComments)
      return () => {
        const result = setJSXValueAtPath(attributes, propertyPath, newValue, 'include-in-printing')
        forEachLeft(result, (error) => {
          throw new Error(error)
        })
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'setting a value into the attributes', details: true }),
  )
}
