import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
} from '../shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxConditionalExpression,
  singleLineComment,
} from '../shared/element-template'
import type { ConditionalCase } from './conditionals'
import { getConditionalActiveCase } from './conditionals'
import * as EP from '../../core/shared/element-path'
import { generateUUID } from '../../utils/utils'

describe('conditionals', () => {
  describe('getConditionalActiveCase', () => {
    const path = EP.fromString('foo')
    const tests: {
      name: string
      spy: ElementInstanceMetadataMap
      conditional: JSXConditionalExpression
      want: ConditionalCase | null
    }[] = [
      {
        name: 'no override, no spy',
        spy: {},
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        want: 'true-case',
      },
      {
        name: 'with override (true)',
        spy: {},
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          {
            leadingComments: [
              singleLineComment(
                '@utopia/conditional=true',
                '// @utopia/conditional=true',
                false,
                null,
              ),
            ],
            trailingComments: [],
          },
        ),
        want: 'true-case',
      },
      {
        name: 'with override (false)',
        spy: {},
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          {
            leadingComments: [
              singleLineComment(
                '@utopia/conditional=false',
                '// @utopia/conditional=false',
                false,
                null,
              ),
            ],
            trailingComments: [],
          },
        ),
        want: 'false-case',
      },
      {
        name: 'no override, not a conditional',
        spy: {
          foo: { conditionValue: 'not-a-conditional' } as ElementInstanceMetadata,
        },
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        want: null,
      },
      {
        name: 'no override, spy (true)',
        spy: {
          foo: { conditionValue: { active: true } } as ElementInstanceMetadata,
        },
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        want: 'true-case',
      },
      {
        name: 'no override, spy (false)',
        spy: {
          foo: { conditionValue: { active: false } } as ElementInstanceMetadata,
        },
        conditional: jsxConditionalExpression(
          'foo',
          jsExpressionValue(true, emptyComments),
          'true',
          jsExpressionValue(null, emptyComments),
          jsExpressionValue(null, emptyComments),
          emptyComments,
        ),
        want: 'false-case',
      },
    ]
    tests.forEach((test, index) => {
      it(`${index + 1}/${tests.length} ${test.name}`, async () => {
        const got = getConditionalActiveCase(path, test.conditional, test.spy)
        expect(got).toEqual(test.want)
      })
    })
  })
})
