import {
  emptyComments,
  jsExpressionValue,
  jsIdentifier,
  jsPropertyAccess,
} from '../../../../core/shared/element-template'
import { jsxElementChildToValuePath } from './data-picker-utils'

describe('jsxElementChildToValuePath', () => {
  it('returns null for top-level attribute', () => {
    expect(jsxElementChildToValuePath(jsExpressionValue('Hello', emptyComments))).toEqual(null)
  })

  it('returns path for property expression', () => {
    expect(
      jsxElementChildToValuePath(
        jsPropertyAccess(
          jsIdentifier('data', '', null, emptyComments),
          'header',
          '',
          null,
          emptyComments,
          '',
          'not-optionally-chained',
        ),
      ),
    ).toEqual(['data', 'header'])
  })
})
