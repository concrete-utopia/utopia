import { cssKeyword, cssNumber, gridCSSKeyword, gridCSSNumber } from './common/css-utils'
import { mergeGridTemplateValues } from './flex-section'

describe('mergeGridTemplateValues', () => {
  it('empty values', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [],
        fromProps: [],
        autoValues: [],
      }),
    ).toEqual([])
  })

  it('only calculated', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [gridCSSNumber(cssNumber(1), null)],
        fromProps: [],
        autoValues: [],
      }),
    ).toEqual([gridCSSKeyword(cssKeyword('auto'), null)])
  })

  it('competing calculated and from props', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [gridCSSNumber(cssNumber(1), null)],
        fromProps: [gridCSSNumber(cssNumber(2), null)],
        autoValues: [],
      }),
    ).toEqual([gridCSSNumber(cssNumber(2), null)])
  })

  it('multiple elements, empty from props', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [gridCSSNumber(cssNumber(1), null), gridCSSNumber(cssNumber(2), null)],
        fromProps: [gridCSSNumber(cssNumber(2), null)],
        autoValues: [],
      }),
    ).toEqual([gridCSSNumber(cssNumber(2), null), gridCSSNumber(cssNumber(2), null)])
  })

  it('multiple elements, competing', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [gridCSSNumber(cssNumber(1), null), gridCSSNumber(cssNumber(2), null)],
        fromProps: [gridCSSNumber(cssNumber(2), null), gridCSSKeyword(cssKeyword('auto'), null)],
        autoValues: [],
      }),
    ).toEqual([gridCSSNumber(cssNumber(2), null), gridCSSKeyword(cssKeyword('auto'), null)])
  })

  it('one auto value, auto calculated', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [gridCSSKeyword(cssKeyword('auto'), null)],
        fromProps: [],
        autoValues: [gridCSSNumber(cssNumber(42), null)],
      }),
    ).toEqual([gridCSSNumber(cssNumber(42), null)])
  })

  it('one auto value, multiple auto calculated', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
        ],
        fromProps: [],
        autoValues: [gridCSSNumber(cssNumber(42), null)],
      }),
    ).toEqual([
      gridCSSNumber(cssNumber(42), null),
      gridCSSNumber(cssNumber(42), null),
      gridCSSNumber(cssNumber(42), null),
    ])
  })

  it('multiple auto values, multiple auto calculated', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
        ],
        fromProps: [],
        autoValues: [
          gridCSSNumber(cssNumber(1), null),
          gridCSSNumber(cssNumber(2), null),
          gridCSSNumber(cssNumber(3), null),
        ],
      }),
    ).toEqual([
      gridCSSNumber(cssNumber(1), null),
      gridCSSNumber(cssNumber(2), null),
      gridCSSNumber(cssNumber(3), null),
      gridCSSNumber(cssNumber(1), null),
      gridCSSNumber(cssNumber(2), null),
    ])
  })

  it('one auto value, multiple auto calculated, but values from props', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
        ],
        fromProps: [gridCSSNumber(cssNumber(42), null)],
        autoValues: [gridCSSNumber(cssNumber(1), null)],
      }),
    ).toEqual([
      gridCSSNumber(cssNumber(42), null),
      gridCSSNumber(cssNumber(1), null),
      gridCSSNumber(cssNumber(1), null),
      gridCSSNumber(cssNumber(1), null),
      gridCSSNumber(cssNumber(1), null),
    ])
  })

  it('multiple auto values, multiple auto calculated, but values from props', async () => {
    expect(
      mergeGridTemplateValues({
        calculated: [
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSKeyword(cssKeyword('auto'), null),
        ],
        fromProps: [
          gridCSSNumber(cssNumber(42), null),
          gridCSSKeyword(cssKeyword('auto'), null),
          gridCSSNumber(cssNumber(23), null),
        ],
        autoValues: [gridCSSNumber(cssNumber(1), null), gridCSSNumber(cssNumber(2), null)],
      }),
    ).toEqual([
      gridCSSNumber(cssNumber(42), null),
      gridCSSNumber(cssNumber(2), null),
      gridCSSNumber(cssNumber(23), null),
      gridCSSNumber(cssNumber(2), null),
      gridCSSNumber(cssNumber(1), null),
    ])
  })
})
