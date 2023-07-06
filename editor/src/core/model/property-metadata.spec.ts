import type { PropMetadata } from './property-metadata'
import { dumbGetter } from './property-metadata'

interface MyProps {
  propA: string
  propB: {
    a: string
    b: 15
    c: 'cica'
    deeper: {
      value: 'value'
      deepest: {
        tooMuch: 'no!'
      }
    }
  }
  largeObjectValue: {
    aLargeObject: {
      singleValue: {
        nothingFancy: {
          asItShouldBe: 'yes!'
          orASuddenExpression: 'oh no!'
        }
      }
    }
  }
}

const props: MyProps = {
  propA: 'hello',
  propB: {
    a: 'szia',
    b: 15,
    c: 'cica',
    deeper: {
      value: 'value',
      deepest: {
        tooMuch: 'no!',
      },
    },
  },
  largeObjectValue: {
    aLargeObject: {
      singleValue: {
        nothingFancy: {
          asItShouldBe: 'yes!',
          orASuddenExpression: 'oh no!',
        },
      },
    },
  },
}

type Ugh = PropMetadata<MyProps>

const test: Ugh = {
  $$metadata: true,
  propA: {
    $$propertyType: 'staticvalue',
    value: 'hello',
  },
  propB: {
    $$propertyType: 'staticvalue',
    value: {
      $$metadata: true,
      a: {
        $$propertyType: 'staticvalue',
        value: 'string',
      },
      b: {
        $$propertyType: 'nodegraphvalue',
        node: 'MyNode',
        field: 'myField1',
      },
      c: {
        $$propertyType: 'propsgettervalue',
        propName: 'hello',
      },
      deeper: {
        $$propertyType: 'staticvalue',
        value: {
          $$metadata: true,
          deepest: {
            $$propertyType: 'staticvalue',
            value: {
              $$metadata: true,
              tooMuch: {
                $$propertyType: 'staticvalue',
                value: 'no!',
              },
            },
          },
          value: {
            $$propertyType: 'staticvalue',
            value: 'value',
          },
        },
      },
    },
  },
  largeObjectValue: {
    $$propertyType: 'staticvalue',
    value: {
      aLargeObject: {
        singleValue: {
          nothingFancy: {
            asItShouldBe: 'yes!',
            orASuddenExpression: 'oh no!',
            // orASuddenExpression: {
            //   $$propertyType: 'staticvalue',
            //   value: 'oh no!',
            // },
          },
        },
      },
    },
  },
}

describe('Prop metadata getter', () => {
  it('works?', () => {
    const stringNo = dumbGetter(test, 'propB', 'deeper', 'deepest', 'tooMuch')
    expect(stringNo).toEqual('no!')
    const deeper = dumbGetter(test, 'propB', 'deeper')
    expect(deeper).toEqual({ value: 'value', deepest: { tooMuch: 'no!' } })

    const getValueFromObjectMetadata = dumbGetter(
      test,
      'largeObjectValue',
      'aLargeObject',
      'singleValue',
      'nothingFancy',
    )

    expect(getValueFromObjectMetadata).toEqual({
      asItShouldBe: 'yes!',
      orASuddenExpression: 'oh no!',
    })

    // const nodeValue = dumbGetter(test, 'propB', 'b')
    // expect(nodeValue).toEqual(15)
  })
})
