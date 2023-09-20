import * as Chai from 'chai'
import Utils from './utils'
import type { CanvasRectangle, LocalPoint, LocalRectangle } from '../core/shared/math-utils'
import { longestCommonArray, projectIdFromURL } from '../core/shared/utils'
import fastDeepEquals from 'fast-deep-equal'
import { left, right } from '../core/shared/either'

// disabling jest/valid-expect because this file uses Chai.expect
/* eslint-disable jest/valid-expect */
const expect = Chai.expect

describe('longestCommonArray', () => {
  it('returns an empty array if both are empty', () => {
    expect(longestCommonArray([], [])).to.deep.equal([])
  })
  it('returns an empty array if the first is empty', () => {
    expect(longestCommonArray([], [1, 2, 3])).to.deep.equal([])
  })
  it('returns an empty array if the second is empty', () => {
    expect(longestCommonArray([1, 2, 3], [])).to.deep.equal([])
  })
  it('returns the entire array if the whole thing matches', () => {
    expect(longestCommonArray(['aaa', 'bbb', 'ccc'], ['aaa', 'bbb', 'ccc'])).to.deep.equal([
      'aaa',
      'bbb',
      'ccc',
    ])
  })
  it('returns the appropriate prefix if it partly matches', () => {
    expect(longestCommonArray(['aaa', 'bbb', 'ccc'], ['aaa', 'bbb', 'ddd'])).to.deep.equal([
      'aaa',
      'bbb',
    ])
  })
  it('returns an empty array if none of it matches', () => {
    expect(longestCommonArray(['ddd', 'eee', 'fff'], ['aaa', 'bbb', 'ccc'])).to.deep.equal([])
  })
  it('returns an empty array if the first element does not match', () => {
    expect(longestCommonArray(['ddd', 'bbb', 'ccc'], ['aaa', 'bbb', 'ccc'])).to.deep.equal([])
  })
})

const testObjectToCopy = {
  testString: 'meow',
  testNumber: 1,
  thisIsAFunction: () => {},
  icecream: {
    chocolate: true,
    someFunction: () => {},
    anotherNestedObject: {
      anotherFunction: () => {},
    },
  },
}

describe('Utils.copyObjectWithoutFunctions', () => {
  it('creates a copy of an object without the functions', () => {
    const copiedObject = Utils.copyObjectWithoutFunctions(testObjectToCopy)
    expect(copiedObject).to.deep.equal({
      testString: 'meow',
      testNumber: 1,
      icecream: {
        chocolate: true,
        anotherNestedObject: {},
      },
    })
  })
})

describe('Utils.nextName', () => {
  it('works in the basic case', () => {
    expect(Utils.nextName('hello 3', ['hello', 'hello 4', 'hello 2', 'world 10'])).to.equal(
      'hello 5',
    )
  })
  it('works with names without spaces', () => {
    expect(
      Utils.nextName('ViewScene', ['ViewScene', 'ViewScene1', 'ViewScene2', 'App'], false),
    ).to.equal('ViewScene3')
  })
})

describe('Utils.getAllObjectPaths', () => {
  it('returns paths for a nested object', () => {
    const objectToSearch = {
      firstLevel: {
        secondLevel: {
          thirdLevel: {
            something: 9,
            otherthing: 'hat',
          },
        },
        thingoverhere: true,
      },
    }
    var expectedResult = [
      ['firstLevel'],
      ['firstLevel', 'secondLevel'],
      ['firstLevel', 'secondLevel', 'thirdLevel'],
      ['firstLevel', 'secondLevel', 'thirdLevel', 'otherthing'],
      ['firstLevel', 'secondLevel', 'thirdLevel', 'something'],
      ['firstLevel', 'thingoverhere'],
    ]
    expectedResult.sort()
    var actualResult = Utils.getAllObjectPaths(objectToSearch)
    actualResult.sort()
    expect(actualResult).to.deep.equal(expectedResult)
  })
})

function checkProxyValue(
  makeOriginalValue: () => any,
  expectedAssignments: Array<{ path: Array<string>; value: any }> = [],
  modifications: (proxy: any) => any = (p) => {},
) {
  const originalValue = makeOriginalValue()
  var actualAssignments: Array<{ path: Array<string>; value: any }> = []
  function recordAssignment(path: Array<string>, value: any): any {
    actualAssignments.push({ path: path, value: value })
  }
  const proxiedValue = Utils.proxyValue(originalValue, recordAssignment)
  modifications(proxiedValue)
  expect(actualAssignments).to.deep.equal(expectedAssignments)
  expect(originalValue).to.deep.equal(makeOriginalValue())
}

describe('Utils.proxyValue', () => {
  it('simple value is the same', () => {
    checkProxyValue(() => 4)
  })
  it('object with no changes is the same', () => {
    checkProxyValue(() => {
      return { a: { b: { c: ['cake'], d: 9 }, e: false } }
    })
  })
  it('simple object updated', () => {
    checkProxyValue(
      () => {
        return { a: 10 }
      },
      [{ path: ['a'], value: 20 }],
      (value) => {
        value.a = 20
      },
    )
  })
  it('nested object updated', () => {
    checkProxyValue(
      () => {
        return { a: { b: 10 } }
      },
      [{ path: ['a', 'b'], value: 20 }],
      (value) => {
        value.a.b = 20
      },
    )
  })
  it('deeply nested object updated', () => {
    checkProxyValue(
      () => {
        return { a: { b: { c: 10 } } }
      },
      [{ path: ['a', 'b', 'c'], value: 20 }],
      (value) => {
        value.a.b.c = 20
      },
    )
  })
  it('simple array updated', () => {
    checkProxyValue(
      () => [10],
      [{ path: ['0'], value: 20 }],
      (value) => {
        value[0] = 20
      },
    )
  })
  it('deeply nested array updated', () => {
    checkProxyValue(
      () => [[1, [2, 3, 4, 5, 10]]],
      [{ path: ['0', '1', '5'], value: 20 }],
      (value) => {
        value[0][1][5] = 20
      },
    )
  })
  it('arrays and objects nested within each other', () => {
    function makeValue() {
      return {
        a: false,
        b: [
          {
            c: [1, 2, 3, 4],
            d: 'hat',
          },
          ['elephant'],
        ],
      }
    }
    checkProxyValue(makeValue, [{ path: ['b', '0', 'c', '2'], value: 20 }], (v) => {
      v.b[0].c[2] = 20
    })
  })
  it('updates on top of other updates', () => {
    function makeValue() {
      return {
        a: false,
        b: [
          {
            c: [1, 2, 3, 4],
            d: 'hat',
          },
          ['elephant'],
        ],
      }
    }
    const expectedAssignments = [
      { path: ['b', '0', 'c', '2'], value: 'elephant with a cape' },
      { path: ['b', '0', 'd'], value: 'elephant with a cape and a cane' },
    ]
    checkProxyValue(makeValue, expectedAssignments, (v) => {
      v.b[0].c[2] = v.b[1][0] + ' with a cape'
      v.b[0].d = v.b[0].c[2] + ' and a cane'
    })
  })
})

describe('Utils.boundingRectangle', () => {
  it('encompasses two overlapping rectangles', () => {
    const first = { x: 10, y: 20, width: 100, height: 200 } as CanvasRectangle
    const second = { x: 90, y: 150, width: 300, height: 400 } as CanvasRectangle
    const actualResult = Utils.boundingRectangle(first, second)
    const expectedResult = { x: 10, y: 20, width: 380, height: 530 }
    expect(actualResult).to.deep.equal(expectedResult)
  })
})

describe('Utils.rectangleIntersection', () => {
  it('calculates the intersection of two rectangles', () => {
    const first = { x: 10, y: 20, width: 100, height: 200 } as CanvasRectangle
    const second = { x: 50, y: 60, width: 300, height: 400 } as CanvasRectangle
    const actualResult = Utils.rectangleIntersection(first, second)
    const expectedResult = { x: 50, y: 60, width: 60, height: 160 }
    expect(actualResult).to.deep.equal(expectedResult)
  })

  it('calculates the empty intersection of two rectangles', () => {
    const first = { x: 10, y: 20, width: 100, height: 200 } as CanvasRectangle
    const second = {
      x: 150,
      y: 260,
      width: 300,
      height: 400,
    } as CanvasRectangle
    const actualResult = Utils.rectangleIntersection(first, second)
    const expectedResult = null
    expect(actualResult).to.equal(expectedResult)
  })
})

describe('Utils.traverseArray', () => {
  it('when everything returns a result', () => {
    const actualResult = Utils.traverseArray((elem) => elem * 2, [1, 2, 3])
    const expectedResult = [2, 4, 6]
    expect(actualResult).to.deep.equal(expectedResult)
  })
  it('when something returns a null', () => {
    const actualResult = Utils.traverseArray(
      (elem) => {
        if (elem == 2) {
          return null
        } else {
          return elem * 2
        }
      },
      [1, 2, 3],
    )
    const expectedResult = null
    expect(actualResult).to.deep.equal(expectedResult)
  })
})

describe('Utils.stepInArray', () => {
  it('when stepping forwards inside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, 1, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(4)
  })
  it('when stepping backwards inside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, -1, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(2)
  })
  it('when stepping forwards outside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, 4, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(2)
  })
  it('when stepping backwards outside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, -4, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(4)
  })
  it('when stepping forwards vastly outside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, 14, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(2)
  })
  it('when stepping backwards vastly outside the length of the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, -14, [1, 2, 3, 4, 5], 3)
    expect(actualResult).to.deep.equal(4)
  })
  it('when the originating element is not in the array', () => {
    const actualResult = Utils.stepInArray(fastDeepEquals, 1, [1, 2, 3, 4, 5], 999)
    expect(actualResult).to.deep.equal(null)
  })
})

describe('Utils.scaleRect', () => {
  it('correctly scales from the center', () => {
    const rect = { x: 0, y: 0, width: 1, height: 1 } as LocalRectangle
    const expectedResult = { x: -0.5, y: -0.5, width: 2, height: 2 }
    const actualResult = Utils.scaleRect(rect, 2, true)
    expect(actualResult).to.deep.equal(expectedResult)
  })
})

describe('Utils.closestPointOnLine', () => {
  const lineA = { x: 10, y: 10 } as LocalPoint
  const lineB = { x: 20, y: 20 } as LocalPoint
  it('finds point on line', () => {
    const pointOnLine = { x: 15, y: 15 } as LocalPoint
    expect(Utils.closestPointOnLine(lineA, lineB, pointOnLine)).to.deep.equal(pointOnLine)
  })

  it('finds point outside edge limits', () => {
    const pointOnLine = { x: 45, y: 45 } as LocalPoint
    expect(Utils.closestPointOnLine(lineA, lineB, pointOnLine)).to.deep.equal(pointOnLine)
  })

  it('finds point not on line', () => {
    const point = { x: 10, y: 20 } as LocalPoint
    expect(Utils.closestPointOnLine(lineA, lineB, point)).to.deep.equal({
      x: 15,
      y: 15,
    })
  })
})

describe('ObjectFlattenKeys', () => {
  it('works for simple case', () => {
    const obj = { a: 5, b: 10 }
    const expected = ['a', 'b']
    expect(Utils.objectFlattenKeys(obj)).deep.equal(expected)
  })

  it('works for deep object', () => {
    const obj = { a: 5, b: { cica: { kutya: 20 }, lo: 8 } }
    const expected = ['a', 'b', 'cica', 'kutya', 'lo']
    expect(Utils.objectFlattenKeys(obj)).deep.equal(expected)
  })

  it('arrays doesnt trip it up', () => {
    const obj = { a: 5, b: [{ cica: { kutya: 20 }, lo: 8 }] }
    const expected = ['a', 'b', 'cica', 'kutya', 'lo']
    expect(Utils.objectFlattenKeys(obj)).deep.equal(expected)
  })

  it('arrays really doesnt trip it up', () => {
    const obj = {
      a: 5,
      b: [{ cica: { kutya: 20 }, lo: 8 }, { kitty: 3 }, { cat: 10, loaf: 0 }],
    }
    const expected = ['a', 'b', 'cica', 'kutya', 'lo', 'kitty', 'cat', 'loaf']
    expect(Utils.objectFlattenKeys(obj)).deep.equal(expected)
  })
})

describe('projectIdFromURL', () => {
  it.each([
    ['cake', left(`Invalid value passed that isn't a URL.`)],
    [
      'https://utopia.app/something',
      left(`URL does not appear to have the project ID or be for a project.`),
    ],
    ['https://utopia.app/p/abc123/rest/of/the/url', right('abc123')],
    ['https://utopia.app/p/abc123', right('abc123')],
    ['https://utopia.app/p/abc123-nice-hat', right('abc123')],
    ['https://utopia.app/p/abc123-nice-hat/rest-of-the-url', right('abc123')],
    ['http://localhost:8000/p/abc123/rest/of/the/url', right('abc123')],
    ['http://localhost:8000/p/abc123', right('abc123')],
    ['http://localhost:8000/p/abc123-nice-hat', right('abc123')],
    ['http://localhost:8000/p/abc123-nice-hat/rest-of-the-url', right('abc123')],
    ['https://utopia.app/project/abc123/rest/of/the/url', right('abc123')],
    ['https://utopia.app/project/abc123', right('abc123')],
    ['https://utopia.app/project/abc123-nice-hat', right('abc123')],
    ['https://utopia.app/project/abc123-nice-hat/rest-of-the-url', right('abc123')],
    ['http://localhost:8000/project/abc123/rest/of/the/url', right('abc123')],
    ['http://localhost:8000/project/abc123', right('abc123')],
    ['http://localhost:8000/project/abc123-nice-hat', right('abc123')],
    ['http://localhost:8000/project/abc123-nice-hat/rest-of-the-url', right('abc123')],
  ])('the value %s should produce a %s', (input, expectedResult) => {
    const actualResult = projectIdFromURL(input)
    expect(actualResult).deep.equal(expectedResult)
  })
})
