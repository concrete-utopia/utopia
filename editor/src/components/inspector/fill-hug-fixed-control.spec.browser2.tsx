import type { LayoutPinnedProp } from '../../core/layout/layout-helpers-new'
import type { ConstraintsMode } from './fill-hug-fixed-control'
import { makeUpdatedConstraintsPropArray } from './fill-hug-fixed-control'

describe('makeUniqueConstrainedProps', () => {
  const tests: {
    name: string
    input: any[]
    mode: ConstraintsMode
    dimension: LayoutPinnedProp
    want: any[]
  }[] = [
    {
      name: 'adds a new value (empty)',
      input: [],
      mode: 'add',
      dimension: 'top',
      want: ['top'],
    },
    {
      name: 'adds a new value (non-empty)',
      input: ['width'],
      mode: 'add',
      dimension: 'top',
      want: ['width', 'top'],
    },
    {
      name: 'removes a new value (empty)',
      input: [],
      mode: 'remove',
      dimension: 'top',
      want: [],
    },
    {
      name: 'removes a new value (non-empty)',
      input: ['width', 'top', 'left'],
      mode: 'remove',
      dimension: 'top',
      want: ['width', 'left'],
    },
    {
      name: 'trims existing duplicated constraints (add)',
      input: ['top', 'left', 'left', 'left'],
      mode: 'add',
      dimension: 'width',
      want: ['top', 'left', 'width'],
    },
    {
      name: 'removes new duplicated constraints (add)',
      input: ['top', 'left', 'left', 'width'],
      mode: 'add',
      dimension: 'width',
      want: ['top', 'left', 'width'],
    },
    {
      name: 'trims existing duplicated constraints (remove)',
      input: ['top', 'left', 'left', 'left'],
      mode: 'remove',
      dimension: 'top',
      want: ['left'],
    },
    {
      name: 'removes new duplicated constraints (remove)',
      input: ['top', 'left', 'left', 'width'],
      mode: 'remove',
      dimension: 'width',
      want: ['top', 'left'],
    },
    {
      name: 'removes all constraints when duplicated',
      input: ['left', 'left', 'left'],
      mode: 'remove',
      dimension: 'left',
      want: [],
    },
    {
      name: 'when setting width, if there are already left and right, remove the oldest',
      input: ['top', 'left', 'right'],
      mode: 'add',
      dimension: 'width',
      want: ['top', 'right', 'width'],
    },
    {
      name: 'when setting height, if there are already top and bottom, remove the oldest',
      input: ['top', 'left', 'bottom'],
      mode: 'add',
      dimension: 'height',
      want: ['left', 'bottom', 'height'],
    },
    {
      name: 'when setting left, if there are already right/width, remove the width',
      input: ['right', 'width'],
      mode: 'add',
      dimension: 'left',
      want: ['right', 'left'],
    },
    {
      name: 'when setting right, if there are already left/width, remove the width',
      input: ['left', 'width'],
      mode: 'add',
      dimension: 'right',
      want: ['left', 'right'],
    },
    {
      name: 'when setting top, if there are already bottom/height, remove the height',
      input: ['bottom', 'height'],
      mode: 'add',
      dimension: 'top',
      want: ['bottom', 'top'],
    },
    {
      name: 'when setting bottom, if there are already top/height, remove the height',
      input: ['top', 'height'],
      mode: 'add',
      dimension: 'bottom',
      want: ['top', 'bottom'],
    },
  ]
  tests.forEach((test, index) => {
    it(`(${index + 1}) ${test.name}`, async () => {
      const got = makeUpdatedConstraintsPropArray(test.input, test.mode, test.dimension)
      expect(got).toEqual(test.want)
    })
  })
})
