import { calculateDragDirectionDelta } from './number-input'

describe('number input', () => {
  it('calculateDragDirectionDelta should be able to invert its results', () => {
    const testCases = [
      [5, 2],
      [2, 5],
      [-5.5, 2],
      [-2, 5],
      [0, 4],
      [3.2, 1],
      [-6, 3],
      [-3, 6],
    ]

    testCases.forEach(([delta, scaling]) => {
      const { result, inverse } = calculateDragDirectionDelta(delta, scaling)
      expect(inverse(result)).toEqual(delta)
    })
  })
})
