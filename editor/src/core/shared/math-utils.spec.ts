import {
  canvasRectangle,
  doRectanglesIntersect,
  infinityCanvasRectangle,
  wrapValue,
} from './math-utils'

describe('math utils', () => {
  describe('wrapValue', () => {
    it('wraps values inside the given boundaries', async () => {
      expect(wrapValue(0, 0, 5)).toEqual(0)
      expect(wrapValue(3, 0, 5)).toEqual(3)
      expect(wrapValue(5, 0, 5)).toEqual(5)
      expect(wrapValue(6, 0, 5)).toEqual(0)
      expect(wrapValue(8, 0, 5)).toEqual(2)
      expect(wrapValue(14, 0, 5)).toEqual(2)
      expect(wrapValue(-1, 0, 5)).toEqual(5)
      expect(wrapValue(-3, 0, 5)).toEqual(3)
      expect(wrapValue(-14, 0, 5)).toEqual(4)
      expect(wrapValue(14, 10, 15)).toEqual(14)
      expect(wrapValue(18, 10, 15)).toEqual(12)
      expect(wrapValue(9, 10, 15)).toEqual(15)
      expect(wrapValue(7, 10, 15)).toEqual(13)
      expect(wrapValue(-4, -7, -3)).toEqual(-4)
      expect(wrapValue(-11, -7, -3)).toEqual(-6)
      expect(wrapValue(-2, -7, -3)).toEqual(-7)
    })
  })
  describe('doRectanglesIntersect', () => {
    it('when either value is an infinity rectangle returns true', () => {
      const regularRectangle = canvasRectangle({ x: 10, y: 20, width: 100, height: 200 })
      expect(doRectanglesIntersect(infinityCanvasRectangle, regularRectangle)).toEqual(true)
      expect(doRectanglesIntersect(regularRectangle, infinityCanvasRectangle)).toEqual(true)
      expect(doRectanglesIntersect(infinityCanvasRectangle, infinityCanvasRectangle)).toEqual(true)
    })
    it('when the rectangles are independent returns false', () => {
      const regularRectangle1 = canvasRectangle({ x: 10, y: 20, width: 100, height: 200 })
      const regularRectangle2 = canvasRectangle({ x: 400, y: 20, width: 100, height: 200 })
      expect(doRectanglesIntersect(regularRectangle1, regularRectangle2)).toEqual(false)
    })
    it('when the rectangles intersect on a corner returns true', () => {
      const regularRectangle1 = canvasRectangle({ x: 10, y: 20, width: 100, height: 200 })
      const regularRectangle2 = canvasRectangle({ x: 40, y: 50, width: 100, height: 200 })
      expect(doRectanglesIntersect(regularRectangle1, regularRectangle2)).toEqual(true)
    })
    it('when the rectangles fully overlap returns true', () => {
      const regularRectangle1 = canvasRectangle({ x: 10, y: 20, width: 100, height: 200 })
      const regularRectangle2 = canvasRectangle({ x: 5, y: 5, width: 400, height: 400 })
      expect(doRectanglesIntersect(regularRectangle1, regularRectangle2)).toEqual(true)
    })
  })
})
