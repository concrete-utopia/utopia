describe('Hide canvas controls when element is small', () => {
  it('No controls are shown when either width or height is less than 40px', () => {
    expect(0).toEqual(1)
  })

  it('All controls are shown when both width and height is greater than than 80px', () => {
    expect(0).toEqual(1)
  })

  describe('Some controls are shown when both width and height are between 40px and 80px', () => {
    it('no tests', () => {
      expect(0).toEqual(1)
    })
  })
})
