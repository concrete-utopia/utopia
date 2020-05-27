describe('Linked views', () => {
  describe('When creating, or when a source or target is selected', () => {
    it('should draw a 2pt green line from the target to the source being created', () => {})
    it('should draw a 2pt green line connecting the selected element to all exitings sources and targets', () => {})
    it('the joining line should be between the centers if the source and target overlap', () => {})
    it('the joining line should be between the two closest segments of the frames of the source and target', () => {})
    it('the joining line should have a circle at each end, with 8pt diameter, 2pt green line, and a white fill', () => {})
    it('should draw a 2pt green frame around the source and the target', () => {})
  })

  it('should create a linked view source on mouse up when holding ctrl and dragging an element', () => {})
})

describe('Deleting an element', () => {
  it("should delete the scene if it's its root view", () => {})
  it("should apply all the element's properties to all of its targets", () => {})
  it('should delete all of its children', () => {})
  it('should remove the Source ID from any linked view targets of the element', () => {})
  it('should copy all of its children to any linked view targets of the element', () => {})
})

describe('Zooming', () => {
  describe('Via cmd +/-', () => {
    it('should zoom focusing on the selected element if one is selected', () => {})
    it('should zoom focusing on the center of the visible portion of the canvas if no element is selected', () => {})
  })
  describe('Via the toolbar buttons', () => {
    it('should zoom focusing on the selected element if one is selected', () => {})
    it('should zoom focusing on the center of the visible portion of the canvas if no element is selected', () => {})
  })
  describe('Via z / alt+z and mouse click', () => {
    it('should zoom focusing on the mouse position', () => {})
  })

  it('should double the current zoom percentage when zooming in', () => {})
  it('should halve the current zoom percentage when zooming out', () => {})
})
