import { pointerEventsFromWorldState, RestrictedPointerEvents, WorldState } from './cursor-overlay'

describe('cursor overlay', () => {
  it('pointer events from world state', () => {
    const states: Array<[WorldState, RestrictedPointerEvents]> = [
      [{ imageDragInProgress: false, isCursorSet: false }, 'none'],
      [{ imageDragInProgress: true, isCursorSet: false }, 'none'],
      [{ imageDragInProgress: false, isCursorSet: true }, 'all'],
      [{ imageDragInProgress: true, isCursorSet: true }, 'none'],
    ]

    for (const [state, events] of states) {
      expect(pointerEventsFromWorldState(state)).toEqual(events)
    }
  })
})
