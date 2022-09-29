import { act, fireEvent } from '@testing-library/react'
import { emptyModifiers, Modifiers } from '../../utils/modifiers'
import { resetMouseStatus } from '../mouse-move'
import keycode from 'keycode'
import { NO_OP } from '../../core/shared/utils'

// TODO Should the mouse move and mouse up events actually be fired at the parent of the event source?
// Or document.body?

interface Point {
  x: number
  y: number
}

export async function mouseDownAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  await act(async () => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mousedown', {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
  })
}

export async function mouseMoveToPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  await act(async () => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mousemove', {
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        movementX: 1,
        movementY: 1,
        ...eventOptions,
      }),
    )
  })

  resetMouseStatus()
}

export async function mouseUpAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  await act(async () => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseup', {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        ...eventOptions,
      }),
    )
  })
}

export async function mouseDragFromPointWithDelta(
  eventSourceElement: HTMLElement,
  startPoint: Point,
  dragDelta: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
    staggerMoveEvents?: boolean
    midDragCallback?: () => void
  } = {},
) {
  const endPoint: Point = {
    x: startPoint.x + dragDelta.x,
    y: startPoint.y + dragDelta.y,
  }
  return mouseDragFromPointToPoint(eventSourceElement, startPoint, endPoint, options)
}

export async function mouseDragFromPointToPoint(
  eventSourceElement: HTMLElement,
  startPoint: Point,
  endPoint: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
    staggerMoveEvents?: boolean
    midDragCallback?: () => void
  } = {},
) {
  const { buttons, ...mouseUpOptions } = options.eventOptions ?? {}
  const staggerMoveEvents = options.staggerMoveEvents ?? true
  const midDragCallback = options.midDragCallback ?? NO_OP

  const delta: Point = {
    x: endPoint.x - startPoint.x,
    y: endPoint.y - startPoint.y,
  }

  await mouseDownAtPoint(eventSourceElement, startPoint, options)

  if (staggerMoveEvents) {
    const numberOfSteps = 5
    for (let step = 1; step < numberOfSteps + 1; step++) {
      const stepSize: Point = {
        x: delta.x / numberOfSteps,
        y: delta.y / numberOfSteps,
      }

      await mouseMoveToPoint(
        eventSourceElement,
        {
          x: startPoint.x + step * stepSize.x,
          y: startPoint.y + step * stepSize.y,
        },
        {
          ...options,
          eventOptions: {
            movementX: stepSize.x,
            movementY: stepSize.y,
            buttons: 1,
            ...options.eventOptions,
          },
        },
      )
    }
  } else {
    await mouseMoveToPoint(
      eventSourceElement,
      {
        x: endPoint.x,
        y: endPoint.y,
      },
      {
        ...options,
        eventOptions: {
          movementX: delta.x,
          movementY: delta.y,
          buttons: 1,
          ...options.eventOptions,
        },
      },
    )
  }

  midDragCallback()

  await mouseUpAtPoint(eventSourceElement, endPoint, {
    ...options,
    eventOptions: mouseUpOptions,
  })
}

export async function mouseClickAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }
  const { buttons, ...mouseUpOptions } = eventOptions ?? {}

  await act(async () => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mousedown', {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )

    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseup', {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...mouseUpOptions,
      }),
    )

    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseclick', {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
  })
}

export async function mouseDoubleClickAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
    initialClickCount?: number
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }
  const { buttons, ...mouseUpOptions } = eventOptions ?? {}
  const initialClickCount = options.initialClickCount ?? 0

  await act(async () => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mousedown', {
        detail: initialClickCount + 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseup', {
        detail: initialClickCount + 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        ...mouseUpOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('click', {
        detail: initialClickCount + 1,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('mousedown', {
        detail: initialClickCount + 2,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseup', {
        detail: initialClickCount + 2,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        ...mouseUpOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('click', {
        detail: initialClickCount + 2,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
    fireEvent(
      eventSourceElement,
      new MouseEvent('dblclick', {
        detail: initialClickCount + 2,
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        buttons: 1,
        ...eventOptions,
      }),
    )
  })
}

export async function keyDown(
  key: string,
  options: {
    modifiers?: Modifiers
    eventOptions?: KeyboardEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  await act(async () => {
    fireEvent(
      document.body,
      new KeyboardEvent('keydown', {
        bubbles: true,
        cancelable: true,
        key: key,
        keyCode: keycode(key),
        ...eventOptions,
      }),
    )
  })
}

export async function pressKey(
  key: string,
  options: {
    modifiers?: Modifiers
    eventOptions?: KeyboardEventInit
  } = {},
) {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  await act(async () => {
    fireEvent(
      document.body,
      new KeyboardEvent('keydown', {
        bubbles: true,
        cancelable: true,
        key: key,
        keyCode: keycode(key),
        ...eventOptions,
      }),
    )

    fireEvent(
      document.body,
      new KeyboardEvent('keyup', {
        bubbles: true,
        cancelable: true,
        key: key,
        keyCode: keycode(key),
        ...eventOptions,
      }),
    )
  })
}
