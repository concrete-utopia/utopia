import { act, createEvent, fireEvent } from '@testing-library/react'
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
): Promise<void> {
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
): Promise<void> {
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

export function mouseEnterAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
): void {
  const modifiers = options.modifiers ?? emptyModifiers
  const passedEventOptions = options.eventOptions ?? {}
  const eventOptions = {
    ctrlKey: modifiers.ctrl,
    metaKey: modifiers.cmd,
    altKey: modifiers.alt,
    shiftKey: modifiers.shift,
    ...passedEventOptions,
  }

  act(() => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseover', {
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        ...eventOptions,
      }),
    )
  })

  act(() => {
    fireEvent(
      eventSourceElement,
      new MouseEvent('mouseenter', {
        bubbles: true,
        cancelable: true,
        clientX: point.x,
        clientY: point.y,
        ...eventOptions,
      }),
    )
  })
}

export async function mouseUpAtPoint(
  eventSourceElement: HTMLElement,
  point: Point,
  options: {
    modifiers?: Modifiers
    eventOptions?: MouseEventInit
  } = {},
): Promise<void> {
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
    midDragCallback?: () => Promise<void>
  } = {},
): Promise<void> {
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
    midDragCallback?: () => Promise<void>
  } = {},
): Promise<void> {
  const { buttons, ...mouseUpOptions } = options.eventOptions ?? {}
  const staggerMoveEvents = options.staggerMoveEvents ?? true

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

  if (options.midDragCallback != null) {
    await options.midDragCallback()
  }

  await mouseUpAtPoint(eventSourceElement, endPoint, {
    ...options,
    eventOptions: mouseUpOptions,
  })
}

export function mouseDragFromPointToPointNoMouseDown(
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

  if (staggerMoveEvents) {
    const numberOfSteps = 5
    for (let step = 1; step < numberOfSteps + 1; step++) {
      const stepSize: Point = {
        x: delta.x / numberOfSteps,
        y: delta.y / numberOfSteps,
      }

      mouseMoveToPoint(
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
    mouseMoveToPoint(
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

  mouseUpAtPoint(eventSourceElement, endPoint, {
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
): Promise<void> {
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

    fireEvent(
      eventSourceElement,
      new MouseEvent('click', {
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

export function mouseDoubleClickAtPoint(
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

  act(() => {
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

export function keyDown(
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

  act(() => {
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

export function pressKey(
  key: string,
  options: {
    modifiers?: Modifiers
    eventOptions?: KeyboardEventInit
    targetElement?: HTMLElement
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

  const target = options?.targetElement ?? document.body

  act(() => {
    fireEvent(
      target,
      new KeyboardEvent('keydown', {
        bubbles: true,
        cancelable: true,
        key: key,
        keyCode: keycode(key),
        ...eventOptions,
      }),
    )

    fireEvent(
      target,
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

// https://github.com/testing-library/react-testing-library/issues/339
export function makeDragEvent(
  type: 'dragstart' | 'dragenter' | 'dragover' | 'dragleave' | 'dragend' | 'drop',
  target: Element | Node,
  clientCoords: { x: number; y: number },
  fileList?: Array<File>,
): Event {
  const opts = {
    clientX: clientCoords.x,
    clientY: clientCoords.y,
    buttons: 1,
    bubbles: true,
    cancelable: true,
  }

  let createEventForType: (node: Node, options: any) => Event = createEvent.drop
  switch (type) {
    case 'dragend':
      createEventForType = createEvent.dragLeave
      break
    case 'dragenter':
      createEventForType = createEvent.dragEnter
      break
    case 'dragover':
      createEventForType = createEvent.dragOver
      break
    case 'dragleave':
      createEventForType = createEvent.dragLeave
      break
    case 'dragstart':
      createEventForType = createEvent.dragStart
      break
    default:
    case 'drop':
      createEventForType = createEvent.drop
      break
  }

  const fileDropEvent: Event = createEventForType(target, opts)

  if (fileList != null) {
    Object.defineProperty(fileDropEvent, 'dataTransfer', {
      value: {
        getData: () => '',
        items: fileList.map((f) => ({ kind: 'file', getAsFile: () => f })),
        files: {
          item: (itemIndex: number) => fileList[itemIndex],
          length: fileList.length,
        },
        types: ['Files'],
      },
    })
  }

  return fileDropEvent
}

export function dragElementToPoint(
  eventSourceElement: HTMLElement | null,
  targetElement: HTMLElement,
  startPoint: Point,
  endPoint: Point,
  fileList: Array<File>,
) {
  if (eventSourceElement != null) {
    act(() => {
      fireEvent(
        eventSourceElement,
        makeDragEvent('dragstart', eventSourceElement, startPoint, fileList),
      )
    })
  }
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragenter', targetElement, { x: 0, y: 0 }, fileList))
  })
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragover', targetElement, { x: 0, y: 0 }, fileList))
  })
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragover', targetElement, endPoint, fileList))
  })
}

export function dropElementAtPoint(
  targetElement: HTMLElement,
  endPoint: Point,
  fileList: Array<File>,
) {
  act(() => {
    fireEvent(targetElement, makeDragEvent('drop', targetElement, endPoint, fileList))
  })
}

export function switchDragAndDropElementTargets(
  startingElement: HTMLElement,
  targetElement: HTMLElement,
  startPoint: Point,
  endPoint: Point,
  fileList: Array<File>,
) {
  act(() => {
    fireEvent(startingElement, makeDragEvent('dragleave', startingElement, startPoint, fileList))
  })
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragenter', targetElement, { x: 0, y: 0 }, fileList))
  })
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragover', targetElement, { x: 0, y: 0 }, fileList))
  })
  act(() => {
    fireEvent(targetElement, makeDragEvent('dragover', targetElement, endPoint, fileList))
  })
}
