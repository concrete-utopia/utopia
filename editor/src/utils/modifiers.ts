export type Modifiers = {
  alt: boolean
  cmd: boolean
  ctrl: boolean
  shift: boolean
}

export const Modifier = {
  modifiersForKeyboardEvent: function (event: KeyboardEvent): Modifiers {
    let result: Modifiers = { ...Modifier.modifiersForEvent(event) }
    if (event.key === 'Meta' || event.key === 'OS') {
      if (event.type === 'keydown') {
        result.cmd = true
      } else if (event.type === 'keyup') {
        result.cmd = false
      }
    }
    return result
  },
  modifiersForEvent: function (event: {
    altKey: boolean
    metaKey: boolean
    ctrlKey: boolean
    shiftKey: boolean
  }): Modifiers {
    return {
      alt: event.altKey,
      cmd: event.metaKey,
      ctrl: event.ctrlKey,
      shift: event.shiftKey,
    }
  },
  none: function (modifiers: Modifiers): boolean {
    return !(modifiers.alt || modifiers.cmd || modifiers.ctrl || modifiers.shift)
  },
}
