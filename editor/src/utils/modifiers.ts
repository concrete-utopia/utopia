import { optionalDeepFreeze } from './deep-freeze'

export interface Modifiers {
  alt: boolean
  cmd: boolean
  ctrl: boolean
  shift: boolean
}

export const emptyModifiers: Modifiers = optionalDeepFreeze({
  alt: false,
  cmd: false,
  ctrl: false,
  shift: false,
})

export const ctrlModifier: Modifiers = {
  alt: false,
  cmd: false,
  ctrl: true,
  shift: false,
}

export const shiftModifier: Modifiers = {
  alt: false,
  cmd: false,
  ctrl: false,
  shift: true,
}

export const cmdModifier: Modifiers = {
  alt: false,
  cmd: true,
  ctrl: false,
  shift: false,
}

export const altModifier: Modifiers = {
  alt: true,
  cmd: false,
  ctrl: false,
  shift: false,
}

export const shiftCmdModifier: Modifiers = {
  alt: false,
  cmd: true,
  ctrl: false,
  shift: true,
}

export const altCmdModifier: Modifiers = { shift: false, cmd: true, alt: true, ctrl: false }

export const altShiftModifier: Modifiers = {
  alt: true,
  cmd: false,
  ctrl: false,
  shift: true,
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
  equal: function (first: Modifiers, second: Modifiers): boolean {
    if (first === second) {
      return true
    } else {
      return (
        first.shift === second.shift &&
        first.ctrl === second.ctrl &&
        first.cmd === second.cmd &&
        first.alt === second.alt
      )
    }
  },
}
