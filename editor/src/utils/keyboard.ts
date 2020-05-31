import * as R from 'ramda'

export type Modifier = 'alt' | 'cmd' | 'ctrl' | 'shift'
export type Letter =
  | 'a'
  | 'b'
  | 'c'
  | 'd'
  | 'e'
  | 'f'
  | 'g'
  | 'h'
  | 'i'
  | 'j'
  | 'k'
  | 'l'
  | 'm'
  | 'n'
  | 'o'
  | 'p'
  | 'q'
  | 'r'
  | 's'
  | 't'
  | 'u'
  | 'v'
  | 'w'
  | 'x'
  | 'y'
  | 'z'
export type Digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
export type Bracket = '[' | ']'

export type KeyCharacter =
  | 'backspace'
  | 'comma'
  | 'delete'
  | 'down'
  | 'enter'
  | 'esc'
  | 'left'
  | 'minus'
  | 'right'
  | 'space'
  | 'period'
  | 'plus'
  | 'up'
  | 'tab'
  | 'undefined-character'
  | 'grave'
  | 'forwardslash'
  | 'backslash'
  | Modifier
  | Letter
  | Digit
  | Bracket

// please don't add more keycharacters here, use them directly from keyboard events
export const StoredKeyCharacters = ['alt', 'cmd', 'ctrl', 'shift', 'z']
export type StoredKeyCharacter = Modifier | 'z'
export type KeysPressed = { [key in StoredKeyCharacter]?: boolean }

export type Key = {
  character: KeyCharacter
  modifiers: Array<Modifier>
}

export enum KeyCode {
  BACKSPACE = 8,
  DELETE = 46,
}

export function modifiersForEvent(event: KeyboardEvent): Array<Modifier> {
  let modifiers: Array<Modifier> = []
  if (event.altKey) {
    modifiers.push('alt')
  }
  if (event.metaKey) {
    modifiers.push('cmd')
  } else if (event.key === 'Meta' || event.key === 'OS') {
    modifiers.push('cmd')
  }
  if (event.ctrlKey) {
    modifiers.push('ctrl')
  }
  if (event.shiftKey) {
    modifiers.push('shift')
  }
  return modifiers
}

function keyCharacterFromCode(keyCode: number): KeyCharacter {
  switch (true) {
    case keyCode === KeyCode.BACKSPACE:
      return 'backspace'
    case keyCode === 9:
      return 'tab'
    case keyCode === 13:
      return 'enter'
    case keyCode === 16:
      return 'shift'
    case keyCode === 17:
      return 'ctrl'
    case keyCode === 18:
      return 'alt'
    case keyCode === 27:
      return 'esc'
    case keyCode === 32:
      return 'space'
    case keyCode === 37:
      return 'left'
    case keyCode === 38:
      return 'up'
    case keyCode === 39:
      return 'right'
    case keyCode === 40:
      return 'down'
    case keyCode === KeyCode.DELETE:
      return 'delete'
    case keyCode === 91:
    case keyCode === 93:
      return 'cmd' // Or "Windows" key.
    case keyCode === 187:
      return 'plus'
    case keyCode === 188:
      return 'comma'
    case keyCode === 189:
      return 'minus'
    case keyCode === 190:
      return 'period'
    case keyCode === 191:
      return 'forwardslash'
    case keyCode === 192:
      return 'grave'
    case keyCode === 219:
      return '['
    case keyCode === 220:
      return 'backslash'
    case keyCode === 221:
      return ']'
    case keyCode >= 48 && keyCode <= 90:
      return String.fromCharCode(keyCode).toLowerCase() as Letter | Digit
    default:
      return 'undefined-character'
  }
}

export const Keyboard = {
  keyFromEvent: function (event: KeyboardEvent): Key {
    return {
      character: keyCharacterFromCode(event.keyCode).toLowerCase() as KeyCharacter,
      modifiers: modifiersForEvent(event),
    }
  },
  key: function (character: KeyCharacter, modifiers: Array<Modifier> = []): Key {
    return {
      character: character,
      modifiers: modifiers,
    }
  },
  areSameKey: function (key1: Key, key2: Key): boolean {
    return (
      key1.character === key2.character && R.equals(key1.modifiers.sort(), key2.modifiers.sort())
    )
  },
  keyCharacterForCode: keyCharacterFromCode,
  keyIsModifier: function (key: KeyCharacter): boolean {
    return R.any(
      (char) => {
        return char === key
      },
      ['alt', 'cmd', 'ctrl', 'shift'],
    )
  },
  keyTriggersScroll: function (key: KeyCharacter, keysPressed: KeysPressed): boolean {
    return (
      R.any(
        (char) => {
          return char === key
        },
        ['up', 'down', 'left', 'right', 'space'],
      ) ||
      (keysPressed['ctrl'] && (key === 'p' || key === 'n')) ||
      false
    )
  },
}

// looseCheckModifier returns true if the modifiers array contain the expectedModifier value
export function looseCheckModifier(
  modifiers: Array<Modifier>,
  expectedModifier: Modifier,
): boolean {
  return modifiers.indexOf(expectedModifier) > -1
}

// strictCheckModifiers returns true if the modifiers and expectedModifiers are contain exactly
// the same values
export function strictCheckModifiers(
  modifiers: Array<Modifier>,
  expectedModifiers: Array<Modifier>,
): boolean {
  return (
    modifiers.length == expectedModifiers.length &&
    expectedModifiers.every((m) => looseCheckModifier(modifiers, m))
  )
}

export default Keyboard
