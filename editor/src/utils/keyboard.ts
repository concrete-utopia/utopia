import Utils from '../utils/utils'

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

const Digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] as const
export type Digit = (typeof Digits)[number]

export const isDigit = (c: string): c is Digit => (Digits as readonly string[]).includes(c)

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

let modifiersCache: { [key: string]: ReadonlyArray<Modifier> } = {}

function modifiersToString(modifiers: ReadonlyArray<Modifier>): string {
  const uniqueAndSortedModifiers = Array.from(new Set(modifiers)).sort()
  return uniqueAndSortedModifiers.join('-')
}

function getCachedModifier(modifiers: ReadonlyArray<Modifier>): ReadonlyArray<Modifier> {
  const uniqueAndSortedModifiers = Array.from(new Set(modifiers)).sort()
  const cacheKey = modifiersToString(uniqueAndSortedModifiers)
  if (cacheKey in modifiersCache) {
    // Provably exists because of the `in` check.
    return modifiersCache[cacheKey]!
  } else {
    modifiersCache[cacheKey] = uniqueAndSortedModifiers
    return uniqueAndSortedModifiers
  }
}

let keysCache: { [key: string]: Key } = {}

function keyPartsToString(
  character: KeyCharacter,
  modifiers: ReadonlyArray<Modifier>,
  keyDownOrUp: KeyDownOrUp,
): string {
  return `${modifiersToString(modifiers)}-${keyDownOrUp}-${character}`
}

function keyToString(key: Key): string {
  return keyPartsToString(key.character, key.modifiers, key.keyDownOrUp)
}

function getCachedKey(
  character: KeyCharacter,
  modifiers: ReadonlyArray<Modifier>,
  keyDownOrUp: KeyDownOrUp,
): Key {
  const cachedModifiers = getCachedModifier(modifiers)
  const cacheKey = keyPartsToString(character, modifiers, keyDownOrUp)
  if (cacheKey in keysCache) {
    // Provably exists because of the `in` check.
    return keysCache[cacheKey]!
  } else {
    const result: Key = {
      character: character,
      modifiers: cachedModifiers,
      keyDownOrUp: keyDownOrUp,
    }
    keysCache[cacheKey] = result
    return result
  }
}

// please don't add more keycharacters here, use them directly from keyboard events
export const StoredKeyCharacters = ['alt', 'cmd', 'ctrl', 'shift', 'z', 'space']
export type StoredKeyCharacter = Modifier | 'z' | 'space'
export type KeysPressed = { [key in StoredKeyCharacter]?: boolean }

export type KeyDownOrUp = 'keydown' | 'keyup'

export interface Key {
  character: KeyCharacter
  modifiers: ReadonlyArray<Modifier>
  keyDownOrUp: KeyDownOrUp
}

export enum KeyCode {
  BACKSPACE = 8,
  DELETE = 46,
}

export function modifiersForEvent(event: KeyboardEvent): ReadonlyArray<Modifier> {
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
  return getCachedModifier(modifiers)
}

export function keyCharacterFromCode(keyCode: number): KeyCharacter {
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
  keyToString: keyToString,
  keyFromEvent: function (event: KeyboardEvent): Key {
    return getCachedKey(
      keyCharacterFromCode(event.keyCode).toLowerCase() as KeyCharacter,
      modifiersForEvent(event),
      event.type !== 'keyup' ? 'keydown' : 'keyup',
    )
  },
  key: function (
    character: KeyCharacter,
    modifiers: Modifier | Array<Modifier>,
    isKeyDown: KeyDownOrUp = 'keydown',
  ): Key {
    return getCachedKey(character, Array.isArray(modifiers) ? modifiers : [modifiers], isKeyDown)
  },
  modifiersEquals: function (
    first: ReadonlyArray<Modifier>,
    second: ReadonlyArray<Modifier>,
  ): boolean {
    if (first === second) {
      return true
    } else {
      let workingSet: Set<Modifier> = Utils.emptySet()
      for (let firstModifier of first) {
        workingSet.add(firstModifier)
      }
      for (let secondModifier of second) {
        if (workingSet.has(secondModifier)) {
          workingSet.delete(secondModifier)
        } else {
          return false
        }
      }
      return workingSet.size === 0
    }
  },
  areSameKey: function (first: Key, second: Key): boolean {
    if (first === second) {
      return true
    } else {
      return (
        first.character === second.character &&
        first.keyDownOrUp === second.keyDownOrUp &&
        Keyboard.modifiersEquals(first.modifiers, second.modifiers)
      )
    }
  },
  keyCharacterForCode: keyCharacterFromCode,
  keyIsModifier: function (keyChar: KeyCharacter): boolean {
    return ['alt', 'cmd', 'ctrl', 'shift'].some((char) => {
      return char === keyChar
    })
  },
  keyIsArrow: function (keyChar: KeyCharacter): boolean {
    switch (keyChar) {
      case 'left':
      case 'right':
      case 'up':
      case 'down':
        return true
      default:
        return false
    }
  },
  keyTriggersFontSizeStrategy: function (keyChar: KeyCharacter): boolean {
    switch (keyChar) {
      case 'period':
      case 'comma':
        return true
      default:
        return false
    }
  },
  keyTriggersFontWeightStrategy: function (keyChar: KeyCharacter): boolean {
    switch (keyChar) {
      case 'period':
      case 'comma':
        return true
      default:
        return false
    }
  },
  keyTriggersOpacityStrategy: function (keyChar: KeyCharacter): boolean {
    return isDigit(keyChar)
  },
  // This needs to be extended when we introduce new keys in canvas strategies
  keyIsInteraction: function (keyChar: KeyCharacter): boolean {
    return (
      this.keyIsArrow(keyChar) ||
      this.keyTriggersFontSizeStrategy(keyChar) ||
      this.keyTriggersFontWeightStrategy(keyChar) ||
      this.keyTriggersOpacityStrategy(keyChar)
    )
  },
  keyTriggersScroll: function (keyChar: KeyCharacter, keysPressed: KeysPressed): boolean {
    return (
      ['up', 'down', 'left', 'right', 'space'].some((char) => {
        return char === keyChar
      }) ||
      ('ctrl' in keysPressed && (keyChar === 'p' || keyChar === 'n'))
    )
  },
}

// looseCheckModifier returns true if the modifiers array contain the expectedModifier value
export function looseCheckModifier(
  modifiers: ReadonlyArray<Modifier>,
  expectedModifier: Modifier,
): boolean {
  return modifiers.indexOf(expectedModifier) > -1
}

// strictCheckModifiers returns true if the modifiers and expectedModifiers are contain exactly
// the same values
export function strictCheckModifiers(
  modifiers: ReadonlyArray<Modifier>,
  expectedModifiers: ReadonlyArray<Modifier>,
): boolean {
  return (
    modifiers.length == expectedModifiers.length &&
    expectedModifiers.every((m) => looseCheckModifier(modifiers, m))
  )
}

export default Keyboard
