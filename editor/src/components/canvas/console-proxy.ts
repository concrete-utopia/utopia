import * as React from 'react'
import Utils from '../../utils/utils'
import Parse from 'console-feed/lib/Hook/parse'
import { ConsoleLog } from '../editor/store/editor-state'

const ConsoleMethodsToProxy: Array<string> = [
  'log',
  'debug',
  'info',
  'warn',
  'error',
  'table',
  'clear',
  'time',
  'timeEnd',
  'count',
  'assert',
]

const SuppressedReactMessages = [
  'Warning: componentWillReceiveProps has been renamed',
  'Warning: useLayoutEffect does nothing on the server',
  'inside a test was not wrapped in act',
]

export function proxyConsole(
  targetConsole: Console,
  addToConsoleLogs: (log: ConsoleLog) => void,
): void {
  // Remove any existing proxy first.
  removeConsoleProxy(targetConsole)

  // Setup the console ready for our nefarious overriding purposes.
  const targetConsoleAny = targetConsole as any
  const originalMethods: { [key: string]: any } = {}
  targetConsoleAny.originalConsoleMethods = originalMethods

  Utils.fastForEach(ConsoleMethodsToProxy, (consoleMethodName) => {
    // Squirrel away the original method for unpacking later.
    const originalMethod = targetConsoleAny[consoleMethodName]
    originalMethods[consoleMethodName] = originalMethod
    targetConsoleAny[consoleMethodName] = function (...args: Array<any>) {
      if (
        SuppressedReactMessages.some(
          (suppressed) => typeof args[0] === 'string' && args[0].includes(suppressed),
        )
      ) {
        // PRINT NOTHING, WE SUPPRESS THIS MESSAGE
        return
      }
      // Call the original method first.
      originalMethod(...args)
      // Invoke our dispatcher.
      const parsed = Parse(consoleMethodName, args)
      if (parsed != false) {
        addToConsoleLogs(parsed)
      }
    }
  })
}

export function removeConsoleProxy(targetConsole: Console): void {
  const targetConsoleAny = targetConsole as any
  if (targetConsoleAny.originalConsoleMethods != null) {
    // Access the original methods.
    const originalMethods: { [key: string]: any } = targetConsoleAny.originalConsoleMethods
    // Restore the original methods.
    Utils.fastForEach(Object.keys(originalMethods), (originalMethodName) => {
      targetConsoleAny[originalMethodName] = originalMethods[originalMethodName]
    })
    // Remove this field, thereby restoring this console to its original state.
    delete targetConsoleAny['originalConsoleMethods']
  }
}
