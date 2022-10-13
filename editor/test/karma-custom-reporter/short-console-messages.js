/* eslint-disable */
var Levels = ['error', 'warn', 'info', 'debug', 'log']
var ShortConsoleMessages = function (baseReporterDecorator, formatError, config) {
  baseReporterDecorator(this)
  var self = this
  /**
   * @type Array<{type: string, log: string }>
   */
  var consoleMessagesForSpec = []
  /**
   * @type Array<{ {type: string, log: string, specName: string}}>
   */
  var allConsoleMessages = []

  function shortenMessage(message) {
    var truncated = message.length > 200 ? `${message.substring(0, 200)}...` : message
    return truncated.replace(/[\r\n]+/g, '')
  }

  function printCleanConsoleMessages(consoleMessages) {
    var messagesFiltered = []
    consoleMessages.forEach((message) => {
      const shortenedMessage = shortenMessage(message.log)
      const alreadyAdded = messagesFiltered.findIndex(
        (filtered) => filtered.log === shortenedMessage,
      )
      if (alreadyAdded > -1) {
        messagesFiltered[alreadyAdded].count += 1
        if (!messagesFiltered[alreadyAdded].specNames.includes(message.specName)) {
          messagesFiltered[alreadyAdded].specNames.push(message.specName)
        }
      } else {
        messagesFiltered.push({
          type: message.type,
          log: shortenedMessage,
          count: 1,
          specNames: [message.specName],
        })
      }
    })
    var messagesGrouped = {}
    Levels.forEach((level) => {
      messagesGrouped[level] = []
    })
    messagesFiltered.forEach((message) => {
      messagesGrouped[message.type].push(message)
      if (!Levels.includes(message.type)) {
        Levels.push(message.type)
      }
    })
    Levels.forEach((l) => {
      const OnlyShowFirstN = 3
      messagesGrouped[l].forEach((message) => {
        self.write(`\n${l.toUpperCase()} (${message.count}) ${message.log}`)
        self.write(
          `\nthis console message is from: \n  ${message.specNames
            .slice(0, OnlyShowFirstN)
            .map((n) => `• ${n}`)
            .join('\n  ')}`,
        )
        if (message.count > OnlyShowFirstN) {
          self.write(`\n ...and ${message.count - OnlyShowFirstN} more\n`)
        } else {
          self.write(`\n`)
        }
      })
    })
    self.write(`\n`)
  }

  self.onBrowserLog = function (browser, log, type) {
    consoleMessagesForSpec.push({
      type: type,
      log: log,
    })
  }

  self.onRunComplete = function (browsers, results) {
    printCleanConsoleMessages(allConsoleMessages)
  }

  self.onSpecComplete = function (browsers, results) {
    const withSpecName = consoleMessagesForSpec.map((messageForSpec) => ({
      ...messageForSpec,
      specName: `${results.suite} ${results.description}`,
    }))
    allConsoleMessages.push(...withSpecName)
    consoleMessagesForSpec = []
  }
}

ShortConsoleMessages.$inject = ['baseReporterDecorator', 'formatError', 'config']

module.exports = {
  'reporter:short-console-messages': ['type', ShortConsoleMessages],
}
