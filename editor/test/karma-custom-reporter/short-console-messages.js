/* eslint-disable */
var Levels = ['error', 'warn', 'info', 'debug', 'log']
var UtopiaReporter = function (baseReporterDecorator, formatError, config) {
  baseReporterDecorator(this)
  var self = this
  var consoleMessagesForSpec = [] // {type: string, log: string }
  var allConsoleMessages = [] // {type: string, log: string, specName: string}

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
      messagesGrouped[l].forEach((message) => {
        self.write(`\n${l.toUpperCase()} (${message.count}) ${message.log}`)
        self.write(`\nthis console message is from: \n  ${message.specNames.join('\n  ')}\n`)
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

UtopiaReporter.$inject = ['baseReporterDecorator', 'formatError', 'config']

module.exports = {
  'reporter:utopia': ['type', UtopiaReporter],
}
