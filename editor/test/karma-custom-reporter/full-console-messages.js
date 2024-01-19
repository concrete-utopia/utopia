/* eslint-disable */
var Levels = ['error', 'warn', 'info', 'debug', 'log']
var FullConsoleMessages = function (baseReporterDecorator, formatError, config) {
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

  function printCleanConsoleMessages(consoleMessages) {
    var messagesFiltered = []
    consoleMessages.forEach((message) => {
      self.write(
        `${message.timestamp.toISOString()} ${message.type.toUpperCase()} ${message.log}\n`,
      )
      self.write(`this console message is from: \n  ${message.specName}\n`)
      self.write(`\n`)
    })
  }

  self.onBrowserLog = function (browser, log, type) {
    consoleMessagesForSpec.push({
      timestamp: new Date(),
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

FullConsoleMessages.$inject = ['baseReporterDecorator', 'formatError', 'config']

module.exports = {
  'reporter:full-console-messages': ['type', FullConsoleMessages],
}
