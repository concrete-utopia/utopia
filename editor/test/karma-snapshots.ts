const prettyFormat = require('pretty-format')

// using a helper function from jest to work with the same snapshots in browser and unit tests
// https://github.com/facebook/jest/blob/341843f41e5d3be8b8b4db68e651070551a86c56/packages/jest-snapshot/src/index.ts#L73
const INDENTATION_REGEX = /^([^\S\n]*)\S/m
function stripAddedIndentation(inlineSnapshot: string) {
  // Find indentation if exists.
  const match = inlineSnapshot.match(INDENTATION_REGEX)
  if (match == null || match[1] == null) {
    // No indentation.
    return inlineSnapshot
  }

  const indentation = match[1]
  const lines = inlineSnapshot.split('\n')
  if (lines.length <= 2) {
    // Must be at least 3 lines.
    return inlineSnapshot
  }

  const firstLine = lines[0]
  const lastLine = lines[lines.length - 1]
  if (
    firstLine === undefined ||
    lastLine === undefined ||
    firstLine.trim() !== '' ||
    lastLine.trim() !== ''
  ) {
    // If not blank first and last lines, abort.
    return inlineSnapshot
  }

  for (let i = 1; i < lines.length - 1; i++) {
    const line = lines[i]
    if (line !== undefined && line !== '') {
      if (line.indexOf(indentation) !== 0) {
        // All lines except first and last should either be blank or have the same
        // indent as the first line (or more). If this isn't the case we don't
        // want to touch the snapshot at all.
        return inlineSnapshot
      }

      lines[i] = line.substr(indentation.length)
    }
  }

  // Last line is a special case because it won't have the same indent as others
  // but may still have been given some indent to line up.
  lines[lines.length - 1] = ''

  // Return inline snapshot, now at indent 0.
  const updatedInlineSnapshot = lines.join('\n')
  return updatedInlineSnapshot
}

// using a helper function from jest to work with the same snapshots in browser and unit tests
// https://github.com/facebook/jest/blob/341843f41e5d3be8b8b4db68e651070551a86c56/packages/jest-snapshot/src/utils.ts#L136
function removeExtraLineBreaks(string: string) {
  return string.length > 2 && string.startsWith('\n') && string.endsWith('\n')
    ? string.slice(1, -1)
    : string
}

export function printSnapshotformat(received: unknown): string {
  return removeExtraLineBreaks(
    prettyFormat(received, {
      escapeRegex: true,
      indent: 2,
      printFunctionName: false,
    }),
  )
}

export function matchInlineSnapshotBrowser(received: unknown, inlineSnapshot: string): void {
  const resultSnapshot = printSnapshotformat(received)
  const trimmedMultilineSnapshot = removeExtraLineBreaks(stripAddedIndentation(inlineSnapshot))
  expect(resultSnapshot).toEqual(trimmedMultilineSnapshot)
}
