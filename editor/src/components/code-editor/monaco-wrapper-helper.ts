const EMPTY_ELEMENTS: string[] = [
  'area',
  'base',
  'br',
  'col',
  'embed',
  'hr',
  'img',
  'input',
  'keygen',
  'link',
  'menuitem',
  'meta',
  'param',
  'source',
  'track',
  'wbr',
]

// copied from https://github.com/microsoft/vscode/blob/58fe34bb77aae5b596559d6dfe3cb81772e1dfca/extensions/typescript-language-features/src/features/languageConfiguration.ts#L60
// Modifications:
// - the closing > has to be the last character
// - = is not allowed just before the closing > character
export const OPENING_TAG_REGEX = new RegExp(
  `<(?!(?:${EMPTY_ELEMENTS.join('|')}))([_:\\w][_:\\w\\-.\\d]*)(>|[^/<]*[^=/>]>)$`,
  'i',
)
