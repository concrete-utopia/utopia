import * as csstree from 'css-tree'
import { CanvasContainerID, SceneContainerName } from '../../components/canvas/canvas-types'

function scopePseudoClassSelector(): csstree.PseudoClassSelector {
  return csstree.fromPlainObject({
    type: 'PseudoClassSelector',
    name: 'scope',
    children: null,
  }) as csstree.PseudoClassSelector
}

function isSelectorToChange(node: csstree.CssNode): boolean {
  switch (node.type) {
    case 'TypeSelector':
      return ['body', 'html', 'head'].includes(node.name)
    case 'PseudoClassSelector':
      return node.name === 'root'
    default:
      return false
  }
}

export function convertCssToUtopia(input: string): string {
  // First wrap it in an @scope
  const scopedInput = `@scope (#${CanvasContainerID}) {
    ${input}
  }`

  let ast = csstree.parse(scopedInput)

  csstree.walk(ast, function (node, item, list) {
    rescopeCSSToTargetCanvasOnly(node, item, list, this.rule)
    changeMediaQueryToContainer(node, SceneContainerName)
  })

  return csstree.generate(ast)
}

function rescopeCSSToTargetCanvasOnly(
  node: csstree.CssNode,
  item: any,
  list: any,
  rule: csstree.Rule | null,
) {
  if (isSelectorToChange(node) && list != null) {
    list.insertData(scopePseudoClassSelector(), item)
    list.remove(item)
    // we need to remove dimensions since they now apply to our canvas
    if (rule != null) {
      removeDimensionsFromCssRule(rule)
    }
  }
}
const WIDTH_CONDITIONS_REGEX = /\([^()]*?\b(min-width|max-width|width)\b[^()]*?\)/g
export function changeMediaQueryToContainer(node: csstree.CssNode, containerName?: string) {
  if (node.type === 'Atrule' && node.name === 'media') {
    const queryText = csstree.generate(node.prelude as csstree.CssNode)
    const widthConditions = queryText.match(WIDTH_CONDITIONS_REGEX) ?? []

    if (widthConditions.length > 0) {
      // Get non-width conditions
      const nonWidthConditions = queryText
        .split(/\s+and\s+/)
        .filter((condition) => condition.match(WIDTH_CONDITIONS_REGEX) == null)
        .join(' and ')
        .trim()

      if (nonWidthConditions != '') {
        // Keep the media query and nest a container query inside
        const containerQuery = csstree.parse(
          `@container ${containerName ?? ''} ${widthConditions.join(' and ')}
                ${csstree.generate(node.block as csstree.CssNode)}
            `,
          { context: 'stylesheet' },
        )

        node.prelude = csstree.parse(nonWidthConditions, {
          context: 'mediaQueryList',
        }) as csstree.AtrulePrelude
        // Create a new block containing the container query
        node.block = csstree.parse(
          `{
         ${csstree.generate(containerQuery)}
          }`,
          { context: 'block' },
        ) as csstree.Block
      } else {
        // If there are only width conditions, convert directly to container query
        node.name = 'container'
        node.prelude = csstree.parse(`${containerName ?? ''} ${widthConditions.join(' and ')}`, {
          context: 'mediaQueryList',
        }) as csstree.AtrulePrelude
      }
    }
  }
}

const propertiesToRemove = ['width', 'height', 'max-width', 'max-height', 'min-width', 'min-height']
function removeDimensionsFromCssRule(rule: csstree.Rule): void {
  csstree.walk(rule, (node, item, list) => {
    if (node.type === 'Declaration' && list != null) {
      if (propertiesToRemove.includes(node.property)) {
        list.remove(item)
      }
    }
  })
}
