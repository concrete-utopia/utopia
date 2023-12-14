import * as csstree from 'css-tree'
import { CanvasContainerID } from '../../components/canvas/canvas-types'

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

export function rescopeCSSToTargetCanvasOnly(input: string): string {
  // First wrap it in an @scope
  const scopedInput = `@scope (#${CanvasContainerID}) {
    ${input}
  }`

  let ast = csstree.parse(scopedInput)

  csstree.walk(ast, (node, item, list) => {
    // As we are wrapping in an @scope, we need to redirect certain selectors to :scope
    if (isSelectorToChange(node) && list != null) {
      list.insertData(scopePseudoClassSelector(), item)
      list.remove(item)
    }
  })

  return csstree.generate(ast)
}
