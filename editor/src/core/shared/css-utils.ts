import * as csstree from 'css-tree'
import { CanvasContainerID } from '../../components/canvas/canvas-types'

const SelectorTypes = ['ClassSelector', 'IdSelector', 'TypeSelector']

export function rescopeCSSToTargetCanvasOnly(input: string): string {
  let ast = csstree.parse(input)

  csstree.walk(ast, (node) => {
    // We want to find all selectors, and prepend '#canvas-container ' (i.e. the canvas-container
    // ID Selector and a ' ' combinator) so that they will only apply to descendents of the canvas
    if (node.type === 'Selector') {
      const firstChild = node.children.first
      const firstChildIsSelector = firstChild != null && SelectorTypes.includes(firstChild.type)
      if (firstChildIsSelector) {
        if (firstChild.type === 'TypeSelector' && firstChild.name === 'body') {
          // The closest analogy to the body here is a child of the canvas, so
          // we want to replace the 'body' selector with a '#canvas-container > *'
          firstChild.name = '*'

          node.children.prependData(
            csstree.fromPlainObject({
              type: 'Combinator',
              name: '>',
            }),
          )
        } else {
          node.children.prependData(
            csstree.fromPlainObject({
              type: 'Combinator',
              name: ' ',
            }),
          )
        }

        node.children.prependData(
          csstree.fromPlainObject({
            type: 'IdSelector',
            name: CanvasContainerID,
          }),
        )
      }
    }
  })

  return csstree.generate(ast)
}
