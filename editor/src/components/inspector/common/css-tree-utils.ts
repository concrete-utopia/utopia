import * as csstree from 'css-tree'

export function parseCssTreeNodeValue(str: string): csstree.CssNode {
  return csstree.parse(str, { context: 'value' })
}

export function cssTreeNodeList(nodes: csstree.CssNode[]): csstree.List<csstree.CssNode> {
  let list = new csstree.List<csstree.CssNode>()
  nodes.forEach((node) => list.appendData(node))
  return list
}

export function cssTreeNodeValue(children: csstree.List<csstree.CssNode>): csstree.Value {
  return {
    type: 'Value',
    children: children,
  }
}
