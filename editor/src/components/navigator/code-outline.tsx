import React from 'react'
import { Size } from 'react-virtualized-auto-sizer'
import {
  ArbitraryJSBlock,
  ElementsWithin,
  getJSXElementNameAsString,
  JSXAttributesPart,
  JSXElement,
  JSXElementChild,
  TopLevelElement,
} from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ParseSuccess, ParsedTextFile, ProjectContents } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { contentsToTree, ProjectContentTreeRoot, treeToContents } from '../assets'

interface CodeOutlineEntryBase {
  depth: number
  key: string
  path?: string
}

type CodeOutlineEntryModel =
  | (CodeOutlineEntryBase & { type: 'directory'; name: string })
  | (CodeOutlineEntryBase & { type: 'imageFile'; name: string })
  | (CodeOutlineEntryBase & { type: 'assetFile'; name: string })
  | (CodeOutlineEntryBase & {
      type: 'codeFile'
      name: string
      parsed: 'parsed' | 'unparsed'
    })
  | (CodeOutlineEntryBase & {
      type: 'placeholder'
      value: string
    })
  | (CodeOutlineEntryBase & {
      type: 'component'
      name: string
    })
  | (CodeOutlineEntryBase & {
      type: 'jsxElement'
      name: string
    })
  | (CodeOutlineEntryBase & {
      type: 'jsxTag'
      half: 'open' | 'close' | 'uni'
      name: string
    })
  | (CodeOutlineEntryBase & {
      type: 'text'
      content: string
    })

function jsxElementChildToModel(
  depth: number,
  key: string,
  child: JSXElementChild,
): Array<CodeOutlineEntryModel> {
  if (child.type === 'JSX_ELEMENT') {
    return jsxElementToModel(depth, key, child)
  }

  const fullKey = key + ':' + child.uniqueID

  if (child.type === 'JSX_FRAGMENT') {
    if (child.children.length === 0) {
      return [
        {
          type: 'text',
          depth: depth + 1,
          key: fullKey,
          content: '</>',
        },
      ]
    }

    return [
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: '<>',
      },
      ...child.children.flatMap((c) => jsxElementChildToModel(depth + 1, fullKey, c)),
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: '</>',
      },
    ]
  }

  if (child.type === 'JSX_TEXT_BLOCK') {
    return [
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: child.text,
      },
    ]
  }

  if (child.type === 'JSX_ARBITRARY_BLOCK') {
    return [
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: '{' + child.originalJavascript + '}',
      },
    ]
  }
  assertNever(child)
}

function jsxElementToModel(
  depth: number,
  key: string,
  jsxElement: JSXElement,
): Array<CodeOutlineEntryModel> {
  const fullKey = key + ':' + jsxElement.uid
  const elementsWithin: Array<CodeOutlineEntryModel> = jsxElement.props
    .map((p) => elementsWithinProp(p))
    .filter(notNull)
    .flatMap((p) =>
      Object.entries(p).map(([kkey, t]) => ({
        type: 'text',
        depth: depth + 2,
        key: fullKey + kkey,
        content: formatAsOpenCloseTag(getJSXElementNameAsString(t.name)),
      })),
    )

  if (jsxElement.children.length === 0 && elementsWithin.length === 0) {
    return [
      {
        type: 'jsxTag',
        depth: depth + 1,
        half: 'uni',
        key: fullKey,
        name: getJSXElementNameAsString(jsxElement.name),
      },
    ]
  }

  if (jsxElement.children.length === 0) {
    return [
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: '<' + getJSXElementNameAsString(jsxElement.name),
      },
      ...elementsWithin,
      {
        type: 'text',
        depth: depth + 1,
        key: fullKey,
        content: '/>',
      },
    ]
  }

  return [
    {
      type: 'jsxTag',
      depth: depth + 1,
      key: fullKey,
      half: 'open',
      name: getJSXElementNameAsString(jsxElement.name),
    },
    ...elementsWithin,
    ...jsxElement.children.flatMap((c) => jsxElementChildToModel(depth + 1, fullKey, c)),
    {
      type: 'jsxTag',
      depth: depth + 1,
      key: fullKey + 'close',
      half: 'close',
      name: getJSXElementNameAsString(jsxElement.name),
    },
  ]
}

function arbitraryJsBlockModel(
  depth: number,
  key: string,
  block: ArbitraryJSBlock,
): Array<CodeOutlineEntryModel> {
  if (Object.entries(block.elementsWithin).length === 0) {
    return []
  }

  return [
    ...Object.entries(block.elementsWithin).flatMap(([k, e]) =>
      jsxElementToModel(depth + 1, k + key, e),
    ),
  ]
}

function topLevelElementToModel(
  depth: number,
  key: string,
  topLevelElement: TopLevelElement,
): Array<CodeOutlineEntryModel> {
  if (topLevelElement.type === 'IMPORT_STATEMENT') {
    return []
  }

  if (topLevelElement.type === 'UNPARSED_CODE') {
    return [] // [{ type: 'placeholder', depth: depth, key: key + 'unparsed', value: '<<Unparsed>>' }]
  }

  if (topLevelElement.type === 'ARBITRARY_JS_BLOCK') {
    return arbitraryJsBlockModel(depth + 1, key, topLevelElement)
  }

  const name = topLevelElement.name ?? 'name'

  const fullName = key + ':' + name

  return [
    { type: 'jsxTag', half: 'open', depth: depth, key: fullName, name: name },
    ...jsxElementChildToModel(depth + 1, fullName, topLevelElement.rootElement),
    { type: 'jsxTag', half: 'close', depth: depth, key: fullName, name: name },
  ]
}

function codeFileToModel(
  depth: number,
  key: string,
  success: ParseSuccess,
): Array<CodeOutlineEntryModel> {
  return [
    ...success.topLevelElements.flatMap((t) =>
      topLevelElementToModel(depth + 1, key + 'topLevelElements', t).map((e) => ({
        ...e,
        path: (e.path ?? '') + e.key,
      })),
    ),
    ...(optionalMap(
      (a): Array<CodeOutlineEntryModel> => arbitraryJsBlockModel(depth + 1, key, a),
      success.combinedTopLevelArbitraryBlock,
    ) ?? []),
  ]
}

export function codeOutlineToModel(
  path: string,
  root: ProjectContentTreeRoot,
): Array<CodeOutlineEntryModel> {
  const contents = treeToContents(root)
  return Object.keys(contents).flatMap((key) => {
    const entry = contents[key]
    const depth = key.split('/').length
    switch (entry.type) {
      case 'ASSET_FILE':
        return [{ type: 'assetFile', name: key, key: key, depth: depth }]
      case 'DIRECTORY':
        if (key === path) {
          return []
        }
        const childrenKeys = Object.keys(contents).filter(
          (e) => e !== key && e.startsWith(key + '/'),
        )
        if (childrenKeys.length === 0) {
          return []
        }
        const childrenTree: ProjectContents = {}
        childrenKeys.forEach((k) => {
          childrenTree[k] = contents[k]
        })
        return [
          { type: 'directory', name: key, key: key, depth: depth },
          ...codeOutlineToModel(key, contentsToTree(childrenTree)),
        ]
      case 'IMAGE_FILE':
        return [{ type: 'imageFile', name: key, key: key, depth: depth }]
      case 'TEXT_FILE':
        const parsed = parsedTextFile(entry.fileContents.parsed)
        return [
          {
            type: 'codeFile',
            name: key,
            depth: depth,
            key: key,
            parsed: parsed != null ? 'parsed' : 'unparsed',
          },
          ...(optionalMap((p) => codeFileToModel(depth + 1, key, p), parsed) ?? []),
        ]
    }
  })
}

function renderCodeOutlineEntry(entry: CodeOutlineEntryModel, collapsed: boolean) {
  switch (entry.type) {
    case 'directory':
      return `Directory: ${entry.name}`
    case 'imageFile':
      return `Image: ${entry.name}`
    case 'assetFile':
      return `Asset: ${entry.name}`
    case 'codeFile':
      return `Code file: ${entry.name}`
    case 'placeholder':
      return entry.value
    case 'component':
      return `Component: ${entry.name}`
    case 'jsxElement':
      return `JSXElement: ${entry.name}`
    case 'jsxTag':
      if (entry.half === 'uni') {
        return `<${entry.name} />`
      }
      return entry.half === 'open' ? `<${entry.name}>` : `</${entry.name}>`
    case 'text':
      return entry.content
    default:
      assertNever(entry)
  }
}

interface CodeOutlineViewProps {
  entry: CodeOutlineEntryModel
  collapsed: boolean
}

export const CodeOutlineList = React.memo(
  ({ outlineModel, size }: { outlineModel: CodeOutlineEntryModel[]; size: Size }) => {
    const [collapsed, setCollapsed] = React.useState<Array<string>>([])

    const collapseEntry = React.useCallback(
      (entry: CodeOutlineEntryModel) => () => {
        if (entry.path == null) {
          return
        }
        if (collapsed.includes(entry.path)) {
          setCollapsed(collapsed.filter((c) => c !== entry.path))
        } else {
          setCollapsed(collapsed.concat(entry.path))
        }
      },
      [collapsed],
    )
    const isEntryOpen = (entry: CodeOutlineEntryModel) => {
      return !collapsed.some((c) => entry.path?.startsWith(c + ':'))
    }

    return (
      <div
        style={{
          width: '100%',
          height: size.height,
          overflowX: 'hidden',
        }}
      >
        {outlineModel.filter(isEntryOpen).map((entry, i) => (
          <div key={i} onClick={collapseEntry(entry)}>
            <CodeOutlineView
              entry={entry}
              collapsed={entry.path != null && collapsed.includes(entry.path)}
            />
          </div>
        ))}
      </div>
    )
  },
)

export const CodeOutlineView = React.memo<CodeOutlineViewProps>((props) => {
  return (
    <div
      style={{
        paddingLeft: props.entry.depth * 12,
      }}
    >
      {renderCodeOutlineEntry(props.entry, props.collapsed)}
    </div>
  )
})

function parsedTextFile(file: ParsedTextFile): ParseSuccess | null {
  if (file.type === 'PARSE_SUCCESS') {
    return file
  }
  return null
}

function elementsWithinProp(attribute: JSXAttributesPart): ElementsWithin | null {
  if (
    attribute.type === 'JSX_ATTRIBUTES_SPREAD' ||
    attribute.value.type !== 'ATTRIBUTE_OTHER_JAVASCRIPT'
  ) {
    return null
  }

  return attribute.value.elementsWithin
}

function notNull<T>(t: T | null): t is T {
  return t != null
}

function formatAsOpenCloseTag(name: string): string {
  return '<' + name + '/>'
}
