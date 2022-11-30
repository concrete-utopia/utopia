import React from 'react'
import {
  ArbitraryJSBlock,
  getJSXElementNameAsString,
  isJSXElement,
  JSXElement,
  TopLevelElement,
} from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ParseSuccess, ParsedTextFile } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { ProjectContentTreeRoot } from '../assets'

interface CodeOutlineEntryBase {
  depth: number
  key: string
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
      half: 'open' | 'close'
      name: string
    })

function jsxElementToModel(
  depth: number,
  key: string,
  jsxElement: JSXElement,
): Array<CodeOutlineEntryModel> {
  if (jsxElement.children.length === 0) {
    return [
      {
        type: 'jsxTag',
        depth: depth + 1,
        half: 'close',
        key: key + jsxElement.uid,
        name: getJSXElementNameAsString(jsxElement.name),
      },
    ]
  }

  return [
    {
      type: 'jsxTag',
      depth: depth + 1,
      key: key + jsxElement.uid + 'open',
      half: 'open',
      name: getJSXElementNameAsString(jsxElement.name),
    },
    ...jsxElement.children.flatMap((c) =>
      isJSXElement(c) ? jsxElementToModel(depth + 1, key + jsxElement.uid, c) : [],
    ),
    {
      type: 'jsxTag',
      depth: depth + 1,
      key: key + jsxElement.uid + 'close',
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
  return [
    { type: 'placeholder', depth: depth, key: key + block.uniqueID, value: 'Elements within' },
    ...Object.entries(block.elementsWithin).flatMap(([k, e]) =>
      jsxElementToModel(depth + 1, key + k, e),
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
    return []
    // return [{ type: 'placeholder', depth: depth, key: key + 'unparsed', value: '<<Unparsed>>' }]
  }

  if (topLevelElement.type === 'ARBITRARY_JS_BLOCK') {
    return arbitraryJsBlockModel(depth + 1, key, topLevelElement)
  }

  const name = topLevelElement.name ?? 'name'
  return [
    { type: 'component', depth: depth, key: key + name, name: name },
    ...(isJSXElement(topLevelElement.rootElement)
      ? jsxElementToModel(depth + 1, key + topLevelElement + name, topLevelElement.rootElement)
      : []),
  ]
}

function codeFileModel(
  depth: number,
  key: string,
  success: ParseSuccess,
): Array<CodeOutlineEntryModel> {
  return [
    { type: 'placeholder', depth: depth, key: key + '-imports', value: '<<Imports>>' }, // placeholder for success.imports
    {
      type: 'placeholder',
      depth: depth,
      key: key + '-topLevelElements',
      value: 'Top level elements:',
    },
    ...success.topLevelElements.flatMap((t) => topLevelElementToModel(depth + 1, key, t)),
    ...(optionalMap(
      (a): Array<CodeOutlineEntryModel> => [
        {
          type: 'placeholder',
          depth: depth,
          key: key + '-combinedTopLevelArbitraryBlock',
          value: 'Combined top-level arbitrary block:',
        },
        ...arbitraryJsBlockModel(depth + 1, key, a),
      ],
      success.combinedTopLevelArbitraryBlock,
    ) ?? []),
  ]
}

export function codeOutlineModel(
  depth: number,
  contents: ProjectContentTreeRoot,
): Array<CodeOutlineEntryModel> {
  return Object.entries(contents).flatMap(([key, branch]): Array<CodeOutlineEntryModel> => {
    if (branch.type === 'PROJECT_CONTENT_DIRECTORY') {
      return [
        { type: 'directory', name: branch.fullPath, key: key, depth: depth },
        ...codeOutlineModel(depth + 1, branch.children),
      ]
    }

    if (branch.content.type === 'ASSET_FILE') {
      return [{ type: 'assetFile', name: branch.fullPath, key: key, depth: depth }]
    }

    if (branch.content.type === 'IMAGE_FILE') {
      return [{ type: 'imageFile', name: branch.fullPath, key: key, depth: depth }]
    }

    if (branch.content.type === 'TEXT_FILE') {
      const parsed = parsedTextFile(branch.content.fileContents.parsed)
      return [
        {
          type: 'codeFile',
          name: branch.fullPath,
          depth: depth,
          key: key,
          parsed: parsed != null ? 'parsed' : 'unparsed',
        },
        ...(optionalMap((p) => codeFileModel(depth + 1, key, p), parsed) ?? []),
      ]
    }

    return []
  })
}

function renderCodeOutlineEntry(entry: CodeOutlineEntryModel) {
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
      return entry.half === 'open' ? `<${entry.name}>` : `</${entry.name}>`
    default:
      assertNever(entry)
  }
}

interface CodeOutlineViewProps {
  entry: CodeOutlineEntryModel
}

export const CodeOutlineView = React.memo<CodeOutlineViewProps>((props) => {
  return (
    <div
      style={{
        paddingLeft: props.entry.depth * 12,
      }}
    >
      {renderCodeOutlineEntry(props.entry)}
    </div>
  )
})

function parsedTextFile(file: ParsedTextFile): ParseSuccess | null {
  if (file.type === 'PARSE_SUCCESS') {
    return file
  }
  return null
}
