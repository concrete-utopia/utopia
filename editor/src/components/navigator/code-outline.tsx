import React from 'react'
import { ProjectContentsTree, ProjectContentTreeRoot } from '../assets'

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
      return [
        {
          type: 'codeFile',
          name: branch.fullPath,
          depth: depth,
          key: key,
          parsed:
            branch.content.fileContents.parsed.type === 'PARSE_SUCCESS' ? 'parsed' : 'unparsed', // TODO: cleanup
        },
      ]
    }

    return []
  })
}

function renderCodeOutlineEntry(entry: CodeOutlineEntryModel) {
  switch (entry.type) {
    case 'directory':
      return `Dir: ${entry.name}`
    case 'imageFile':
      return `Image: ${entry.name}`
    case 'assetFile':
      return `Asset: ${entry.name}`
    case 'codeFile':
      return `Code: ${entry.name}`
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
