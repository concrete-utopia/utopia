/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import * as React from 'react'
import {
  jsxAttributeValue,
  JSXElement,
  jsxElement,
  jsxElementName,
  JSXAttributes,
  setJSXAttributesAttribute,
  jsxAttributesFromMap,
} from '../../core/shared/element-template'
import { getAllUniqueUids } from '../../core/model/element-template-utils'
import { generateUID } from '../../core/shared/uid-utils'
import {
  getUtopiaJSXComponentsFromSuccess,
  isModifiedFile,
} from '../../core/model/project-file-utils'
import { ErrorMessage } from '../../core/shared/error-messages'
import {
  isParseSuccess,
  ProjectFileType,
  importDetails,
  importAlias,
} from '../../core/shared/project-file-types'
import { ProjectContentTreeRoot, walkContentsTree } from '../assets'
import { setFocus } from '../common/actions'
import { CodeResultCache, isJavascriptOrTypescript } from '../custom-code/code-file'
import * as EditorActions from '../editor/actions/action-creators'
import {
  getAllCodeEditorErrors,
  getOpenFilename,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { FileBrowserItem } from './fileitem'
import { dropFileExtension } from '../../core/shared/file-utils'
import { objectMap } from '../../core/shared/object-utils'
import { defaultPropertiesForComponentInFile } from '../../core/property-controls/property-controls-utils'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { betterReactMemo, useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import {
  Section,
  SectionBodyArea,
  Button,
  SectionTitleRow,
  FlexRow,
  Title,
  ActionSheet,
  SquareButton,
  Icons,
} from '../../uuiui'
import { isLocal } from '../editor/persistence'

export type FileBrowserItemType = 'file' | 'export'

export interface FileBrowserItemInfo {
  path: string
  type: FileBrowserItemType
  fileType: ProjectFileType | null
  typeInformation?: string | null
  originatingPath: string
  modified: boolean
  hasErrorMessages: boolean
  exportedFunction: boolean
  isUploadedAssetFile: boolean
}

export function fileHasErrorMessages(path: string, errorMessages: ErrorMessage[] | null): boolean {
  if (errorMessages == null) {
    return false
  } else {
    return errorMessages.some((m) => m.fileName === path)
  }
}

function collectFileBrowserItems(
  projectContents: ProjectContentTreeRoot,
  collapsedPaths: string[],
  codeResultCache: CodeResultCache | null,
  errorMessages: ErrorMessage[] | null,
): FileBrowserItemInfo[] {
  let fileBrowserItems: FileBrowserItemInfo[] = []
  walkContentsTree(projectContents, (fullPath, element) => {
    const originatingPath = fullPath

    const hasErrorMessages = fileHasErrorMessages(fullPath, errorMessages)

    if (collapsedPaths.every((collapsed) => !fullPath.startsWith(collapsed + '/'))) {
      fileBrowserItems.push({
        path: fullPath,
        type: 'file',
        fileType: element.type,
        originatingPath: originatingPath,
        hasErrorMessages: hasErrorMessages,
        modified: isModifiedFile(element),
        exportedFunction: false,
        isUploadedAssetFile:
          !isLocal() && (element.type === 'IMAGE_FILE' || element.type === 'ASSET_FILE'),
      })
      if (
        element.type === 'TEXT_FILE' &&
        isJavascriptOrTypescript(fullPath) &&
        codeResultCache != null
      ) {
        const codeResult = codeResultCache.cache[fullPath]
        if (codeResult != null) {
          Object.keys(codeResult.exports).forEach((exportedVar) => {
            if (exportedVar != 'default') {
              const typeInformation = codeResult.exports[exportedVar].type
              fileBrowserItems.push({
                path: `${fullPath}/${exportedVar}`,
                type: 'export',
                fileType: null,
                typeInformation: typeInformation,
                originatingPath: originatingPath,
                hasErrorMessages: false,
                modified: false,
                exportedFunction: typeInformation.includes('=>'),
                isUploadedAssetFile: false,
              })
            }
          })
        }
      }
    }
  })
  return fileBrowserItems
}

export const FileBrowser = betterReactMemo('FileBrowser', () => {
  const { dispatch, minimised, focusedPanel } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      minimised: store.editor.fileBrowser.minimised,
      focusedPanel: store.editor.focusedPanel,
    }
  }, 'FileBrowser')

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('filebrowser')], 'leftpane')
  }, [dispatch])

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'filebrowser') {
        dispatch([setFocus('filebrowser')])
      }
    },
    [dispatch, focusedPanel],
  )

  return (
    <>
      <Section data-name='FileBrowser' onFocus={onFocus} tabIndex={-1}>
        <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
          <FlexRow flexGrow={1} style={{ position: 'relative' }}>
            <Title>Project</Title>
            <FileBrowserActionSheet visible={!minimised} />
          </FlexRow>
        </SectionTitleRow>
        <SectionBodyArea minimised={minimised}>
          {minimised ? null : <FileBrowserItems />}
        </SectionBodyArea>
      </Section>
    </>
  )
})

interface FileBrowserActionSheetProps {
  visible: boolean
}

const FileBrowserItems = betterReactMemo('FileBrowserItems', () => {
  const {
    dispatch,
    projectContents,
    editorSelectedFile,
    errorMessages,
    codeResultCache,
    renamingTarget,
    dropTarget,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      projectContents: store.editor.projectContents,
      editorSelectedFile: getOpenFilename(store.editor),
      errorMessages: getAllCodeEditorErrors(store.editor, false, true),
      codeResultCache: store.editor.codeResultCache,
      propertyControlsInfo: store.editor.propertyControlsInfo,
      renamingTarget: store.editor.fileBrowser.renamingTarget,
      dropTarget: store.editor.fileBrowser.dropTarget,
    }
  }, 'FileBrowserItems')

  const [selectedPath, setSelectedPath] = React.useState(editorSelectedFile)

  const [collapsedPaths, setCollapsedPaths] = React.useState<string[]>([])

  const Expand = React.useCallback(
    (filePath: string) => {
      setCollapsedPaths(collapsedPaths.filter((path) => filePath !== path))
    },
    [collapsedPaths],
  )

  const toggleCollapse = React.useCallback(
    (filePath: string) => {
      if (collapsedPaths.indexOf(filePath) > -1) {
        setCollapsedPaths(collapsedPaths.filter((path) => filePath !== path))
      } else {
        setCollapsedPaths([...collapsedPaths, filePath])
      }
    },
    [collapsedPaths],
  )

  const setSelected = React.useCallback((item: FileBrowserItemInfo | null) => {
    if (item != null) {
      setSelectedPath(item.path)
    }
  }, [])

  const fileBrowserItems = React.useMemo(
    () => collectFileBrowserItems(projectContents, collapsedPaths, codeResultCache, errorMessages),
    [projectContents, collapsedPaths, codeResultCache, errorMessages],
  )

  return (
    <React.Fragment>
      {fileBrowserItems.map((element: FileBrowserItemInfo, index: number) => (
        <div
          key={element.path}
          style={{
            alignSelf: 'stretch',
          }}
        >
          <FileBrowserItem
            {...element}
            isSelected={element.path === selectedPath}
            renamingTarget={renamingTarget}
            key={`filebrowser-${index}`}
            dispatch={dispatch}
            toggleCollapse={toggleCollapse}
            Expand={Expand}
            setSelected={setSelected}
            collapsed={element.type === 'file' && collapsedPaths.indexOf(element.path) > -1}
            hasErrorMessages={fileHasErrorMessages(element.path, errorMessages)}
            dropTarget={dropTarget}
          />
        </div>
      ))}
    </React.Fragment>
  )
})

const FileBrowserActionSheet = betterReactMemo(
  'FileBrowserActionSheet',
  (props: FileBrowserActionSheetProps) => {
    const { dispatch } = useEditorState(
      (store) => ({ dispatch: store.dispatch }),
      'FileBrowserActionSheet dispatch',
    )
    const addFolderClick = React.useCallback(
      () => dispatch([EditorActions.addFolder('')], 'everyone'),
      [dispatch],
    )
    const addTextFileClick = React.useCallback(
      () => dispatch([EditorActions.addTextFile('', 'untitled')], 'everyone'),
      [dispatch],
    )
    if (props.visible) {
      return (
        <ActionSheet>
          <SquareButton highlight onClick={addFolderClick}>
            <Icons.NewFolder tooltipText='Add New Folder' />
          </SquareButton>
          <SquareButton highlight onClick={addTextFileClick}>
            <Icons.NewTextFile tooltipText='Add Code File' />
          </SquareButton>
        </ActionSheet>
      )
    } else {
      return null
    }
  },
)
