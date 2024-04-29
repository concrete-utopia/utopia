/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { isModifiedFile } from '../../core/model/project-file-utils'
import type { ErrorMessage } from '../../core/shared/error-messages'
import type { ProjectFileType, ImageFile } from '../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../assets'
import { walkContentsTree } from '../assets'
import { setFocus } from '../common/actions'
import type { CodeResultCache } from '../custom-code/code-file'
import { isJavascriptOrTypescript } from '../custom-code/code-file'
import * as EditorActions from '../editor/actions/action-creators'
import type { GithubRepo } from '../editor/store/editor-state'
import { getAllCodeEditorErrors, getOpenFilename } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { addingChildElement, FileBrowserItem } from './fileitem'
import {
  Section,
  SectionBodyArea,
  SectionTitleRow,
  FlexRow,
  Title,
  ActionSheet,
  SquareButton,
  Icons,
} from '../../uuiui'
import { unless } from '../../utils/react-conditionals'
import type { AddingFile } from './filepath-utils'
import { applyAddingFile } from './filepath-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import type {
  Conflict,
  GithubFileChanges,
  GithubFileStatus,
  TreeConflicts,
} from '../../core/shared/github/helpers'
import { useGithubFileChanges } from '../../core/shared/github/helpers'
import { useDispatch } from '../editor/store/dispatch-context'

export type FileBrowserItemType = 'file' | 'export'

export interface FileBrowserItemInfo {
  path: string
  type: FileBrowserItemType
  fileType: ProjectFileType | null
  typeInformation?: string | null
  originatingPath: string
  modified: boolean
  errorMessages: ErrorMessage[]
  exportedFunction: boolean
  isUploadedAssetFile: boolean
  imageFile: ImageFile | null
  projectContents: ProjectContentTreeRoot
  githubStatus?: GithubFileStatus
  conflict: Conflict | null
  githubRepo: GithubRepo | null
  projectID: string | null
}

export function filterErrorMessages(
  path: string,
  errorMessages: ErrorMessage[] | null,
): ErrorMessage[] {
  if (errorMessages == null) {
    return []
  } else {
    return errorMessages.filter((m) => m.fileName === path)
  }
}

function collectFileBrowserItems(
  projectContents: ProjectContentTreeRoot,
  collapsedPaths: string[],
  codeResultCache: CodeResultCache | null,
  errorMessages: ErrorMessage[] | null,
  githubChanges: GithubFileChanges | null,
  projectID: string | null,
  conflicts: TreeConflicts,
  githubRepo: GithubRepo | null,
): FileBrowserItemInfo[] {
  const getGithubStatus = (filename: string) => {
    if (githubChanges?.untracked.includes(filename)) {
      return 'untracked'
    }
    if (githubChanges?.modified.includes(filename)) {
      return 'modified'
    }
    return undefined
  }

  let fileBrowserItems: FileBrowserItemInfo[] = []
  walkContentsTree(projectContents, (fullPath, element) => {
    const originatingPath = fullPath

    if (collapsedPaths.every((collapsed) => !fullPath.startsWith(collapsed + '/'))) {
      fileBrowserItems.push({
        path: fullPath,
        type: 'file',
        fileType: element.type,
        originatingPath: originatingPath,
        errorMessages: filterErrorMessages(fullPath, errorMessages),
        modified: isModifiedFile(element),
        exportedFunction: false,
        isUploadedAssetFile:
          (element.type === 'IMAGE_FILE' || element.type === 'ASSET_FILE') &&
          element.base64 == undefined,
        imageFile: element.type === 'IMAGE_FILE' ? element : null,
        projectContents: projectContents,
        githubStatus: getGithubStatus(fullPath),
        githubRepo: githubRepo,
        conflict: conflicts[fullPath] ?? null,
        projectID: projectID,
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
                errorMessages: [],
                modified: false,
                exportedFunction: typeInformation.includes('=>'),
                isUploadedAssetFile: false,
                imageFile: null,
                projectContents: projectContents,
                githubRepo: githubRepo,
                conflict: conflicts[fullPath] ?? null,
                projectID: projectID,
              })
            }
          })
        }
      }
    }
  })
  return fileBrowserItems
}

export const FileBrowser = React.memo(() => {
  const dispatch = useDispatch()
  const { minimised, focusedPanel } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        minimised: store.editor.fileBrowser.minimised,
        focusedPanel: store.editor.focusedPanel,
      }
    },
    'FileBrowser',
  )

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

  const [addingFile, setAddingFile] = React.useState<AddingFile | null>(null)

  const confirmAddingFile = React.useCallback(() => {
    applyAddingFile(dispatch, '', addingFile)
    setAddingFile(null)
  }, [dispatch, addingFile])

  const abandonAddingFile = React.useCallback(() => {
    setAddingFile(null)
  }, [])

  const inputLabelKeyDown = React.useCallback(
    (event: React.KeyboardEvent<HTMLInputElement>) => {
      if (event.key === 'Enter') {
        confirmAddingFile()
      }
      if (event.key === 'Escape') {
        abandonAddingFile()
      }
    },
    [confirmAddingFile, abandonAddingFile],
  )

  const inputLabelChange = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      if (addingFile != null) {
        setAddingFile({
          fileOrFolder: addingFile.fileOrFolder,
          filename: event.target.value,
        })
      }
    },
    [addingFile],
  )

  const setAddingFileOrFolder = React.useCallback((fileOrFolder: AddingFile['fileOrFolder']) => {
    setAddingFile({ fileOrFolder: fileOrFolder, filename: '' })
  }, [])

  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.projectName
    },
    'TitleBar projectName',
  )

  return (
    <>
      <Section data-name='FileBrowser' onFocus={onFocus} tabIndex={-1}>
        <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
          <FlexRow flexGrow={1} style={{ position: 'relative' }}>
            <span>(root)</span>
            <FileBrowserActionSheet
              visible={!minimised}
              setAddingFileOrFolder={setAddingFileOrFolder}
            />
          </FlexRow>
        </SectionTitleRow>
        <SectionBodyArea minimised={minimised}>
          {unless(minimised, () => (
            <>
              {addingFile == null
                ? null
                : addingChildElement(
                    0,
                    addingFile.filename,
                    inputLabelKeyDown,
                    inputLabelChange,
                    abandonAddingFile,
                  )}
              <FileBrowserItems />
            </>
          ))}
        </SectionBodyArea>
      </Section>
    </>
  )
})

interface FileBrowserActionSheetProps {
  visible: boolean
  setAddingFileOrFolder: (fileOrFolder: 'file' | 'folder') => void
}

const FileBrowserItems = React.memo(() => {
  const dispatch = useDispatch()
  const { errorMessages, codeResultCache, renamingTarget, dropTarget, projectID } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        errorMessages: getAllCodeEditorErrors(store.editor.codeEditorErrors, 'warning', true),
        codeResultCache: store.editor.codeResultCache,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        renamingTarget: store.editor.fileBrowser.renamingTarget,
        dropTarget: store.editor.fileBrowser.dropTarget,
        projectID: store.editor.id,
      }
    },
    'FileBrowserItems',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'FileBrowserItems projectContents',
  )

  const { githubRepo, conflicts } = useEditorState(
    Substores.github,
    (store) => {
      return {
        githubRepo: store.editor.githubSettings.targetRepository,
        conflicts: store.editor.githubData.treeConflicts,
      }
    },
    'FileBrowserItems github',
  )

  const editorSelectedFile = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'FileBrowserItems editorSelectedFile',
  )

  const [selectedPath, setSelectedPath] = React.useState(editorSelectedFile)

  const [collapsedPaths, setCollapsedPaths] = React.useState<string[]>([])

  const expand = React.useCallback(
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

  const githubFileChanges = useGithubFileChanges()

  const fileBrowserItems = React.useMemo(() => {
    return collectFileBrowserItems(
      projectContents,
      collapsedPaths,
      codeResultCache,
      errorMessages,
      githubFileChanges,
      projectID,
      conflicts,
      githubRepo,
    )
  }, [
    projectContents,
    collapsedPaths,
    codeResultCache,
    errorMessages,
    githubFileChanges,
    projectID,
    conflicts,
    githubRepo,
  ])

  const generateNewUid = React.useCallback(
    () => generateUidWithExistingComponents(projectContents),
    [projectContents],
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
            expand={expand}
            setSelected={setSelected}
            collapsed={element.type === 'file' && collapsedPaths.indexOf(element.path) > -1}
            errorMessages={filterErrorMessages(element.path, errorMessages)}
            dropTarget={dropTarget}
            imageFile={element.imageFile}
            generateNewUid={generateNewUid}
          />
        </div>
      ))}
    </React.Fragment>
  )
})

const FileBrowserActionSheet = React.memo((props: FileBrowserActionSheetProps) => {
  const dispatch = useDispatch()
  const addFolderClick = React.useCallback(() => props.setAddingFileOrFolder('folder'), [props])
  const addTextFileClick = React.useCallback(() => props.setAddingFileOrFolder('file'), [props])
  if (props.visible) {
    return (
      <ActionSheet>
        <SquareButton highlight onClick={addFolderClick} style={{ margin: 0 }}>
          <Icons.FolderClosed tooltipText='Add New Folder' />
        </SquareButton>
        <SquareButton highlight onClick={addTextFileClick} style={{ margin: 0 }}>
          <Icons.NewTextFile tooltipText='Add Code File' />
        </SquareButton>
      </ActionSheet>
    )
  } else {
    return null
  }
})
