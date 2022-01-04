/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
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
import { addingChildElement, FileBrowserItem } from './fileitem'
import { dropFileExtension } from '../../core/shared/file-utils'
import { objectMap } from '../../core/shared/object-utils'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
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
import { unless, when } from '../../utils/react-conditionals'
import { AddingFile, applyAddingFile } from './filepath-utils'

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
): FileBrowserItemInfo[] {
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

  return (
    <>
      <Section data-name='FileBrowser' onFocus={onFocus} tabIndex={-1}>
        <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
          <FlexRow flexGrow={1} style={{ position: 'relative' }}>
            <Title>Project</Title>
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
      errorMessages: getAllCodeEditorErrors(store.editor, 'warning', true),
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
            expand={Expand}
            setSelected={setSelected}
            collapsed={element.type === 'file' && collapsedPaths.indexOf(element.path) > -1}
            errorMessages={filterErrorMessages(element.path, errorMessages)}
            dropTarget={dropTarget}
          />
        </div>
      ))}
    </React.Fragment>
  )
})

const FileBrowserActionSheet = React.memo((props: FileBrowserActionSheetProps) => {
  const { dispatch } = useEditorState(
    (store) => ({ dispatch: store.dispatch }),
    'FileBrowserActionSheet dispatch',
  )
  const addFolderClick = React.useCallback(() => props.setAddingFileOrFolder('folder'), [props])
  const addTextFileClick = React.useCallback(() => props.setAddingFileOrFolder('file'), [props])
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
})
