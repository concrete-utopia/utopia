/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import {
  ActionSheet,
  FlexRow,
  Icons,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  SquareButton,
  Title,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import {
  jsxAttributeValue,
  JSXElement,
  jsxElement,
  jsxElementName,
  JSXAttributes,
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
  ProjectContents,
  ProjectFileType,
  importDetails,
  importAlias,
} from '../../core/shared/project-file-types'
import { contentsToTree, walkContentsTree } from '../assets'
import { setFocus } from '../common/actions'
import { CodeResultCache, isJavascriptOrTypescript } from '../custom-code/code-file'
import * as EditorActions from '../editor/actions/actions'
import {
  getAllCodeEditorErrors,
  getOpenFilename,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { useKeepReferenceEqualityIfPossible } from '../inspector/common/property-path-hooks'
import { FileBrowserItem } from './fileitem'
import { dropExtension } from '../../core/shared/string-utils'
import { objectMap } from '../../core/shared/object-utils'
import { defaultPropertiesForComponentInFile } from '../../core/property-controls/property-controls-utils'

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
}

export function fileHasErrorMessages(path: string, errorMessages: ErrorMessage[] | null): boolean {
  if (errorMessages == null) {
    return false
  } else {
    return errorMessages.some((m) => m.fileName === path)
  }
}

function collectFileBrowserItems(
  projectContents: ProjectContents,
  collapsedPaths: string[],
  codeResultCache: CodeResultCache | null,
  errorMessages: ErrorMessage[] | null,
): FileBrowserItemInfo[] {
  const tree = contentsToTree(projectContents)
  let fileBrowserItems: FileBrowserItemInfo[] = []
  walkContentsTree(tree, (fullPath, element) => {
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
      })
      if (
        (element.type === 'CODE_FILE' || element.type === 'UI_JS_FILE') &&
        isJavascriptOrTypescript(fullPath) &&
        codeResultCache != null
      ) {
        const codeResult = codeResultCache.cache[fullPath]
        if (codeResult != null) {
          if (codeResult.error == null) {
            Object.keys(codeResult.exports).forEach((exportedVar) => {
              if (exportedVar != 'default') {
                const exportedVarValue = codeResult.exports[exportedVar].value
                const exportedVarIsFunction = typeof exportedVarValue === 'function'
                fileBrowserItems.push({
                  path: `${fullPath}/${exportedVar}`,
                  type: 'export',
                  fileType: null,
                  typeInformation: codeResult.exports[exportedVar].type,
                  originatingPath: originatingPath,
                  hasErrorMessages: false,
                  modified: false,
                  exportedFunction: exportedVarIsFunction,
                })
              }
            })
          } else {
            console.warn(`Error parsing JS file "${fullPath}": ${codeResult.error.toString()}`)
          }
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
  })

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
    isOpenUIFileParseSuccess,
    codeResultCache,
    renamingTarget,
    openUiFileName,
  } = useEditorState((store) => {
    const uiFile = getOpenUIJSFile(store.editor)
    return {
      dispatch: store.dispatch,
      projectContents: store.editor.projectContents,
      editorSelectedFile: getOpenFilename(store.editor),
      errorMessages: getAllCodeEditorErrors(store.editor, false, true),
      isOpenUIFileParseSuccess: uiFile != null && isParseSuccess(uiFile.fileContents),
      codeResultCache: store.editor.codeResultCache,
      renamingTarget: store.editor.fileBrowser.renamingTarget,
      openUiFileName: getOpenUIJSFileKey(store.editor),
    }
  })

  // since useEditorState uses a shallow equality check, we use a separate one to return the entire (string) array of componentUIDs
  // because the generated array keept loosing its reference equality
  const componentUIDs = useEditorState((store) => {
    const uiFile = getOpenUIJSFile(store.editor)
    return uiFile != null && isParseSuccess(uiFile.fileContents)
      ? getAllUniqueUids(getUtopiaJSXComponentsFromSuccess(uiFile.fileContents.value))
      : []
  })
  const componentUIDsWithStableRef = useKeepReferenceEqualityIfPossible(componentUIDs)

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

  const setSelected = React.useCallback(
    (item: FileBrowserItemInfo | null) => {
      if (item != null) {
        setSelectedPath(item.path)
        if (item.type === 'export' && isOpenUIFileParseSuccess && item.exportedFunction) {
          const newUID = generateUID(componentUIDsWithStableRef)
          const fileVarSeparatorIdx = item.path.lastIndexOf('/')
          const exportVarName = item.path.slice(fileVarSeparatorIdx + 1)
          const filePath = item.path.slice(0, fileVarSeparatorIdx)
          const filePathWithoutExtension = dropExtension(filePath)
          const insertingToItself = openUiFileName === filePath
          const defaultProps = defaultPropertiesForComponentInFile(
            exportVarName,
            filePathWithoutExtension,
            codeResultCache,
          )
          let props: JSXAttributes = objectMap(jsxAttributeValue, defaultProps)
          props['data-uid'] = jsxAttributeValue(newUID)
          const element: JSXElement = jsxElement(jsxElementName(exportVarName, []), props, [], null)
          dispatch(
            [
              EditorActions.enableInsertModeForJSXElement(
                element,
                newUID,
                insertingToItself
                  ? {}
                  : {
                      [filePathWithoutExtension]: importDetails(
                        null,
                        [importAlias(exportVarName)],
                        null,
                      ),
                    },
                null,
              ),
            ],
            'everyone',
          )
        }
      }
    },
    [
      codeResultCache,
      dispatch,
      isOpenUIFileParseSuccess,
      componentUIDsWithStableRef,
      openUiFileName,
    ],
  )

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
          />
        </div>
      ))}
    </React.Fragment>
  )
})

const FileBrowserActionSheet = betterReactMemo(
  'FileBrowserActionSheet',
  (props: FileBrowserActionSheetProps) => {
    const { dispatch } = useEditorState((store) => ({ dispatch: store.dispatch }))
    const addFolderClick = React.useCallback(
      () => dispatch([EditorActions.addFolder('')], 'everyone'),
      [dispatch],
    )
    const addCodeFileClick = React.useCallback(
      () => dispatch([EditorActions.addCodeFile('', 'untitled')], 'everyone'),
      [dispatch],
    )
    if (props.visible) {
      return (
        <ActionSheet>
          <SquareButton highlight onClick={addFolderClick}>
            <Icons.NewFolder tooltipText='Add New Folder' />
          </SquareButton>
          <SquareButton highlight onClick={addCodeFileClick}>
            <Icons.NewCodeFile tooltipText='Add Code File' />
          </SquareButton>
        </ActionSheet>
      )
    } else {
      return null
    }
  },
)
