import * as R from 'ramda'
import * as React from 'react'
import { useDrag, useDrop } from 'react-dnd'
import { colorTheme, Icn, TabComponent } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { ErrorMessage } from '../../core/shared/error-messages'
import Utils from '../../utils/utils'
import { ProjectContents } from '../../core/shared/project-file-types'
import { isModifiedFile } from '../../core/model/project-file-utils'
import * as EditorActions from '../editor/actions/actions'
import {
  EditorTab,
  getAllCodeEditorErrors,
  getOpenEditorTab,
  isOpenFileTab,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { fileHasErrorMessages } from './filebrowser'
import { getIconTypeForFileBrowserItem } from './fileitem'
import {
  getContentsTreeFileFromString,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../assets'

function getKeyForEditorTab(editorTab: EditorTab): string {
  switch (editorTab.type) {
    case 'OPEN_FILE_TAB':
      return `file:${editorTab.filename}`
    case 'RELEASE_NOTES_TAB':
      return 'release-notes'
    case 'USER_CONFIGURATION_TAB':
      return 'user-configuration'
    default:
      const _exhaustiveCheck: never = editorTab
      throw new Error(`Unhandled type ${JSON.stringify(editorTab)}`)
  }
}

function getLabelForEditorTab(editorTab: EditorTab): string {
  switch (editorTab.type) {
    case 'OPEN_FILE_TAB':
      return Utils.forceNotNull('Invalid state.', R.last(editorTab.filename.split('/')))
    case 'RELEASE_NOTES_TAB':
      return 'Welcome 👋'
    case 'USER_CONFIGURATION_TAB':
      return 'Settings'
    default:
      const _exhaustiveCheck: never = editorTab
      throw new Error(`Unhandled type ${JSON.stringify(editorTab)}`)
  }
}

function isModifiedForEditorTab(
  projectContents: ProjectContentTreeRoot,
  editorTab: EditorTab,
): boolean {
  switch (editorTab.type) {
    case 'OPEN_FILE_TAB':
      const file = getContentsTreeFileFromString(projectContents, editorTab.filename)
      return file == null ? false : isModifiedFile(file)
    case 'RELEASE_NOTES_TAB':
      return false
    case 'USER_CONFIGURATION_TAB':
      return false
    default:
      const _exhaustiveCheck: never = editorTab
      throw new Error(`Unhandled type ${JSON.stringify(editorTab)}`)
  }
}

function hasErrorMessagesForEditorTab(
  errorMessages: Array<ErrorMessage> | null,
  editorTab: EditorTab,
): boolean {
  switch (editorTab.type) {
    case 'OPEN_FILE_TAB':
      return fileHasErrorMessages(editorTab.filename, errorMessages)
    case 'RELEASE_NOTES_TAB':
      return false
    case 'USER_CONFIGURATION_TAB':
      return false
    default:
      const _exhaustiveCheck: never = editorTab
      throw new Error(`Unhandled type ${JSON.stringify(editorTab)}`)
  }
}

export const FileTabs = betterReactMemo('FileTabs', () => {
  const { dispatch, openFiles, errorMessages, selectedFile, projectContents } = useEditorState(
    (store) => {
      return {
        dispatch: store.dispatch,
        selectedFile: getOpenEditorTab(store.editor),
        openFiles: store.editor.openFiles,
        errorMessages: getAllCodeEditorErrors(store.editor, false, true),
        projectContents: store.editor.projectContents,
      }
    },
    'FileTabs',
  )

  const openFileTypes = useEditorState((store) => {
    return store.editor.openFiles.map((editorTab) => {
      if (isOpenFileTab(editorTab)) {
        const filepath = editorTab.filename
        return getIconTypeForFileBrowserItem(
          'file',
          filepath,
          Utils.propOrNull(
            'type',
            getContentsTreeFileFromString(store.editor.projectContents, filepath),
          ),
          false,
          false,
        )
      } else {
        return 'ui'
      }
    })
  }, 'FileTabs openFileTypes')

  const onTabClose = React.useCallback(
    (editorTab: EditorTab) => {
      if (isModifiedForEditorTab(projectContents, editorTab)) {
        dispatch([
          EditorActions.showModal({
            type: 'file-close',
            editorTab: editorTab,
          }),
        ])
      } else {
        dispatch([EditorActions.closeEditorTab(editorTab)], 'everyone')
      }
    },
    [dispatch, projectContents],
  )

  const onTabOpen = React.useCallback(
    (editorTab: EditorTab) => {
      dispatch([EditorActions.openEditorTab(editorTab, null)], 'everyone')
    },
    [dispatch],
  )

  const onTabReorder = React.useCallback(
    (editorTab: EditorTab, index: number) => {
      dispatch([EditorActions.reorderOpenFiles(editorTab, index)], 'everyone')
    },
    [dispatch],
  )

  if (openFiles.length === 0) {
    return null
  } else {
    return (
      <React.Fragment>
        {openFiles.map((editorTab, index) => {
          return (
            <FileTab
              index={index}
              key={getKeyForEditorTab(editorTab)}
              editorTab={editorTab}
              label={getLabelForEditorTab(editorTab)}
              type={openFileTypes[index]}
              selected={R.equals(editorTab, selectedFile)}
              modified={isModifiedForEditorTab(projectContents, editorTab)}
              onClose={onTabClose}
              onOpen={onTabOpen}
              onDrop={onTabReorder}
              hasErrorMessages={hasErrorMessagesForEditorTab(errorMessages, editorTab)}
            />
          )
        })}
      </React.Fragment>
    )
  }
})
FileTabs.displayName = 'FileTabs'

interface TabProps {
  index: number
  editorTab: EditorTab
  label: string
  type: string
  selected: boolean
  modified: boolean
  hasErrorMessages: boolean
  onClose: (editorTab: EditorTab) => void
  onOpen: (editorTab: EditorTab) => void
  onDrop: (editorTab: EditorTab, newIndex: number) => void
}

const onDrop = (draggedOntoProps: TabProps, draggedProps: TabProps) => {
  const target = draggedProps.index
  draggedProps.onDrop(draggedOntoProps.editorTab, target)
}

interface TabDragProps {
  forwardedRef: any
}

const TabInner: React.StatelessComponent<TabProps & TabDragProps> = (props) => {
  const fileIcon = (
    <Icn
      category='filetype'
      type={props.type}
      color={props.hasErrorMessages ? 'red' : 'darkgray'}
      width={18}
      height={18}
    />
  )

  const tabMouseDown = React.useCallback(() => props.onOpen(props.editorTab), [props])

  const tabClose = React.useCallback(() => props.onClose(props.editorTab), [props])

  const tab = (
    <div
      style={{
        display: 'flex',
        alignItems: 'stretch',
        backgroundColor: colorTheme.tabRailBackground.value,
      }}
    >
      <TabComponent
        icon={fileIcon}
        label={props.label}
        selected={props.selected}
        showModifiedIndicator={props.modified}
        hasErrorMessages={props.hasErrorMessages}
        onMouseDown={tabMouseDown}
        onClose={tabClose}
      />
    </div>
  )
  return tab
}

export function FileTab(props: TabProps) {
  const [dragSpec, drag] = useDrag({
    item: { type: 'filetabs', id: props },
    end: (dragResult, monitor) => {
      const didDrop = monitor.didDrop()
      if (didDrop) {
        onDrop(monitor.getDropResult(), props)
      }
    },
  })
  const [dropSpec, drop] = useDrop({
    accept: 'filetabs',
  })

  const innerProps: TabProps & TabDragProps = {
    ...props,
    forwardedRef: (node: any) => drag(drop(node)),
  }

  return <TabInner {...innerProps} />
}

/*
const FileTab = DropTarget(
  'filetabs',
  onDragActions,
  connectDropTarget,
)(DragSource('filetabs', cardSource, connectDragSource)(TabInner as any))
*/
