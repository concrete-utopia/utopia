/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag */
import { jsx } from '@emotion/react'
import * as Path from 'path'
import pathParse from 'path-parse'
import React from 'react'
import type { ConnectableElement, ConnectDragPreview } from 'react-dnd'
import { useDrag, useDrop } from 'react-dnd'
import type { ImageFile, ProjectFileType } from '../../core/shared/project-file-types'
import { Clipboard } from '../../utils/clipboard'
import Utils from '../../utils/utils'
import type { ContextMenuItem } from '../context-menu-items'
import { requireDispatch } from '../context-menu-items'
import { ContextMenuWrapper } from '../context-menu-wrapper'
import type { EditorAction, EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { ExpandableIndicator } from '../navigator/navigator-item/expandable-indicator'
import type { FileBrowserItemInfo, FileBrowserItemType } from './filebrowser'
import type { PasteResult } from '../../utils/clipboard-utils'
import { last } from '../../core/shared/array-utils'
import type { FileResult } from '../../core/shared/file-utils'
import { WarningIcon } from '../../uuiui/warning-icon'
import { fileResultUploadAction } from '../editor/image-insert'
import type { IcnColor, useColorTheme } from '../../uuiui'
import {
  Icn,
  Icons,
  flexRowStyle,
  OnClickOutsideHOC,
  StringInput,
  colorTheme,
  UtopiaTheme,
  SimpleFlexRow,
  Button,
} from '../../uuiui'
import { notice } from '../common/notice'
import { appendToPath, getParentDirectory } from '../../utils/path-utils'
import type { AddingFile } from './filepath-utils'
import { applyAddingFile } from './filepath-utils'
import CanvasActions from '../canvas/canvas-actions'
import { imagePathURL } from '../../common/server'
import { useEditorState } from '../editor/store/store-hook'
import { EditorModes } from '../editor/editor-modes'
import type { DraggedImageProperties } from '../editor/store/editor-state'
import { draggingFromSidebar, notDragging } from '../editor/store/editor-state'
import { fileExists } from '../../core/model/project-file-utils'
import type { FileUploadInfo } from '../editor/store/editor-state'
import { fileOverwriteModal } from '../editor/store/editor-state'
import { optionalMap } from '../../core/shared/optional-utils'
import type { GithubFileStatus } from '../../core/shared/github/helpers'
import { getFilenameParts } from '../images'
import { getConflictMenuItems } from '../../core/shared/github-ui'
import { useDispatch } from '../editor/store/dispatch-context'
import type { ErrorMessage } from '../../core/shared/error-messages'
import { assertNever } from '../../core/shared/utils'

export interface FileBrowserItemProps extends FileBrowserItemInfo {
  isSelected: boolean
  renamingTarget: string | null
  key: string
  dispatch: EditorDispatch
  collapsed: boolean
  dropTarget: string | null
  toggleCollapse: (filePath: string) => void
  expand: (filePath: string) => void
  setSelected: (selectedItem: FileBrowserItemInfo | null) => void
  generateNewUid: () => string
}

interface FileBrowserItemState {
  isRenaming: boolean
  isHovered: boolean
  adding: AddingFile | null
  filename: string
  pathParts: Array<string>
}

const BaseIndentationPadding = 15

function onDrop(
  draggedOntoProps: FileBrowserItemProps | 'CANVAS',
  draggedProps: FileBrowserItemProps,
) {
  if (draggedOntoProps !== 'CANVAS') {
    const isRootArea = draggedOntoProps.fileType != null
    if (
      draggedOntoProps.fileType == null ||
      (draggedOntoProps.dropTarget != null &&
        draggedOntoProps.path.includes(draggedOntoProps.dropTarget)) ||
      isRootArea
    ) {
      const newDirectory = draggedOntoProps.dropTarget != null ? draggedOntoProps.dropTarget : '/'
      const newFilePath = appendToPath(newDirectory, last(draggedProps.path.split('/')) as string)
      if (draggedProps.path !== newFilePath && draggedProps.path !== draggedOntoProps.path) {
        draggedOntoProps.dispatch(
          [
            EditorActions.updateFilePath(draggedProps.path, newFilePath),
            EditorActions.setFilebrowserDropTarget(null),
          ],
          'everyone',
        )
      } else {
        draggedOntoProps.dispatch([EditorActions.setFilebrowserDropTarget(null)], 'everyone')
      }
    } else {
      draggedOntoProps.dispatch([EditorActions.setFilebrowserDropTarget(null)], 'everyone')
    }
  }
}

export function getIconNameForFilePath(filePath: string): string {
  const parts = filePath.toLowerCase().split('.')
  const extension = parts.length > 0 ? last(parts) : null

  switch (extension) {
    case 'jsx':
    case 'js':
    case 'tsx':
    case 'ts':
    case 'html':
    case 'css':
    case 'txt':
    case 'md':
    case 'json':
    case 'svg':
      return extension
    case 'otf':
    case 'ttf':
    case 'woff':
    case 'woff2':
      return 'font'
    case 'jpg':
    case 'gif':
    case 'png':
    case 'jpeg':
    case 'webp':
      return 'img'
    case 'avi':
    case 'mpeg':
    case 'mpg':
    case 'qt':
    case 'mp4':
    case 'webm':
    case 'ogv':
    case 'mov':
      return 'video'
    case 'mp3':
    case 'wav':
    case 'ogg':
    case 'm4a':
    case 'flac':
    case 'aac':
      return 'audio'
    default:
      return 'other'
  }
}

export function getIconTypeForFileBrowserItem(
  fileBrowserItemType: FileBrowserItemType,
  filePath: string,
  fileType: ProjectFileType | null,
  collapsed: boolean,
  exportedFunction: boolean = false,
): string {
  if (fileBrowserItemType === 'export') {
    if (exportedFunction) {
      return 'function'
    } else {
      return 'export'
    }
  }
  if (fileType == null) {
    return 'folder-open'
  } else {
    switch (fileType) {
      case 'DIRECTORY':
        if (collapsed) {
          return 'folder-closed'
        } else {
          return 'folder-open'
        }
      case 'IMAGE_FILE':
      case 'TEXT_FILE':
      case 'ASSET_FILE':
        return getIconNameForFilePath(filePath)
      default:
        const _exhaustiveCheck: never = fileType
        throw new Error(`Unhandled type ${JSON.stringify(fileType)}`)
    }
  }
}

const canDelete = (fileBrowserItem: FileBrowserItemInfo) => {
  return isFileButNotPackageJson(fileBrowserItem)
}

const canDragnDrop = (fileBrowserItem: FileBrowserItemInfo) => {
  return isFileButNotPackageJson(fileBrowserItem)
}

const canRename = (fileBrowserItem: FileBrowserItemInfo) => {
  return isFileButNotPackageJson(fileBrowserItem)
}

const isFileButNotPackageJson = (fileBrowserItem: FileBrowserItemInfo) => {
  return fileBrowserItem.type === 'file' && fileBrowserItem.path != 'package.json'
}

const isFile = (fileBrowserItem: FileBrowserItemInfo) => {
  return fileBrowserItem.type === 'file'
}

export const getGithubFileStatusColor = (type: GithubFileStatus): string => {
  // NOTE: these are placeholder colors, we should update them once we finalize the design
  switch (type) {
    case 'untracked':
      return colorTheme.githubMUDUntracked.value
    case 'modified':
      return colorTheme.githubMUDModified.value
    case 'deleted':
      return colorTheme.githubMUDDeleted.value
    default:
      return colorTheme.githubMUDDefault.value
  }
}

export const GithubFileStatusLetter: React.FunctionComponent<{ status: GithubFileStatus }> = (
  props,
) => {
  return (
    <div
      style={{
        fontWeight: 500,
        width: 15,
        color: getGithubFileStatusColor(props.status),
      }}
    >
      {props.status.charAt(0).toUpperCase()}
    </div>
  )
}

export function addingChildElement(
  indentation: number,
  addingChildName: string,
  onKeyDown: (event: React.KeyboardEvent<HTMLInputElement>) => void,
  onChange: (event: React.ChangeEvent<HTMLInputElement>) => void,
  onBlur: () => void,
): React.ReactNode {
  return (
    <SimpleFlexRow
      style={{
        // make it look indented, plus extra to account for missing icon
        paddingLeft: (indentation + 1) * BaseIndentationPadding + 20,
        paddingTop: 3,
        paddingBottom: 3,
        height: 32,
        display: 'flex',
        alignItems: 'center',
      }}
    >
      <StringInput
        testId=''
        value={addingChildName}
        autoFocus
        onKeyDown={onKeyDown}
        onChange={onChange}
        onBlur={onBlur}
      />
    </SimpleFlexRow>
  )
}

interface FileBrowserItemDragProps {
  isDragging: boolean
  isOver: boolean
  connectDragPreview: ConnectDragPreview
  forwardedRef: any
}

class FileBrowserItemInner extends React.PureComponent<
  FileBrowserItemProps & FileBrowserItemDragProps,
  FileBrowserItemState
> {
  constructor(props: FileBrowserItemProps & FileBrowserItemDragProps) {
    super(props)
    this.state = {
      isRenaming: false,
      isHovered: false,
      adding: null,
      filename: '',
      pathParts: [],
    }
  }

  static getDerivedStateFromProps(
    props: FileBrowserItemProps,
    state: FileBrowserItemState,
  ): Partial<FileBrowserItemState> | null {
    if (state.isRenaming) {
      return {
        isRenaming: props.renamingTarget === props.path,
      }
    }
    const pathParts = props.path.split('/').filter((s) => s !== '')
    const filename = Utils.last(pathParts)
    return {
      filename: filename,
      isRenaming: props.renamingTarget === props.path,
      pathParts: pathParts,
    }
  }

  toggleCollapse = () => this.props.toggleCollapse(this.props.path)

  renderGithubStatus = () => {
    if (this.props.conflict != null) {
      return <GithubFileStatusLetter status={'conflicted'} />
    }
    if (this.props.githubStatus != undefined) {
      return <GithubFileStatusLetter status={this.props.githubStatus} />
    }
    return null
  }

  renderIcon() {
    return (
      <Icn
        style={{
          marginRight: 0,
        }}
        category='filetype'
        type={getIconTypeForFileBrowserItem(
          this.props.type,
          this.props.path,
          this.props.fileType,
          this.props.collapsed,
          this.props.exportedFunction,
        )}
        color={severityFromErrors(this.props.errorMessages) === 'error' ? 'error' : undefined}
        width={12}
        height={12}
        onDoubleClick={this.toggleCollapse}
      />
    )
  }

  renderModifiedIcon() {
    return this.props.modified ? <Icons.CircleSmall color='dynamic' /> : null
  }

  onChangeFilename = (event: React.ChangeEvent<HTMLInputElement>) => {
    this.setState({ filename: event.target.value })
  }

  onKeyDownFilename = (event: React.KeyboardEvent) => {
    if (event.key === 'Enter' && this.state.filename.trim().length > 0) {
      const newFileName = this.state.filename
      const parsedPath = pathParse(this.props.path)
      const newPath = Path.normalize(`${parsedPath.dir}/${newFileName}`)
      this.props.dispatch(
        [
          EditorActions.updateFilePath(this.props.path, newPath),
          EditorActions.setFilebrowserRenamingTarget(null),
        ],
        'everyone',
      )
    }
    if (event.key === 'Escape') {
      this.props.dispatch([EditorActions.setFilebrowserRenamingTarget(null)], 'everyone')
    }
  }

  onBlurFilename = () => {
    this.props.dispatch([EditorActions.setFilebrowserRenamingTarget(null)], 'everyone')
  }

  onFocusFilename = (event: React.FocusEvent<HTMLInputElement>) => {
    event.target.select()
  }

  onDoubleClickFilename = () => {
    if (canRename(this.props)) {
      this.props.dispatch([EditorActions.setFilebrowserRenamingTarget(this.props.path)], 'everyone')
    } else if (isFile(this.props)) {
      this.props.dispatch(
        [EditorActions.showToast(notice('We need this file. You cannot rename it.', 'WARNING'))],
        'everyone',
      )
    }
  }

  onClickOutsideLabel = () => {
    this.props.dispatch([EditorActions.setFilebrowserRenamingTarget(null)], 'everyone')
  }

  renderLabel() {
    if (this.state.isRenaming) {
      return (
        <div style={{ ...flexRowStyle, marginLeft: 2 }}>
          <OnClickOutsideHOC onClickOutside={this.onClickOutsideLabel}>
            <StringInput
              testId=''
              value={this.state.filename}
              onChange={this.onChangeFilename}
              onKeyDown={this.onKeyDownFilename}
              onFocus={this.onFocusFilename}
              focusOnMount
            />
          </OnClickOutsideHOC>
        </div>
      )
    } else {
      const labelColor =
        this.props.fileType === 'ASSET_FILE'
          ? colorTheme.dynamicBlue.value
          : getLabelColor(colorTheme, this.props.errorMessages)

      return (
        <>
          <span
            onDoubleClick={this.onDoubleClickFilename}
            style={{
              marginLeft: 6,
              display: 'inline-block',
              whiteSpace: 'nowrap',
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              color: labelColor,
            }}
          >
            {this.state.filename}
          </span>
          {this.props.typeInformation != null ? (
            <span style={{ color: labelColor }}>: {this.props.typeInformation}</span>
          ) : null}
        </>
      )
    }
  }

  renderDeleteButton() {
    return <Icons.CrossSmall />
  }

  deleteContextMenuItem(): ContextMenuItem<unknown> {
    return {
      name: `Delete ${
        this.props.fileType != null
          ? this.props.fileType === 'DIRECTORY'
            ? 'folder'
            : 'file'
          : 'item'
      }`,
      enabled: this.props.fileType != null && canDelete(this.props),
      action: (data: unknown, dispatch?: EditorDispatch) => {
        requireDispatch(dispatch)(
          [
            EditorActions.showModal({
              type: 'file-delete',
              filePath: this.props.path,
            }),
          ],
          'everyone',
        )
      },
    }
  }

  renameContextMenuItem(): ContextMenuItem<unknown> {
    return {
      name: `Rename ${
        this.props.fileType != null
          ? this.props.fileType === 'DIRECTORY'
            ? 'folder'
            : 'file'
          : 'this'
      }`,
      enabled: this.props.fileType != null && canRename(this.props),
      action: () => {
        this.props.dispatch(
          [EditorActions.setFilebrowserRenamingTarget(this.props.path)],
          'everyone',
        )
      },
    }
  }

  revertContextMenuItem(): ContextMenuItem<unknown> {
    return {
      name: `Revert changes`,
      enabled: this.props.githubStatus != null,
      action: (_data: unknown, dispatch?: EditorDispatch) => {
        requireDispatch(dispatch)(
          [
            EditorActions.showModal({
              type: 'file-revert',
              filePath: this.props.path,
              status: this.props.githubStatus ?? null,
            }),
          ],
          'everyone',
        )
      },
    }
  }
  delete = () => {
    this.props.dispatch(
      [
        EditorActions.showModal({
          type: 'file-delete',
          filePath: this.props.path,
        }),
      ],
      'everyone',
    )
  }

  onItemDrop = (event: React.DragEvent<HTMLDivElement>) => {
    // this disables react-dnd while dropping external files
    if (!this.props.isOver) {
      event.preventDefault()
      event.stopPropagation()
    }

    this.props.dispatch([
      EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
      EditorActions.setFilebrowserDropTarget(null),
    ])

    void Clipboard.parseClipboardData(event.dataTransfer).then((result: PasteResult) => {
      let actions: Array<EditorAction> = []
      let overwriteFiles: Array<FileUploadInfo> = []
      Utils.fastForEach(result.files, (resultFile: FileResult) => {
        let targetPath: string | null = null
        let replace = false
        switch (this.props.fileType) {
          case 'DIRECTORY':
            // Put the image "into" the directory.
            targetPath = `${this.props.path}/${resultFile.filename}`
            break
          case 'ASSET_FILE':
          case 'IMAGE_FILE':
            // Overwrite the image.
            targetPath = this.props.path
            replace = true
            break
          default:
          // Do nothing, overwriting an image onto a UI file seems wrong.
        }

        if (targetPath != null) {
          if (fileExists(this.props.projectContents, targetPath)) {
            overwriteFiles.push({ fileResult: resultFile, targetPath: targetPath })
          } else {
            actions.push(fileResultUploadAction(resultFile, targetPath, replace))
          }
        }
      })
      if (overwriteFiles.length > 1) {
        actions.push(EditorActions.showModal(fileOverwriteModal(overwriteFiles)))
      }
      this.props.dispatch(actions, 'everyone')
    })
  }

  setItemIsHovered = () => {
    this.setState({ isHovered: true })
  }

  setItemIsNotHovered = () => {
    this.setState({ isHovered: false })
  }

  onItemDragOver = (event: React.DragEvent<HTMLDivElement>) => {
    // this disables react-dnd while dropping external files

    if (!this.props.isOver) {
      event.preventDefault()
      event.stopPropagation()
    }
  }

  onDragEnter = (e: React.DragEvent) => {
    // this disables react-dnd while dropping external files
    if (
      this.props.isOver ||
      (e.dataTransfer?.items != null &&
        Array.from(e.dataTransfer?.items).filter((item) => item.kind === 'file').length === 0)
    ) {
      return
    }
    const filesBeingDragged = e.dataTransfer?.items?.length ?? 0
    if (filesBeingDragged > 0) {
      this.setState({ isHovered: true })
    }
    const targetDirectory =
      this.props.fileType === 'DIRECTORY' ? this.props.path : getParentDirectory(this.props.path)
    this.props.dispatch(
      [
        CanvasActions.clearInteractionSession(false),
        EditorActions.setFilebrowserDropTarget(targetDirectory),
      ],
      'leftpane',
    )
  }

  onDragLeave = () => {
    this.setState({
      isHovered: false,
    })
  }

  onMouseClick = (e: React.MouseEvent) => {
    if (e.button === 0) {
      if (this.props.fileType !== 'ASSET_FILE' && this.props.fileType !== 'IMAGE_FILE') {
        this.props.setSelected(this.props)
        if (this.props.fileType != null && this.props.fileType !== 'DIRECTORY') {
          this.props.dispatch([EditorActions.openCodeEditorFile(this.props.path, true)], 'everyone')
        }
      }
    }
  }

  onMouseDown = (e: React.MouseEvent) => {
    const imageProperties: DraggedImageProperties | null =
      this.props.imageFile == null
        ? null
        : draggedImagePropertiesFromImageFile(this.props.path, this.props.imageFile)

    this.props.dispatch(
      [EditorActions.setImageDragSessionState(draggingFromSidebar(imageProperties))],
      'everyone',
    )
  }

  onMouseUp = () =>
    this.props.dispatch([
      CanvasActions.clearInteractionSession(false),
      EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
      EditorActions.setImageDragSessionState(notDragging()),
    ])

  showAddingFileRow = () => {
    this.setState({
      adding: {
        fileOrFolder: 'file',
        filename: '',
      },
    })
  }

  showAddingFolderRow = () => {
    this.setState({
      adding: {
        fileOrFolder: 'folder',
        filename: '',
      },
    })
  }

  confirmAddingFile = () => {
    this.props.expand(this.props.path)
    applyAddingFile(this.props.dispatch, this.props.path, this.state.adding)
    this.setState({
      adding: null,
    })
  }

  inputLabelChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    if (this.state.adding != null) {
      this.setState({
        adding: {
          ...this.state.adding,
          filename: event.target.value,
        },
      })
    }
  }

  abandonAddingFile = () => {
    this.setState({
      adding: null,
    })
  }

  inputLabelKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') {
      this.confirmAddingFile()
    }
    if (event.key === 'Escape') {
      this.abandonAddingFile()
    }
  }

  isCurrentDropTarget = () => {
    return this.props.dropTarget === this.props.path
  }

  render() {
    const extraIndentationForExport = 0
    const indentation = this.state.pathParts.length + extraIndentationForExport - 1

    const getBackground = () => {
      if (this.props.isSelected) {
        return colorTheme.subtleBackground.value
      } else if (this.state.isHovered) {
        return colorTheme.secondaryBackground.value
      } else if (this.isCurrentDropTarget()) {
        return colorTheme.subtleBackground.value
      } else {
        return 'transparent'
      }
    }

    const fileIconStyle = {
      marginRight: 4,
      marginLeft: 'auto',
    }

    const displayAddFolder =
      this.state.isHovered && this.props.fileType != null && this.props.fileType === 'DIRECTORY'
    const displayAddTextFile =
      this.state.isHovered && this.props.fileType != null && this.props.fileType === 'DIRECTORY'
    const displayDelete = this.state.isHovered

    let fileBrowserItem = (
      <div style={{ width: '100%' }}>
        <div
          tabIndex={0}
          data-testid={`fileitem-${this.props.path}`}
          onDrop={this.onItemDrop}
          onMouseEnter={this.setItemIsHovered}
          onMouseLeave={this.setItemIsNotHovered}
          onDragEnter={this.onDragEnter}
          onDragOver={this.onItemDragOver}
          onDragLeave={this.onDragLeave}
          onMouseDown={this.onMouseDown}
          onMouseUp={this.onMouseUp}
          onClick={this.onMouseClick}
          key={this.props.key}
          className='FileItem'
          style={{
            marginLeft: 8,
            marginRight: 8,
            paddingLeft: indentation * BaseIndentationPadding,
            paddingTop: 3,
            paddingBottom: 3,
            height: UtopiaTheme.layout.rowHeight.smaller,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'space-between',
            opacity: this.props.isDragging ? 0.5 : undefined,
            backgroundColor: getBackground(),
            borderRadius: 2,
            position: 'relative',
          }}
        >
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              overflow: 'hidden',
              gap: 5,
              alignItems: 'center',
            }}
          >
            <ExpandableIndicator
              key='expandable-indicator-small'
              visible={
                this.props.type === 'file' &&
                this.props.fileType != null &&
                this.props.fileType === 'DIRECTORY'
              }
              collapsed={this.props.collapsed}
              selected={false}
              onMouseDown={this.toggleCollapse}
            />
            {this.props.connectDragPreview(
              <div
                style={{
                  ...flexRowStyle,
                  overflow: 'hidden',
                  gap: 5,
                }}
              >
                {this.renderIcon()}
                {this.renderLabel()}
                {this.renderModifiedIcon()}
              </div>,
            )}
          </div>
          {this.props.type === 'file' ? (
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                gap: '2px',
                flexDirection: 'row',
                marginRight: '2px',
              }}
            >
              {this.props.isUploadedAssetFile ? (
                <span
                  style={{
                    padding: '2px 4px',
                    fontSize: 9,
                    color: colorTheme.dynamicBlue.value,
                  }}
                >
                  S3
                </span>
              ) : null}
              {displayAddFolder ? (
                <Button onClick={this.showAddingFolderRow}>
                  <Icons.FolderClosed style={fileIconStyle} tooltipText='Add New Folder' />
                </Button>
              ) : null}
              {displayAddTextFile ? (
                <Button onClick={this.showAddingFileRow}>
                  <Icons.NewTextFile style={fileIconStyle} tooltipText='Add Code File' />
                </Button>
              ) : null}
              {displayDelete ? (
                <Button onClick={this.delete}>
                  <Icons.Cross tooltipText='Delete' />
                </Button>
              ) : null}
              {this.props.errorMessages.length > 0 ? (
                <span style={{ margin: '0px 4px' }}>
                  <WarningIcon
                    color={errorAwareIconColor(this.props.errorMessages)}
                    tooltipText={this.props.errorMessages
                      .map(
                        (errorMessage) =>
                          `${errorMessage.startLine}:${errorMessage.startColumn} - ${errorMessage.source}: ${errorMessage.message}`,
                      )
                      .join(`,\n`)}
                  />
                </span>
              ) : null}
              {this.renderGithubStatus()}
            </div>
          ) : null}
        </div>
        {this.state.adding == null
          ? null
          : addingChildElement(
              indentation,
              this.state.adding.filename,
              this.inputLabelKeyDown,
              this.inputLabelChange,
              this.abandonAddingFile,
            )}
      </div>
    )

    // The context menu wrapper causes focus issues with renaming.
    if (this.props.type === 'file' && !this.state.isRenaming) {
      const contextMenuID = `file-context-menu-${this.state.filename.replace(' ', '_space_')}`
      const items = [this.deleteContextMenuItem(), this.renameContextMenuItem()]
      if (this.props.githubStatus != undefined) {
        items.push(this.revertContextMenuItem())
      }
      if (
        this.props.conflict != null &&
        this.props.githubRepo != null &&
        this.props.projectID != null
      ) {
        items.push(
          ...getConflictMenuItems(
            this.props.githubUserDetails,
            this.props.githubRepo,
            this.props.projectID,
            this.props.dispatch,
            this.props.path,
            this.props.conflict,
            'Resolve Conflict',
          ),
        )
      }
      fileBrowserItem = (
        <div
          ref={this.props.forwardedRef}
          style={{ width: '100%' }}
          key={`${contextMenuID}-wrapper`}
        >
          <ContextMenuWrapper
            id={contextMenuID}
            dispatch={this.props.dispatch}
            items={items}
            data={{}}
          >
            {fileBrowserItem}
          </ContextMenuWrapper>
        </div>
      )
    }

    return fileBrowserItem
  }
}

export const FileBrowserItem: React.FC<FileBrowserItemProps> = (props: FileBrowserItemProps) => {
  const dispatch = useDispatch()

  const [{ isDragging }, drag, dragPreview] = useDrag(
    () => ({
      type: 'files',
      canDrag: () => canDragnDrop(props),
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
      item: () => {
        return props
      },
      end: () => {
        dispatch([
          CanvasActions.clearInteractionSession(false),
          EditorActions.setFilebrowserDropTarget(null),
        ])
      },
    }),
    [props],
  )
  const [{ isOver }, drop] = useDrop(
    {
      accept: 'files',
      canDrop: () => true,
      drop: (item, monitor) => {
        dispatch([
          CanvasActions.clearInteractionSession(false),
          EditorActions.setImageDragSessionState(notDragging()),
          EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
          EditorActions.setFilebrowserDropTarget(null),
        ])
        onDrop(props, item)
      },
      hover: (item: FileBrowserItemProps) => {
        const targetDirectory =
          props.fileType === 'DIRECTORY' ? props.path : getParentDirectory(props.path)
        // do not trigger highlight when it tries to move to it's descendant directories
        if (targetDirectory.includes(item.path) && props.dropTarget != null) {
          props.dispatch(
            [
              CanvasActions.clearInteractionSession(false),
              EditorActions.setFilebrowserDropTarget(null),
            ],
            'leftpane',
          )
        } else if (props.dropTarget !== targetDirectory) {
          props.dispatch(
            [
              CanvasActions.clearInteractionSession(false),
              EditorActions.setFilebrowserDropTarget(targetDirectory),
            ],
            'leftpane',
          )
        }
      },
      collect: (monitor) => ({
        isOver: monitor.isOver(),
      }),
    },
    [props],
  )

  const forwardedRef = (node: ConnectableElement) => drag(drop(node))

  return (
    <FileBrowserItemInner
      {...props}
      isDragging={isDragging}
      isOver={isOver}
      connectDragPreview={dragPreview}
      // eslint-disable-next-line react/jsx-no-bind
      forwardedRef={forwardedRef}
    />
  )
}

function draggedImagePropertiesFromImageFile(
  path: string,
  imageFile: ImageFile,
): DraggedImageProperties {
  const imageMultiplier = getFilenameParts(path)?.multiplier ?? 1
  return {
    src: imagePathURL(path),
    width: optionalMap((w) => w / imageMultiplier, imageFile.width) ?? 200,
    height: optionalMap((h) => h / imageMultiplier, imageFile.height) ?? 200,
  }
}

function severityFromErrors(errors: ErrorMessage[]): 'no-error' | 'warning' | 'error' {
  if (errors.length === 0) {
    return 'no-error'
  }

  if (errors.every((e) => e.severity === 'warning')) {
    return 'warning'
  }

  return 'error'
}

function getLabelColor(theme: ReturnType<typeof useColorTheme>, errors: ErrorMessage[]): string {
  const severity = severityFromErrors(errors)
  switch (severity) {
    case 'error':
      return theme.errorForeground.value
    case 'no-error':
    case 'warning':
      return theme.neutralForeground.value
    default:
      assertNever(severity)
  }
}

function errorAwareIconColor(errors: ErrorMessage[]): IcnColor {
  return severityFromErrors(errors) === 'warning' ? 'warning' : 'error'
}
