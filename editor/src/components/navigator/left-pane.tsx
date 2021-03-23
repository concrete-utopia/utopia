/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import * as React from 'react'
import { Component as ReactComponent } from 'react'
import { thumbnailURL } from '../../common/server'
import { getAllUniqueUids } from '../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../core/model/project-file-utils'
import { isParseSuccess, isTextFile, ProjectFile } from '../../core/shared/project-file-types'
import { NO_OP } from '../../core/shared/utils'
import Utils from '../../utils/utils'
import {
  PopupList,
  colorTheme,
  UtopiaTheme,
  FlexColumn,
  Section,
  SectionTitleRow,
  FlexRow,
  Title,
  SectionBodyArea,
  StringInput,
  Button,
  marginTop,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { EditorAction, EditorDispatch, LoginState } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import {
  clearSelection,
  regenerateThumbnail,
  setProjectName,
} from '../editor/actions/action-creators'
import { InsertMenu } from '../editor/insertmenu'
import { DerivedState, EditorState, getOpenFile } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { closeTextEditorIfPresent } from '../editor/text-editor'
import { FileBrowser } from '../filebrowser/filebrowser'
import { getControlStyles } from '../inspector/common/control-status'
import { SelectOption } from '../inspector/controls/select-control'
import { GridRow } from '../inspector/widgets/grid-row'
import { DependencyList } from './dependency-list'
import { GenericExternalResourcesList } from './external-resources/generic-external-resources-list'
import { GoogleFontsResourcesList } from './external-resources/google-fonts-resources-list'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const enum LeftMenuTab {
  UIInsert = 'ui-insert',
  ProjectStructure = 'project-structure',
  ProjectSettings = 'project-settings',
}

export function updateSelectedLeftMenuTab(editorState: EditorState, tab: LeftMenuTab): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      selectedTab: tab,
    },
  }
}

export function updateLeftMenuExpanded(editorState: EditorState, expanded: boolean): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      expanded: expanded,
    },
  }
}

export function setLeftMenuTabFromFocusedPanel(editorState: EditorState): EditorState {
  switch (editorState.focusedPanel) {
    case 'misccodeeditor':
      return updateSelectedLeftMenuTab(editorState, LeftMenuTab.ProjectStructure)
    case 'inspector':
    case 'canvas':
    case 'uicodeeditor':
    default:
      return editorState
  }
}

function setTab(tab: LeftMenuTab): EditorAction {
  return {
    action: 'SET_LEFT_MENU_TAB',
    tab: tab,
  }
}

interface ThumbnailProps {
  action: () => void
  projectId: string
  thumbnailLastGenerated: number
}

const previewImageContainerPadding = 4

class ThumbnailComponent extends ReactComponent<ThumbnailProps> {
  render() {
    const urlToRequest: string = `${thumbnailURL(this.props.projectId)}?lastUpdated=${
      this.props.thumbnailLastGenerated
    }`
    return (
      <div
        onClick={this.props.action}
        style={{ position: 'relative', cursor: 'pointer' }}
        // all of these are defined via `css` rather than `style` so that they are animateable;
        // since `css= {{'&:hover' : {...}}}` renders to className, any style prop will overwrite it
        data-label='previewImageContainer'
        css={{
          width: LeftPaneDefaultWidth - previewImageContainerPadding * 2,
          height: (LeftPaneDefaultWidth - previewImageContainerPadding * 2) / 1.6,
          paddingLeft: 4,
          paddingRight: 4,
          '& .refreshButton': {
            backgroundColor: colorTheme.emphasizedBackground.o(70).value,
            transition:
              'background-color .4s linear, border .4s linear, color .4s linear, box-shadow .1s linear',
            color: '#ccc',
            border: `1px solid ${colorTheme.secondaryBorder.value}`,
          },
          '&:hover .refreshButton': {
            border: `1px solid ${colorTheme.primary.value}`,
            textShadow: `0px 0px 0px ${colorTheme.primary.value}`,
            backgroundColor: colorTheme.emphasizedBackground.o(70).value,
            color: colorTheme.primary.value,
          },
          '&:active .refreshButton': {
            transform: 'scale(0.98)',
            boxShadow: `2px 2px 0px 0px ${colorTheme.primary.value}`,
          },
        }}
      >
        <div
          css={{
            boxShadow: `inset 0 0 0 1px ${colorTheme.secondaryBorder.value}`,
            borderRadius: 1,
            display: 'block',
            width: '100%',
            height: '100%',
            transition: 'all .4s ease-in-out',
            backgroundImage: `url('${urlToRequest}')`,
            backgroundSize: 'cover',
            backgroundColor: colorTheme.canvasBackground.value,
            opacity: 1,
            '.previewImageContainer:hover &': {
              transform: 'scale(1.1) skewX(-2deg) skewY(-2deg)',
              opacity: 0.7,
            },
          }}
        />
        <div
          data-label='ReloadButtonContainer'
          style={{
            position: 'absolute',
            left: 0,
            right: 0,
            top: 0,
            bottom: 0,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            // required to create its own stacking context and remain above the image
            transform: 'scale(1.0)',
          }}
        >
          <div
            //  refreshButton set above for animations
            className='refreshButton'
            style={{
              width: 160,
              height: UtopiaTheme.layout.rowHeight.medium,
              borderRadius: 1,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <span style={{ fontWeight: 500 }}>Retake</span>
          </div>
        </div>
      </div>
    )
  }
}

function getExistingUIDs(projectFile: ProjectFile | null): Array<string> {
  if (projectFile == null) {
    return []
  } else {
    if (isTextFile(projectFile)) {
      if (isParseSuccess(projectFile.fileContents.parsed)) {
        const components = getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed)
        return getAllUniqueUids(components)
      } else {
        return []
      }
    } else {
      return []
    }
  }
}

export const existingUIDs = Utils.memoize(getExistingUIDs)

export const LeftPaneMinimumWidth = 5

export const LeftPaneDefaultWidth = 260

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneOverflowScrollId = 'left-pane-overflow-scroll'

export const LeftPaneComponent = betterReactMemo('LeftPaneComponent', () => {
  const selectedTab = useEditorState(
    (store) => store.editor.leftMenu.selectedTab,
    'LeftPaneComponent selectedTab',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'LeftPaneComponent dispatch')

  return (
    <div
      id={LeftPaneComponentId}
      className='leftPane'
      style={{
        height: '100%',
        position: 'relative',
        backgroundColor: colorTheme.leftPaneBackground.value,
        borderRight: `1px solid ${colorTheme.subduedBorder.value}`,
      }}
      onMouseDown={() => closeTextEditorIfPresent()}
    >
      <div
        id={LeftPaneOverflowScrollId}
        className='overflow-y-scroll'
        style={{
          height: '100%',
          flexGrow: 1,
        }}
        onMouseDown={(mouseEvent: React.MouseEvent<HTMLDivElement>) => {
          if (mouseEvent.target instanceof HTMLDivElement) {
            if (mouseEvent.target.id === LeftPaneOverflowScrollId) {
              dispatch([clearSelection()])
            }
          }
        }}
      >
        {selectedTab === LeftMenuTab.ProjectStructure ? <ProjectStructurePane /> : null}
        {selectedTab === LeftMenuTab.ProjectSettings ? <ProjectSettingsPanel /> : null}
      </div>
    </div>
  )
})

const ProjectStructurePane = betterReactMemo('ProjectStructurePane', () => {
  return (
    <FlexColumn
      id='leftPaneProjectStructure'
      key='leftPaneProjectStructure'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <ProjectSettingsPanel />
      {/* <FileBrowser /> */}
      <DependencyList />
      <GenericExternalResourcesList />
      <GoogleFontsResourcesList />
      {/* <ResizableFlexColumn
        enable={{ bottom: true, right: false }}
        minHeight={100}
        style={{
          borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
          paddingBottom: 8,
          marginBottom: 8,
          overflow: 'scroll',
        }}
      >
      </ResizableFlexColumn> */}
    </FlexColumn>
  )
})

export const InsertMenuPane = betterReactMemo('InsertMenuPane', () => {
  const { dispatch, focusedPanel } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
    }
  }, 'InsertMenuPane')

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'insertmenu') {
        dispatch([setFocus('insertmenu')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  return (
    <Section data-name='InsertMenu' onFocus={onFocus} tabIndex={-1} style={{ width: '100%' }}>
      <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Insert</Title>
        </FlexRow>
      </SectionTitleRow>
      <SectionBodyArea
        minimised={false}
        style={{ paddingLeft: 8, paddingRight: 8, overflow: 'auto' }}
      >
        <InsertMenu />
      </SectionBodyArea>
    </Section>
  )
})

const SilentInput = styled.input({
  height: UtopiaTheme.layout.inputHeight.default,
  paddingBottom: 2,
  border: '1px solid transparent',
  backgroundColor: 'transparent',
  fontWeight: 600,
  cursor: 'pointer',
  transition: 'all .2s ease-in-out',
  '&:hover, &:active': {
    color: `${colorTheme.primary.value}`,
    cursor: 'text',
  },
  '&:focus': {
    paddingLeft: 2,
    color: `${colorTheme.primary.value}`,
    borderBottom: `1px solid ${colorTheme.primary.value}`,
    cursor: 'text',
  },
})

class RetakeButton extends ReactComponent<ThumbnailProps> {
  render() {
    const urlToRequest: string = `${thumbnailURL(this.props.projectId)}?lastUpdated=${
      this.props.thumbnailLastGenerated
    }`
    return (
      <div
        onClick={this.props.action}
        style={{ position: 'relative', cursor: 'pointer' }}
        // all of these are defined via `css` rather than `style` so that they are animateable;
        // since `css= {{'&:hover' : {...}}}` renders to className, any style prop will overwrite it
        data-label='previewImageContainer'
        css={{
          width: 100,
          height: 22,
          paddingLeft: 4,
          paddingRight: 4,
          '& .refreshButton': {
            backgroundColor: colorTheme.emphasizedBackground.o(70).value,
            transition:
              'background-color .4s linear, border .4s linear, color .4s linear, box-shadow .1s linear',
            color: '#ccc',
            border: `1px solid ${colorTheme.secondaryBorder.value}`,
          },
          '&:hover .refreshButton': {
            border: `1px solid ${colorTheme.primary.value}`,
            textShadow: `0px 0px 0px ${colorTheme.primary.value}`,
            backgroundColor: colorTheme.emphasizedBackground.o(70).value,
            color: colorTheme.primary.value,
          },
          '&:active .refreshButton': {
            transform: 'scale(0.98)',
            boxShadow: `2px 2px 0px 0px ${colorTheme.primary.value}`,
          },
        }}
      >
        <div
          css={{
            boxShadow: `inset 0 0 0 1px ${colorTheme.secondaryBorder.value}`,
            borderRadius: 1,
            display: 'block',
            width: '100%',
            height: '100%',
            transition: 'all .4s ease-in-out',
            backgroundImage: `url('${urlToRequest}')`,
            backgroundSize: 'cover',
            backgroundColor: colorTheme.canvasBackground.value,
            opacity: 1,
            '.previewImageContainer:hover &': {
              transform: 'scale(1.1) skewX(-2deg) skewY(-2deg)',
              opacity: 0.7,
            },
          }}
        />
        <div
          data-label='ReloadButtonContainer'
          style={{
            position: 'absolute',
            left: 0,
            right: 0,
            top: 0,
            bottom: 0,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            // required to create its own stacking context and remain above the image
            transform: 'scale(1.0)',
          }}
        >
          <div
            //  refreshButton set above for animations
            className='refreshButton'
            style={{
              width: 100,
              height: UtopiaTheme.layout.rowHeight.medium,
              borderRadius: 1,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <span style={{ fontWeight: 500 }}>Retake Snapshot</span>
          </div>
        </div>
      </div>
    )
  }
}

class ThumbnailComponentStripped extends ReactComponent<ThumbnailProps> {
  render() {
    const urlToRequest: string = `${thumbnailURL(this.props.projectId)}?lastUpdated=${
      this.props.thumbnailLastGenerated
    }`
    return (
      <FlexColumn>
        <div
          onClick={this.props.action}
          style={{ position: 'relative', cursor: 'pointer' }}
          // all of these are defined via `css` rather than `style` so that they are animateable;
          // since `css= {{'&:hover' : {...}}}` renders to className, any style prop will overwrite it
          data-label='previewImageContainer'
          css={{
            width: 172,
            height: 22,
            // paddingLeft: 4,
            // paddingRight: 4,
            // paddingBottom: 3,
            '& .refreshButton': {
              backgroundColor: colorTheme.emphasizedBackground.o(70).value,
              transition:
                'background-color .4s linear, border .4s linear, color .4s linear, box-shadow .1s linear',
              color: '#ccc',
              border: `1px solid ${colorTheme.secondaryBorder.value}`,
            },
            '&:hover .refreshButton': {
              border: `1px solid ${colorTheme.primary.value}`,
              textShadow: `0px 0px 0px ${colorTheme.primary.value}`,
              backgroundColor: colorTheme.emphasizedBackground.o(70).value,
              color: colorTheme.primary.value,
            },
            '&:active .refreshButton': {
              transform: 'scale(0.98)',
              boxShadow: `2px 2px 0px 0px ${colorTheme.primary.value}`,
            },
          }}
        >
          <div
            css={{
              boxShadow: `inset 0 0 0 1px ${colorTheme.secondaryBorder.value}`,
              borderRadius: 1,
              display: 'block',
              width: '100%',
              height: '100%',
              transition: 'all .4s ease-in-out',
              backgroundImage: `url('${urlToRequest}')`,
              backgroundSize: 'cover',
              backgroundColor: colorTheme.canvasBackground.value,
              opacity: 1,
              '.previewImageContainer:hover &': {
                transform: 'scale(1.1) skewX(-2deg) skewY(-2deg)',
                opacity: 0.7,
              },
            }}
          />
          <div
            data-label='ReloadButtonContainer'
            style={{
              position: 'absolute',
              left: 0,
              right: 0,
              top: 0,
              bottom: 0,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              // required to create its own stacking context and remain above the image
              transform: 'scale(1.0)',
            }}
          >
            {/* <div
              //  refreshButton set above for animations
              className='refreshButton'
              style={{
                width: 160,
                height: UtopiaTheme.layout.rowHeight.medium,
                borderRadius: 1,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            >
              <span style={{ fontWeight: 500 }}>Retake</span>
            </div> */}
          </div>
        </div>
      </FlexColumn>
    )
  }
}

const ProjectSettingsPanel = betterReactMemo('ProjectSettingsPanel', () => {
  const {
    dispatch,
    projectName,
    projectId,
    thumbnailLastGenerated,
    userState,
    focusedPanel,
    minimised,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      projectName: store.editor.projectName,
      projectId: store.editor.id,
      thumbnailLastGenerated: store.editor.thumbnailLastGenerated,
      userState: store.userState,
      focusedPanel: store.editor.focusedPanel,
      minimised: store.editor.projectSettings.minimised,
    }
  }, 'ProjectSettingsPanel')

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('projectsettings')], 'leftpane')
  }, [dispatch])

  const updateProjectName = React.useCallback(
    (newProjectName: string) => {
      dispatch([setProjectName(newProjectName)])
    },
    [dispatch],
  )

  const triggerRegenerateThumbnail = React.useCallback(() => {
    dispatch([regenerateThumbnail()])
  }, [dispatch])

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'projectsettings') {
        dispatch([setFocus('projectsettings')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  const handleBlur = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      updateProjectName(e.target.value)
    },
    [updateProjectName],
  )

  const handleKeyPress = React.useCallback((e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      let target = e.target as HTMLInputElement
      target.blur()
    }
  }, [])

  const dontPropagate = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  return (
    <FlexColumn key='leftPaneProjectTab'>
      {projectId == null ? null : (
        <FlexColumn>
          <GridRow padded={true} type='<---1fr--->|------172px-------|'>
            <span style={{ fontWeight: 700 }}>Project</span>
          </GridRow>
          <GridRow padded={true} type='<---1fr--->|------172px-------|'>
            <span style={{ fontWeight: 700 }}> Name </span>
            <StringInput testId='' value={projectName} />
          </GridRow>
          <GridRow padded={true} type='<---1fr--->|------172px-------|'>
            <span style={{ fontWeight: 700 }}> Description </span>
            <StringInput testId='' value={projectName} />
          </GridRow>
          <GridRow padded={true} type='<---1fr--->|------172px-------|'>
            <span style={{ fontWeight: 700 }}> Preview </span>
            <FlexColumn>
              <ThumbnailComponentStripped
                projectId={projectId}
                action={triggerRegenerateThumbnail}
                thumbnailLastGenerated={thumbnailLastGenerated}
              />
            </FlexColumn>
          </GridRow>
          <GridRow padded={true} type='<---1fr--->|------172px-------|'>
            <div style={{ paddingLeft: 70 }}>
              <RetakeButton
                projectId={projectId}
                action={triggerRegenerateThumbnail}
                thumbnailLastGenerated={thumbnailLastGenerated}
              />
            </div>
          </GridRow>
        </FlexColumn>
      )}
    </FlexColumn>
  )
})
