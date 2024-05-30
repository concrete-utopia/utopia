export type PreviewPanel = 'preview'

export type LeftMenuPanel =
  | 'filebrowser'
  | 'dependencylist'
  | 'genericExternalResources'
  | 'googleFontsResources'
  | 'insertmenu'
  | 'projectsettings'
  | 'githuboptions'

export type CenterPanel = 'canvas' | 'misccodeeditor'

export type InspectorPanel = 'inspector'

export type CodeEditorPanel = 'codeEditor'

export type NavigatorPanel = 'navigator'

export type EditorPanel =
  | LeftMenuPanel
  | CenterPanel
  | CodeEditorPanel
  | InspectorPanel
  | PreviewPanel
  | NavigatorPanel

export type EditorPane = 'leftmenu' | 'center' | 'inspector' | 'preview' | 'rightmenu'

export function paneForPanel(panel: EditorPanel | null): EditorPane | null {
  switch (panel) {
    case null:
      return null
    case 'filebrowser':
      return 'leftmenu'
    case 'dependencylist':
      return 'leftmenu'
    case 'genericExternalResources':
      return 'leftmenu'
    case 'googleFontsResources':
      return 'leftmenu'
    case 'githuboptions':
      return 'leftmenu'
    case 'insertmenu':
      return 'rightmenu'
    case 'projectsettings':
      return 'leftmenu'
    case 'navigator':
      return 'center'
    case 'canvas':
      return 'center'
    case 'misccodeeditor':
      return 'center'
    case 'inspector':
      return 'rightmenu'
    case 'codeEditor':
      return 'center'
    case 'preview':
      return 'preview'
    default:
      const _exhaustiveCheck: never = panel
      throw new Error(`Unhandled panel ${panel}`)
  }
}

export interface SetFocus {
  action: 'SET_FOCUS'
  focusedPanel: EditorPanel | null
}

export function setFocus(panel: EditorPanel | null): SetFocus {
  return {
    action: 'SET_FOCUS',
    focusedPanel: panel,
  }
}
