## Brief Overview

TL;DR: Code = React + TS, heavy use of useCallback / memo for performance.

NB: all filenames given without path unless there are two identical filenames.

### Architecture

We use a redux-like architecture for most editor-wide settings, with the main exception being the code<>design loop. The state is in `editor-state.ts`. State contains both transient and non-transient parts for undo purposes. The following is a quick guided tour to add a setting, and the actions to change it.

In your component, you may want to call an action to change setting. To do that, call dispatch with EditorActions (if you search for this, you'll find lots of examples & the required imports).

```
const toggleAdditionalControls = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])
```

The action lives in `action-creators.ts` and returns the action - it could also return other fields for payload.

```
export function toggleInterfaceDesignerAdditionalControls(): ToggleInterfaceDesignerAdditionalControls {
  return {
    action: 'TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS',
  }
}
```

In `action-utils.ts` there's a list of _transient_ actions, of which this is one. If you're creating a transient action, add it there!

In `editor-update.ts` we map `TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS` to an actual function, `return UPDATE_FNS.TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS(action, state)` specifically.

In `actions.ts` you'll find the actual action. It returns the editor, with only the changing values overwritten. The action here doesn't have a payload (it just toggles the `additionalControls`, as you can see here. If there were a payload, it'd be part of the `action` object.

```
TOGGLE_INTERFACEDESIGNER_ADDITIONAL_CONTROLS: (
    action: ToggleInterfaceDesignerAdditionalControls,
    editor: EditorModel,
  ): EditorModel => {
    return {
      ...editor,
      interfaceDesigner: {
        ...editor.interfaceDesigner,
        additionalControls: !editor.interfaceDesigner.additionalControls,
      },
    }
  },
```

If you want to add a new property to the editor state, the safest way to do that is to add it to the EditorState interface, and then work your way through the compile errors.

### Layout, Theme, Icons, Standard Components

Most of these live in `uuiui`. We mainly use flexbox and css grid for layouts. Most of our code relies on `InspectorRow` (css grid), FlexRow, and FlexColumn.
Our theme lives in `theme.js` for the overall editor (excluding VSCode).

### Code Editor

We embed VSCode (not Monaco), with an extension to keep it in sync with the design tool and vice versa.
