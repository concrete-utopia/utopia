import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  getFilePathForImportedComponent,
  isAnimatedElement,
  isImportedComponentNPM,
} from '../../../../core/model/project-file-utils'
import { maybeEitherToMaybe } from '../../../../core/shared/either'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { LargerIcons } from '../../../../uuiui'
import { IconToggleButton } from '../../../../uuiui/icon-toggle-button'
import { InlineButton, InlineLink } from '../../../../uuiui/inline-button'
import { normalisePathToUnderlyingTarget } from '../../../custom-code/code-file'
import { openCodeEditorFile, setFocusedElement } from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'
import * as EP from '../../../../core/shared/element-path'
import { when } from '../../../../utils/react-conditionals'
import { safeIndex } from '../../../../core/shared/array-utils'

function useComponentType(path: ElementPath | null): string | null {
  return useEditorState('metadata')((store) => {
    const metadata = store.editor.jsxMetadata
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
    if (path != null && MetadataUtils.isProbablyScene(metadata, path)) {
      return 'Scene'
    }
    if (path != null && MetadataUtils.isEmotionOrStyledComponent(path, metadata)) {
      return 'Styled Component'
    }
    const isAnimatedComponent = isAnimatedElement(elementMetadata)
    if (isAnimatedComponent) {
      return 'Animated Component'
    }
    const isImported = isImportedComponentNPM(elementMetadata)
    if (isImported) {
      return 'Component'
    }
    const isComponent = path != null && MetadataUtils.isFocusableComponent(path, metadata)
    return isComponent ? 'Component' : null
  }, 'useComponentType')
}

export const ComponentInfoBox = () => {
  const dispatch = useEditorState('restOfStore')(
    (state) => state.dispatch,
    'ComponentInfoBox dispatch',
  )
  const selectedViews = useEditorState('selectedViews')(
    (store) => store.editor.selectedViews,
    'ComponentInfoBox selectedViews',
  )

  const focusedElementPath = useEditorState('oldEditor')(
    (store) => store.editor.focusedElementPath,
    'ComponentInfoBox focusedElementPath',
  )

  // TODO MULTISELECT
  const target = safeIndex(selectedViews, 0) ?? null

  const isFocused = target == null ? false : EP.isFocused(focusedElementPath, target)

  const toggleFocusMode = React.useCallback(() => {
    dispatch([setFocusedElement(isFocused ? null : target)])
  }, [dispatch, isFocused, target])

  const locationOfComponentInstance = useEditorState('fullOldStore')((state) => {
    const element = MetadataUtils.findElementByElementPath(state.editor.jsxMetadata, target)
    const importResult = getFilePathForImportedComponent(element)
    if (importResult == null) {
      const underlyingTarget = normalisePathToUnderlyingTarget(
        state.editor.projectContents,
        state.editor.nodeModules.files,
        state.editor.canvas.openFile?.filename ?? '',
        target,
      )

      return underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' ? underlyingTarget.filePath : null
    } else {
      return importResult
    }
  }, 'ComponentSectionInner locationOfComponentInstance')

  const componentPackageName = useEditorState('metadata')((state) => {
    const componentMetadata = MetadataUtils.findElementByElementPath(
      state.editor.jsxMetadata,
      target,
    )
    return componentMetadata?.importInfo?.filePath
  }, 'ComponentSectionInner componentPackageName')

  const componentPackageMgrLink = `https://www.npmjs.com/package/${componentPackageName}`

  const isFocusable = useEditorState('metadata')((state) => {
    return target == null
      ? false
      : MetadataUtils.isFocusableComponent(target, state.editor.jsxMetadata)
  }, 'ComponentSectionInner isFocusable')
  const isImportedComponent = useEditorState('metadata')((state) => {
    const componentMetadata = MetadataUtils.findElementByElementPath(
      state.editor.jsxMetadata,
      target,
    )
    return isImportedComponentNPM(componentMetadata)
  }, 'ComponentSectionInner isImportedComponent')

  const componentType = useComponentType(target)

  const OpenFile = React.useCallback(() => {
    if (locationOfComponentInstance != null) {
      dispatch([openCodeEditorFile(locationOfComponentInstance, true)])
    }
  }, [dispatch, locationOfComponentInstance])
  return (
    <>
      {isImportedComponent ? (
        <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
          <span
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <LargerIcons.NpmLogo />
          </span>
          <p>
            {`This ${componentType} is imported from`}
            {when(
              componentPackageName != null,
              <>
                {' '}
                <InlineLink href={componentPackageMgrLink}>{`${componentPackageName}`}</InlineLink>
                {' via'}
              </>,
            )}
            {' NPM.'}
          </p>
        </UIGridRow>
      ) : isFocusable && !isFocused ? (
        <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
          <span
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <IconToggleButton
              value={false}
              srcOn={`/editor/icons/light/element/componentinstance-purple-18x18@2x.png`}
              srcOff={`/editor/icons/light/element/componentinstance-black-18x18@2x.png`}
              onToggle={toggleFocusMode}
            />
          </span>
          <p>
            {`This ${componentType} is imported from `}
            <InlineLink onClick={OpenFile}>{locationOfComponentInstance}</InlineLink>{' '}
            <InlineButton onClick={toggleFocusMode}>Edit it</InlineButton>
          </p>
        </UIGridRow>
      ) : isFocusable && isFocused ? (
        <UIGridRow padded tall={false} variant={'|--32px--|<--------auto-------->'}>
          <span
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <IconToggleButton
              value={true}
              srcOn={`/editor/icons/light/element/component-purple-18x18@2x.png`}
              srcOff={`/editor/icons/light/element/component-black-18x18@2x.png`}
              onToggle={toggleFocusMode}
            />
          </span>
          <p>
            {`This ${componentType} is imported from `}
            <InlineLink onClick={OpenFile}>{locationOfComponentInstance}</InlineLink>
            <InlineButton onClick={toggleFocusMode}>Exit Editing</InlineButton>
          </p>
        </UIGridRow>
      ) : null}
    </>
  )
}
