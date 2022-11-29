import React from 'react'
import { imagePathURL } from '../../../../common/server'
import { treeToContents } from '../../../../components/assets'
import { isImageFile } from '../../../../core/model/project-file-utils'
import { emptyComments, jsxAttributeValue } from '../../../../core/shared/element-template'
import { ProjectContents } from '../../../../core/shared/project-file-types'
import { setOverrideProp } from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import * as PP from '../../../../core/shared/property-path'
import { useInspectorElementInfo } from '../../common/property-path-hooks'
import { OptionChainControl } from '../../controls/option-chain-control'
import { SelectControl, SelectOption } from '../../controls/select-control'
import { StringControl } from '../../controls/string-control'

type ImageSrcType = 'url' | 'local'

function getProjectImageFileNames(projectContents: ProjectContents): string[] {
  return Object.keys(projectContents).filter((key) => isImageFile(projectContents[key]))
}

function srcTypeFromSrcValue(value: string, filenames: string[]): ImageSrcType {
  if (value.startsWith('./') && filenames.includes(value)) {
    return 'local'
  }
  return 'url'
}

const chainControlOptions: Array<SelectOption> = [
  { value: 'url', label: 'URL' },
  { value: 'local', label: 'Local' },
]

export const ImageSourceControl = React.memo(() => {
  const {
    value: srcValue,
    controlStyles: srcControlStyles,
    controlStatus: srcControlStatus,
    onSubmitValue: srcOnSubmitValue,
  } = useInspectorElementInfo('src')

  const dispatch = useEditorState((store) => store.dispatch, 'ImageSourceControl dispatch')
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'ImageSourceControl selectedElements',
  )

  const dispatchOverrideAction = React.useCallback(
    (newSrc: string) => {
      if (selectedElements.length === 0) {
        return
      }
      dispatch([
        setOverrideProp(
          selectedElements[0],
          PP.create(['src']),
          jsxAttributeValue(newSrc, emptyComments),
        ),
      ])
    },
    [dispatch, selectedElements],
  )

  const { projectContents } = useEditorState((store) => {
    return {
      projectContents: store.editor.projectContents,
    }
  }, 'ImgSection')

  const localImageFilenames = React.useMemo(() => {
    return getProjectImageFileNames(treeToContents(projectContents)).map(imagePathURL)
  }, [projectContents])

  const localImageFilesOptions = React.useMemo(() => {
    return localImageFilenames.map((filename) => ({ label: filename, value: filename }))
  }, [localImageFilenames])

  const [srcType, setSrcType] = React.useState<ImageSrcType>(
    srcTypeFromSrcValue(srcValue, localImageFilenames),
  )

  React.useEffect(() => {
    setSrcType(srcTypeFromSrcValue(srcValue, localImageFilenames))
  }, [srcValue, localImageFilenames])

  const onChangeSrcType = React.useCallback(
    (value: ImageSrcType) => {
      setSrcType(value)
    },
    [setSrcType],
  )

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 4 }}>
      <OptionChainControl
        id='image-density-control'
        key='image-density-control'
        testId='image-density-control'
        value={srcType}
        options={chainControlOptions}
        onSubmitValue={onChangeSrcType}
        controlStatus={srcControlStatus}
        controlStyles={srcControlStyles}
      />
      {srcType === 'local' ? (
        <SelectControl
          id='image-src-local'
          key='image-src-local'
          testId='image-src-local'
          value={srcValue}
          options={localImageFilesOptions}
          onSubmitValue={dispatchOverrideAction}
          controlStyles={srcControlStyles}
          controlStatus={srcControlStatus}
        />
      ) : (
        <StringControl
          id='image-src-url'
          key='image-src-url'
          testId='image-src-url'
          value={srcValue}
          onSubmitValue={dispatchOverrideAction}
          controlStyles={srcControlStyles}
          controlStatus={srcControlStatus}
        />
      )}
    </div>
  )
})
