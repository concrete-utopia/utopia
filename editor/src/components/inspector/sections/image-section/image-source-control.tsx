import React from 'react'
import { treeToContents } from '../../../../components/assets'
import { isImageFile } from '../../../../core/model/project-file-utils'
import { ProjectContents } from '../../../../core/shared/project-file-types'
import { useEditorState } from '../../../editor/store/store-hook'
import { useInspectorElementInfo } from '../../common/property-path-hooks'
import { OptionChainControl } from '../../controls/option-chain-control'
import { SelectControl, SelectOption } from '../../controls/select-control'
import { StringControl } from '../../controls/string-control'

enum ImageSrcType {
  URL = 1,
  LOCAL,
}

const getProjectImageFileNames = (projectContents: ProjectContents): string[] => {
  return Object.keys(projectContents).filter((key) => isImageFile(projectContents[key]))
}

const isImgSrcLocal = (src: string) => {
  return src.startsWith('./')
}

const chainControlOptions: Array<SelectOption> = [
  { value: ImageSrcType.URL, label: 'URL' },
  { value: ImageSrcType.LOCAL, label: 'Local' },
]

export const ImageSourceControl = React.memo(() => {
  const {
    value: srcValue,
    controlStyles: srcControlStyles,
    controlStatus: srcControlStatus,
    onSubmitValue: srcOnSubmitValue,
  } = useInspectorElementInfo('src')

  const [srcType, setSrcType] = React.useState<ImageSrcType>(
    isImgSrcLocal(srcValue) ? ImageSrcType.LOCAL : ImageSrcType.URL,
  )

  const { projectContents } = useEditorState((store) => {
    return {
      projectContents: store.editor.projectContents,
    }
  }, 'ImgSection')

  const localImageFiles = React.useMemo(() => {
    return getProjectImageFileNames(treeToContents(projectContents)).map((filename) => {
      const localFilename = `.${filename}` // prepending '.' to the absolute path to make it reference project files correctly
      return {
        label: localFilename,
        value: localFilename,
      }
    })
  }, [projectContents])

  const onChangeSrcType = React.useCallback(
    (value: number) => {
      setSrcType(value === ImageSrcType.LOCAL ? ImageSrcType.LOCAL : ImageSrcType.URL)
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
      {srcType === ImageSrcType.LOCAL ? (
        <SelectControl
          id='image-src-local'
          key='image-src-local'
          testId='image-src-local'
          value={srcValue}
          options={localImageFiles}
          onSubmitValue={srcOnSubmitValue}
          controlStyles={srcControlStyles}
          controlStatus={srcControlStatus}
        />
      ) : (
        <StringControl
          id='image-src-url'
          key='image-src-url'
          testId='image-src-url'
          value={srcValue}
          onSubmitValue={srcOnSubmitValue}
          controlStyles={srcControlStyles}
          controlStatus={srcControlStatus}
        />
      )}
    </div>
  )
})
