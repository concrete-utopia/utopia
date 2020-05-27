/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import {
  Button,
  colorTheme,
  FlexColumn,
  FlexRow,
  Icn,
  LargerIcons,
  PopupList,
  Subdued,
  UtopiaTheme,
  SquareButton,
} from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { DeviceInfo, deviceInfoList } from '../../common/devices'
import { BASE_URL, FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { useEditorState } from '../editor/store/store-hook'
import { SelectOption } from '../inspector/controls/select-control'
import { isCodeFile, CodeFile } from '../../core/shared/project-file-types'
import { objectKeyParser, parseString } from '../../utils/value-parser-utils'
import { eitherToMaybe } from '../../core/shared/either'

export const PreviewIframeId = 'preview-column-container'

interface IntermediatePreviewColumnProps {
  id: string | null
  connected: boolean
  packageJSONFile: CodeFile | null
}

export interface PreviewColumnProps {
  id: string | null
  connected: boolean
  editedFilename: string | null
}

export interface PreviewColumnState {
  selectedBackgroundOptionIndex: number
  running: boolean
  useDevice: boolean
  scale: number
  selectedScaleOption: number
  deviceInfo: DeviceInfo
  rotated: boolean
  refreshCount: number
  width: number
  height: number
}

export const PreviewColumn = betterReactMemo('PreviewColumn', () => {
  const { id, connected, packageJSONFile }: IntermediatePreviewColumnProps = useEditorState(
    (store) => {
      const possiblePackageJSON = store.editor.projectContents['/package.json']
      return {
        id: store.editor.id,
        connected: store.editor.preview.connected,
        packageJSONFile: isCodeFile(possiblePackageJSON) ? possiblePackageJSON : null,
      }
    },
  )
  const props = React.useMemo(() => {
    let editedFilename: string | null = null
    if (packageJSONFile != null) {
      try {
        // Get the `utopia` -> `js` value out of the package.json file contents.
        const packageJSONContent = JSON.parse(packageJSONFile.fileContents)
        const jsFieldParser = objectKeyParser(objectKeyParser(parseString, 'js'), 'utopia')
        const parseResult = jsFieldParser(packageJSONContent)
        editedFilename = eitherToMaybe(parseResult)
      } catch (error) {
        console.error('Invalid package.json contents.', error)
      }
    }
    return {
      id: id,
      connected: connected,
      editedFilename: editedFilename,
    }
  }, [id, connected, packageJSONFile])
  return <PreviewColumnContent {...props} />
})
PreviewColumn.displayName = 'PreviewColumn'

class PreviewColumnContent extends React.Component<PreviewColumnProps, PreviewColumnState> {
  scaleOptions: number[]
  scaleDropdownOptions: any
  backgroundOptions: any
  constructor(props: PreviewColumnProps) {
    super(props)

    this.scaleOptions = [0.25, 0.5, 0.67, 1, 1.25, 1.5, 2, 3, 4]
    this.backgroundOptions = [
      {
        label: 'White',
        value: { backgroundColor: '#ffffff' },
      },
      {
        label: 'Checkerboard (light)',
        value: {
          backgroundImage:
            'linear-gradient(45deg, #d4d4d4 25%, transparent 25%), linear-gradient(-45deg, #d4d4d4 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #d4d4d4 75%), linear-gradient(-45deg, transparent 75%, #d4d4d4 75%)',
          backgroundSize: '10px 10px',
          backgroundPosition: '0 0, 0 5px, 5px -5px, -5px 0px',
        },
      },
      {
        label: 'Light Grey',
        value: { backgroundColor: '#f4f4f4' },
      },
      {
        label: 'Mid Grey',
        value: { backgroundColor: '#888888' },
      },
      { label: 'Dark Grey', value: { backgroundColor: '#333' } },
      { label: 'Black', value: { backgroundColor: '#000' } },
    ]

    this.scaleDropdownOptions = this.scaleOptions.map((s) => ({
      value: s,
      label: `${Math.round(s * 100)}%`,
    }))

    this.state = {
      selectedBackgroundOptionIndex: 0,
      running: true,
      useDevice: false,
      scale: 1,
      selectedScaleOption: 4,
      deviceInfo: deviceInfoList.iPhoneXS,
      rotated: false,
      refreshCount: 0,
      width: deviceInfoList.iPhoneXS.width as number,
      height: deviceInfoList.iPhoneXS.height as number,
    }
  }

  scaleDown = () => {
    this.setState((prevState) => ({ scale: Math.max(0.25, prevState.scale * 0.5) }))
  }

  scaleUp = () => {
    this.setState((prevState) => ({ scale: Math.min(8, prevState.scale * 2) }))
  }

  setSelectedValue = (selectedValue: SelectOption) => this.setState({ scale: selectedValue.value })

  render() {
    const ColorButtonGroup = () => (
      <FlexRow
        css={{
          marginLeft: 4,
          marginRight: 4,
          '& > *': {
            marginRight: 8,
          },
        }}
      >
        {this.backgroundOptions.map((background: any, i: number) => (
          <Button
            key={`colorbutton${i}`}
            // eslint-disable-next-line react/jsx-no-bind
            onClick={() => this.setState(() => ({ selectedBackgroundOptionIndex: i }))}
          >
            <div
              style={{
                width: 18,
                height: 18,
                borderRadius: '50%',
                border:
                  i === this.state.selectedBackgroundOptionIndex
                    ? `1px solid ${colorTheme.primary.value}`
                    : `1px solid ${colorTheme.secondaryBorder.value}`,
                ...background.value,
              }}
            />
          </Button>
        ))}
      </FlexRow>
    )

    const toggleRunning = () => {
      this.setState((prevState) => ({ running: !prevState.running }))
    }

    const onRestartClick = () => {
      this.setState((prevState) => ({
        refreshCount: prevState.refreshCount + 1,
      }))
    }

    const iFrame = (
      <iframe
        key={PreviewIframeId}
        id={PreviewIframeId}
        width={this.state.useDevice ? this.state.width : '100%'}
        height={this.state.useDevice ? this.state.height : '100%'}
        src={`${FLOATING_PREVIEW_BASE_URL}share/${this.props.id}?embedded=true&refreshCount=${this.state.refreshCount}`}
        allow='autoplay'
        style={{
          backgroundColor: 'transparent',
          width: this.state.useDevice ? this.state.width : '100%',
          height: this.state.useDevice ? this.state.height : '100%',
          borderWidth: 0,
          transform: `scale(${this.state.scale})`,
          transformOrigin: 'top left',
        }}
      />
    )

    const handleKeyPress = (e: React.KeyboardEvent) => {
      if (e.key === 'Enter') {
        e.stopPropagation
        onRestartClick()
      }
    }

    return (
      <FlexColumn
        style={{
          height: '100%',
          overflow: 'scroll',
        }}
      >
        <FlexRow
          css={{
            height: UtopiaTheme.layout.rowHeight.medium,
            flexShrink: 0,
            paddingLeft: 4,
            paddingRight: 4,
            overflowX: 'scroll',
            '& > *': {
              marginRight: 8,
            },
          }}
        >
          <SquareButton highlight onClick={onRestartClick}>
            <LargerIcons.Refresh color='black' />
          </SquareButton>
          <input
            onKeyPress={handleKeyPress}
            value={`${BASE_URL}share/${this.props.id}`}
            css={{
              fontSize: 11,
              borderRadius: '20px',
              height: 23,
              fontFamily: 'inter',
              transition: 'all .2s ease-in-out',
              padding: '2px 8px',
              border: '1px solid transparent',
              background: colorTheme.secondaryBackground.value,
              minWidth: 240,
              '&:focus': {
                background: 'white',
                border: `1px solid ${colorTheme.primary.value}`,
              },
            }}
          />
          {this.props.editedFilename == null ? null : (
            <Subdued>{this.props.editedFilename}</Subdued>
          )}
          <SquareButton highlight onClick={toggleRunning}>
            {this.state.running ? (
              <LargerIcons.StopButton color='black' />
            ) : (
              <LargerIcons.PlayButton color='black' />
            )}
          </SquareButton>
          <a
            target='_blank'
            rel='noopener noreferrer'
            href={`${FLOATING_PREVIEW_BASE_URL}share/${this.props.id}`}
          >
            <SquareButton highlight>
              <LargerIcons.ExternalLink color='black' />
            </SquareButton>
          </a>
        </FlexRow>
        <FlexRow
          className='preview-menu'
          style={{
            height: UtopiaTheme.layout.rowHeight.medium,
            borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
            flexShrink: 0,
            flexGrow: 0,
          }}
        >
          <FlexRow
            css={{
              height: UtopiaTheme.layout.rowHeight.medium,
              flexShrink: 0,
              paddingLeft: 4,
              paddingRight: 4,
              overflowX: 'scroll',
              '& > *': {
                marginRight: 8,
              },
            }}
          >
            <SquareButton
              highlight
              disabled={!this.state.running || this.state.scale <= 0.25}
              onMouseUp={this.scaleDown}
            >
              <LargerIcons.MagnifyingGlassMinus />
            </SquareButton>
            <PopupList
              style={{ minWidth: 56 }}
              value={{
                value: this.state.scale,
                label: `${Math.round(this.state.scale * 100)}%`,
              }}
              onSubmitValue={this.setSelectedValue}
              options={this.scaleDropdownOptions}
              containerMode='noBorder'
            />
            <SquareButton
              highlight
              disabled={!this.state.running || this.state.scale >= 4}
              onClick={this.scaleUp}
            >
              <LargerIcons.MagnifyingGlassPlus />
            </SquareButton>
            <LargerIcons.Divider width={5} height={18} style={{ marginLeft: 4, marginRight: 8 }} />
            <ColorButtonGroup />
          </FlexRow>
        </FlexRow>
        <FlexRow
          style={{
            ...this.backgroundOptions[this.state.selectedBackgroundOptionIndex].value,
            justifyContent: 'flex-start',
            alignItems: 'flex-start',
            flexGrow: 1,
            flexShrink: 0,
          }}
        >
          {this.state.running ? (
            iFrame
          ) : (
            <FlexColumn
              style={{
                justifyContent: 'center',
                alignItems: 'center',
                flexGrow: 1,
                flexShrink: 0,
                height: '100%',
                backgroundColor: colorTheme.secondaryBackground.value,
              }}
            >
              <Button onClick={toggleRunning} spotlight>
                <Icn category='semantic' type='playbutton' width={24} height={24} color='black' />
              </Button>
              <Subdued style={{ marginTop: 8 }}>Start Preview</Subdued>
            </FlexColumn>
          )}
        </FlexRow>
      </FlexColumn>
    )
  }
}
