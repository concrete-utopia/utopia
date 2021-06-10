/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { Global } from '@emotion/react'
import {
  PreviewReactSelectDeviceSelector,
  DeviceReactSelectOption,
  getDeviceReactSelectOption,
  calculatePreviewScale,
} from '../common/preview-devices'
import { DeviceInfo, deviceInfoList } from '../common/devices'

export type PreviewWindowProps = {
  id: string
}

export type PreviewWindowState = {
  scale: number
  scaleToFit: boolean
  deviceInfo: DeviceInfo
  width: number
  height: number
  fullscreenViewportOverride: boolean
}

export class PreviewWindow extends React.Component<PreviewWindowProps, PreviewWindowState> {
  private previewWrapperRef: React.RefObject<HTMLDivElement>
  private iFrameRef: React.RefObject<HTMLIFrameElement>
  private edgePaddingSetting = 20

  constructor(props: PreviewWindowProps) {
    super(props)

    this.previewWrapperRef = React.createRef<HTMLDivElement>()
    this.iFrameRef = React.createRef<HTMLIFrameElement>()

    this.state = {
      scale: 1,
      scaleToFit: true,
      deviceInfo: deviceInfoList.iPhoneXS,
      width: deviceInfoList.iPhoneXS.width as number,
      height: deviceInfoList.iPhoneXS.height as number,
      fullscreenViewportOverride: false,
    }
  }

  shouldBeFullscreen() {
    // eslint-disable-next-line no-restricted-globals
    const userAgentIsMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(navigator.userAgent)
    const isBelowThreshold = window.innerWidth <= this.state.width
    return userAgentIsMobile || isBelowThreshold
  }

  resizePreviewWindow = () => {
    if (this.previewWrapperRef.current != null) {
      const previewWindowDimensions = {
        width: this.previewWrapperRef.current.clientWidth,
        height: this.previewWrapperRef.current.clientHeight,
      }
      if (this.shouldBeFullscreen()) {
        this.setState({
          scale: 1,
          fullscreenViewportOverride: true,
        })
      } else {
        this.setState((previousState) => {
          const scale = calculatePreviewScale(
            previewWindowDimensions,
            this.edgePaddingSetting,
            previousState.width,
            previousState.height,
          )
          return {
            scale,
            fullscreenViewportOverride: false,
          }
        })
      }
    }
  }

  componentDidMount() {
    if (this.previewWrapperRef.current != null) {
      this.resizePreviewWindow()
      window.addEventListener('resize', this.resizePreviewWindow)
    }
  }

  onRestartClick = () => {
    if (this.iFrameRef.current != null && this.iFrameRef.current.contentWindow != null) {
      this.iFrameRef.current.contentWindow.location.reload()
    }
  }

  onDeviceChange = (newValue: DeviceReactSelectOption) => {
    if (this.previewWrapperRef.current != null) {
      const newDevice = deviceInfoList[newValue.value]
      const viewport = {
        width: this.previewWrapperRef.current.clientWidth,
        height: this.previewWrapperRef.current.clientHeight,
      }

      this.setState((previousState) => {
        const edgePadding = window.innerWidth > previousState.width ? this.edgePaddingSetting : 0
        return {
          scale: calculatePreviewScale(
            viewport,
            edgePadding,
            previousState.width,
            previousState.height,
          ),
          deviceInfo: newDevice,
          width: newDevice.width,
          height: newDevice.height,
          fullscreenViewportOverride: false,
        }
      })
    }
  }

  render() {
    const TopBarHeight = 44

    let previewBoxShadow: string
    let previewWrapperAlignItems: string
    let previewBorderRadius: number

    if (this.state.fullscreenViewportOverride) {
      previewBoxShadow = ''
      previewWrapperAlignItems = 'flex-start'
      previewBorderRadius = 0
    } else {
      previewBoxShadow = '0 3px 6px rgba(0,0,0,0.16), 0 3px 6px rgba(0,0,0,0.23)'
      previewWrapperAlignItems = 'center'
      previewBorderRadius = 4
    }

    const idString = this.props.id !== '' ? ` (${this.props.id})` : null

    const displayWidth = this.state.fullscreenViewportOverride ? '100%' : this.state.width
    const displayHeight = this.state.fullscreenViewportOverride ? '100%' : this.state.height
    const scaledDisplayWidth = this.state.fullscreenViewportOverride
      ? '100%'
      : this.state.width * this.state.scale
    const scaledDisplayHeight = this.state.fullscreenViewportOverride
      ? '100%'
      : this.state.height * this.state.scale

    return (
      <React.Fragment>
        <Global
          styles={{
            html: {
              height: '100%',
            },
            body: {
              margin: 0,
              height: '100%',
              overflow: 'hidden',
            },
            '@media screen and (max-width: 425px)': {
              '.preview-topbar': {
                display: 'none !important',
              },
            },
          }}
        />
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            height: '100%',
          }}
        >
          <div
            className='preview-topbar'
            css={{
              backgroundColor: '#F5F5F5',
              height: TopBarHeight,
              width: '100%',
              boxShadow: '0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)',
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              fontFamily:
                '-apple-system, BlinkMacSystemFont, Helvetica, "Segoe UI", Roboto,  Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
              fontSize: 12,
              lineHeight: '18px',
            }}
          >
            <div
              style={{
                margin: '0 5px',
              }}
            >
              Untitled{idString}
            </div>
            <div
              style={{
                margin: '0 5px',
                opacity: 0.25,
              }}
            >
              |
            </div>
            <div
              style={{
                margin: '0 5px',
                display: 'block',
                textAlign: 'center',
                height: 18,
              }}
            >
              <img
                src='/editor/icons/icon-preview-rotatePreview-vertical-gray-18x18@2x.png'
                width={18}
                height={18}
                style={{
                  marginRight: 5,
                }}
              />
              <div
                style={{
                  display: 'inline-block',
                  height: 18,
                }}
              >
                {
                  <PreviewReactSelectDeviceSelector
                    value={getDeviceReactSelectOption(this.state.deviceInfo.id)}
                    onChange={this.onDeviceChange}
                    caratOffset={7}
                  />
                }
              </div>
            </div>
            <div
              style={{
                margin: '0 5px',
                opacity: 0.25,
              }}
            >
              |
            </div>
            <div
              style={{
                margin: '0 5px',
                textAlign: 'center',
              }}
            >
              Scale{' '}
              {this.state.scale != null ? `(${Number((this.state.scale * 100).toFixed(2))}%)` : ''}
            </div>
            <div
              style={{
                margin: '0 5px',
                opacity: 0.25,
              }}
            >
              |
            </div>
            <div
              style={{
                margin: '0 5px',
              }}
            >
              <img
                onClick={this.onRestartClick}
                src='/editor/icons/icon-preview-restart-gray-18x18@2x.png'
                width={18}
                height={18}
                style={{
                  position: 'relative',
                  top: 3,
                }}
              />
            </div>
          </div>
          <div
            ref={this.previewWrapperRef}
            className='preview-wrapper'
            style={{
              display: 'flex',
              flex: 1,
              width: '100%',
              height: '100%',
              alignItems: previewWrapperAlignItems,
              justifyContent: 'center',
              overflow: 'hidden',
            }}
          >
            {!(this.state.scale != null) ? null : (
              <div
                className='preview-box'
                style={{
                  width: scaledDisplayWidth,
                  height: scaledDisplayHeight,
                  backgroundColor: '#d6d6d6',
                }}
              >
                <iframe
                  ref={this.iFrameRef}
                  width={displayWidth}
                  height={displayHeight}
                  src={`/share/${this.props.id}/`}
                  allow='autoplay'
                  style={{
                    width: displayWidth,
                    height: displayHeight,
                    borderWidth: 0,
                    borderRadius: previewBorderRadius,
                    boxShadow: previewBoxShadow,
                    zoom: this.state.scale,
                    transformOrigin: 'top left',
                  }}
                />
              </div>
            )}
          </div>
        </div>
      </React.Fragment>
    )
  }
}

export function PreviewPage({ projectId }: { projectId?: string }) {
  if (projectId != null) {
    return <PreviewWindow id={projectId} />
  } else {
    return null
  }
}
