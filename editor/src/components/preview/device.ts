import Utils from '../../utils/utils'
import { CanvasRectangle } from '../../core/shared/math-utils'

export type DeviceModel = {
  key: string
  description: string
  backgroundImage: string | null
  maskImage: string | null
  scale: number
  frame: CanvasRectangle
  viewport: CanvasRectangle
  showFakeOSOverlay: boolean
  showMaskOverlay: boolean
}

export const Devices: { [key: string]: DeviceModel } = {
  iPhone7: {
    key: 'iphone7',
    description: 'iPhone 7 - Matt Black',
    backgroundImage: 'url(/editor/iphone_7_matt_black_shadow.png)',
    maskImage: 'none',
    showFakeOSOverlay: false,
    showMaskOverlay: false,
    frame: {
      x: 0,
      y: 0,
      width: 495,
      height: 967,
    } as CanvasRectangle,
    viewport: {
      x: 60,
      y: 150,
      width: 375,
      height: 667,
    } as CanvasRectangle,
    scale: 1,
  },
  iPhone7white: {
    key: 'iPhone7white',
    description: 'iPhone 7 - Silver',
    backgroundImage: 'url(/editor/apple_iphone_7_silver_shadow.png)',
    maskImage: 'none',
    showFakeOSOverlay: false,
    showMaskOverlay: false,
    frame: {
      x: 0,
      y: 0,
      width: 495,
      height: 967,
    } as CanvasRectangle,
    viewport: {
      x: 60,
      y: 150,
      width: 375,
      height: 667,
    } as CanvasRectangle,
    scale: 1,
  },
  iPhoneXblack: {
    key: 'iPhoneX',
    description: 'iPhone X',
    backgroundImage: 'url(/editor/apple_iphone_x_black_shadow.png)',
    maskImage: 'url(/editor/apple_iphone_x_mask.svg)',
    showFakeOSOverlay: false,
    showMaskOverlay: false,
    frame: {
      x: 0,
      y: 0,
      width: 451,
      height: 864,
    } as CanvasRectangle,
    viewport: {
      x: 38,
      y: 26,
      width: 375,
      height: 812,
    } as CanvasRectangle,
    scale: 1,
  },
}

export const Device = {
  fitIn: function (area: CanvasRectangle, device: DeviceModel): DeviceModel {
    let scale = Math.min(area.width / device.frame.width, area.height / device.frame.height)
    const scaledFrame = Utils.scaleRect(device.frame, scale)

    return {
      ...device,
      scale: scale,
      frame: {
        x: (area.width - scaledFrame.width) / 2 + scaledFrame.x,
        y: (area.height - scaledFrame.height) / 2 + scaledFrame.y,
        width: scaledFrame.width,
        height: scaledFrame.height,
      } as CanvasRectangle,
      viewport: Utils.scaleRect(device.viewport, scale),
    }
  },

  asScreenOnly: function (device: DeviceModel): DeviceModel {
    const viewport = {
      width: device.viewport.width,
      height: device.viewport.height,
      x: 0,
      y: 0,
    } as CanvasRectangle
    return {
      key: device.key,
      description: device.description,
      backgroundImage: null,
      maskImage: 'url(apple_iphone_x_mask.svg)',
      scale: device.scale,
      frame: viewport,
      viewport: viewport,
      showFakeOSOverlay: false,
      showMaskOverlay: viewport.width === 375 && viewport.height === 812,
    }
  },
}
