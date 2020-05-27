export interface DeviceInfo {
  id: DeviceID
  prettyName: string
  width: number
  height: number
  scaling: number
}

export type DeviceID = 'iPhoneXSMax' | 'iPhoneXS' | 'iPhone8' | 'iPhoneSE'
export type DeviceIDOrCustom = DeviceID | 'custom'

export type DeviceOrientation = 'landscape' | 'portrait' | 'square'

export type DeviceInfoList = {
  [key in DeviceID]: DeviceInfo
}

export const defaultDeviceID: DeviceID = 'iPhoneXS'
export const customDeviceID: DeviceIDOrCustom = 'custom'

export const deviceInfoList: DeviceInfoList = {
  iPhoneXS: {
    id: 'iPhoneXS',
    prettyName: 'iPhone XS',
    width: 375,
    height: 812,
    scaling: 2,
  },
  iPhoneXSMax: {
    id: 'iPhoneXSMax',
    prettyName: 'iPhone XS Max',
    width: 414,
    height: 896,
    scaling: 3,
  },
  iPhone8: {
    id: 'iPhone8',
    prettyName: 'iPhone 8',
    width: 375,
    height: 667,
    scaling: 2,
  },
  iPhoneSE: {
    id: 'iPhoneSE',
    prettyName: 'iPhone SE',
    width: 320,
    height: 568,
    scaling: 2,
  },
}
