import { mapArrayToDictionary } from '../../../core/shared/array-utils'
import type { IcnColor } from '../../../uuiui'
import { colorTheme } from '../../../uuiui/styles/theme'
import type { ControlStatus } from './control-status'

export interface ControlStyles {
  fontStyle: string
  fontWeight: number
  mainColor: string
  secondaryColor: string
  borderColor: string
  backgroundColor: string
  focusedBackgroundColor: string
  strokePrimaryColor: string
  strokeSecondaryColor: string
  strokeTertiaryColor: string
  segmentSelectorColor: string
  set: boolean
  interactive: boolean
  mixed: boolean
  invalid: boolean
  unknown: boolean
  trackColor: string
  railColor: string
  showContent: boolean
  unsettable: boolean
  iconColor: IcnColor
}

export const AllControlStatuses: Array<ControlStatus> = [
  'off',
  'simple',
  'simple-unknown-css',
  'unset',
  'disabled',
  'unoverwritable',
  'controlled',
  'detected-fromcss',
  'detected',
  'trivial-default',
  'overridden',
  'multiselect-identical-simple',
  'multiselect-simple-unknown-css',
  'multiselect-identical-unset',
  'multiselect-mixed-simple-or-unset',
  'multiselect-controlled',
  'multiselect-detected-fromcss',
  'multiselect-detected',
  'multiselect-unoverwritable',
  'multiselect-disabled',
  'multiselect-trivial-default',
]

const controlStylesByStatus: { [key: string]: ControlStyles } = mapArrayToDictionary(
  AllControlStatuses,
  (status: ControlStatus) => status,
  (status: ControlStatus): ControlStyles => {
    let fontStyle = 'normal'
    let fontWeight = 400
    let mainColor: string = colorTheme.fg1.value
    let secondaryColor: string = colorTheme.fg7.value

    let borderColor: string = 'transparent'
    // text inputs
    let backgroundColor: string = colorTheme.bg2.value
    let focusedBackgroundColor: string = colorTheme.bg0.value
    let segmentSelectorColor: string = colorTheme.bg3.value
    let trackColor = colorTheme.fg7.value
    let railColor = colorTheme.bg3.value
    let strokePrimaryColor = colorTheme.fg5.value
    let strokeSecondaryColor = colorTheme.fg7.value
    let strokeTertiaryColor = colorTheme.fg9.value
    let set = true
    let interactive = true
    let mixed = false
    let unknown = false
    let showContent = true
    let unsettable = true
    let invalid = false
    let iconColor: IcnColor = 'main'

    switch (status) {
      case 'simple':
      case 'multiselect-identical-simple':
        break
      case 'detected':
      case 'multiselect-detected':
      case 'unset':
        set = false
        unsettable = false
        mainColor = 'var(--control-styles-interactive-unset-secondary-color)'
        secondaryColor = 'var(--control-styles-interactive-unset-secondary-color)'
        break
      case 'multiselect-identical-unset':
        set = false
        unsettable = false
        // this makes it interactive if the component provides these values
        mainColor = 'var(--control-styles-interactive-unset-main-color)'
        secondaryColor = 'var(--control-styles-interactive-unset-secondary-color)'
        trackColor = 'var(--control-styles-interactive-unset-track-color)'
        railColor = 'var(--control-styles-interactive-unset-rail-color)'
        break
      case 'multiselect-mixed-simple-or-unset':
        mixed = true
        interactive = true
        showContent = true
        mainColor = colorTheme.fg6Opacity50.value
        secondaryColor = colorTheme.fg6Opacity50.value
        trackColor = colorTheme.fg6Opacity50.value
        strokePrimaryColor = colorTheme.fg6Opacity50.value
        break
      case 'controlled':
      case 'multiselect-controlled':
        interactive = true
        mainColor = colorTheme.dynamicBlue.value
        secondaryColor = colorTheme.dynamicBlue.value
        trackColor = colorTheme.dynamicBlue.value
        strokePrimaryColor = colorTheme.dynamicBlue.value
        showContent = true
        break
      case 'trivial-default':
      case 'multiselect-trivial-default':
        interactive = true
        showContent = false
        mainColor = colorTheme.fg7.value
        secondaryColor = colorTheme.fg7.value
        trackColor = colorTheme.bg5.value
        railColor = colorTheme.bg3.value
        break
      case 'detected-fromcss':
      case 'multiselect-detected-fromcss':
        mainColor = colorTheme.css.value
        secondaryColor = colorTheme.css.value
        trackColor = colorTheme.css.value
        break
      case 'simple-unknown-css':
      case 'multiselect-simple-unknown-css':
        unknown = true
        interactive = true
        mainColor = 'orange'
        secondaryColor = 'orange'
        trackColor = 'orange'
        backgroundColor = 'transparent'
        break
      case 'unoverwritable':
      case 'multiselect-unoverwritable':
        interactive = false
        mainColor = colorTheme.fg7.value
        secondaryColor = colorTheme.fg8.value
        trackColor = colorTheme.fg8.value
        railColor = colorTheme.fg8.value
        unsettable = false
        break

      case 'off':
        set = false
        interactive = false
        mainColor = colorTheme.fg7.value
        secondaryColor = colorTheme.fg7.value
        trackColor = colorTheme.fg7.value
        showContent = false
        unsettable = false
        break
      case 'disabled':
      case 'multiselect-disabled':
        interactive = false
        mainColor = colorTheme.fg7.value
        secondaryColor = colorTheme.fg7.value
        backgroundColor = 'transparent'
        segmentSelectorColor = 'transparent'
        trackColor = 'transparent'
        showContent = true
        unsettable = false
        break
      case 'overridden':
        interactive = true
        mainColor = colorTheme.brandNeonPink.value
        borderColor = colorTheme.brandNeonPink.value
        secondaryColor = colorTheme.primary.value
        trackColor = colorTheme.primary.value
        strokePrimaryColor = colorTheme.primary.value
        showContent = true
        iconColor = 'overridden'
        break
      default:
        break
    }

    return {
      fontStyle,
      fontWeight,
      mainColor,
      secondaryColor,
      borderColor,
      backgroundColor,
      focusedBackgroundColor,
      segmentSelectorColor,
      strokePrimaryColor,
      strokeSecondaryColor,
      strokeTertiaryColor,
      trackColor,
      railColor,
      set,
      interactive,
      mixed,
      showContent,
      unknown,
      unsettable,
      iconColor,
      invalid,
    }
  },
)

export function getControlStyles(controlStatus: ControlStatus): ControlStyles {
  return controlStylesByStatus[controlStatus]
}
