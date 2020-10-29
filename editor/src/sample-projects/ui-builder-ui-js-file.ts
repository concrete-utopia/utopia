import {
  RevisionsState,
  textFile,
  TextFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'
import { lintAndParse } from '../core/workers/parser-printer/parser-printer'

// FIXME I've disabled the below because Jest can't deal with it, but we're not using it anyway
// This needs fixing if we want to include the uuiui folder in the project

// const uuiuiFolder = require('!!val-loader!../uuiui/loader.js')

// export function getUuiUiFolder() {
//   return utils.objectMap((projectFile, fileName) => {
//     if (isTextFile(projectFile)) {
//       return {
//         ...projectFile,
//         fileContents: atob(projectFile.fileContents), // the uuiuiFolder stores the file contents as base64 strings
//       }
//     } else {
//       return projectFile
//     }
//   }, uuiuiFolder)
// }

export function getUiBuilderUIJSFile(): TextFile {
  const result = lintAndParse('code.tsx', sampleCode)
  return textFile(textFileContents(sampleCode, result, RevisionsState.BothMatch), null, 0)
}

export const sampleCode = `/** @jsx jsx */
import * as React from 'react'
import {
  Ellipse,
  Image,
  Rectangle,
  Text,
  UtopiaUtils,
  View,
  jsx,
} from 'utopia-api'
import {
  colorTheme,
  Button,
  Dialog,
  Icn,
  Icons,
  LargerIcons,
  FunctionIcons,
  MenuIcons,
  Isolator,
  TabComponent,
  Tooltip,
  ActionSheet,
  Avatar,
  ControlledTextArea,
  Title,
  H1,
  H2,
  H3,
  Subdued,
  InspectorSectionHeader,
  InspectorSubsectionHeader,
  FlexColumn,
  FlexRow,
  ResizableFlexColumn,
  PopupList,
  Section,
  SectionTitleRow,
  SectionBodyArea,
  UtopiaListSelect,
  UtopiaListItem,
  CheckboxInput,
  NumberInput,
  StringInput,
  OnClickOutsideHOC,
} from 'uuiui'

export var App = (props) => {
  return (
    <View
      style={{ ...props.style, backgroundColor: colorTheme.white.value }}
      layout={{ layoutSystem: 'pinSystem' }}
    ></View>
  )
}

`

export function getSampleComponentsFile(): TextFile {
  return textFile(
    textFileContents(sampleComponentsFile, unparsed, RevisionsState.BothMatch),
    null,
    0,
  )
}

const sampleComponentsFile = `// component library
import * as React from 'react'
import { Text, View } from 'utopia-api'

export default (props) => (
  <View layout={props.layout} style={props.style} onMouseDown={props.onMouseDown}>
    <Text
      style={{ fontSize: 16, textAlign: 'center' }}
      text={props.text}
      layout={{
        left: 0,
        top: 10,
        width: '100%',
        height: '100%',
      }}
      textSizing={'fixed'}
    />
  </View>
)

export const LABEL = 'press me! 😉'

export const ComponentWithProps = (props) => {
  return (
    <div
      style={{
        ...props.style,
        backgroundColor: props.pink ? 'hotpink' : 'transparent',
        whiteSpace: 'normal',
      }}
    >
      {(props.text + ' ').repeat(props.num)}
    </div>
  )
}

ComponentWithProps.propertyControls = {
  text: {
    type: 'string',
    title: 'Title',
    defaultValue: 'Change me',
  },
  num: {
    type: 'number',
    title: 'amount',
    defaultValue: 2,
  },
  pink: {
    type: 'boolean',
    title: 'Enabled',
    defaultValue: true,
  },
}

`
