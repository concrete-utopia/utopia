/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { ResizeCallback } from 're-resizable'
import { Resizable } from 're-resizable'
import React from 'react'
import type { ErrorMessage, ErrorMessageSeverity } from '../../core/shared/error-messages'
import { messageIsFatalOrError, messageIsWarning } from '../../core/shared/error-messages'
import type { CursorPosition } from './code-editor-utils'
import { clampValue } from '../../core/shared/math-utils'
import { WarningIcon } from '../../uuiui/warning-icon'
import { VariableSizeList as List } from 'react-window'
import { useColorTheme } from '../../uuiui/styles/theme'
import { FlexRow } from '../../uuiui/widgets/layout/flex-row'
import { NO_OP } from '../../core/shared/utils'
import { TabComponent } from '../../uuiui/tab'
import { Icons } from '../../uuiui/icons'
import { SimpleFlexColumn } from '../../uuiui/widgets/layout/flex-column'
import { UIRow } from '../../uuiui'
import { groupBy } from '../../core/shared/array-utils'

interface ErrorMessageRowProps {
  errorMessage: ErrorMessage
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

const ErrorMessageRowHeight = 31

const ErrorMessageRow = (props: ErrorMessageRowProps) => {
  const colorTheme = useColorTheme()
  const { onOpenFile } = props
  const { fileName, startLine, startColumn } = props.errorMessage

  const onClick = React.useCallback(() => {
    const cursorPositon =
      startLine == null || startColumn == null
        ? null
        : {
            line: startLine,
            column: startColumn,
          }
    onOpenFile(fileName, cursorPositon)
  }, [onOpenFile, fileName, startLine, startColumn])

  const isSourceKnown = props.errorMessage.fileName != ''

  return (
    <FlexRow
      css={{
        height: ErrorMessageRowHeight,
        flexGrow: 1,
        color: colorTheme.neutralForeground.value,
        fontSize: 12,
        paddingTop: 6,
        paddingBottom: 6,
        paddingLeft: 20,
        paddingRight: 8,
        cursor: isSourceKnown ? 'pointer' : 'default',
        '&:hover': {
          background: colorTheme.emphasizedBackground.value,
        },
      }}
      onClick={isSourceKnown ? onClick : NO_OP}
    >
      <WarningIcon color={errorMessageSeverityToColor(props.errorMessage.severity)} />
      <span style={{ marginLeft: 4, userSelect: 'text' }}>{props.errorMessage.source}: </span>
      <span style={{ marginLeft: 4, userSelect: 'text' }}>{props.errorMessage.message}</span>
      <span
        style={{
          marginLeft: 4,
          color: colorTheme.subduedForeground.value,
        }}
      >
        {isSourceKnown
          ? `${props.errorMessage.startLine}:${props.errorMessage.startColumn}, ${props.errorMessage.fileName}`
          : null}
      </span>
    </FlexRow>
  )
}

function errorMessageSeverityToColor(severity: ErrorMessageSeverity) {
  switch (severity) {
    case 'warning':
      return 'warning'
    case 'error':
    case 'fatal':
      return 'error'
    default:
      const _exhaustiveCheck: never = severity
      throw new Error(`Unknown severity ${severity}}`)
  }
}

function getTabStyleForErrors(
  errorMessages: Array<ErrorMessage>,
  colorTheme: any,
): { backgroundColor: string } {
  const errorStyle = { backgroundColor: colorTheme.error.value }
  const warningStyle = { backgroundColor: colorTheme.warningBgSolid.value }
  const defaultStyle = { backgroundColor: colorTheme.fg7.value }

  const isFatalOrError = errorMessages.some(messageIsFatalOrError)
  const isWarning = errorMessages.some(messageIsWarning)

  if (isFatalOrError) {
    return errorStyle
  } else if (isWarning) {
    return warningStyle
  }
  return defaultStyle
}

interface CodeEditorTabPaneProps {
  errorMessages: Array<ErrorMessage>
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

const ProblemRowHeight = 29
const ProblemTabBarHeight = 32

type OpenCodeEditorTab = 'problems'

export const CodeEditorTabPane = React.memo<CodeEditorTabPaneProps>(
  ({ errorMessages, onOpenFile }) => {
    const colorTheme = useColorTheme()
    const defaultHeightWhenOpen =
      ProblemTabBarHeight + ProblemRowHeight * clampValue(errorMessages.length, 3, 10)
    const [userDefinedHeightWhenOpen, setHeightWhenOpen] = React.useState<number | null>(null)
    const [isOpen, setIsOpen] = React.useState(false)
    const resizableRef = React.useRef<Resizable>(null)
    const heightWhenOpen = userDefinedHeightWhenOpen ?? defaultHeightWhenOpen

    const toggleIsOpen = React.useCallback(() => {
      setIsOpen((value) => {
        const newValue = !value
        if (resizableRef.current != null) {
          resizableRef.current.updateSize({
            height: newValue ? heightWhenOpen : ProblemTabBarHeight,
            width: resizableRef.current.size.width,
          })
        }
        return newValue
      })
    }, [heightWhenOpen])

    const onResize: ResizeCallback = React.useCallback((_, __, elementRef) => {
      if (elementRef.clientHeight > ProblemTabBarHeight) {
        setHeightWhenOpen(elementRef.clientHeight)
        setIsOpen(true)
      } else {
        setIsOpen(false)
      }
    }, [])

    const [selectedTab, setSelectedTab] = React.useState<OpenCodeEditorTab>('problems')

    const selectProblemsTab = React.useCallback(() => {
      if (!isOpen) {
        toggleIsOpen()
      }
      setSelectedTab('problems')
    }, [setSelectedTab, isOpen, toggleIsOpen])

    const problemsTabBackgroundColor = getTabStyleForErrors(
      errorMessages,
      colorTheme,
    ).backgroundColor
    const ProblemsTabLabel = React.useMemo(() => {
      return (
        <span>
          Problems
          <span
            style={{
              marginLeft: 8,
              fontSize: 10,
              padding: '1px 5px',
              borderRadius: 2,
              fontWeight: 500,
              color: colorTheme.neutralInvertedForeground.value,
              backgroundColor: problemsTabBackgroundColor,
            }}
          >
            {errorMessages.length}
          </span>
        </span>
      )
    }, [colorTheme, problemsTabBackgroundColor, errorMessages.length])

    function getTabContents() {
      switch (selectedTab) {
        case 'problems':
          return (
            <ProblemsTab
              errorMessages={errorMessages}
              height={heightWhenOpen}
              onOpenFile={onOpenFile}
            />
          )
        default:
          return null
      }
    }

    return (
      <Resizable
        ref={resizableRef}
        defaultSize={{ height: ProblemTabBarHeight + (isOpen ? heightWhenOpen : 0), width: '100%' }}
        minHeight={ProblemTabBarHeight}
        onResize={onResize}
        enable={{
          top: true,
        }}
        style={{
          backgroundColor: colorTheme.neutralBackground.value,
          flexGrow: 0,
        }}
      >
        <UIRow
          style={{
            alignItems: 'stretch',
            height: 32,
          }}
        >
          <TabComponent
            onClick={selectProblemsTab}
            onDoubleClick={toggleIsOpen}
            selected={selectedTab === 'problems'}
            showCloseIndicator={false}
            showModifiedIndicator={false}
            label={ProblemsTabLabel}
          />
        </UIRow>
        {isOpen ? getTabContents() : null}
      </Resizable>
    )
  },
)

interface ProblemsTabProps {
  errorMessages: Array<ErrorMessage>
  height: number
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

interface ProblemsHeaderRowData {
  type: 'HEADER'
  fileName: string
}

interface ProblemsErrorRowData {
  type: 'ERROR_MESSAGE'
  fileName: string
  errorMessage: ErrorMessage
}

type ProblemRowData = ProblemsHeaderRowData | ProblemsErrorRowData

interface ProblemsHeaderRowProps {
  fileName: string
}

interface ProblemsErrorRowProps {
  errorMessage: ErrorMessage
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

const ProblemsHeaderRowHeight = 25

const ProblemsHeaderRow = React.memo((props: ProblemsHeaderRowProps) => {
  const { fileName } = props
  return (
    <FlexRow style={{ height: ProblemsHeaderRowHeight, padding: '4px 8px' }}>
      <Icons.React color='dynamic' />
      <span style={{ marginLeft: 4 }}> {fileName}</span>
    </FlexRow>
  )
})

const ProblemsErrorRow = React.memo((props: ProblemsErrorRowProps) => {
  const { errorMessage, onOpenFile } = props
  return <ErrorMessageRow errorMessage={errorMessage} onOpenFile={onOpenFile} />
})

function getRowHeight(index: number, data: Array<ProblemRowData>): number {
  const row = data[index]
  if (row == null) {
    return 0
  } else if (row.type === 'HEADER') {
    return ProblemsHeaderRowHeight
  } else {
    return ErrorMessageRowHeight
  }
}

const ProblemsTab = React.memo((props: ProblemsTabProps) => {
  const { errorMessages, height, onOpenFile } = props

  const rowData: Array<ProblemRowData> = React.useMemo(() => {
    const errorsByFile = groupBy((error: ErrorMessage) => error.fileName, errorMessages)

    let rows: Array<ProblemRowData> = []
    Object.keys(errorsByFile).forEach(function (fileName) {
      if (fileName != '') {
        rows.push({
          type: 'HEADER',
          fileName: fileName,
        })
      }
      errorsByFile[fileName].forEach(function (errorMessage) {
        rows.push({
          type: 'ERROR_MESSAGE',
          fileName: fileName,
          errorMessage: errorMessage,
        })
      })
    })

    return rows
  }, [errorMessages])

  const getItemSize = React.useCallback(
    (index: number) => {
      const row = rowData[index]
      if (row == null) {
        return 0
      } else if (row.type === 'HEADER') {
        return ProblemsHeaderRowHeight
      } else {
        return ErrorMessageRowHeight
      }
    },
    [rowData],
  )

  const Row = React.useCallback(
    ({ index, style }: any) => {
      const row = rowData[index]
      if (row == null) {
        return null
      } else if (row.type === 'HEADER') {
        return (
          <div style={style} key={`error-row-${row.fileName}-filename`}>
            <ProblemsHeaderRow fileName={row.fileName} />
          </div>
        )
      } else {
        return (
          <div
            style={style}
            key={`error-row-${row.fileName}-${index}-${row.errorMessage.startLine}-${row.errorMessage.startColumn}`}
          >
            <ProblemsErrorRow errorMessage={row.errorMessage} onOpenFile={onOpenFile} />
          </div>
        )
      }
    },
    [rowData, onOpenFile],
  )

  return (
    <SimpleFlexColumn>
      <List height={height} itemCount={rowData.length} itemSize={getItemSize} width={'100%'}>
        {Row}
      </List>
    </SimpleFlexColumn>
  )
})
