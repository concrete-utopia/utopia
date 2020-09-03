/** @jsx jsx */
import { jsx } from '@emotion/core'
import { Console } from 'console-feed'
import * as R from 'ramda'
import { Resizable, ResizeCallback } from 're-resizable'
import * as React from 'react'
import { colorTheme, FlexRow, Icons, TabComponent, UtopiaTheme, SimpleFlexColumn } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import {
  ErrorMessage,
  ErrorMessageSeverity,
  messageIsFatalOrError,
  messageIsWarning,
} from '../../core/shared/error-messages'
import { Utils } from '../../uuiui-deps'
import { ConsoleLog } from '../editor/store/editor-state'
import { CursorPosition } from './code-editor-utils'
import { clampValue } from '../../core/shared/math-utils'
import { WarningIcon } from '../../uuiui/warning-icon'

interface ErrorMessageRowProps {
  errorMessage: ErrorMessage
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

const ErrorMessageRow = (props: ErrorMessageRowProps) => {
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
      onClick={isSourceKnown ? onClick : Utils.NO_OP}
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
      return 'orange'
    case 'error':
    case 'fatal':
      return 'red'
    default:
      const _exhaustiveCheck: never = severity
      throw new Error(`Unknown severity ${severity}}`)
  }
}

function getTabStyleForErrors(errorMessages: Array<ErrorMessage>): { backgroundColor: string } {
  const errorStyle = { backgroundColor: colorTheme.errorBgSolid.value }
  const warningStyle = { backgroundColor: colorTheme.warningBgSolid.value }
  const defaultStyle = { backgroundColor: colorTheme.subtleBackground.value }

  const isFatalOrError = errorMessages.some(messageIsFatalOrError)
  const isWarning = errorMessages.some(messageIsWarning)

  if (isFatalOrError) {
    return errorStyle
  } else if (isWarning) {
    return warningStyle
  }
  return defaultStyle
}

function getTabStyleForLogs(canvasConsoleLogs: Array<ConsoleLog>): { backgroundColor: string } {
  const errorStyle = { backgroundColor: colorTheme.errorBgSolid.value }
  const warningStyle = { backgroundColor: colorTheme.warningBgSolid.value }
  const defaultStyle = { backgroundColor: 'grey' }

  const isError = canvasConsoleLogs.some((log) => {
    return log.method === 'error'
  })
  const isWarning = canvasConsoleLogs.some((log) => {
    return log.method === 'warn'
  })

  if (isError) {
    return errorStyle
  } else if (isWarning) {
    return warningStyle
  }
  return defaultStyle
}

interface CodeEditorTabPaneProps {
  errorMessages: Array<ErrorMessage>
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
  canvasConsoleLogs: Array<ConsoleLog>
}

const ProblemRowHeight = UtopiaTheme.layout.rowHeight.smaller
const ProblemTabBarHeight = UtopiaTheme.layout.rowHeight.small

type OpenCodeEditorTab = 'problems' | 'console'

export const CodeEditorTabPane = betterReactMemo<CodeEditorTabPaneProps>(
  'CodeEditorTabPane',
  ({ errorMessages, onOpenFile, canvasConsoleLogs }) => {
    const defaultHeightWhenOpen =
      ProblemTabBarHeight +
      ProblemRowHeight * clampValue(Math.max(errorMessages.length, canvasConsoleLogs.length), 3, 10)
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

    const onResizeStop: ResizeCallback = React.useCallback((_, __, elementRef) => {
      if (elementRef.clientHeight > ProblemTabBarHeight) {
        setHeightWhenOpen(elementRef.clientHeight)
        setIsOpen(true)
      } else {
        setIsOpen(false)
      }
    }, [])

    let outputRows: Array<React.ReactElement> = []
    const groupErrors = R.groupBy((error: ErrorMessage) => error.fileName)
    const errorsByFile = groupErrors(errorMessages)

    Object.keys(errorsByFile).forEach(function (fileName) {
      if (fileName != '') {
        outputRows.push(
          <FlexRow key={`error-row-${fileName}-filename`} style={{ padding: '4px 8px' }}>
            <Icons.React />
            <span style={{ marginLeft: 4 }}> {fileName}</span>
          </FlexRow>,
        )
      }
      errorsByFile[fileName].forEach(function (errormessage, index) {
        outputRows.push(
          <ErrorMessageRow
            key={`error-row-${fileName}-${index}-${errormessage.startLine}-${errormessage.startColumn}`}
            errorMessage={errormessage}
            onOpenFile={onOpenFile}
          />,
        )
      })
    })

    const [selectedTab, setSelectedTab] = React.useState<OpenCodeEditorTab>('problems')

    const selectProblemsTab = React.useCallback(() => {
      if (!isOpen) {
        toggleIsOpen()
      }
      setSelectedTab('problems')
    }, [setSelectedTab, isOpen, toggleIsOpen])

    const selectConsoleTab = React.useCallback(() => {
      if (!isOpen) {
        toggleIsOpen()
      }
      setSelectedTab('console')
    }, [setSelectedTab, isOpen, toggleIsOpen])

    function getTabContents() {
      switch (selectedTab) {
        case 'problems':
          return (
            <SimpleFlexColumn style={{ overflowY: 'scroll', overscrollBehavior: 'contain' }}>
              {outputRows.length > 0 ? (
                outputRows
              ) : (
                <div style={{ padding: 8 }}>There are no problems 😎</div>
              )}
            </SimpleFlexColumn>
          )
        case 'console':
          return (
            <div
              className='label-consolewrapper overflow-y-scroll'
              // we need increased specificity because of our global settings for user-selection,
              // and console-feed 2.8x doesn't allow for style injection, despite the docs.
              css={{
                '& *': {
                  userSelect: 'text',
                  WebkitUserSelect: 'text',
                  cursor: 'text',
                },
              }}
              style={{
                backgroundColor: colorTheme.emphasizedInvertedBackground.value,
                color: 'white',
                height: '100%',
                // There probably is a better fix but I've run out of goats to sacrifice
                paddingBottom: ProblemTabBarHeight,
                overscrollBehavior: 'contain',
                scrollSnapType: 'y proximity',
              }}
            >
              <Console
                logs={canvasConsoleLogs}
                variant={'dark'}
                styles={{
                  BASE_FONT_FAMILY: 'utopian-inconsolata, mono',
                }}
              />
              {/* since we can't know the last console item as logged in console,
                we attach a cheat anchor here
              */}
              <span
                style={{
                  width: 0,
                  height: 0,
                  display: 'block',
                  scrollSnapAlign: 'end',
                  scrollMarginBlockEnd: '50px',
                }}
              />
            </div>
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
        onResizeStop={onResizeStop}
        enable={{
          top: true,
        }}
        style={{
          backgroundColor: UtopiaTheme.color.neutralBackground.value,
          flexGrow: 0,
          boxShadow: `0px 1px 0px 0px ${UtopiaTheme.color.subduedBorder.value}`,
        }}
      >
        <FlexRow
          style={{
            height: UtopiaTheme.layout.rowHeight.small,
            borderBottom: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
            alignItems: 'stretch',
          }}
        >
          <TabComponent
            onClick={selectProblemsTab}
            onDoubleClick={toggleIsOpen}
            selected={selectedTab === 'problems'}
            showCloseIndicator={false}
            showModifiedIndicator={false}
            label={
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
                    ...getTabStyleForErrors(errorMessages),
                  }}
                >
                  {errorMessages.length}
                </span>
              </span>
            }
          />
          <TabComponent
            onClick={selectConsoleTab}
            onDoubleClick={toggleIsOpen}
            selected={selectedTab == 'console'}
            showCloseIndicator={false}
            showModifiedIndicator={false}
            label={
              <span>
                Console
                <span
                  style={{
                    marginLeft: 8,
                    fontSize: 10,
                    padding: '1px 5px',
                    borderRadius: 2,
                    fontWeight: 500,
                    color: colorTheme.neutralInvertedForeground.value,
                    ...getTabStyleForLogs(canvasConsoleLogs),
                  }}
                >
                  {canvasConsoleLogs.length}
                </span>
              </span>
            }
          />
        </FlexRow>
        {getTabContents()}
      </Resizable>
    )
  },
)
