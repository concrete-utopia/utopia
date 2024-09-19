/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type {
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
  RequireFn,
} from '../../core/shared/npm-dependency-types'
import { requestedNpmDependency } from '../../core/shared/npm-dependency-types'
import type { ProjectFile } from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import type { EditorPanel } from '../common/actions'
import { setFocus } from '../common/actions'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { clearSelection, addToast } from '../editor/actions/action-creators'
import type { VersionLookupResult } from '../editor/npm-dependency/npm-dependency'
import {
  dependenciesFromPackageJson,
  findLatestVersion,
  checkPackageVersionExists,
} from '../editor/npm-dependency/npm-dependency'
import type { DependencyPackageDetails } from '../editor/store/editor-state'
import { DefaultPackagesList } from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { DependencyListItems } from './dependency-list-items'
import { fetchNodeModules } from '../../core/es-modules/package-manager/fetch-packages'
import {
  SectionTitleRow,
  FlexRow,
  Title,
  SquareButton,
  FunctionIcons,
  SectionBodyArea,
  Section,
  FlexColumn,
  Button,
  colorTheme,
} from '../../uuiui'
import { notice } from '../common/notice'
import { isFeatureEnabled } from '../../utils/feature-switches'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { useDispatch } from '../editor/store/dispatch-context'
import type { ProjectContentTreeRoot } from '../assets'
import {
  getProjectFileByFilePath,
  packageJsonFileFromProjectContents,
  walkContentsTree,
} from '../assets'
import { TailwindConfigPath } from '../../core/tailwind/tailwind-config'
import { importDefault } from '../../core/es-modules/commonjs-interop'
import type { Config } from 'tailwindcss'
import { CanvasContainerID } from '../canvas/canvas-types'
import { createTailwindcss } from '@mhsdesign/jit-browser-tailwindcss'
import { interactionSessionIsActive } from '../canvas/canvas-strategies/interaction-state'
import { rescopeCSSToTargetCanvasOnly } from '../../core/shared/css-utils'

type DependencyListProps = {
  editorDispatch: EditorDispatch
  minimised: boolean
  toggleMinimised?: () => void
  focusedPanel: EditorPanel | null
  packageJsonFile: ProjectFile | null
  packageStatus: PackageStatusMap
  builtInDependencies: BuiltInDependencies
}

function packageDetails(
  name: string,
  version: string | null,
  status: PackageStatus,
): DependencyPackageDetails {
  return {
    name: name,
    version: version,
    status: status,
  }
}

export type DependencyLoadingStatus = 'not-loading' | 'adding' | 'removing'

type DependencyListState = {
  dependencyBeingEdited: string | null
  newlyLoadedItems: Array<DependencyPackageDetails['name']>
  dependencyLoadingStatus: DependencyLoadingStatus
}

function packageDetailsFromDependencies(
  npmDependencies: Array<RequestedNpmDependency>,
  packageStatus: PackageStatusMap,
): Array<DependencyPackageDetails> {
  const userAddedPackages: Array<DependencyPackageDetails> = []
  Utils.fastForEach(npmDependencies, (dep) => {
    const foundDefaultDependency = DefaultPackagesList.find((p) => p.name === dep.name)
    const status =
      foundDefaultDependency != null && foundDefaultDependency.version === dep.version
        ? 'default-package'
        : packageStatus[dep.name]?.status ?? 'loaded'
    userAddedPackages.push(packageDetails(dep.name, dep.version, status))
  })

  return userAddedPackages
}

export const DependencyList = React.memo(() => {
  const props = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        minimised: store.editor.dependencyList.minimised,
        focusedPanel: store.editor.focusedPanel,
        packageStatus: store.editor.nodeModules.packageStatus,
      }
    },
    'DependencyList',
  )

  const builtInDependencies = useEditorState(
    Substores.builtInDependencies,
    (store) => store.builtInDependencies,
    'DependencyList builtInDependencies',
  )

  const packageJsonFile = useEditorState(
    Substores.projectContents,
    (store) => packageJsonFileFromProjectContents(store.editor.projectContents),
    'DependencyList packageJsonFile',
  )

  const dispatch = useDispatch()

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('dependencylist')], 'leftpane')
  }, [dispatch])

  const dependencyProps = { ...props, toggleMinimised: toggleMinimised }

  return (
    <DependencyListInner
      editorDispatch={dispatch}
      {...dependencyProps}
      packageJsonFile={packageJsonFile}
      builtInDependencies={builtInDependencies}
    />
  )
})

function unwrapLookupResult(lookupResult: VersionLookupResult): string | null {
  switch (lookupResult.type) {
    case 'PACKAGE_NOT_FOUND':
      return null
    case 'VERSION_LOOKUP_SUCCESS':
      return lookupResult.version.version
    default:
      const _exhaustiveCheck: never = lookupResult
      throw new Error(`Unhandled version lookup type ${JSON.stringify(lookupResult)}`)
  }
}

class DependencyListInner extends React.PureComponent<DependencyListProps, DependencyListState> {
  DependencyListContainerId = 'dependencyList'
  constructor(props: DependencyListProps) {
    super(props)
    this.state = {
      dependencyLoadingStatus: 'not-loading',
      dependencyBeingEdited: null,
      newlyLoadedItems: [],
    }
  }

  // TODO: this is a very very naughty lifecycle event
  UNSAFE_componentWillReceiveProps(newProps: DependencyListProps): void {
    if (
      newProps.packageJsonFile === this.props.packageJsonFile ||
      Utils.shallowEqual(newProps.packageJsonFile, this.props.packageJsonFile)
    ) {
      return
    } else {
      this.setState((state) => {
        return {
          dependencyLoadingStatus: 'not-loading',
          dependencyBeingEdited: null,
          newlyLoadedItems: state.newlyLoadedItems,
        }
      })
    }
  }

  handleAbandonEdit = () =>
    this.setState({
      dependencyBeingEdited: null,
    })

  openDependencyEditField = (dependencyName: string) => {
    this.setState({
      dependencyBeingEdited: dependencyName,
    })
  }

  removeDependency = (key: string) => {
    let npmDependencies = dependenciesFromPackageJson(this.props.packageJsonFile, 'regular-only')
    // If we can't get the dependencies that implies something is broken, so avoid changing it.
    if (npmDependencies != null) {
      npmDependencies = npmDependencies.filter((dep) => dep.name != key)

      this.props.editorDispatch([EditorActions.updatePackageJson(npmDependencies)])

      void fetchNodeModules(npmDependencies, this.props.builtInDependencies).then(
        (fetchNodeModulesResult) => {
          if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
            this.packagesUpdateFailed(
              `Failed to download the following dependencies: ${JSON.stringify(
                fetchNodeModulesResult.dependenciesWithError.map((d) => d.name),
              )}`,
              fetchNodeModulesResult.dependenciesWithError[0]?.name,
            )
          }
          this.setState({ dependencyLoadingStatus: 'not-loading' })
          this.props.editorDispatch([
            EditorActions.updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
          ])
        },
      )

      this.setState({ dependencyLoadingStatus: 'removing' })
    }
  }

  packagesUpdateSuccess = (packageName: string) => {
    this.props.editorDispatch([EditorActions.setPackageStatus(packageName, 'loaded')])
    this.setState((prevState) => {
      const newlyLoadedItems = [...prevState.newlyLoadedItems, packageName]
      return {
        newlyLoadedItems,
        dependencyLoadingStatus: 'not-loading',
      }
    })
  }

  packagesUpdateFailed = (e: any, packageName: string) => {
    // TODO make this a packageNames Array
    console.error(e)
    this.props.editorDispatch(
      [
        EditorActions.setPackageStatus(packageName, 'error'),
        addToast(
          notice(`${packageName} couldn't be added. Check the console for details.`, 'ERROR', true),
        ),
      ],
      'leftpane',
    )
    this.setState((prevState) => {
      return {
        dependencyLoadingStatus: 'not-loading',
      }
    })
  }

  packagesUpdateNotFound = (packageName: string) => {
    this.props.editorDispatch(
      [EditorActions.setPackageStatus(packageName, 'not-found')],
      'leftpane',
    )
    this.setState((prevState) => {
      return {
        dependencyLoadingStatus: 'not-loading',
      }
    })
  }

  packageVersionLookup = (
    packageName: string,
    version: string | undefined,
  ): Promise<string | null> => {
    if (version == null || version === '') {
      return this.latestPackageVersionLookup(packageName).then(unwrapLookupResult)
    } else {
      return checkPackageVersionExists(packageName, version).then((exists) => {
        return exists ? version : null
      })
    }
  }

  latestPackageVersionLookup = (packageName: string): Promise<VersionLookupResult> => {
    this.props.editorDispatch(
      [EditorActions.setPackageStatus(packageName, 'version-lookup')],
      'leftpane',
    )
    this.setState((prevState) => {
      return {
        dependencyLoadingStatus: 'adding',
      }
    })
    return findLatestVersion(packageName)
  }

  addDependency = (packageName: string | null, packageVersion: string | null) => {
    this.setState({
      dependencyBeingEdited: null,
    })

    if (packageName !== null) {
      this.props.editorDispatch(
        [addToast(notice(`Adding ${packageName} to your project.`, 'INFO'))],
        'leftpane',
      )
    }

    if (DefaultPackagesList.some((pkg) => pkg.name === packageName)) {
      this.props.editorDispatch(
        [
          addToast(
            notice(
              `${packageName} is already available as a default package, no need to add it again :)`,
              'SUCCESS',
            ),
          ),
        ],
        'leftpane',
      )
    } else if (packageName !== '' && packageName !== null) {
      const lowerCasePackageName = packageName.toLowerCase()

      const trimmedPackageVersion = packageVersion?.trim()
      const dependencyBeingEdited = this.state.dependencyBeingEdited
      const editedPackageName = lowerCasePackageName

      const packageAlreadyExists = this.props.packageStatus[editedPackageName] != null
      const loadingOrUpdating = packageAlreadyExists ? 'updating' : 'loading'

      const packageNameAndVersion =
        trimmedPackageVersion == null || trimmedPackageVersion === ''
          ? lowerCasePackageName
          : `${lowerCasePackageName}@${trimmedPackageVersion}`

      const editedPackageVersionPromise = this.packageVersionLookup(
        lowerCasePackageName,
        trimmedPackageVersion,
      )
      editedPackageVersionPromise
        .then((editedPackageVersion) => {
          if (editedPackageVersion == null) {
            this.props.editorDispatch(
              [
                addToast(
                  notice(`No npm registry entry found for ${packageNameAndVersion}`, 'ERROR'),
                ),
              ],
              'leftpane',
            )

            this.packagesUpdateNotFound(editedPackageName)
          } else {
            this.setState((prevState) => {
              const currentNpmDeps = dependenciesFromPackageJson(
                this.props.packageJsonFile,
                'regular-only',
              )
              const npmDepsWithoutCurrentDep = currentNpmDeps.filter(
                (p) => p.name !== editedPackageName && p.name !== dependencyBeingEdited,
              )
              const updatedNpmDeps = [
                ...npmDepsWithoutCurrentDep,
                requestedNpmDependency(editedPackageName, editedPackageVersion!),
              ]

              this.props.editorDispatch([
                EditorActions.setPackageStatus(editedPackageName, loadingOrUpdating),
                EditorActions.updatePackageJson(updatedNpmDeps),
              ])
              fetchNodeModules(
                [requestedNpmDependency(editedPackageName, editedPackageVersion!)],
                this.props.builtInDependencies,
              )
                .then((fetchNodeModulesResult) => {
                  if (fetchNodeModulesResult.dependenciesWithError.length > 0) {
                    this.packagesUpdateFailed(
                      `Failed to download the following dependencies: ${JSON.stringify(
                        fetchNodeModulesResult.dependenciesWithError.map((d) => d.name),
                      )}`,
                      editedPackageName,
                    )
                  } else {
                    this.packagesUpdateSuccess(editedPackageName)
                    this.props.editorDispatch([
                      EditorActions.updateNodeModulesContents(fetchNodeModulesResult.nodeModules),
                    ])
                  }
                })
                .catch((e) => this.packagesUpdateFailed(e, editedPackageName))

              return {
                dependencyLoadingStatus: 'adding',
              }
            })
          }
        })
        .catch((reason) => {
          this.props.editorDispatch(
            [addToast(notice(`Couldn't fetch metadata for ${packageNameAndVersion}`, 'ERROR'))],
            'leftpane',
          )
          console.error('Reason for failing to locate the latest version.', reason)
          this.packagesUpdateFailed(reason, editedPackageName)
        })
    }
    this.setState({
      dependencyBeingEdited: null,
    })
  }

  onFocus = (e: React.FocusEvent<HTMLElement>) => {
    if (this.props.focusedPanel !== 'dependencylist') {
      this.props.editorDispatch([setFocus('dependencylist')], 'everyone')
    }
    if ((e.target as any).id === this.DependencyListContainerId) {
      this.props.editorDispatch([clearSelection()], 'everyone')
    }
  }

  updateDependencyToLatestVersion = (dependencyName: string) => {
    this.addDependency(dependencyName, null)
  }

  render() {
    const packagesWithStatus: Array<DependencyPackageDetails> = packageDetailsFromDependencies(
      dependenciesFromPackageJson(this.props.packageJsonFile, 'regular-only'),
      this.props.packageStatus,
    )

    const loadingPackages =
      packagesWithStatus.filter((dependency) => dependency.status === 'loading') ?? []
    let statusNode: React.ReactNode
    if (this.state.dependencyLoadingStatus === 'adding' && loadingPackages.length > 0) {
      statusNode = (
        <React.Fragment>
          <span style={{ fontStyle: 'italic' }}>{`(loading ${loadingPackages.length} new…)`}</span>
        </React.Fragment>
      )
    } else if (this.state.dependencyLoadingStatus === 'removing') {
      statusNode = (
        <React.Fragment>
          <span style={{ fontStyle: 'italic' }}>{`(removing…)`}</span>
        </React.Fragment>
      )
    }

    return (
      <Section onFocus={this.onFocus} tabIndex={-1} id={this.DependencyListContainerId}>
        <SectionTitleRow
          minimised={this.props.minimised}
          toggleMinimised={this.props.toggleMinimised}
        >
          <FlexRow flexGrow={1}>
            <Title>
              Dependencies
              {statusNode != null ? '\u00A0' : null}
              {statusNode}
            </Title>
          </FlexRow>
        </SectionTitleRow>
        <SectionBodyArea minimised={this.props.minimised}>
          {!this.props.minimised ? (
            <FlexColumn
              role='listContainer'
              style={{
                paddingLeft: 8,
                paddingRight: 8,
                paddingTop: 4,
                paddingBottom: 4,
              }}
            >
              <AddTailwindButton packagesWithStatus={packagesWithStatus} />
              <DependencyListItems
                packages={packagesWithStatus}
                editingLocked={this.state.dependencyLoadingStatus != 'not-loading'}
                openDependencyEditField={this.openDependencyEditField}
                updateDependencyToLatestVersion={this.updateDependencyToLatestVersion}
                removeDependency={this.removeDependency}
                newlyLoadedItems={this.state.newlyLoadedItems}
                dependencyBeingEdited={this.state.dependencyBeingEdited}
                addDependency={this.addDependency}
                handleAbandonEdit={this.handleAbandonEdit}
              />
            </FlexColumn>
          ) : null}
        </SectionBodyArea>
      </Section>
    )
  }
}

interface AddTailwindButtonProps {
  packagesWithStatus: DependencyPackageDetails[]
}

const AddTailwindButton = (props: AddTailwindButtonProps) => {
  const dispatch = useDispatch()
  const onButtonClicked = React.useCallback(() => {
    dispatch([EditorActions.addTailwindConfig()])
  }, [dispatch])

  const tailwindAlreadyAdded =
    props.packagesWithStatus.some((p) => p.name === 'tailwindcss') &&
    props.packagesWithStatus.some((p) => p.name === 'postcss')
  if (tailwindAlreadyAdded) {
    return null
  }
  return (
    <Button
      primary
      highlight
      style={{
        margin: 8,
        height: 24,
        backgroundImage: 'linear-gradient(3deg, #92ABFF 0%, #1FCCB7 99%)',
        boxShadow: 'inset 0 0 0 1px rgba(94,94,94,0.20)',
        borderRadius: 2,
        color: colorTheme.bg1.value,
      }}
      onClick={onButtonClicked}
    >
      Add &nbsp;<b>Tailwind</b>&nbsp; To Project
    </Button>
  )
}

function ensureElementExists({ type, id }: { type: string; id: string }) {
  let tag = document.getElementById(id)
  if (tag == null) {
    tag = document.createElement(type)
    tag.id = id
    document.head.appendChild(tag)
  }
  return tag
}

async function generateTailwindStyles(config: Config | null, allCSSFiles: string) {
  const tailwindCss = createTailwindcss({ tailwindConfig: config ?? undefined })

  const contentElement = document.getElementById(CanvasContainerID)

  const content = contentElement?.outerHTML ?? ''

  const styleString = await tailwindCss.generateStylesFromContent(allCSSFiles, [content])
  const style = ensureElementExists({ type: 'style', id: 'utopia-tailwind-jit-styles' })
  style.textContent = rescopeCSSToTargetCanvasOnly(styleString)
}

function getCssFilesFromProjectContents(projectContents: ProjectContentTreeRoot) {
  let files: string[] = []
  walkContentsTree(projectContents, (path, file) => {
    if (file.type === 'TEXT_FILE' && path.endsWith('.css')) {
      files.push(file.fileContents.code)
    }
  })
  return files
}

export const useTailwindCompilation = (requireFn: RequireFn) => {
  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useTailwindCompilation projectContents',
  )

  const isInteractionActiveRef = useRefEditorState((store) =>
    interactionSessionIsActive(store.editor.canvas.interactionSession),
  )

  const observerCallback = React.useCallback(() => {
    if (isInteractionActiveRef.current) {
      return
    }
    const tailwindFile = getProjectFileByFilePath(projectContents, TailwindConfigPath)
    const allCSSFiles = getCssFilesFromProjectContents(projectContents).join('\n')
    const rawConfig =
      tailwindFile == null || tailwindFile.type !== 'TEXT_FILE'
        ? null
        : importDefault(requireFn('/', TailwindConfigPath))
    void generateTailwindStyles(rawConfig as Config, allCSSFiles)
  }, [isInteractionActiveRef, projectContents, requireFn])

  React.useEffect(() => {
    const observer = new MutationObserver(observerCallback)

    observer.observe(document.getElementById(CanvasContainerID)!, {
      attributes: true,
      childList: true,
      subtree: true,
    })

    observerCallback()

    return () => {
      observer.disconnect()
    }
  }, [isInteractionActiveRef, observerCallback, projectContents, requireFn])
}
