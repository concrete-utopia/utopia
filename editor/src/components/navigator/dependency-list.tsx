/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import {
  FlexRow,
  FunctionIcons,
  SectionBodyArea,
  SectionTitleRow,
  SquareButton,
  Title,
} from 'uuiui'
import {
  requestedNpmDependency,
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import { ProjectFile } from '../../core/shared/project-file-types'
import { betterReactMemo } from '../../utils/react-performance'
import Utils from '../../utils/utils'
import { EditorPanel, setFocus } from '../common/actions'
import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/actions'
import { clearSelection, pushToast } from '../editor/actions/actions'
import {
  dependenciesFromPackageJson,
  findLatestVersion,
  checkPackageVersionExists,
  VersionLookupResult,
} from '../editor/npm-dependency/npm-dependency'
import { packageJsonFileFromProjectContents } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { DependencyListItems } from './dependency-list-items'
import { fetchNodeModules } from '../../core/es-modules/package-manager/fetch-packages'

type DependencyListProps = {
  editorDispatch: EditorDispatch
  minimised: boolean
  toggleMinimised?: () => void
  focusedPanel: EditorPanel | null
  packageJsonFile: ProjectFile | null
  packageStatus: PackageStatusMap
}

// TODO: this should just contain an NpmDependency and a status
export interface PackageDetails {
  name: string
  version: string | null
  status: PackageStatus
}

function packageDetails(
  name: string,
  version: string | null,
  status: PackageStatus,
): PackageDetails {
  return {
    name: name,
    version: version,
    status: status,
  }
}

export type DependencyLoadingStatus = 'not-loading' | 'adding' | 'removing'

type DependencyListState = {
  showInsertField: boolean
  dependencyBeingEdited: string | null
  openVersionInput: boolean
  newlyLoadedItems: Array<PackageDetails['name']>
  dependencyLoadingStatus: DependencyLoadingStatus
}

export const DefaultPackagesList: Array<PackageDetails> = [
  {
    name: 'react',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: 'react-dom',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: 'utopia-api',
    version: '0.4.1',
    status: 'default-package',
  },
  {
    name: 'react-spring',
    version: '8.0.27',
    status: 'default-package',
  },
]

function packageDetailsFromDependencies(
  npmDependencies: Array<RequestedNpmDependency>,
  packageStatus: PackageStatusMap,
): Array<PackageDetails> {
  const userAddedPackages: Array<PackageDetails> = []
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

export const DependencyList = betterReactMemo('DependencyList', () => {
  const props = useEditorState((store) => {
    return {
      editorDispatch: store.dispatch,
      minimised: store.editor.dependencyList.minimised,
      focusedPanel: store.editor.focusedPanel,
      packageJsonFile: packageJsonFileFromProjectContents(store.editor.projectContents),
      packageStatus: store.editor.nodeModules.packageStatus,
    }
  }, 'DependencyList')

  const dispatch = props.editorDispatch

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('dependencylist')], 'leftpane')
  }, [dispatch])

  const dependencyProps = { ...props, toggleMinimised: toggleMinimised }

  return <DependencyListInner {...dependencyProps} />
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
      showInsertField: false,
      dependencyLoadingStatus: 'not-loading',
      dependencyBeingEdited: null,
      openVersionInput: false,
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
          showInsertField: false,
          dependencyLoadingStatus: 'not-loading',
          dependencyBeingEdited: null,
          openVersionInput: false,
          newlyLoadedItems: state.newlyLoadedItems,
        }
      })
    }
  }

  closeField = () =>
    this.setState({
      dependencyBeingEdited: null,
      showInsertField: false,
    })

  openDependencyEditField = (dependencyName: string, openVersionInput: boolean = false) => {
    this.setState({
      dependencyBeingEdited: dependencyName,
      showInsertField: false,
      openVersionInput: openVersionInput,
    })
  }

  removeDependency = (key: string) => {
    let npmDependencies = dependenciesFromPackageJson(this.props.packageJsonFile)
    // If we can't get the dependencies that implies something is broken, so avoid changing it.
    if (npmDependencies != null) {
      npmDependencies = npmDependencies.filter((dep) => dep.name != key)

      this.props.editorDispatch([EditorActions.updatePackageJson(npmDependencies)])

      fetchNodeModules(npmDependencies).then((fetchNodeModulesResult) => {
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
          EditorActions.updateNodeModulesContents(fetchNodeModulesResult.nodeModules, 'full-build'),
        ])
      })

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
        pushToast({
          message: `${packageName} couldn't be added. Check the console for details.`,
          level: 'ERROR',
          persistent: true,
        }),
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
    this.props.editorDispatch(
      [
        pushToast({
          message: `Adding ${packageName} to your project.`,
          level: 'INFO',
        }),
      ],
      'leftpane',
    )

    if (DefaultPackagesList.find((pkg) => pkg.name === packageName)) {
      this.props.editorDispatch(
        [
          pushToast({
            message: `${packageName} is already available as a default package, no need to add it again :)`,
            level: 'SUCCESS',
          }),
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
                pushToast({
                  message: `No npm registry entry found for ${packageNameAndVersion}`,
                  level: 'ERROR',
                }),
              ],
              'leftpane',
            )

            this.packagesUpdateNotFound(editedPackageName)
          } else {
            this.setState((prevState) => {
              const currentNpmDeps = dependenciesFromPackageJson(this.props.packageJsonFile)
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
              fetchNodeModules([requestedNpmDependency(editedPackageName, editedPackageVersion!)])
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
                      EditorActions.updateNodeModulesContents(
                        fetchNodeModulesResult.nodeModules,
                        'incremental',
                      ),
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
            [
              pushToast({
                message: `Couldn't fetch metadata for ${packageNameAndVersion}`,
                level: 'ERROR',
              }),
            ],
            'leftpane',
          )
          console.error('Reason for failing to locate the latest version.', reason)
          this.packagesUpdateFailed(reason, editedPackageName)
        })
    }
    this.setState({
      showInsertField: false,
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

  toggleOpenAddInsertField = (e: React.MouseEvent) => {
    e.stopPropagation()
    this.setState((prevState) => ({
      showInsertField: !prevState.showInsertField,
      dependencyBeingEdited: null,
    }))
  }

  updateDependencyToLatestVersion = (dependencyName: string) => {
    this.addDependency(dependencyName, null)
  }

  render() {
    const packagesWithStatus: Array<PackageDetails> = packageDetailsFromDependencies(
      dependenciesFromPackageJson(this.props.packageJsonFile),
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
      <div onFocus={this.onFocus} tabIndex={-1} id={this.DependencyListContainerId}>
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
          {this.props.minimised ? null : (
            <SquareButton
              highlight
              onClick={this.toggleOpenAddInsertField}
              disabled={this.state.dependencyLoadingStatus != 'not-loading'}
            >
              <FunctionIcons.Add
                style={{
                  flexGrow: 0,
                  flexShrink: 0,
                }}
              />
            </SquareButton>
          )}
        </SectionTitleRow>
        <SectionBodyArea minimised={this.props.minimised}>
          {!this.props.minimised ? (
            <DependencyListItems
              packages={packagesWithStatus}
              editingLocked={this.state.dependencyLoadingStatus != 'not-loading'}
              openDependencyEditField={this.openDependencyEditField}
              updateDependencyToLatestVersion={this.updateDependencyToLatestVersion}
              removeDependency={this.removeDependency}
              openVersionInput={this.state.openVersionInput}
              newlyLoadedItems={this.state.newlyLoadedItems}
              dependencyBeingEdited={this.state.dependencyBeingEdited}
              addDependency={this.addDependency}
              closeField={this.closeField}
              showInsertField={this.state.showInsertField}
            />
          ) : null}
        </SectionBodyArea>
      </div>
    )
  }
}
