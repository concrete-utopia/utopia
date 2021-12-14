/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  requestedNpmDependency,
  RequestedNpmDependency,
  PackageStatusMap,
  PackageStatus,
} from '../../core/shared/npm-dependency-types'
import { ProjectFile } from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import { EditorPanel, setFocus } from '../common/actions'
import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { clearSelection, addToast } from '../editor/actions/action-creators'
import {
  dependenciesFromPackageJson,
  findLatestVersion,
  checkPackageVersionExists,
  VersionLookupResult,
} from '../editor/npm-dependency/npm-dependency'
import {
  DefaultPackagesList,
  DependencyPackageDetails,
  packageJsonFileFromProjectContents,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
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
} from '../../uuiui'
import { notice } from '../common/notice'
import { isFeatureEnabled } from '../../utils/feature-switches'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'

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
  const props = useEditorState((store) => {
    return {
      editorDispatch: store.dispatch,
      minimised: store.editor.dependencyList.minimised,
      focusedPanel: store.editor.focusedPanel,
      packageJsonFile: packageJsonFileFromProjectContents(store.editor.projectContents),
      packageStatus: store.editor.nodeModules.packageStatus,
      builtInDependencies: store.builtInDependencies,
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

      fetchNodeModules(npmDependencies, this.props.builtInDependencies).then(
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
              style={{ paddingLeft: 8, paddingRight: 8, paddingTop: 4, paddingBottom: 4 }}
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
  const dispatch = useEditorState((store) => store.dispatch, 'AddTailwindButton')
  const onButtonClicked = React.useCallback(() => {
    dispatch([EditorActions.addTailwindConfig()])
  }, [dispatch])

  const tailwindAlreadyAdded =
    props.packagesWithStatus.find((p) => p.name === 'tailwindcss') &&
    props.packagesWithStatus.find((p) => p.name === 'postcss')
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
      }}
      onClick={onButtonClicked}
    >
      Add &nbsp;<b>Tailwind</b>&nbsp; To Project
    </Button>
  )
}
