/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as R from 'ramda'
import * as React from 'react'
import * as Semver from 'semver'
import {
  FlexRow,
  FunctionIcons,
  SectionBodyArea,
  SectionTitleRow,
  SquareButton,
  Title,
} from 'uuiui'
import { npmDependency, NpmDependency } from '../../core/shared/npm-dependency-types'
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
}

type PackageStatus = 'version-lookup' | 'loading' | 'loaded' | 'error' | 'default-package'

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
  packages: Array<PackageDetails> | null
  dependencyBeingEdited: string | null
  openVersionInput: boolean
  packageJsonFile: ProjectFile | null
  newlyLoadedItems: Array<PackageDetails['name']>
  dependencyLoadingStatus: DependencyLoadingStatus
}

function setPackageStatus(
  packages: Array<PackageDetails>,
  name: string,
  status: PackageStatus,
): Array<PackageDetails> {
  return packages.map((npmPackage) => {
    if (npmPackage.name === name) {
      return packageDetails(npmPackage.name, npmPackage.version, status)
    } else {
      return npmPackage
    }
  })
}

function addInPackage(
  packages: Array<PackageDetails>,
  name: string,
  version: string | null,
  status: PackageStatus,
  oldName: string | null,
): Array<PackageDetails> {
  let updatedPackages = packages.filter((p) => p.name !== name && p.name !== oldName)
  updatedPackages = [...updatedPackages, packageDetails(name, version, status)]
  return updatedPackages
}

function dependenciesFromPackageDetails(packages: Array<PackageDetails>): Array<NpmDependency> {
  const result: Array<NpmDependency> = []
  Utils.fastForEach(packages, (p) => {
    if (
      (p.status === 'loaded' || p.status === 'loading' || p.status === 'default-package') &&
      p.version != null
    ) {
      result.push(npmDependency(p.name, p.version))
    }
  })
  return result
}

export const DefaultPackagesList: Array<PackageDetails> = [
  {
    name: 'react',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: '@types/react',
    version: '16.8.17',
    status: 'default-package',
  },
  {
    name: 'csstype',
    version: '2.6.7',
    status: 'default-package',
  },
  {
    name: 'react-dom',
    version: '16.13.1',
    status: 'default-package',
  },
  {
    name: '@types/react-dom',
    version: '16.8.4',
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
  npmDependencies: Array<NpmDependency>,
): Array<PackageDetails> {
  const userAddedPackages: Array<PackageDetails> = []
  Utils.fastForEach(npmDependencies, (dep) => {
    const foundDefaultDependency = DefaultPackagesList.find((p) => p.name === dep.name)
    const status =
      foundDefaultDependency != null && foundDefaultDependency.version === dep.version
        ? 'default-package'
        : 'loaded'
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
    }
  })

  const dispatch = props.editorDispatch

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('dependencylist')], 'leftpane')
  }, [dispatch])

  const dependencyProps = { ...props, toggleMinimised: toggleMinimised }

  return <DependencyListInner {...dependencyProps} />
})

class DependencyListInner extends React.PureComponent<DependencyListProps, DependencyListState> {
  DependencyListContainerId = 'dependencyList'
  dependencyVersionInputRef = React.createRef<HTMLInputElement>()
  constructor(props: DependencyListProps) {
    super(props)
    const dependencies = dependenciesFromPackageJson(this.props.packageJsonFile)
    this.state = {
      showInsertField: false,
      dependencyLoadingStatus: 'not-loading',
      packages: Utils.optionalMap(packageDetailsFromDependencies, dependencies),
      dependencyBeingEdited: null,
      openVersionInput: false,
      packageJsonFile: props.packageJsonFile,
      newlyLoadedItems: [],
    }
  }

  static getDerivedStateFromProps(
    props: DependencyListProps,
    state: DependencyListState,
  ): DependencyListState | null {
    if (
      props.packageJsonFile === state.packageJsonFile ||
      Utils.shallowEqual(props.packageJsonFile, state.packageJsonFile)
    ) {
      return null
    } else {
      const dependencies = dependenciesFromPackageJson(props.packageJsonFile)
      return {
        showInsertField: false,
        dependencyLoadingStatus: 'not-loading',
        packages: Utils.optionalMap(packageDetailsFromDependencies, dependencies),
        dependencyBeingEdited: null,
        openVersionInput: false,
        packageJsonFile: props.packageJsonFile,
        newlyLoadedItems: state.newlyLoadedItems,
      }
    }
  }

  closeField = () =>
    this.setState({
      dependencyBeingEdited: null,
      showInsertField: false,
    })

  openDependencyEditField = (dependencyName: string, openVersionInput: boolean = false) => {
    this.setState(
      {
        dependencyBeingEdited: dependencyName,
        showInsertField: false,
        openVersionInput: openVersionInput,
      },
      () => {
        if (openVersionInput) {
          if (this.dependencyVersionInputRef.current != null) {
            this.dependencyVersionInputRef.current.focus()
          }
        }
      },
    )
  }

  removeDependency = (key: string) => {
    let npmDependencies = dependenciesFromPackageJson(this.props.packageJsonFile)
    // If we can't get the dependencies that implies something is broken, so avoid changing it.
    if (npmDependencies != null) {
      npmDependencies = npmDependencies.filter((dep) => dep.name != key)

      this.props.editorDispatch([EditorActions.updatePackageJson(npmDependencies)])

      fetchNodeModules(npmDependencies).then((nodeModules) => {
        this.props.editorDispatch([EditorActions.updateNodeModulesContents(nodeModules, true)])
        this.setState({ dependencyLoadingStatus: 'not-loading' })
      })

      this.setState({ dependencyLoadingStatus: 'removing' })

      this.setState((prevState) => {
        return {
          packages: Utils.optionalMap(
            (packages) => packages.filter((p) => p.name !== key),
            prevState.packages,
          ),
        }
      })
    }
  }

  packagesUpdateSuccess = (packageName: string) => {
    this.setState((prevState) => {
      const updatedPackages = Utils.optionalMap(
        (packages) => setPackageStatus(packages, packageName, 'loaded'),
        prevState.packages,
      )
      const newlyLoadedItems = [...prevState.newlyLoadedItems, packageName]
      return {
        packages: updatedPackages,
        newlyLoadedItems,
        dependencyLoadingStatus: 'not-loading',
      }
    })
  }

  packagesUpdateFailed = (e: any, packageName: string) => {
    console.error(e)
    this.props.editorDispatch(
      [
        pushToast({
          message: `${packageName} couldn't be added. Check the console for details.`,
          level: 'ERROR',
          persistent: true,
        }),
      ],
      'leftpane',
    )
    this.setState((prevState) => {
      const updatedPackages = Utils.optionalMap(
        (packages) => setPackageStatus(packages, packageName, 'error'),
        prevState.packages,
      )

      return {
        packages: updatedPackages,
        dependencyLoadingStatus: 'not-loading',
      }
    })
  }

  packageVersionLookup = (packageName: string, oldName: string | null): Promise<string> => {
    this.setState((prevState) => {
      const updatedPackages = Utils.optionalMap(
        (packages) => addInPackage(packages, packageName, null, 'version-lookup', oldName),
        prevState.packages,
      )
      return {
        dependencyLoadingStatus: 'adding',
        packages: updatedPackages,
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

      const emptyVersion: boolean = packageVersion == null || packageVersion === ''
      const packageVersionCoerced = Semver.valid(Semver.coerce(packageVersion + '') + '')
      const dependencyBeingEdited = this.state.dependencyBeingEdited
      const editedPackageName = lowerCasePackageName
      const editedPackageVersionPromise = emptyVersion
        ? this.packageVersionLookup(lowerCasePackageName, dependencyBeingEdited)
        : Promise.resolve(packageVersionCoerced)
      editedPackageVersionPromise
        .then((editedPackageVersion) => {
          this.setState((prevState) => {
            const updatedPackages = addInPackage(
              Utils.defaultIfNull<Array<PackageDetails>>([], prevState.packages),
              editedPackageName,
              editedPackageVersion,
              'loading',
              dependencyBeingEdited,
            )
            const updatedNpmDeps = Utils.stripNulls(
              updatedPackages.map((pack) =>
                pack.version == null ? null : npmDependency(pack.name, pack.version),
              ),
            )
            this.props.editorDispatch([EditorActions.updatePackageJson(updatedNpmDeps)])
            fetchNodeModules([npmDependency(editedPackageName, editedPackageVersion!)])
              .then((nodeModules) => {
                this.packagesUpdateSuccess(editedPackageName)
                this.props.editorDispatch([
                  EditorActions.updateNodeModulesContents(nodeModules, false),
                ])
              })
              .catch((e) => this.packagesUpdateFailed(e, editedPackageName))

            return {
              dependencyLoadingStatus: 'adding',
              packages: updatedPackages,
            }
          })
        })
        .catch((reason) => {
          this.props.editorDispatch(
            [
              pushToast({
                message: `Couldn't automatically locate latest version for ${lowerCasePackageName}`,
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
    const loadingPackages =
      this.state.packages?.filter((dependency) => dependency.status === 'loading') ?? []
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
          {this.state.packages != null && !this.props.minimised ? (
            <DependencyListItems
              packages={this.state.packages}
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
