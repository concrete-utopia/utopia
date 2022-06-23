/** @jsxRuntime classic */
/** @jsx jsx */
import React, { useState } from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import { layout, FlexRow, FlexColumn, FlexWrappingList } from './layout'
import { H2 } from './style'
import { colors } from './theme'

import { Global } from '@emotion/react'
import { fetchProjectList, fetchShowcaseProjects } from '../common/server'
import {
  ProjectListing,
  deleteProject,
  fetchProjectListFromLocalStorage,
} from '../common/persistence'
import * as timeago from 'timeago.js'
import { Card, cardLayout, cardLayoutStyle } from './cards'

interface navItemProps {
  selected: boolean
}

const FlexNavItem = styled('div')<navItemProps>(
  {
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    flexBasis: 180,
    padding: '10px 20px',
  },
  (props) => ({
    background: props.selected ? colors.default : 'undefined',
    color: props.selected ? 'white' : colors.default,
  }),
)

interface sortButtonProps {
  selected: boolean
  sortOrder: number
}

const SortButton = styled('div')<sortButtonProps>(
  {
    color: colors.mainBlue,
    textDecoration: 'none',
    cursor: 'pointer',
    padding: '0px 4px',
    userSelect: 'none',
    borderRadius: 3,
    transition: '.2s ease',
    '&:hover': {
      opacity: .7,
    },
    '&:active': {
      background: colors.mainBlue,
      color: 'white',
    },
  },
  (props) => ({
    background: props.sortOrder === 0 ? colors.mainPink : props.sortOrder === 1 ? colors.mainBlue : 'undefined',
    color: props.sortOrder === 0 ? colors.mainBlue : props.sortOrder === 1 ? colors.mainPink : colors.mainBlue,
  }),
)

interface ProjectCardProps {
  selected: boolean
  project: ProjectListing
  onSelect?: () => void
}

class ProjectCard extends React.Component<ProjectCardProps> {
  constructor(props: ProjectCardProps) {
    super(props)
  }

  onMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
    e.stopPropagation()
    if (e.button === 0 && this.props.onSelect != null) {
      this.props.onSelect()
    }
  }

  onDoubleClick = (e: React.MouseEvent<HTMLDivElement>) => {
    if (e.button === 0) {
      if (this.props.selected) {
        window.open(`/project/${this.props.project.id}/`, '_self')
      }
    }
  }

  render() {
    const { id: projectId, title, modifiedAt, thumbnail } = this.props.project
    return (
      <Card
        selected={this.props.selected}
        data-label='project card'
        onMouseDown={this.onMouseDown}
        onDoubleClick={this.onDoubleClick}
        key={projectId}
      >
        <div
          className='projecttile-thumbnail'
          style={{
            background: `url(${thumbnail}) no-repeat 50% 50%`,
            backgroundSize: 'cover',
            height: cardLayout.imageHeight,
          }}
        ></div>
        <div
          className='projecttile-description'
          style={{
            display: 'flex',
            height: cardLayout.footerHeight,
            padding: '12px',
            boxShadow: `0px -1px 0px ${colors.default}`,
          }}
        >
          <div>
            <div
              className='projecttile-description-head'
              style={{
                fontSize: 13,
                whiteSpace: 'nowrap',
                overflow: 'hidden',
                textOverflow: 'ellipsis',
                paddingRight: 12,
              }}
            >
              {title == null ? 'Unnamed' : title}
            </div>
            <div
              className='projecttile-description-subhead'
              style={{
                color: '#888',
                fontWeight: 400,
                fontSize: 11,
                display: 'inline-block',
                wordWrap: 'break-word',
              }}
            >
              <span className='timeago'>Last edited about {timeago.format(modifiedAt)}</span>
            </div>
          </div>
        </div>
      </Card>
    )
  }
}

interface ProjectsState {
  localProjects: Array<ProjectListing>
  filteredLocalProjects: Array<ProjectListing>
  projects: Array<ProjectListing>
  filteredProjects: Array<ProjectListing>
  showcase: Array<ProjectListing>
  selectedProjectId: string | null
  projectTitleFilter: string | null
  mode: 'projects' | 'filter' | 'docs'
  sortMode: 'date' | 'title' | 'datereversed' | 'titlereversed'
  dateSortOrder: number
  titleSortOrder: number
}

interface EmptyProps {}

export class ProjectsPage extends React.Component<EmptyProps, ProjectsState> {
  constructor(props: EmptyProps) {
    super(props)
    this.state = {
      localProjects: [],
      filteredLocalProjects: [],
      projects: [],
      filteredProjects: [],
      showcase: [],

      selectedProjectId: null,
      projectTitleFilter: null,
      mode: 'projects',
      sortMode: 'date',
      dateSortOrder: 2,
      titleSortOrder: 2,
    }
  }

  componentDidMount() {
    fetchProjectListFromLocalStorage().then((projects) => {
      var orderedProjects = projects.sort(function (a, b) {
        var aDateNumber = new Date(a.modifiedAt).getTime()
        var bDateNumber = new Date(b.modifiedAt).getTime()
        return bDateNumber - aDateNumber
      })
      this.setState({
        localProjects: orderedProjects,
      })
    })
    fetchProjectList().then((projects) => {
      var orderedProjects = projects.sort(function (a, b) {
        var aDateNumber = new Date(a.modifiedAt).getTime()
        var bDateNumber = new Date(b.modifiedAt).getTime()
        return bDateNumber - aDateNumber
      })

      this.setState({
        projects: orderedProjects,
        filteredProjects: orderedProjects,
      })
    })
    fetchShowcaseProjects().then((showcase) => {
      this.setState({
        showcase: showcase,
      })
    })

    window.addEventListener('keydown', this.keyHandler)
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.keyHandler)
  }

  keyHandler = (event: KeyboardEvent) => {
    event.stopPropagation()

    switch (event.keyCode) {
      case 8:
      case 46:
        if (this.state.selectedProjectId != null) {
          const projectFilter = (project: ProjectListing) =>
            project.id !== this.state.selectedProjectId

          deleteProject(this.state.selectedProjectId)

          this.setState((previousState) => {
            return {
              localProjects: previousState.localProjects.filter(projectFilter),
              projects: previousState.projects.filter(projectFilter),
              selectedProjectId: null,
            }
          })
        }
        break
      case 27:
        this.setState({
          mode: 'projects',
        })
        break
      case 70:
        if (event.metaKey) {
          event.preventDefault()
          this.setState({
            mode: 'filter',
          })
        }
        break
      default:
    }
  }

  onFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const rawProjects = this.state.projects
    const rawLocalProjects = this.state.localProjects

    if (event.target.value !== '') {
      this.setState({
        projectTitleFilter: event.target.value,
        filteredProjects: rawProjects.filter((project) =>
          project.title.toLowerCase().includes(event.target.value.toLowerCase()),
        ),
        filteredLocalProjects: rawLocalProjects.filter((project) =>
          project.title.toLowerCase().includes(event.target.value.toLowerCase()),
        ),
      })
    } else {
      this.setState({
        projectTitleFilter: null,
        filteredProjects: rawProjects,
      })
    }
  }

  projectComponent = (project: ProjectListing) => {
    return (
      <ProjectCard
        project={project}
        selected={project.id === this.state.selectedProjectId}
        // eslint-disable-next-line react/jsx-no-bind
        onSelect={() => this.setState({ selectedProjectId: project.id })}
      />
    )
  }

  createNewProject = () => {
    window.open(`/project/`, '_self')
  }

  newProjectCard = (
    <div
      role='button'
      data-label='Create New Project'
      css={{
        ...cardLayoutStyle,
        WebkitUserSelect: 'none',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        position: 'relative',
        cursor: 'pointer',
        fontWeight: 700,
        fontSize: '130px',
        border: `1px solid ${colors.default}`,
        WebkitTextStroke: colors.default,
        WebkitTextStrokeWidth: '1px',
        color: '#00000005',
        transition: 'all .1s ease-in-out',
        '&:hover': {
          color: 'black',
        },
        '&:active': {
          transform: 'scale(.99)',
        },
      }}
      onMouseUp={this.createNewProject}
    >
      +
    </div>
  )

  clearSelectedProjectId = () => {
    this.setState({ selectedProjectId: null })
  }

  showcaseComponent = (project: ProjectListing) => {
    return (
      <ProjectCard
        project={project}
        selected={project.id === this.state.selectedProjectId}
        onSelect={this.clearSelectedProjectId}
      />
    )
  }

  clearSelectedProject = () => this.setState({ selectedProjectId: null })

  setProjectsMode = () => this.setState({ mode: 'projects' })
  setFilterMode = () => this.setState({ mode: 'filter' })

  setSortByDateMode = () => this.setState({ sortMode: 'date' })
  setSortByTitleMode = () => this.setState({ sortMode: 'title' })

  render() {
    const hasProjects = this.state.filteredProjects.length > 0
    const hasLocalProjects = this.state.filteredLocalProjects.length > 0
    const visibleProjectCount =
      this.state.filteredProjects.length + this.state.filteredLocalProjects.length

    const reversedProjects = [...this.state.filteredProjects].reverse()
    const projectsSortedByTitle = [...this.state.filteredProjects].sort(function (a, b) {
      let x = a.title.toLowerCase()
      let y = b.title.toLowerCase()
      if (x < y) {
        return -1
      }
      if (x > y) {
        return 1
      }
      return 0
    })
    const projectsSortedByDate = [...this.state.filteredProjects].sort(function (a, b) {
      var aDateNumber = new Date(a.modifiedAt).getTime()
      var bDateNumber = new Date(b.modifiedAt).getTime()
      return bDateNumber - aDateNumber
    })

    const dateSortOrder = this.state.dateSortOrder
    const handleSortByDate = () => {
      this.setSortByDateMode()
      if (dateSortOrder === 2) {
        this.setState({ filteredProjects: projectsSortedByDate })
      } else if (dateSortOrder === 0) {
        this.setState({ filteredProjects: reversedProjects })
      } else if (dateSortOrder === 1) {
        this.setState({ filteredProjects: this.state.projects })
      }
      this.setState({ dateSortOrder: (dateSortOrder + 1) % 3 })
      this.setState({ titleSortOrder: 2 })
      console.log('date sort order: ' + dateSortOrder)
    }

    const titleSortOrder = this.state.titleSortOrder
    const handleSortByTitle = () => {
      this.setSortByTitleMode()
      if (titleSortOrder === 2) {
        this.setState({ filteredProjects: projectsSortedByTitle })
      } else if (titleSortOrder === 0) {
        this.setState({ filteredProjects: reversedProjects })
      } else if (titleSortOrder === 1) {
        this.setState({ filteredProjects: this.state.projects })
      }
      this.setState({ titleSortOrder: (titleSortOrder + 1) % 3 })
      this.setState({ dateSortOrder: 2 })
      console.log('title sort order: ' + titleSortOrder)
    }

    return (
      <React.Fragment>
        <Global
          styles={{
            html: {
              height: '100%',
            },
            body: {
              overflow: 'hidden',
              height: '100%',
              margin: 0,
              fontFamily: 'Inter, sans-serif',
              fontSize: 13,
              color: colors.default,
            },
          }}
        />

        <FlexColumn
          onMouseDown={this.clearSelectedProject}
          style={{
            alignItems: 'flex-start',

            height: '100vh',
          }}
        >
          <FlexRow
            data-label='Navigation'
            style={{
              width: '100%',
              boxShadow: `inset 0px -1px 0px 0px ${colors.default}`,
              overflow: 'visible',
              cursor: 'pointer',
            }}
          >
            <FlexNavItem selected={this.state.mode === 'projects'} onClick={this.setProjectsMode}>
              Projects
            </FlexNavItem>
            <FlexNavItem selected={this.state.mode === 'filter'} onClick={this.setFilterMode}>
              Search
            </FlexNavItem>
          </FlexRow>
          <div
            data-label='sticky-header'
            style={{
              backgroundColor: 'white',
              zIndex: 99999,
              paddingLeft: layout.margins.wide,
              paddingRight: layout.margins.wide,
              paddingTop: layout.margins.wide,
              paddingBottom: layout.margins.regular,
            }}
          >
            <div>
              <H2>
                Recent Projects &nbsp;
                <span style={{ opacity: 0.3 }}>{visibleProjectCount}</span>
              </H2>
            </div>

            <div
              style={{
                marginTop: layout.margins.regular + 10,
                fontSize: 12,
                opacity: 0.7,
                display: 'flex',
                gap: '10px',
              }}
            >
              <label>Sort:</label>
              <SortButton
                selected={this.state.sortMode === 'date'}
                onClick={() => handleSortByDate()}
                sortOrder={this.state.dateSortOrder}
              >
                Date Edited
              </SortButton>
              <SortButton
                selected={this.state.sortMode === 'title'}
                onClick={() => handleSortByTitle()}
                sortOrder={this.state.titleSortOrder}
              >
                Title
              </SortButton>
            </div>
          </div>

          <div
            style={{
              background: colors.default,
              color: 'white',
              width: '100%',
            }}
          >
            {this.state.mode === 'filter' ? (
              <div style={{ padding: layout.margins.wide, minHeight: '80px' }}>
                <input
                  autoFocus={true}
                  onChange={this.onFilterChange}
                  style={{
                    fontSize: 40,
                    width: '100%',
                    minHeight: 60,
                    border: 'none',
                    background: 'transparent',
                    color: 'white',
                    outline: 'none',
                  }}
                  placeholder='Search for project names'
                  value={this.state.projectTitleFilter || ''}
                />
              </div>
            ) : null}
          </div>
          <FlexColumn
            style={{
              overflowY: 'scroll',
              flexGrow: 1,
              width: '100%',
              alignItems: 'stretch',
            }}
          >
            <FlexWrappingList
              className='roleProjectsSection'
              style={{
                flexGrow: 1,
                paddingTop: layout.margins.regular,
                paddingLeft: layout.margins.regular,
                paddingRight: layout.margins.regular,
                paddingBottom: layout.margins.wide,
              }}
            >
              {this.newProjectCard}
              {hasProjects ? this.state.filteredProjects.map(this.projectComponent) : null}
              {hasLocalProjects
                ? this.state.filteredLocalProjects.map(this.projectComponent)
                : null}
            </FlexWrappingList>
          </FlexColumn>
        </FlexColumn>
      </React.Fragment>
    )
  }
}

interface ShowcaseState {
  showcase: Array<ProjectListing>
}

export class FeaturedPage extends React.PureComponent<EmptyProps, ShowcaseState> {
  constructor(props: EmptyProps) {
    super(props)
    this.state = {
      showcase: [],
    }
  }

  componentDidMount() {
    fetchShowcaseProjects().then((showcase) => {
      this.setState({
        showcase: showcase,
      })
    })
  }

  render() {
    return (
      <div>
        {this.state.showcase.map((project) => (
          <ProjectCard key={project.id} project={project} selected={false} />
        ))}
      </div>
    )
  }
}
