/** @jsxRuntime classic */
/** @jsx jsx */
import React, { useState } from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import { layout, FlexRow, FlexColumn, FlexWrappingList } from './layout'
import { H2 } from './style'
import { colors, darkColors } from './theme'

import { Global } from '@emotion/react'
import { fetchProjectList, fetchShowcaseProjects } from '../common/server'
import type { ProjectListing } from '../common/persistence'
import { deleteProject, fetchProjectListFromLocalStorage } from '../common/persistence'
import * as timeago from 'timeago.js'
import { Card, cardLayout, cardLayoutStyle } from './cards'

interface SortButtonProps {
  selected: boolean
  sortOrder: number
}

const SortButton = styled('div')<SortButtonProps>(
  {
    color: colors.mainBlue,
    textDecoration: 'none',
    cursor: 'pointer',
    padding: '6px',
    userSelect: 'none',
    borderRadius: 3,
    transition: '.2s ease',
    '&:hover': {
      opacity: 0.7,
    },
    '&:active': {
      background: colors.mainBlue,
      color: 'white',
    },
  },
  (props) => ({
    background:
      props.sortOrder === 0
        ? colors.mainPink
        : props.sortOrder === 1
        ? colors.mainBlue
        : 'undefined',
    color:
      props.sortOrder === 0
        ? colors.mainBlue
        : props.sortOrder === 1
        ? colors.mainPink
        : colors.mainBlue,
  }),
)

interface ProjectCardProps {
  selected: boolean
  project: ProjectListing | null
  thumbnail: any
  modifiedAt: any
  title: any
  key: any
  url: any

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
      if (this.props.project === null) {
        window.open(this.props.url, '_self')
      } else {
        window.open(`/project/${this.props.project.id}/`, '_self')
      }
    }
  }

  render() {
    return (
      <Card
        selected={this.props.selected}
        data-label='project card'
        onMouseDown={this.onMouseDown}
        onDoubleClick={this.onDoubleClick}
        key={this.props.key}
      >
        <div
          className='projecttile-thumbnail'
          style={{ zIndex: 0 }}
          css={{
            background: `url(${this.props.thumbnail}) no-repeat 50% 50%`,
            backgroundSize: '100%',
            height: cardLayout.imageHeight,
            transition: 'all 0.2s ease-in-out',
            '&:hover': { backgroundSize: '120%' },
          }}
        ></div>
        <div
          className='projecttile-description'
          style={{
            display: 'flex',
            height: cardLayout.footerHeight,
            padding: '12px',
            boxShadow: `0px -1px 0px ${colors.default}`,
            zIndex: 1000,
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
              {this.props.title == null ? 'Unnamed' : this.props.title}
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
              <span className='timeago'>
                {this.props.modifiedAt
                  ? 'Last edited about ' + timeago.format(this.props.modifiedAt)
                  : 'By The Utopia Team'}
              </span>
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
  sortMode: 'title' | 'date'
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
      sortMode: 'title',
      dateSortOrder: 0,
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
        break
      case 70:
        if (event.metaKey) {
          event.preventDefault()
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
        key={project.id}
        url={null}
        thumbnail={project.thumbnail}
        modifiedAt={project.modifiedAt}
        title={project.title}
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
        WebkitUserSelect: 'none',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        position: 'relative',
        cursor: 'pointer',
        fontWeight: 700,
        fontSize: '130px',
        border: `1px solid ${colors.default}`,
        background: 'linear-gradient(180deg, #F3FFA8 0%, #FFE7A9 50%, #FFB8F8 100%)',

        WebkitTextStroke: colors.default,
        WebkitTextStrokeWidth: '1px',
        color: '#00000005',
        transition: 'all .1s ease-in-out',
        '&:hover': {
          color: colors.default,
        },
        '&:active': {
          transform: 'scale(.99)',
        },
        borderRadius: '5px',
        overflow: 'hidden',
        ['@media (prefers-color-scheme: dark)']: {
          border: `1px solid ${darkColors.default}`,
        },
        ...cardLayoutStyle,
      }}
      onMouseUp={this.createNewProject}
    >
      +
    </div>
  )

  clearSelectedProjectId = () => {
    this.setState({ selectedProjectId: null })
  }

  clearSelectedProject = () => this.setState({ selectedProjectId: null })

  render() {
    const hasProjects = this.state.filteredProjects.length > 0
    const hasLocalProjects = this.state.filteredLocalProjects.length > 0
    const visibleProjectCount =
      this.state.filteredProjects.length + this.state.filteredLocalProjects.length

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
      if (dateSortOrder === 2) {
        this.setState({ filteredProjects: projectsSortedByDate })
        this.setState({ dateSortOrder: (dateSortOrder + 1) % 3 })
      } else if (dateSortOrder === 0) {
        this.setState({ filteredProjects: projectsSortedByDate.reverse() })
        this.setState({ dateSortOrder: (dateSortOrder + 1) % 3 })
      } else if (dateSortOrder === 1) {
        this.setState({ filteredProjects: projectsSortedByDate })
        this.setState({ dateSortOrder: 0 })
      }
      this.setState({ titleSortOrder: 2 })
    }

    const titleSortOrder = this.state.titleSortOrder
    const handleSortByTitle = () => {
      if (titleSortOrder === 2) {
        this.setState({ filteredProjects: projectsSortedByTitle })
        this.setState({ titleSortOrder: (titleSortOrder + 1) % 3 })
      } else if (titleSortOrder === 0) {
        this.setState({ filteredProjects: projectsSortedByTitle.reverse() })
        this.setState({ titleSortOrder: (titleSortOrder + 1) % 3 })
      } else if (titleSortOrder === 1) {
        this.setState({ filteredProjects: projectsSortedByTitle })
        this.setState({ titleSortOrder: 0 })
      }

      this.setState({ dateSortOrder: 2 })
    }

    return (
      <React.Fragment>
        <Global
          styles={{
            body: {
              fontFamily: 'Inter, sans-serif',
              fontSize: 13,
              color: colors.default,
              overflowX: 'hidden',
            },
            ['@media (prefers-color-scheme: dark)']: {
              body: {
                color: darkColors.default,
                backgroundColor: darkColors.background,
              },
            },
          }}
        />

        <FlexColumn
          onMouseDown={this.clearSelectedProject}
          style={{
            alignItems: 'flex-start',
            height: '100vh',
            margin: layout.margins.regular,
            display: 'flex',
            flexDirection: 'column',
            gap: layout.margins.wide,
          }}
        >
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',

              fontFamily: 'Reckless Neue',
              fontSize: '28pt',
              gap: layout.margins.regular,
            }}
          >
            <div
              css={{
                height: 60,
                width: 45,
                backgroundSize: '45px',
                backgroundRepeat: 'no-repeat',
                backgroundImage:
                  'url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_light.png?raw=true)',
                ['@media (prefers-color-scheme: dark)']: {
                  backgroundImage:
                    'url(https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_dark.png?raw=true)',
                },
              }}
            />
            Utopia
          </div>
          <FlexWrappingList
            style={{ width: '100vw', gap: layout.margins.regular }}
            onClick={this.clearSelectedProject}
          >
            {this.newProjectCard}
            <ProjectCard
              project={null}
              key={null}
              url={'https://utopia.pizza/p/3551bda9-before-i-go-basic/'}
              thumbnail={'https://cdn.utopia.app/editor/sample-assets/preview-images/basic.png'}
              modifiedAt={null}
              title={'Before I Go Basic'}
              selected={false}
            />
            <ProjectCard
              project={null}
              key={null}
              url={'https://utopia.app/p/b7508bfa-before-i-go-(premium)/'}
              thumbnail={'https://cdn.utopia.app/editor/sample-assets/preview-images/premium.png'}
              modifiedAt={null}
              title={'Before I Go Premium'}
              selected={false}
            />
          </FlexWrappingList>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 15, width: '100%' }}>
            <H2>
              Recent Projects &nbsp;
              <span style={{ opacity: 0.3 }}>{visibleProjectCount}</span>
            </H2>

            <div
              style={{
                width: '100%',
                display: 'flex',
                flexDirection: 'row',
                justifyContent: 'space-between',
                alignItems: 'center',
              }}
            >
              <div
                css={{
                  padding: 10,
                  background: '#ececec',
                  borderRadius: '5px',
                  width: '310px',
                  color: 'grey',
                  ['@media (prefers-color-scheme: dark)']: {
                    background: '#535353',
                  },
                }}
              >
                <input
                  autoFocus={true}
                  onChange={this.onFilterChange}
                  style={{
                    border: 'none',
                    background: 'transparent',
                    outline: 'none',
                    color: 'grey',
                  }}
                  placeholder='Search for projects'
                  value={this.state.projectTitleFilter || ''}
                />
              </div>
              <div
                style={{
                  fontSize: 12,
                  opacity: 0.7,
                  display: 'flex',
                  gap: '10px',
                  alignItems: 'center',
                  height: '100%',
                }}
              >
                <label>Sort</label>
                <SortButton
                  selected={this.state.sortMode === 'date'}
                  // eslint-disable-next-line react/jsx-no-bind
                  onClick={handleSortByDate}
                  sortOrder={this.state.dateSortOrder}
                >
                  Date Edited
                </SortButton>
                <SortButton
                  selected={this.state.sortMode === 'title'}
                  // eslint-disable-next-line react/jsx-no-bind
                  onClick={handleSortByTitle}
                  sortOrder={this.state.titleSortOrder}
                >
                  Title
                </SortButton>
              </div>
            </div>
          </div>

          <FlexColumn>
            <FlexWrappingList
              className='roleProjectsSection'
              style={{
                gap: layout.margins.regular,
                marginBottom: 50,
              }}
            >
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
          <ProjectCard
            project={project}
            key={project.id}
            url={null}
            thumbnail={project.thumbnail}
            modifiedAt={project.modifiedAt}
            title={project.title}
            selected={false}
          />
        ))}
      </div>
    )
  }
}
