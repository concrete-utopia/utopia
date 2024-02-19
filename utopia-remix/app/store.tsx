import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { Category } from './routes/projects'

interface ProjectsStore {
  selectedProjectId: string | null
  setSelectedProjectId: (projectId: string | null) => void
  selectedCategory: Category
  setSelectedCategory: (category: Category) => void
  searchQuery: string
  setSearchQuery: (query: string) => void
}

export const useProjectsStore = create<ProjectsStore>()(
  devtools(
    persist(
      (set) => ({
        selectedCategory: 'allProjects',
        setSelectedCategory: (category: Category) => set(() => ({ selectedCategory: category })),
        selectedProjectId: null,
        setSelectedProjectId: (projectId: string | null) =>
          set(() => ({ selectedProjectId: projectId })),
        searchQuery: '',
        setSearchQuery: (query) => set(() => ({ searchQuery: query })),
      }),
      {
        name: 'store',
      },
    ),
  ),
)
