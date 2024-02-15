import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { Category } from './routes/projects'

interface Store {
  selectedProjectId: string | null
  setSelectedProjectId: (projectId: string | null) => void
  selectedCategory: Category
  setSelectedCategory: (category: Category) => void
}

export const useStore = create<Store>()(
  devtools(
    persist(
      (set) => ({
        selectedCategory: 'allProjects',
        setSelectedCategory: (category: Category) => set(() => ({ selectedCategory: category })),
        selectedProjectId: null,
        setSelectedProjectId: (projectId: string | null) =>
          set(() => ({ selectedProjectId: projectId })),
      }),
      {
        name: 'store',
      },
    ),
  ),
)
