import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { Category } from './routes/projects'

interface Store {
  selectedCategory: Category
  setSelectedCategory: (category: Category) => void
}

export const useStore = create<Store>()(
  devtools(
    persist(
      (set) => ({
        selectedCategory: 'allProjects',
        setSelectedCategory: (category: Category) => set(() => ({ selectedCategory: category })),
      }),
      {
        name: 'store',
      },
    ),
  ),
)
