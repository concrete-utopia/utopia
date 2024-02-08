export interface ProjectListing {
  id: string;
  ownerName: string | null;
  ownerPicture: string | null;
  title: string;
  description: string | null;
  createdAt: string;
  modifiedAt: string;
}

export type ListProjectsResponse = {
  projects: ProjectListing[];
};
