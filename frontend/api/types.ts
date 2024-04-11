import * as CheckStage from "./check_stage.ts"

export type ContainerSummary = {
  image: string,
  name: string
}

export type RoleDetails = {
  name: string,
  displayName: string
}

export type UserDetails = {
  id: number,
  login: string,
  roles: [RoleDetails]
}

export type CommonCourseDetails = {
  author: UserDetails | null,
  authorId: number,
  createdAt: Date,
  id: string,
  name: string
}

export type CourseTaskCreate = {
  name: string,
  content: string,
  order: number,
  standIdentifier: string,
  standActions: CheckStage.StageData[]
}

// TODO: add course field
export type CourseTaskDetails = {
  id: number,
  name: string,
  content: string,
  order: number,
  accepted?: Boolean,
  standIdentifier?: string,
  standActions?: [object]
}