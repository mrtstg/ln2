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
  name: string,
  login: string,
  roles: [RoleDetails]
}

export type UserQuery = {
  total: number,
  pageSize: number,
  objects: [UserDetails]
}

export type CommonCourseDetails = {
  author: UserDetails | null,
  authorId: number,
  createdAt: Date,
  id: string,
  name: string,
  description: string
}

export type CourseTaskCreate = {
  name: string,
  content: string,
  order: number,
  standIdentifier: string,
  standActions: CheckStage.StageData[]
}

export type CourseCreate = {
  name: string,
  description: string
}

// TODO: add course field
export type CourseTaskDetails = {
  id: number,
  name: string,
  content: string,
  order: number,
  accepted?: Boolean,
  standIdentifier?: string,
  standActions: CheckStage.StageData[]
}

export type CourseSolvesResponse = {
  total: number,
  pageSize: number,
  objects: [CourseTaskSolve]
}

export type CourseTaskSolve = {
  id: string,
  correct: Boolean,
  input: string
}

export type TaskResultWrapper = {
  result: TaskResult | null,
  status: string
}

export type CheckMessageBlock = {
  type: string,
  content: string
}

export type CheckMessage = {
  title: string,
  blocks: [CheckMessageBlock]
}

export type TaskResult = {
  score: number,
  scoreGate: number,
  values: object,
  accepted: boolean,
  messages: [CheckMessage]
}

export type TaskCreateResponse = {
  uuid: string
}

export type UserCreate = {
  name: string,
  password: string,
  login: string
}

export type UserPatch = {
  name?: string | null,
  password?: string | null,
  login?: string | null
}

export type CourseTaskPatch = {
  name?: string,
  content?: string,
  order?: number
  standActions?: CheckStage.StageData[]
}
