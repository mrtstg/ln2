import * as CheckStage from "./checkStage.ts"
import * as U from "./types/user.ts"

export type ContainerSummary = {
  image: string,
  name: string
}

export type CommonCourseDetails = {
  author: U.UserDetails | null,
  authorId: number,
  createdAt: Date,
  id: string,
  name: string,
  description: string
}

export type CourseTaskType = 'vm' | 'container'

export type TaskPayload = {
  actions?: CheckStage.StageData[],
  standIdentifier?: string,
  type: CourseTaskType
}

export type CourseTaskCreate = {
  name: string,
  content: string,
  order: number,
  payload: TaskPayload
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
  payload?: TaskPayload,
  type?: CourseTaskType
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

export type CourseTaskPatch = {
  name?: string,
  content?: string,
  order?: number
  payload?: TaskPayload
}
