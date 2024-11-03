export type TaskDeploymentWrapper = {
  data: DeploymentRead | null,
  pending: boolean
}

export type DeploymentRead = {
  id: string,
  vmMap: object,
  userId: number,
  courseId: string,
  taskId: number,
  status: DeploymentStatus,
  courseName: string | null,
  taskName: string | null
}

export type DeploymentStatus = 
  "queued" | 
  "creating" |
  "created" |  
  "createError" |
  "deleting" |
  "deleteError" |
  "deleted"

export const allDeploymentErrorKinds = [
  'template',
  'missingNetwork',
  'forbiddenNetwork',
  'emptyVMName',
  'emptyNetworkName',
  'longVMName',
  'longNetworkName',
  'invalidCPU',
  'invalidSockets',
  'invalidMemory',
  'duplicateVMName',
  'duplicateNetworkName',
  'unknown'
  ]

export type DeploymentErrorKind = typeof allDeploymentErrorKinds[number]
