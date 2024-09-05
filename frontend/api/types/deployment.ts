export type DeploymentRead = {
  vmMap: object,
  userId: number,
  courseId: string,
  taskId: number,
  status: DeploymentStatus
}

export type DeploymentStatus = 
  "queued" | 
  "creating" |
  "created" |  
  "createError" |
  "deleting" |
  "deleteError" |
  "deleted"
