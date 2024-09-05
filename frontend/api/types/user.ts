import * as R from "./role"

export type UserDetails = {
  id: number,
  name: string,
  login: string,
  roles: [R.RoleDetails]
}

export type UserQuery = {
  total: number,
  pageSize: number,
  objects: [UserDetails]
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
