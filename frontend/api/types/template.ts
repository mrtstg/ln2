import { genericArrayErrorCreator } from "./errorWrapper"

export type VMTemplateQuery = {
  pageSize: number,
  page: number,
  query: string
}

export type VMTemplate = {
  id: number,
  name: string,
  comment: string
}

export type VMTemplateCreate = {
  id: number,
  name: string,
  comment: string
}

export type VMTemplatePatch = {
  id?: number | null,
  name?: string | null,
  comment?: string | null
}

export const allTemplatesErrors = [
  'templateNameTaken',
  'templateIDTaken',
  'templateNotFound',
  'noUpdateBody',
  'forbiddenTemplateID',
  'emptyTemplateName',
  'longTemplateName',
  'longTemplateComment'
]

export const createTemplateError = genericArrayErrorCreator<TemplateError>(allTemplatesErrors)

export type TemplateError = typeof allTemplatesErrors[number]
