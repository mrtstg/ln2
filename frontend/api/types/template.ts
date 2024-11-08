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
