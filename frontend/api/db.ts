export const allTypes = [
  "varchar",
  "char",
  "serial",
  "boolean",
  "integer",
  "text"
]

export type TableData = {
  name: string,
  columns: Array<ColumnData>
}

export type DatabaseData = {
  tables: Array<TableData>
}

export type ColumnData = {
  name: string,
  type: ColumnType,
  primary: boolean,
  null: boolean,
  unique: boolean,
  references: string | null
}

export type ColumnType = {
  type: string,
  size?: number
}
