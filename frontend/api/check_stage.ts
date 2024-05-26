export enum StageType {
  CopyFile,
  ExecuteCommand,
  CopyAnswer,
  DeclareVariable,
  CompareResults,
  PSQLExists,
  PSQLQuery,
  PSQLAnswerQuery,
  PSQLGenerateDatabase,
  PSQLTableExists,
  PSQLColumnTypeCheck
}

export const actionToStageType = (action: string): StageType | null => {
  switch (action) {
    case 'copy':
      return StageType.CopyFile
    case 'command':
      return StageType.ExecuteCommand
    case 'copyAnswer':
      return StageType.CopyAnswer
    case 'declare':
      return StageType.DeclareVariable
    case 'compareVars':
      return StageType.CompareResults
    case 'psql_answer_query_macro':
      return StageType.PSQLAnswerQuery
    case 'psql_query_macro':
      return StageType.PSQLQuery
    case 'psql_exists_macro':
      return StageType.PSQLExists
    case 'psql_generate_database':
      return StageType.PSQLGenerateDatabase
    case 'psql_column_type_check':
      return StageType.PSQLColumnTypeCheck
    case 'psql_table_exists':
      return StageType.PSQLTableExists
    default:
      return null
  }
}

export const stageTypeList = [
  StageType.CopyFile, 
  StageType.ExecuteCommand, 
  StageType.CopyAnswer, 
  StageType.DeclareVariable,
  StageType.CompareResults,
  StageType.PSQLQuery,
  StageType.PSQLAnswerQuery,
  StageType.PSQLExists,
  StageType.PSQLGenerateDatabase,
  StageType.PSQLTableExists,
  StageType.PSQLColumnTypeCheck
]

export interface StageData {
  [key: string]: any
}

export const stageDataToCheckStage = (data: StageData): CheckStage | null => {
  const t = actionToStageType(data.action)
  if (t != null) {
    return {type: t, data: data}
  }
  return null
}

export type CheckStage = {
  type: StageType,
  data: StageData
}

export const defaultCheckStageData = (stType: StageType): object => {
  switch(stType) {
    case StageType.CopyFile:
      return {action: "copy", container: "", fileContent: "", filePath: ""}
    case StageType.ExecuteCommand:
      return {action: "command", container: "", command: "", recordInto: "", formatOutput: true, reportError: true}
    case StageType.CopyAnswer:
      return {action: "copyAnswer", container: "", filePath: ""}
    case StageType.DeclareVariable:
      return {action: "declare", variableName: "", variableValue: ""}
    case StageType.CompareResults:
      return {action: "compareVars", first: "", second: "", score: 0}
    case StageType.PSQLAnswerQuery:
      return {action: "psql_answer_query_macro", container: "", recordInto: ""}
    case StageType.PSQLQuery:
      return {action: "psql_query_macro", container: "", query: "", recordInto: ""}
    case StageType.PSQLExists:
      return {action: "psql_exists_macro", container: "", query: "", score: 0}
    case StageType.PSQLGenerateDatabase:
      return {action: "psql_generate_database", container: "", database: {tables: []}}
    case StageType.PSQLColumnTypeCheck:
      return {action: "psql_column_type_check", container: "", schema: "public", tableName: "", columnName: "", score: 0, reportError: false, awaitedType: ""}
    case StageType.PSQLTableExists:
      return {action: "psql_table_exists", container: "", schema: "public", tableName: "", score: 0, reportError: false}
  }
}

export const checkStageName = (stType: StageType): string => {
  switch(stType) {
    case StageType.CopyFile:
      return "Создать файл"
    case StageType.ExecuteCommand:
      return "Выполнить команду"
    case StageType.CopyAnswer:
      return "Скопировать ответ"
    case StageType.DeclareVariable:
      return "Объявить переменную"
    case StageType.CompareResults:
      return "Сопоставить переменные"
    case StageType.PSQLQuery:
      return "Выполнить PostgreSQL запрос"
    case StageType.PSQLExists:
      return "Проверить наличие данных в PostgreSQL"
    case StageType.PSQLAnswerQuery:
      return "Выполнить ответ пользователя как запрос PostgreSQL"
    case StageType.PSQLGenerateDatabase:
      return "Создать БД через визуальный конструктор"
    case StageType.PSQLTableExists:
      return "Проверка наличия таблицы"
    case StageType.PSQLColumnTypeCheck:
      return "Проверка типа колонки"
  }
}
