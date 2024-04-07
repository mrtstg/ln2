export enum StageType {
  CopyFile,
  ExecuteCommand,
  CopyAnswer,
  DeclareVariable,
  CompareResults
}

export const stageTypeList = [
  StageType.CopyFile, 
  StageType.ExecuteCommand, 
  StageType.CopyAnswer, 
  StageType.DeclareVariable,
  StageType.CompareResults
]

export interface StageData {
  [key: string]: any
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
      return {action: "command", container: "", command: "", recordInto: "", formatOutput: true}
    case StageType.CopyAnswer:
      return {action: "copyAnswer", container: "", filePath: ""}
    case StageType.DeclareVariable:
      return {action: "declare", variableName: "", variableValue: ""}
    case StageType.CompareResults:
      return {action: "compareVars", first: "", second: "", score: 0}
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
  }
}
