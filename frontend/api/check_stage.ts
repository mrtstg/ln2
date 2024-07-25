export enum StageType {
  CopyFile,
  ExecuteCommand,
  AddPoints,
  CopyAnswer,
  DeclareVariable,
  CompareVariables,
  CompareLatestStatusCode,
  DisplayMessage,
  DisplayVariable,
  StopCheck,
  AcceptCheck,
  SetPointsGate,
  PSQLExists,
  PSQLQuery,
  PSQLAnswerQuery,
  PSQLGenerateDatabase,
  PSQLTableExists,
  PSQLColumnTypeCheck,
}

export const actionToStageType = (action: string): StageType | null => {
  switch (action) {
    case "copy":
      return StageType.CopyFile;
    case "command":
      return StageType.ExecuteCommand;
    case "points":
      return StageType.AddPoints;
    case "copyAnswer":
      return StageType.CopyAnswer;
    case "declare":
      return StageType.DeclareVariable;
    case "stopCheck":
      return StageType.StopCheck;
    case "acceptCheck":
      return StageType.AcceptCheck;
    case "compareVars":
      return StageType.CompareVariables;
    case "compareStatusCode":
      return StageType.CompareLatestStatusCode;
    case "displayMessage":
      return StageType.DisplayMessage;
    case "displayVariable":
      return StageType.DisplayVariable;
    case "setPointsGate":
      return StageType.SetPointsGate;
    case "psql_answer_query_macro":
      return StageType.PSQLAnswerQuery;
    case "psql_query_macro":
      return StageType.PSQLQuery;
    case "psql_exists_macro":
      return StageType.PSQLExists;
    case "psql_generate_database":
      return StageType.PSQLGenerateDatabase;
    case "psql_column_type_check":
      return StageType.PSQLColumnTypeCheck;
    case "psql_table_exists":
      return StageType.PSQLTableExists;
    default:
      return null;
  }
};

export const stageTypeList = [
  StageType.CopyFile,
  StageType.ExecuteCommand,
  StageType.AddPoints,
  StageType.CopyAnswer,
  StageType.DeclareVariable,
  StageType.CompareVariables,
  StageType.CompareLatestStatusCode,
  StageType.DisplayMessage,
  StageType.DisplayVariable,
  StageType.StopCheck,
  StageType.AcceptCheck,
  StageType.SetPointsGate,
  StageType.PSQLExists,
  StageType.PSQLQuery,
  StageType.PSQLAnswerQuery,
  StageType.PSQLGenerateDatabase,
  StageType.PSQLTableExists,
  StageType.PSQLColumnTypeCheck,
];

export interface StageData {
  [key: string]: any;
}

export const countStages = (stages: CheckStage[], f: (arg0: CheckStage) => boolean): CheckStage[] => {
  let filteredStages: CheckStage[] = []
  for (let i = 0; i < stages.length; i++) {
    if (f(stages[i])) {
      filteredStages.push(stages[i])
    }
    console.log(stages[i].data.positiveActions)
    if (stages[i].data.positiveActions != undefined) {
      filteredStages.push.apply(filteredStages, countStages(stages[i].data.positiveActions, f))
    }
    if (stages[i].data.negativeActions != undefined) {
      filteredStages.push.apply(filteredStages, countStages(stages[i].data.negativeActions, f))
    }
  }
  return filteredStages
}

// applies stage data to appropriate for backend form
export const processStageData = (stageData: StageData): StageData => {
  let ndata = {...stageData}
  if (["command", "psql_query_macro", "psql_answer_query_macro"].includes(ndata.action) && ndata.recordInto.length == 0) {
    ndata.recordInto = null
  }
  if (ndata.positiveActions != undefined) {
    ndata.positiveActions = ndata.positiveActions.map(el => processStageData(el.data))
  }
  if (ndata.negativeActions != undefined) {
    ndata.negativeActions = ndata.negativeActions.map(el => processStageData(el.data))
  }

  return ndata
}

export const stageDataToCheckStage = (data: StageData): CheckStage | null => {
  const t = actionToStageType(data.action);
  if (t != null) {
    let ndata = {...data}
    if (data.positiveActions != undefined) {
      ndata.positiveActions = ndata.positiveActions.map(el => stageDataToCheckStage(el))
    }
    if (data.negativeActions != undefined) {
      ndata.negativeActions = ndata.negativeActions.map(el => stageDataToCheckStage(el))
    }
    return { type: t, data: ndata };
  }
  return null;
};

export type CheckStage = {
  type: StageType;
  data: StageData;
};

export const defaultCheckStageData = (stType: StageType): object => {
  switch (stType) {
    case StageType.CopyFile:
      return { action: "copy", target: "", content: "", path: "" };
    case StageType.ExecuteCommand:
      return {
        action: "command",
        target: "",
        command: "",
        recordInto: "",
        formatOutput: true,
        reportError: true,
      };
    case StageType.AddPoints:
      return { action: "points", amount: 0 };
    case StageType.CopyAnswer:
      return { action: "copyAnswer", target: "", filePath: "" };
    case StageType.DeclareVariable:
      return { action: "declare", variableName: "", variableValue: "" };
    case StageType.StopCheck:
      return { action: "stopCheck" };
    case StageType.AcceptCheck:
      return { action: "acceptCheck" };
    case StageType.CompareVariables:
      return {
        action: "compareVars",
        first: "",
        second: "",
        positiveActions: [],
        negativeActions: [],
      };
    case StageType.CompareLatestStatusCode:
      return {
        action: "compareStatusCode",
        awaitedStatus: 0,
        positiveActions: [],
        negativeActions: [],
      };
    case StageType.DisplayMessage:
      return {
        action: "displayMessage",
        message: "",
        title: "",
      };
    case StageType.DisplayVariable:
      return {
        action: "displayVariable",
        variableName: "",
        message: "",
        title: "",
      };
    case StageType.SetPointsGate:
      return {
        action: "setPointsGate",
        amount: 0,
      };
    case StageType.PSQLAnswerQuery:
      return {
        action: "psql_answer_query_macro",
        target: "",
        recordInto: "",
      };
    case StageType.PSQLQuery:
      return {
        action: "psql_query_macro",
        target: "",
        query: "",
        recordInto: "",
      };
    case StageType.PSQLExists:
      return {
        action: "psql_exists_macro",
        target: "",
        query: "",
        positiveActions: [],
        negativeActions: [],
      };
    case StageType.PSQLGenerateDatabase:
      return {
        action: "psql_generate_database",
        target: "",
        database: { tables: [] },
      };
    case StageType.PSQLColumnTypeCheck:
      return {
        action: "psql_column_type_check",
        target: "",
        schema: "public",
        tableName: "",
        columnName: "",
        awaitedType: "",
        positiveActions: [],
        negativeActions: [],
      };
    case StageType.PSQLTableExists:
      return {
        action: "psql_table_exists",
        target: "",
        schema: "public",
        tableName: "",
        positiveActions: [],
        negativeActions: [],
      };
  }
};

export const checkStageName = (stType: StageType): string => {
  switch (stType) {
    case StageType.CopyFile:
      return "Создать файл";
    case StageType.ExecuteCommand:
      return "Выполнить команду";
    case StageType.AddPoints:
      return "Добавить баллы"
    case StageType.CopyAnswer:
      return "Скопировать ответ";
    case StageType.DeclareVariable:
      return "Объявить переменную";
    case StageType.CompareVariables:
      return "Сопоставить переменные";
    case StageType.CompareLatestStatusCode:
      return "Сопоставить последний код выхода"
    case StageType.DisplayMessage:
      return "Вывести сообщение"
    case StageType.DisplayVariable:
      return "Вывести переменную"
    case StageType.StopCheck:
      return "Прервать проверку"
    case StageType.AcceptCheck:
      return "Зачесть проверку"
    case StageType.SetPointsGate:
      return "Установить порог прохождения (в баллах)"
    case StageType.PSQLQuery:
      return "Выполнить PostgreSQL запрос";
    case StageType.PSQLExists:
      return "Проверить наличие данных в PostgreSQL";
    case StageType.PSQLAnswerQuery:
      return "Выполнить ответ пользователя как запрос PostgreSQL";
    case StageType.PSQLGenerateDatabase:
      return "Создать БД через визуальный конструктор";
    case StageType.PSQLTableExists:
      return "Проверка наличия таблицы";
    case StageType.PSQLColumnTypeCheck:
      return "Проверка типа колонки";
  }
};
