<script lang="ts">
  import CodeMirror from "svelte-codemirror-editor"
  import type { CheckStage, StageData } from "../api/check_stage"
  import { StageType } from "../api/check_stage"
  import { stageTypeList, defaultCheckStageData, checkStageName } from "../api/check_stage"
  import DangerMessage from "./DangerMessage.svelte"
  import SelectField from "./SelectField.svelte"
  import DBConstructor from "./DBConstructor.svelte"

  export let data: CheckStage
  export let containers: string[]
  export let updateCallback: (data: CheckStage) => Promise<void>
  export let deleteCallback: () => void = (() => {})
  export let selectedActionIndex: number = stageTypeList.indexOf(data.type)

  let selectedType: StageType
  $: selectedType = stageTypeList[selectedActionIndex]
  for (let i = 0; i < stageTypeList.length; i++) {
    if (stageTypeList[i] == data.type) {
      selectedActionIndex = i
      selectedType = stageTypeList[selectedActionIndex]
    }
  }

  const updateCheckStage = async () => {
    data.type = stageTypeList[selectedActionIndex]
    data.data = defaultCheckStageData(data.type)
    await updateCallback(data)
  }
</script>

<div class="box my-3">
  <div class="field">
    <label class="label"> Тип действия </label>
    <div class="control">
      <div class="select">
        <select bind:value={selectedActionIndex} on:change={async (event) => { await updateCheckStage() }}>
          {#each stageTypeList as stageType, stageIndex}
            <option value={stageIndex}> {checkStageName(stageType)} </option>
          {/each}
        </select>
      </div>
    </div>
  </div>
  {#if selectedType == StageType.CopyFile }
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Путь файла </label>
      <div class="control">
        <input class="input" type="text" placeholder="Путь файла, например /a.sql" bind:value={data.data.filePath} on:change={async () => await updateCallback(data) }>
      </div>
    </div>
    <div class="field">
      <label class="label"> Содержимое файла </label>
      <div class="control">
        <CodeMirror styles={{
          "&": {
          "font-size": "1rem"
          }
        }} bind:value={data.data.fileContent} on:change={async () => await updateCallback(data) }/>
      </div>
    </div>
  {:else if selectedType == StageType.ExecuteCommand }
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Команда для выполнения </label>
      <div class="control">
        <input class="input" type="text" placeholder="Команда, например psql -f /a.sql" bind:value={data.data.command} on:change={async () => await updateCallback(data) }>
      </div>
    </div>
    <label class="checkbox">
      <input type="checkbox" bind:checked={data.data.formatOutput} on:change={async () => await updateCallback(data)}/>
      Форматировать вывод
    </label>
    <label class="checkbox">
      <input type="checkbox" bind:checked={data.data.reportError} on:change={async () => await updateCallback(data)}/>
      Сообщать пользователю об ошибках в процессе выполнения
    </label>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
  {:else if selectedType == StageType.CopyAnswer }
    <SelectField title="Скопировать на" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Путь файла </label>
      <div class="control">
        <input class="input" type="text" placeholder="Путь для сохранения, например a.sql" bind:value={data.data.filePath} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
  {:else if selectedType == StageType.DeclareVariable}
    <div class="field">
      <label class="label"> Название </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.variableName} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Значение </label>
      <div class="control">
        <CodeMirror styles={{
          "&": {
          "font-size": "1rem"
          }
        }} bind:value={data.data.variableValue} on:change={async () => await updateCallback(data) }/>
      </div>
    </div>
  {:else if selectedType == StageType.CompareResults}
    <div class="field">
      <label class="label"> Первая переменная </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной" maxlength="25" bind:value={data.data.first} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Вторая переменная </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной" maxlength="25" bind:value={data.data.second} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Количество зачтенных баллов </label>
      <div class="control">
        <input class="input" type="number" placeholder="Количество баллов" bind:value={data.data.score} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Текст сообщения при несоответствии данных (<i> опционально</i>) </label>
      <div class="control">
        <input class="input" type="text" placeholder="Сообщение" bind:value={data.data.failureMessage} on:change={async () => await updateCallback(data)}> 
      </div>
    </div>
  {:else if selectedType == StageType.PSQLQuery}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> SQL-запрос </label>
      <div class="control">
        <CodeMirror styles={{
          "&": {
          "font-size": "1rem"
          }
        }} bind:value={data.data.query} on:change={async () => await updateCallback(data) }/>
      </div>
      </div>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
  {:else if selectedType == StageType.PSQLAnswerQuery}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
  {:else if selectedType == StageType.PSQLExists}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> SQL-запрос </label>
      <div class="control">
        <CodeMirror styles={{
          "&": {
          "font-size": "1rem"
          }
        }} bind:value={data.data.query} on:change={async () => await updateCallback(data) }/>
      </div>
      <p class="help"> Запрос должен выполнять поиск определенных данных в единичном количестве. Запрос будет автоматически подставлен. Не ставьте точку с запятой в конце. </p>
    </div>
    <div class="field">
      <label class="label"> Количество зачтенных баллов, если данные в базе есть </label>
      <div class="control">
        <input class="input" type="number" placeholder="Количество баллов" bind:value={data.data.score} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
  {:else if selectedType == StageType.PSQLGenerateDatabase}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <DBConstructor dbData={data.data.database} changeCallback={(t) => data.data.database.tables = t}/>
    <br>
  {:else if selectedType == StageType.PSQLTableExists}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Название таблицы </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название таблицы, без кавычек" bind:value={data.data.tableName} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Количество зачтенных баллов, если таблица есть </label>
      <div class="control">
        <input class="input" type="number" placeholder="Количество баллов" bind:value={data.data.score} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="checkbox">
        <input type="checkbox" bind:checked={data.data.reportError} on:change={async () => await updateCallback(data)}/>
        Вывести ошибку пользователю, если таблица не найдена
      </label>
    </div>
  {:else if selectedType == StageType.PSQLColumnTypeCheck}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.container = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Название таблицы </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название таблицы, без кавычек" bind:value={data.data.tableName} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Название столбца </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название столбца" bind:value={data.data.columnName} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Ожидаемый тип </label>
      <div class="control">
        <input class="input" type="text" placeholder="Тип данных" bind:value={data.data.awaitedType} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Количество зачтенных баллов, если тип совпал </label>
      <div class="control">
        <input class="input" type="number" placeholder="Количество баллов" bind:value={data.data.score} on:change={async () => await updateCallback(data)}>
      </div>
    </div>
    <div class="field">
      <label class="checkbox">
        <input type="checkbox" bind:checked={data.data.reportError} on:change={async () => await updateCallback(data)}/>
        Вывести ошибку пользователю, если тип не совпадает
      </label>
    </div>
  {:else}
    <DangerMessage title="Ошибка!" description="Неизвестный тип"/>
  {/if}
  <button class="button is-danger is-fullwidth" on:click={deleteCallback}> Удалить </button>
</div>
