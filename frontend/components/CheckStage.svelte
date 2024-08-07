<script lang="ts">
  import TrashBinOutline from "./icons/TrashBinOutline.svelte"
  import EyeOutline from "./icons/EyeOutline.svelte"
  import EyeSlashOutline from "./icons/EyeSlashOutline.svelte"
  import CodemirrorField from "./CodemirrorField.svelte"
  import type { CheckStage, StageData } from "../api/check_stage"
  import { StageType } from "../api/check_stage"
  import { stageTypeList, defaultCheckStageData, checkStageName } from "../api/check_stage"
  import DangerMessage from "./DangerMessage.svelte"
  import SelectField from "./SelectField.svelte"
  import DBConstructor from "./DBConstructor.svelte"
  import CheckStageContainer from "./CheckStageContainer.svelte"
  
  export let additionalStyle: string = ''
  export let data: CheckStage
  export let containers: string[]
  export let updateCallback: (data: CheckStage) => Promise<void>
  export let deleteCallback: () => void = (() => {})
  export let selectedActionIndex: number = stageTypeList.indexOf(data.type)

  let hidden: boolean = true
  let selectedType: StageType
  $: selectedType = stageTypeList[selectedActionIndex]
  for (let i = 0; i < stageTypeList.length; i++) {
    if (stageTypeList[i] == data.type) {
      selectedActionIndex = i
      selectedType = stageTypeList[selectedActionIndex]
    }
  }

  const updateCallbackWrapper = async () => {
    await updateCallback(data)
  }

  const updateCheckStage = async () => {
    data.type = stageTypeList[selectedActionIndex]
    data.data = defaultCheckStageData(data.type)
    await updateCallback(data)
  }
</script>

<div class="check_action_card {additionalStyle}">
  <div class="flex flex-row justify-between">
    <div class="field">
      <label class="label" on:click={() => { hidden = !hidden }}> Тип действия </label>
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
    <div class="flex flex-wrap flex-row">
      <button on:click={deleteCallback} class="text-white mr-3">
        <div class="icon medium bg-red-700 font-bold rounded-sm text-xl">
          <TrashBinOutline/>
        </div>
      </button>
      <button on:click={() => { hidden = !hidden }} class="text-white">
        <div class="icon medium bg-sky-500 font-bold rounded-sm text-xl">
          {#if hidden }
            <EyeOutline/>
          {:else}
            <EyeSlashOutline/>
          {/if}
        </div>
      </button>
    </div>
  </div>

{#if !hidden}
  {#if selectedType == StageType.CopyFile }
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallbackWrapper() }}/>
    <div class="field">
      <label class="label"> Путь файла </label>
      <div class="control">
        <input class="input" type="text" placeholder="Путь файла, например /a.sql" bind:value={data.data.path} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Содержимое файла </label>
      <div class="control">
        <CodemirrorField bind:doc={data.data.content} onChange={async (v) => { updateCallbackWrapper()}}/>
      </div>
    </div>
  {:else if selectedType == StageType.ExecuteCommand }
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Команда для выполнения </label>
      <div class="control">
        <input class="input" type="text" placeholder="Команда, например psql -f /a.sql" bind:value={data.data.command} on:change={updateCallbackWrapper }>
      </div>
    </div>
    <div class="field">
      <label class="checkbox">
        <input type="checkbox" bind:checked={data.data.formatOutput} on:change={updateCallbackWrapper}/>
        Форматировать вывод
      </label>
    </div>
    <div class="field">
      <label class="checkbox">
        <input type="checkbox" bind:checked={data.data.reportError} on:change={updateCallbackWrapper}/>
        Сообщать пользователю об ошибках в процессе выполнения
      </label>
    </div>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={updateCallbackWrapper}>
      </div>
    </div>
  {:else if selectedType == StageType.AddPoints }
    <div class="field">
      <label class="label"> Количество баллов для добавления </label>
      <div class="control">
        <input class="input" type="number" min="1" placeholder="Количество баллов, больше 0" max="100" bind:value={data.data.amount} on:change={updateCallbackWrapper}/>
      </div>
    </div>
  {:else if selectedType == StageType.CopyAnswer}
    <SelectField title="Скопировать на" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Путь файла </label>
      <div class="control">
        <input class="input" type="text" placeholder="Путь для сохранения, например a.sql" bind:value={data.data.filePath} on:change={updateCallbackWrapper}>
      </div>
    </div>
  {:else if selectedType == StageType.DeclareVariable}
    <div class="field">
      <label class="label"> Название </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.variableName} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Значение </label>
      <div class="control">
        <CodemirrorField bind:doc={data.data.variableValue} onChange={async (v) => await updateCallbackWrapper()}/>
      </div>
    </div>
  {:else if selectedType == StageType.StopCheck || selectedType == StageType.AcceptCheck}
    <!-- TODO: output some message about no details -->
  {:else if selectedType == StageType.DisplayMessage}
    <div class="field">
      <label class="label"> Заголовок сообщения </label>
      <div class="control">
        <input class="input" type="text" bind:value={data.data.title} on:change={updateCallbackWrapper}/>
      </div>
    </div>
    <div class="field">
      <label class="label"> Сообщение </label>
      <div class="control">
        <textarea class="textarea" placeholder="Простое сообщение, Markdown не поддерживается" bind:value={data.data.message} on:change={updateCallbackWrapper}></textarea>
      </div>
    </div>
  {:else if selectedType == StageType.DisplayVariable}
    <div class="field">
      <label class="label"> Заголовок сообщения </label>
      <div class="control">
        <input class="input" type="text" bind:value={data.data.title} on:change={updateCallbackWrapper}/>
      </div>
    </div>
    <div class="field">
      <label class="label"> Подпись к значению переменной </label>
      <div class="control">
        <textarea class="textarea" placeholder="Простое сообщение, Markdown не поддерживается" bind:value={data.data.message} on:change={updateCallbackWrapper}></textarea>
      </div>
    </div>
    <div class="field">
      <label class="label"> Название переменной </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной" maxlength="25" bind:value={data.data.variableName} on:change={updateCallbackWrapper}>
      </div>
    </div>
  {:else if selectedType == StageType.SetPointsGate}
    <div class="field">
      <label class="label"> Количество баллов для принятия задания </label>
      <div class="control">
        <!-- TODO: decide about points limit -->
        <input class="input" type="number" min="1" placeholder="Количество баллов, больше 0" max="1000" bind:value={data.data.amount} on:change={updateCallbackWrapper}/>
      </div>
    </div>
  {:else if selectedType == StageType.CompareVariables}
    <div class="field">
      <label class="label"> Первая переменная </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной" maxlength="25" bind:value={data.data.first} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Вторая переменная </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной" maxlength="25" bind:value={data.data.second} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <CheckStageContainer
      stageStyle="positive"
      stages={data.data.positiveActions}
      containers={containers}
      title="Если переменные равны:"
      updateCallback={async (newStages) => {data.data.positiveActions = newStages; await updateCallbackWrapper()}}
    />
    <CheckStageContainer
      stageStyle="negative"
      stages={data.data.negativeActions}
      containers={containers}
      title="Если переменные не равны:"
      updateCallback={async (newStages) => {data.data.negativeActions = newStages; await updateCallbackWrapper()}}
    />
  {:else if selectedType == StageType.CompareLatestStatusCode}
    <div class="field">
      <label class="label"> Ожидаемый код завершения последней программы </label>
      <div class="control">
        <input class="input" type="number" placeholder="Код выхода" bind:value={data.data.awaitedStatus} on:change={updateCallbackWrapper}/>
      </div>
    </div>
    <CheckStageContainer
      stageStyle="positive"
      stages={data.data.positiveActions}
      containers={containers}
      title="Если код совпадает:"
      updateCallback={async (newStages) => {data.data.positiveActions = newStages; await updateCallbackWrapper()}}
    />
    <CheckStageContainer
      stageStyle="negative"
      stages={data.data.negativeActions}
      containers={containers}
      title="Если код не совпадает:"
      updateCallback={async (newStages) => {data.data.negativeActions = newStages; await updateCallbackWrapper()}}
    />
  {:else if selectedType == StageType.PSQLQuery}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> SQL-запрос </label>
      <div class="control">
        <CodemirrorField bind:doc={data.data.query} onChange={async (v) => await updateCallbackWrapper()}/>
      </div>
    </div>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={updateCallbackWrapper}>
      </div>
    </div>
  {:else if selectedType == StageType.PSQLAnswerQuery}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Записать в переменную (<i>опционально</i>)</label>
      <div class="control">
        <input class="input" type="text" placeholder="Название переменной, например result" maxlength="25" bind:value={data.data.recordInto} on:change={updateCallbackWrapper}>
      </div>
    </div>
  {:else if selectedType == StageType.PSQLExists}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> SQL-запрос </label>
      <div class="control">
        <CodemirrorField bind:doc={data.data.query} onChange={async (v) => await updateCallbackWrapper()}/>
      </div>
      <p class="help"> Запрос должен выполнять поиск определенных данных в единичном количестве. Запрос будет автоматически подставлен. Не ставьте точку с запятой в конце. </p>
    </div>
    <CheckStageContainer
      stageStyle="positive"
      stages={data.data.positiveActions}
      containers={containers}
      title="Если данные есть:"
      updateCallback={async (newStages) => {data.data.positiveActions = newStages; await updateCallbackWrapper()}}
    />
    <CheckStageContainer
      stageStyle="negative"
      stages={data.data.negativeActions}
      containers={containers}
      title="Если данных нет:"
      updateCallback={async (newStages) => {data.data.negativeActions = newStages; await updateCallbackWrapper()}}
    />
  {:else if selectedType == StageType.PSQLGenerateDatabase}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <DBConstructor dbData={data.data.database} changeCallback={(t) => data.data.database.tables = t}/>
  {:else if selectedType == StageType.PSQLTableExists}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Название таблицы </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название таблицы, без кавычек" bind:value={data.data.tableName} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <CheckStageContainer
      stageStyle="positive"
      stages={data.data.positiveActions}
      containers={containers}
      title="Если таблица существует:"
      updateCallback={async (newStages) => {data.data.positiveActions = newStages; await updateCallbackWrapper()}}
    />
    <CheckStageContainer
      stageStyle="negative"
      stages={data.data.negativeActions}
      containers={containers}
      title="Если таблица не существует:"
      updateCallback={async (newStages) => {data.data.negativeActions = newStages; await updateCallbackWrapper()}}
    />
  {:else if selectedType == StageType.PSQLColumnTypeCheck}
    <SelectField title="Контейнер выполнения" items={containers} selectCallback={async (v) => { data.data.target = v; await updateCallback(data) }}/>
    <div class="field">
      <label class="label"> Название таблицы </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название таблицы, без кавычек" bind:value={data.data.tableName} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Название столбца </label>
      <div class="control">
        <input class="input" type="text" placeholder="Название столбца" bind:value={data.data.columnName} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Ожидаемый тип </label>
      <div class="control">
        <input class="input" type="text" placeholder="Тип данных" bind:value={data.data.awaitedType} on:change={updateCallbackWrapper}>
      </div>
    </div>
    <CheckStageContainer
      stageStyle="positive"
      stages={data.data.positiveActions}
      containers={containers}
      title="Если тип совпал:"
      updateCallback={async (newStages) => {data.data.positiveActions = newStages; await updateCallbackWrapper()}}
    />
    <CheckStageContainer
      stageStyle="negative"
      stages={data.data.negativeActions}
      containers={containers}
      title="Если тип не совпал:"
      updateCallback={async (newStages) => {data.data.negativeActions = newStages; await updateCallbackWrapper()}}
    />
  {:else}
    <DangerMessage title="Ошибка!" description="Неизвестный тип"/>
  {/if}
{/if}
</div>
