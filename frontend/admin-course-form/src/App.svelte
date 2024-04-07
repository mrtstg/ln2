<script lang="ts">
  import * as Icon from 'flowbite-svelte-icons'
  import { ApiClient } from "../../api/client"
  import type { ContainerSummary } from "../../api/types";
  import DangerMessage from "../../components/DangerMessage.svelte"
  import CheckStageWidget from "../../components/CheckStage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import { type StageType, type CheckStage, defaultCheckStageData } from "../../api/check_stage" 
  import { stageTypeList } from "../../api/check_stage"

  const url = API_URL;
  const api = new ApiClient(url)

  let selectedStand: string;
  $: selectedStand = ''

  let stages: CheckStage[]
  $: stages = []

  const addStage = () => {
    stages = [...stages, {type: stageTypeList[0], data: defaultCheckStageData(stageTypeList[0])}]
  }

  let standsPromise = api.getStands()
  let containersPromise: Promise<Array<ContainerSummary>> | null = null
</script>

<div class="container p-5">
  {#await standsPromise}
    <SuccessMessage title="Ожидайте..." description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then standsData}
    <h2 class="title is-3"> Схема проверки </h2>
    <h3 class="title is-4"> Выберите стенд </h3>
    <div class="field">
      <div class="control">
        <div class="select">
          <select bind:value={selectedStand} on:change={() => containersPromise = api.getStandContainers(selectedStand)}>
            <option disabled> Выберите стенд </option>
            {#each standsData as standName}
              <option value={standName}> { standName } </option>
            {/each}
          </select>
        </div>
      </div>
    </div>

    {#if containersPromise != null}
      {#await containersPromise}
        <SuccessMessage title="Ожидайте..." description="Загружаем данные..."/>
      {:then containersData}
        <div class="is-flex">
          <h3 class="title is-4 pr-3"> Этапы проверки </h3>
          <div>
            <button class="button is-link" on:click={addStage}>
              <span class="icon is-large">
                <Icon.PlusOutline/>
              </span>
            </button>
          </div>
        </div>
        {#each stages as stage, stageIndex}
          <CheckStageWidget data={stage} containers={containersData.map(v => v.name)} updateCallback={async (v) => { stages[stageIndex] = v }}/>
        {/each}
        <button on:click={addStage} class="is-link button is-fullwidth"> Добавить </button>
        <br>
        { JSON.stringify(stages) }
      {:catch error}
        <DangerMessage title="Ошибка!" description="Не удалось загрузить данные стенда."/>
      {/await}
    {/if}
  {:catch error}
    <DangerMessage title="Ошибка!" description="Не удалось получить данные о доступных стеднах."/>
  {/await}
</div>
