<script lang="ts">
  import { type CheckStage, stageTypeList, defaultCheckStageData } from "../api/check_stage"
  import CheckStageWidget from "../components/CheckStage.svelte"
  export let stages: Array<CheckStage> = []
  export let containers: string[]
  export let updateCallback: (data: CheckStage[]) => Promise<void>
  export let title: string = ''

  const deleteStage = (index: number) => {
    stages.splice(index, 1)
    stages = [...stages]
    updateCallback(stages).then(() => {})
  }

  const addStage = () => {
    stages = [...stages, {type: stageTypeList[0], data: defaultCheckStageData(stageTypeList[0])}]
    updateCallback(stages).then(() => {})
  }
</script>

<div class="flex flex-row justify-between">
  <h4 class="subtitle is-4"> 
    {#if title.length > 0}
      { title }
    {:else}
      Этапы проверки
    {/if}
  </h4>
  <button on:click={addStage} class="text-white">
    <div class="icon medium bg-emerald-400 font-bold rounded-sm">
      +
    </div>
  </button>
</div>
<div class="ml-5 overflow-y-auto">
  {#each stages as stage, stageIndex (stage)}
    <CheckStageWidget 
      data={stage} 
      containers={containers}
      updateCallback={async (v) => {stages[stageIndex] = v; await updateCallback(stages);}}
      deleteCallback={() => {deleteStage(stageIndex)}}
    />
  {/each}
</div>
