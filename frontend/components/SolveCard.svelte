<script lang="ts">
  import { onMount } from "svelte";
  import { ApiClient } from "../api/client"
  import type { CourseTaskSolve, TaskResult, TaskStatus, TaskResultWrapper } from "../api/types"
  import { taskStatusToString } from "../api/utils"

  export let apiUrl: string;
  export let solveData: CourseTaskSolve
  export let selectCallback: (task: CourseTaskSolve) => Promise<void>

  let solveDetails: TaskResultWrapper | null = null
  let invervalID: number | null = null

  const taskDetailsWrapper = async (id: string): Promise<TaskResultWrapper | null> => {
    const res = await api.getTaskDetails(id)
    if (res != null) {
      solveDetails = res

      if (
          invervalID == null && 
          (res.status != 'error' 
            && res.status != 'timeout' 
            && res.status != 'finished' 
            && res.status != 'accepted' 
            && res.status != 'cancelled')
        ) {
          invervalID = setInterval(() => {
            detailsCallback = taskDetailsWrapper(solveData.id)
          }, 1500)
        }

      if (
        invervalID != null &&
        (res.status != 'queued' && res.status != 'taken' && res.status != 'processing')
      ) {
        clearInterval(invervalID)
      }
    }

    return res
  }

  onMount(() => {
    return () => {
      if (invervalID != null) {
        clearInterval(invervalID)
      }
    }
  })

  const api = new ApiClient(apiUrl)
  let detailsCallback: Promise<TaskResultWrapper | null> = taskDetailsWrapper(solveData.id)
</script>

<div class="card" on:click={() => selectCallback(solveData)}>
  <header class="card-header">
    {#if solveData.correct}
      <p class="card-header-title has-text-success"> Решение { solveData.id } </p>
    {:else}
      <p class="card-header-title"> Решение {solveData.id} </p>
    {/if}
  </header>
  <div class="card-content">
    {#await detailsCallback}
    {/await}
    {#if solveDetails != null}
      <p> Статус: { taskStatusToString(solveDetails.status) } </p>
      {#if solveDetails.result != null}
        {#if solveDetails.result.score != 0 && solveDetails.result.scoreGate != 0}
          <p>{ solveDetails.result.score } / { solveDetails.result.scoreGate } баллов</p>
        {/if}
        {#if solveDetails.result.accepted }
          <p class="text-green-500"> Решение зачтено </p>
        {/if}
      {:else}
        Итогов проверки нет
      {/if}
    {:else}
      <p> Данные проверки не представлены </p>
    {/if}
  </div>
</div>
