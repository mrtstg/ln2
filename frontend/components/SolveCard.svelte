<script lang="ts">
  import { ApiClient } from "../api/client"
  import type { CourseTaskSolve, TaskResult, TaskResultWrapper } from "../api/types"
  import { taskStatusToString } from "../api/utils"

  export let apiUrl: string;
  export let solveData: CourseTaskSolve
  export let selectCallback: (task: CourseTaskSolve) => Promise<void>

  const api = new ApiClient(apiUrl)
  const detailsCallback: Promise<TaskResultWrapper | null> = api.getTaskDetails(solveData.id)
</script>

<div class="card" on:click={() => selectCallback(solveData)}>
  <header class="card-header">
    {#if solveData.correct}
      <p class="card-header-title has-text-success"> Решение { solveData.id } </p>
    {:else}
      <p class="card-header-title"> Решение {solveData.id} </p>
    {/if}
  </header>
  {#await detailsCallback}
  {:then res}
    <div class="card-content">
      {#if res != null}
        <p> Статус: { taskStatusToString(res.status) } </p>
        {#if res.result != null}
          <p>{ res.result.score } / { res.result.scoreGate } баллов</p>
          {#if res.result.accepted }
            <p class="text-green-500"> Решение зачтено </p>
          {/if}
        {:else}
          Итогов проверки нет
        {/if}
      {:else}
        <p> Данные проверки не представлены </p>
      {/if}
    </div>
  {/await}
</div>
