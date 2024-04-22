<script lang="ts">
  import { taskStatusToString } from "../../api/utils"
  import { ApiClient } from "../../api/client"
  import type { CourseTaskSolve, CourseSolvesResponse, TaskResult, TaskResultWrapper } from "../../api/types"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import WarningMessage from "../../components/WarningMessage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"

  // client
  const url = API_URL;
  const api = new ApiClient(url)

  let pageNum: number = 1

  let taskID: string | null = null
  let selectedTask: CourseTaskSolve | null
  $: selectedTask = null
  let taskSolvesPromise: Promise<CourseSolvesResponse> | null = null
  let taskResultPromise: Promise<TaskResultWrapper | null> | null = null

  let tasks: Array<CourseTaskSolve>
  $: tasks = []

  const taskSolvesWrapper = async (): Promise<CourseSolvesResponse> => {
    const res = await api.getCourseTaskSolves(parseInt(taskID!), pageNum)
    if (typeof res === 'string') {
      throw res
    }

    tasks = res.objects
    return res
  }

  const parsedURL = new URL(document.URL)
  const taskIDMatch = parsedURL.pathname.match("\/task\/([0-9].?)")
  if (taskIDMatch != null && taskIDMatch.length > 1) {
    taskID = taskIDMatch[1]
    taskSolvesPromise = taskSolvesWrapper()
  }

  const selectTask = (task: CourseTaskSolve) => {
    selectedTask = task
    taskResultPromise = api.getTaskDetails(task.id)
  }

  const unselectTask = () => {
    selectedTask = null
    taskResultPromise = null
  }

  const goToPage = (newNum: number) => {
    pageNum = newNum
    taskSolvesPromise = taskSolvesWrapper()
  }

</script>

{#if taskSolvesPromise != null}
  {#await taskSolvesPromise}
    <SuccessMessage title="Ожидайте" description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then taskResult}
    {#if selectedTask != null}
      <div class="box">
        <h2 class="subtitle is-4"> Решение { selectedTask.id } </h2>
        <div class="field">
          <label class="label"> Ответ пользователя </label>
          <textarea class="textarea" rows="{Math.min(25, selectedTask.input.split('\n').length)}" readonly>{ selectedTask.input }</textarea>
        </div>
        <button class="button is-danger is-fullwidth" on:click={unselectTask}> Назад </button>
        {#if taskResultPromise != null}
          <div class="pt-5">
            {#await taskResultPromise}
              <SuccessMessage title="Ожидайте..." description="Загружаем результаты проверки."/>
            {:then result}
              {#if result == null }
                <DangerMessage title="Ошибка!" description="Результат проверки задания больше не хранится в системе." additionalStyle="is-fullwidth"/>
              {:else}
                  <h3 class="subtitle is-5"> Результаты проверки </h3>
                  <p> Статус проверки: { taskStatusToString(result.status) } </p>
                  {#if result.result != null}
                    <p> Набрано баллов: { result.result.score } из { result.result.maxScore } </p>
                  {/if}
              {/if}
            {:catch e}
              <DangerMessage title="Ошибка!" description="Результат проверки задания был загружен с ошибкой." additionalStyle="is-fullwidth"/>
            {/await}
          </div>
        {/if}
      </div>
    {:else}
      {#if tasks.length > 0}
        <div class="columns is-multiline">
          {#each tasks as task}
            <div class="column is-6">
              <div class="card" on:click={() => selectTask(task)}>
                <header class="card-header">
                  {#if task.correct}
                    <p class="card-header-title has-text-success"> Решение { task.id } </p>
                  {:else}
                    <p class="card-header-title"> Решение { task.id } </p>
                  {/if}
                </header>
              </div>
            </div>
          {/each}
        </div>
        <nav class="pagination is-centered" role="navigation" aria-label="pagination">
          <ul class="pagination-list">
            {#if pageNum == 1}
              <li><a href="#" class="pagination-link is-current" aria-label="Page 1" aria-current="page"> 1 </a></li>
              {#if pageNum * taskResult.pageSize < taskResult.total}
                <li><a href="#" class="pagination-link" aria-label="Go to page 2" on:click={() => goToPage(2)}> 2 </a></li>
              {/if}
            {:else}
              <li><a href="#" class="pagination-link" aria-label="Go to page { pageNum - 1 }" on:click={() => goToPage(pageNum - 1)}> { pageNum - 1} </a></li>
              <li><a href="#" class="pagination-link is-current" aria-label="Page { pageNum }" aria-current="page"> { pageNum } </a></li>
              {#if pageNum * taskResult.pageSize < taskResult.total}
                <li><a href="#" class="pagination-link" aria-label="Go to page { pageNum + 1 }" on:click={() => goToPage(pageNum + 1)}> { pageNum + 1 } </a></li>
              {/if}
            {/if}
          </ul>
        </nav>
      {:else}
        <WarningMessage title="Внимание!" description="Вы еще не подавали решений на это задание!" additionalStyle="is-fullwidth"/>
      {/if}
    {/if}
  {:catch e}
    <DangerMessage title="Ошибка!" description="Не удалось загрузить решения." additionalStyle="is-fullwidth"/>
  {/await}
{/if}
