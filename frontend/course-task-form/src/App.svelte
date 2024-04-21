<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { CourseTaskSolve, CourseSolvesResponse, TaskResult } from "../../api/types"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"

  // client
  const url = API_URL;
  const api = new ApiClient(url)

  let pageNum: number = 1

  let taskID: string | null = null
  let selectedTask: CourseTaskSolve | null
  $: selectedTask = null
  let taskSolvesPromise: Promise<CourseSolvesResponse> | null = null
  let taskResultPromise: Promise<TaskResult | null> | null = null

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

</script>

{#if taskSolvesPromise != null}
  {#await taskSolvesPromise}
    <SuccessMessage title="Ожидайте" description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then result}
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
              {#if result == null}
                <DangerMessage title="Ошибка!" description="Результат проверки задания больше не хранится в системе." additionalStyle="is-fullwidth"/>
              {:else}
                  <h3 class="subtitle is-5"> Результаты проверки </h3>
                  <p> Набрано баллов: { result.score } из { result.maxScore } </p>
              {/if}
            {:catch e}
              <DangerMessage title="Ошибка!" description="Результат проверки задания был загружен с ошибкой." additionalStyle="is-fullwidth"/>
            {/await}
          </div>
        {/if}
      </div>
    {:else}
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
    {/if}
  {:catch e}
    <DangerMessage title="Ошибка!" description="Не удалось загрузить решения." additionalStyle="is-fullwidth"/>
  {/await}
{/if}
