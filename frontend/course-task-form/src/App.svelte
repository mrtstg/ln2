<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { CourseTaskSolve, CourseSolvesResponse } from "../../api/types"
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
          <textarea class="textarea" rows="25" readonly>{ selectedTask.input }</textarea>
        </div>
        <button class="button is-danger is-fullwidth" on:click={() => selectedTask = null}> Назад </button>
      </div>
    {:else}
      <div class="columns is-multiline">
        {#each tasks as task}
          <div class="column is-6">
            <div class="card" on:click={() => selectedTask = task}>
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
