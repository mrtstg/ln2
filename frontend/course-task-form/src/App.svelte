<script lang="ts">
  import CodeMirror, { basicSetup } from "codemirror-svelte"
  import { EditorView } from "@codemirror/view";
  import { EditorState } from "@codemirror/state";

  import { taskStatusToString, taskCreateErrorToString } from "../../api/utils"
  import { ApiClient } from "../../api/client"
  import type { CourseTaskSolve, CourseSolvesResponse, TaskResult, TaskResultWrapper, TaskCreateResponse } from "../../api/types"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import WarningMessage from "../../components/WarningMessage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import CheckMessageForm from "../../components/CheckMessage.svelte"
  import SolveCard from "../../components/SolveCard.svelte"

  let extensions = [
    basicSetup,
    EditorView.theme({
      "&": {
        "font-size": "1rem"
      }
    })
  ]
  let answer_extensions = [
    EditorState.readOnly.of(true),
    ...extensions
  ]
  // client
  const url = API_URL;
  const api = new ApiClient(url)

  let pageNum: number = 1
  let answer: string = '';
  let answerError: string = '';

  let taskID: string | null = null
  let selectedTask: CourseTaskSolve | null
  $: selectedTask = null
  let taskSolvesPromise: Promise<CourseSolvesResponse> | null = null
  let taskResultPromise: Promise<TaskResultWrapper | null> | null = null

  let tasks: Array<CourseTaskSolve>
  $: tasks = []

  const sendSolution = async (): Promise<TaskCreateResponse | null> => {
    answerError = ''
    if (answer.length == 0) {
      answerError = 'Заполните поле решения!'
      return null
    }

    const res = await api.postTaskSolve(parseInt(taskID!), answer)
    if (typeof res === 'string') {
      answerError = taskCreateErrorToString(res)
      return null
    }

    answer = ''
    await refreshTasks()
    return res
  }

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
    goToPage(pageNum)
  }

  const goToPage = (newNum: number) => {
    pageNum = newNum
    taskSolvesPromise = taskSolvesWrapper()
  }

  const refreshTasks = async () => {
    unselectTask()
    goToPage(1)
  }

  let sendPromise: Promise<TaskCreateResponse | null> | null = null
</script>

{#if taskID != null}
  {#if answerError.length > 0}
    <DangerMessage title="Ошибка!" description={answerError} additionalStyle="is-fullwidth"/>
  {/if}
  <div class="field required">
    <label class="label"> Решение </label>
    <CodeMirror bind:doc={answer} {extensions}/>
  </div>
  {#if sendPromise == null}
    <button class="button is-fullwidth is-success" on:click={async () => { sendSolution() }}> Отправить решение </button>
  {:else}
    {#await sendPromise}
      <button class="button is-fullwidth is-success" disabled> Отправляем решение... </button>
    {:then}
      <button class="button is-fullwidth is-success" on:click={async () => { sendSolution() }}> Отправить решение </button>
    {:catch}
      <DangerMessage title="Неизвестная ошибка!" description="Попробуйте обновить страницу."/>
    {/await}
  {/if}
{/if}

{#if taskSolvesPromise != null}
  {#await taskSolvesPromise}
    <SuccessMessage title="Ожидайте" description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then taskResult}
    {#if selectedTask != null}
      <div class="box">
        <h2 class="subtitle is-4"> Решение { selectedTask.id } </h2>
        <div class="field">
          <label class="label"> Ответ пользователя </label>
          <CodeMirror bind:doc={selectedTask.input} bind:extensions={answer_extensions}/>
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
                    {#if result.result.messages.length > 0}
                      <h3 class="subtitle is-5 pt-5"> Сообщения </h3>
                      {#each result.result.messages as message}
                        <CheckMessageForm messageData={message} messageStyles="is-fullwidth"/>
                      {/each}
                    {/if}
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
              <SolveCard apiUrl={url} solveData={task} selectCallback={async (t) => selectTask(t)}/>
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
