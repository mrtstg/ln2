<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { CommonCourseDetails, CourseTaskDetails } from "../../api/types";
  import DangerMessage from "../../components/DangerMessage.svelte"
  import CheckStageWidget from "../../components/CheckStage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import { taskPatchErrorToString } from "../../api/utils"

  // client declaration
  const url = API_URL;
  const api = new ApiClient(url)

  let taskTitle: string
  $: taskTitle = ''
  let taskContent: string
  $: taskContent = ''
  let taskOrder: number
  $: taskOrder = 0

  let taskID: number | null = null
  let courseID: string | null = null
  let taskPromise: Promise<CourseTaskDetails | string> | null = null
  let taskDeletePromise: Promise<string> | null = null
  let taskUpdatePromise: Promise<string> | null = null
  const parsedURL = new URL(document.URL)
  const taskIDMatch = parsedURL.pathname.match("\/task\/(.*?)\/([0-9]*?)\/edit")

  const updateTaskWrapper = async (): Promise<string> => {
    const res = await api.patchTask(taskID!, {
      name: taskTitle,
      content: taskContent,
      order: taskOrder
    })
    if (res == 'ok') {
      taskPromise = getCourseTaskWrapper()
      taskUpdatePromise = null
    }

    return res
  }

  const deleteTaskWrapper = async (): Promise<string> => {
    const res = await api.deleteTask(taskID!)
    if (res == 'ok') {
      window.location.replace("/course/" + courseID!)
    }

    return res
  }

  const getCourseTaskWrapper = async (): Promise<CourseTaskDetails> => {
    const res = await api.getCourseTask(taskID!)
    if (typeof res === 'string') {
      throw res
    }

    taskTitle = res.name
    taskContent = res.content
    taskOrder = res.order
    return res
  }

  if (taskIDMatch != null && taskIDMatch.length > 1) {
    courseID = taskIDMatch[1]
    taskID = parseInt(taskIDMatch[2])
    taskPromise = getCourseTaskWrapper()
  }
</script>

{#if taskPromise != null}
  <section class="hero is-info p-3 my-3">
    <h1 class="title"> Редактирование задания </h1>
  </section>
  {#await taskPromise}
    <SuccessMessage title="Ожидайте..." description="Получаем данные..." additionalStyle="is-fullwidth"/>
  {:then}
    <div class="field">
      <label class="label"> Заголовок задания </label>
      <div class="control">
        <input class="input" type="text" maxlength="100" bind:value={taskTitle}> 
      </div>
    </div>
    <div class="field">
      <label class="label"> Условие задачи </label>
      <div class="control">
        <textarea class="textarea" bind:value={taskContent}></textarea>
      </div>
      <p class="help"> Для разметки можно использовать <a href="https://www.markdownguide.org/cheat-sheet/" target="_blank">Markdown</a></p>
    </div>
    <div class="field">
      <label class="label"> Порядковый номер задачи </label>
      <div class="control">
        <input class="input" type="number" min="0" bind:value={taskOrder}/>
      </div>
      <p class="help"> Меньше - выше в списке задач </p>
    </div>
    {#if taskUpdatePromise != null}
      {#await taskUpdatePromise}
        <SuccessMessage title="Ожидайте..." description="Обновляем задачу..." additionalStyle="is-fullwidth"/>
      {:then res}
        {#if res == 'ok'}
          <article class="message is-success">
            <div class="message-body">
              Задача обновлена!
            </div>
          </article>
        {:else}
          <DangerMessage title="Ошибка!" description={taskPatchErrorToString(res)}/>
        {/if}
      {:catch}
        <DangerMessage title="Ошибка!" description="Не удалось обновить задачу!" additionalStyle="is-fullwidth"/>
      {/await}
    {/if}
    <button class="button is-fullwidth is-success my-3" on:click={() => taskUpdatePromise = updateTaskWrapper()}> Обновить задачу </button>
    {#if taskDeletePromise != null}
      {#await taskDeletePromise}
        <SuccessMessage title="Ожидайте..." description="Удаляем задачу..." additionalStyle="is-fullwidth"/>
      {:then res}
        {#if res != 'ok'}
          <DangerMessage title="Ошибка!" description={taskPatchErrorToString(res)}/>
        {/if}
      {:catch}
        <DangerMessage title="Ошибка!" description="Не удалось удалить задачу!" additionalStyle="is-fullwidth"/>
      {/await}
    {/if}
    <button class="button is-fullwidth is-danger my-3" on:click={() => taskDeletePromise = deleteTaskWrapper()}> Удалить задачу </button>
  {:catch}
    <DangerMessage title="Ошибка!" description="Не удалось получить данные задания" additionalStyle="is-fullwidth"/>
  {/await}
{/if}
