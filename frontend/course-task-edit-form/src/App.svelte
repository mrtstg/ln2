<script lang="ts">
  import { ApiClient } from "../../api/client"
  import { StageType, stageDataToCheckStage, stageTypeList, defaultCheckStageData } from "../../api/check_stage"
  import type { CheckStage, StageData } from "../../api/check_stage";
  import type { CommonCourseDetails, CourseTaskDetails, ContainerSummary } from "../../api/types";
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
  let stages: CheckStage[]
  $: stages = []
  let selectedStand: string;
  $: selectedStand = ''
  let modalMessage: string;
  $: modalMessage = ''
  let modalHideable: boolean;
  modalHideable = true

  let taskID: number | null = null
  let courseID: string | null = null
  let taskPromise: Promise<CourseTaskDetails | string> | null = null
  let taskDeletePromise: Promise<string> | null = null
  let taskUpdatePromise: Promise<string> | null = null
  const parsedURL = new URL(document.URL)
  const taskIDMatch = parsedURL.pathname.match("\/task\/(.*?)\/([0-9]*?)\/edit")

  const updateTaskWrapper = async (): Promise<string> => {
    if (taskTitle.length == 0) {
      return "Заполните название задачи!"
    }

    if (taskContent.length == 0) {
      return "Заполните условие задачи!"
    }

    if (stages.length == 0) {
      return "Проверка задачи должна содержать как минимум один этап!"
    }

    if (stages.map(el => el.type).filter(el => el == StageType.PSQLGenerateDatabase).length > 1) {
      return "В задаче может быть только один этап генерации базы данных!"
    }

    const processStage = (data: StageData): StageData => {
      let ndata = { ...data }
      if (ndata.action === 'command' && ndata.recordInto.length == 0) {
        ndata.recordInto = null
      }
      return ndata
    }

    let stagesToSend = stages.map(v => v.data).map(processStage)
    const res = await api.patchTask(taskID!, {
      name: taskTitle,
      content: taskContent,
      order: taskOrder,
      standActions: stagesToSend
    })
    if (res == 'ok') {
      taskPromise = getCourseTaskWrapper()
      taskUpdatePromise = null
    }

    return res
  }

  const addStage = () => {
    stages = [...stages, {type: stageTypeList[0], data: defaultCheckStageData(stageTypeList[0])}]
  }

  const deleteStage = (index: number) => {
    stages.splice(index, 1)
    stages = [...stages]
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
    if (res.standIdentifier != undefined && res.standActions != undefined) {
      res.standActions.forEach(el => {
        let res = stageDataToCheckStage(el)
        console.log(res)
        if (res != null) {
          stages = [...stages, res]
        }
      })
      selectedStand = res.standIdentifier
      containersPromise = api.getStandContainers(selectedStand)
    }
    return res
  }

  const hideModal = () => {
    if (modalHideable) {
      modalMessage = ''
    }
  }

  if (taskIDMatch != null && taskIDMatch.length > 1) {
    courseID = taskIDMatch[1]
    taskID = parseInt(taskIDMatch[2])
    taskPromise = getCourseTaskWrapper()
  }

  let containersPromise: Promise<Array<ContainerSummary>> | null = null
</script>

{#if modalMessage.length > 0}
  <div class="modal is-active">
    <div class="modal-background" on:click={hideModal}></div>
    <div class="modal-content">
      <div class="box">
        <p> { modalMessage } </p>
      </div>
    </div>
    <button class="modal-close is-large" aria-label="close" on:click={hideModal}></button>
  </div>
{/if}

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
    <h2 class="title is-4"> Схема проверки </h2>
    {#if containersPromise != null}
      {#await containersPromise}
        <SuccessMessage title="Ожидайте..." description="Загружаем данные..."/>
      {:then containersData}
        {#each stages as stage, stageIndex (stage)}
          <CheckStageWidget 
            data={stage} 
            containers={containersData.map(v => v.name)} 
            updateCallback={async (v) => { stages[stageIndex] = v }}
            deleteCallback={() => deleteStage(stageIndex)}
          />
        {/each}
        <button on:click={addStage} class="is-link button is-fullwidth"> Добавить </button>
      {:catch error}
        <DangerMessage title="Ошибка!" description="Не удалось загрузить данные стенда."/>
      {/await}
    {/if}

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
