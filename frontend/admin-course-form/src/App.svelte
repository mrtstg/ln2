<script lang="ts">
  import { PlusOutline } from 'flowbite-svelte-icons'
  import { ApiClient } from "../../api/client"
  import type { ContainerSummary, CommonCourseDetails } from "../../api/types";
  import DangerMessage from "../../components/DangerMessage.svelte"
  import CheckStageWidget from "../../components/CheckStage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import { type StageData, stageTypeList, StageType, type CheckStage, defaultCheckStageData } from "../../api/check_stage" 
  import { courseErrorsToString, courseTaskErrorToString, deleteCourseErrorToString } from "../../api/utils"

  // client declaration
  const url = API_URL;
  const api = new ApiClient(url)

  // course form details

  const patchCourseDetailsWrapper = async (courseID: string, courseName: string, courseDesc: string): Promise<CommonCourseDetails> => {
    const res = await api.patchCourse(courseID, courseName, courseDesc)
    if (typeof res === 'string') {
      throw res
    }

    courseName = res.name
    courseDesc = res.description
    return res
  }

  const deleteCourseWrapper = async (courseID: string): Promise<string> => {
    const res = await api.deleteCourse(courseID)
    if (res === 'Ok') {
      window.location.replace("/courses/admin")
      return ''
    }

    modalMessage = deleteCourseErrorToString(res)
    return res
  }

  const getCourseDetailsWrapper = async (courseID: string): Promise<CommonCourseDetails> => {
    const res = await api.getCourseDetails(courseID, true)
    if (typeof res === 'string') {
      throw res
    }

    courseName = res.name
    courseDesc = res.description
    return res
  }

  let courseID: string | null = null
  let coursePromise: Promise<CommonCourseDetails | string> | null = null
  const parsedURL = new URL(document.URL)
  const courseIDMatch = parsedURL.pathname.match("\/course\/(.*?)\/admin")
  if (courseIDMatch != null && courseIDMatch.length > 1) {
    courseID = courseIDMatch[1]
    coursePromise = getCourseDetailsWrapper(courseID)
  }
  
  let courseName: string;
  $: courseName = ''

  let courseDesc: string;
  $: courseDesc = ''

  // TODO: add confirmation
  const courseDeleteButton = async () => {
    await deleteCourseWrapper(courseID!)
  }

  const courseUpdateButton = async () => {
    coursePromise = patchCourseDetailsWrapper(courseID!, courseName, courseDesc)
  }

  // task form details

  let modalMessage: string;
  $: modalMessage = ''

  let modalHideable: boolean;
  modalHideable = true

  let taskOrder: number;
  $: taskOrder = 0;

  let selectedStand: string;
  $: selectedStand = ''

  let taskTitle: string;
  $: taskTitle = ''

  let taskDesc: string;
  $: taskDesc = ''

  let stages: CheckStage[]
  $: stages = []

  const addStage = () => {
    stages = [...stages, {type: stageTypeList[0], data: defaultCheckStageData(stageTypeList[0])}]
  }

  const deleteStage = (index: number) => {
    stages.splice(index, 1)
    stages = [...stages]
  }

  const createTask = async (exit: Boolean) => {
    if (taskTitle.length == 0) {
      modalMessage = "Заполните название задачи!"
      return
    }

    if (taskDesc.length == 0) {
      modalMessage = "Заполните условие задачи!"
      return
    }

    if (stages.length == 0) {
      modalMessage = "Проверка задачи должна содержать как минимум один этап!"
      return
    }

    if (stages.map(el => el.type).filter(el => el == StageType.PSQLGenerateDatabase).length > 1) {
      modalMessage = "В задаче может быть только один этап генерации базы данных!"
      return
    }

    const processStage = (data: StageData): StageData => {
      let ndata = { ...data }
      if (ndata.action === 'command' && ndata.recordInto.length == 0) {
        ndata.recordInto = null
      }
      return ndata
    }

    let stagesToSend = stages.map(v => v.data).map(processStage)
    const payload = {
      name: taskTitle,
      content: taskDesc,
      order: taskOrder,
      standIdentifier: selectedStand,
      standActions: stagesToSend
    }
    modalHideable = false
    modalMessage = 'Создаем задачу...'
    const res = await api.createCourseTask(courseID!, payload)
    modalHideable = true
    if (typeof res != 'string') {
      taskTitle = ''
      taskDesc = ''
      taskOrder = 0
      stages = []
      if (exit) {
        window.location.replace("/course/" + courseID)
        return
      } else {
        modalMessage = 'Задача создана!'
        return
      }
    }

    modalMessage = courseTaskErrorToString(res)
  }

  const hideModal = () => {
    if (modalHideable) {
      modalMessage = ''
    }
  }

  let standsPromise = api.getStands()
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

<div class="container p-5">
  {#if coursePromise != null}
    <section class="hero is-info p-3 my-3">
      <h1 class="is-size-3 has-text-weight-bold">Редактирование курса</h1>
    </section>
    {#await coursePromise}
      <SuccessMessage title="Ожидайте..." description="Загружаем данные"/>
    {:then courseData}
      <div class="field">
        <label class="label"> Название курса </label>
        <div class="control">
          <input class="input" type="text" maxlength="100" bind:value={courseName}/>
        </div>
        <p class="help"> Уникальное для всего сайта </p>
      </div>
      <div class="field">
        <label class="label"> Описание курса </label>
        <div class="control">
          <textarea class="textarea" rows="3" maxlength="150" bind:value={courseDesc}></textarea>
        </div>
      </div>
      <button class="is-success is-fullwidth button mb-3" on:click={courseUpdateButton}> Обновить курс </button>
      <button class="is-danger is-fullwidth button mb-3" on:click={courseDeleteButton}> Удалить курс </button>
    {:catch error}
      {#if typeof error === 'string'}
        <DangerMessage title="Ошибка!" description={courseErrorsToString(error)}/>
      {:else}
        <DangerMessage title="Ошибка!" description="Не удалось загрузить данные курса."/>
      {/if}
    {/await}
  {/if}

  <section class="hero is-info p-3 is-flex is-flex-direction-row is-fullwidth is-flex-wrap-wrap is-justify-content-space-between is-align-content-cente">
    <h1 class="is-size-3 has-text-weight-bold"> Создание задания </h1>
    <a target="_blank" href="/course/{courseID}" class="button"> Открыть курс </a>
  </section>
  <div class="field">
    <label class="label"> Заголовок задания </label>
    <div class="control">
      <input class="input" type="text" maxlength="100" bind:value={taskTitle}> 
    </div>
  </div>
  <div class="field">
    <label class="label"> Условие задачи </label>
    <div class="control">
      <textarea class="textarea" bind:value={taskDesc}></textarea>
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
  {#await standsPromise}
    <SuccessMessage title="Ожидайте..." description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then standsData}
    <h2 class="title is-4"> Схема проверки </h2>
    <h3 class="title is-5"> Выберите стенд для выполнения проверки </h3>
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
        <div class="is-flex is-align-items-center">
          <h3 class="title is-4 pr-3"> Этапы проверки </h3>
          <div class="mb-5">
            <button class="button is-link" on:click={addStage}>
              <span class="icon is-large">
                <PlusOutline/>
              </span>
            </button>
          </div>
        </div>
        {#each stages as stage, stageIndex (stage)}
          <CheckStageWidget 
            data={stage} 
            containers={containersData.map(v => v.name)} 
            updateCallback={async (v) => { stages[stageIndex] = v }}
            deleteCallback={() => deleteStage(stageIndex)}
          />
        {/each}
        <div class="columns is-multiline">
          <div class="column is-12">
            <button on:click={addStage} class="is-link button is-fullwidth"> Добавить </button>
          </div>
          <div class="column">
            <button class="is-success button is-fullwidth" on:click={async () => createTask(false)}> Создать задачу </button>
          </div>
          <div class="column">
            <button class="is-success button is-fullwidth" on:click={async () => createTask(true) }> Создать задачу и выйти </button>
          </div>
        </div>
      {:catch error}
        <DangerMessage title="Ошибка!" description="Не удалось загрузить данные стенда."/>
      {/await}
    {/if}
  {:catch error}
    <DangerMessage title="Ошибка!" description="Не удалось получить данные о доступных стеднах."/>
  {/await}
</div>
