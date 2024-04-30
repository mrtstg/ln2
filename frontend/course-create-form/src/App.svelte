<script lang="ts">
  import { ApiClient } from "../../api/client"
  import { courseCreateErrorToString } from "../../api/utils"
  import type { CommonCourseDetails, CourseCreate } from "../../api/types"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"

  const url = API_URL;
  const api = new ApiClient(url)

  let courseName: string;
  $: courseName = ''

  let courseDesc: string;
  $: courseDesc = ''

  let errorMessage: string;
  $: errorMessage = ''

  let coursePromise: Promise<CommonCourseDetails | string> | null = null

  const courseCreateWrapper = async (): Promise<CommonCourseDetails | string> => {
    const res = await api.createCourse({name: courseName, description: courseDesc})
    if (typeof res === 'string') {
      errorMessage = courseCreateErrorToString(res)
      coursePromise = null
      return res
    }
    location.reload()
    return res
  }

  const courseCreateButton = () => {
    if (courseName.trim().length == 0) {
      errorMessage = 'Укажите название курса!'
      return
    }

    coursePromise = courseCreateWrapper()
  }
</script>

<div class="box">
  {#if errorMessage.length > 0}
    <DangerMessage title="Ошибка!" description={errorMessage} additionalStyle="is-fullwidth"/>
  {/if}
  <h1 class="title"> Создание курса </h1>
  {#if coursePromise == null}
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
    <button class="is-success is-fullwidth button" on:click={courseCreateButton}> Создать курс </button>
  {:else}
    {#await coursePromise}
      <div class="notification is-info">
        Создаем курс. Ожидайте...
      </div>
    {:then}
      <SuccessMessage title="Успешно!" description="Курс создан!" additionalStyle="is-fullwidth"/>
    {:catch}
      <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
    {/await}
  {/if}
</div>
