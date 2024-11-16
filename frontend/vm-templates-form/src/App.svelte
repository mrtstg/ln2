<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { VMTemplate, VMTemplatePatch, VMTemplateQuery, VMTemplateCreate } from "../../api/types/template"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import { templateErrorToString } from "../../api/utils/template"
  import TemplateForm from "./lib/TemplateForm.svelte";
  import TemplateQueryForm from "./lib/TemplateQueryForm.svelte"

  //@ts-ignore
  let url = API_URL
  const api = new ApiClient(url)
  const defaultVMData: VMTemplateCreate = {id: 0, name: '', comment: ''}

  const dropFormData = () => {
    formData = Object.assign({}, defaultVMData)
  }

  const selectTemplate = async (data: VMTemplate) => {
    formData = Object.assign({}, data)
    selectedTemplate = data
  }

  const unselectTemplate = () => {
    selectedTemplate = null
    dropFormData()
  }

  const updateTemplate = () => {
    updateTemplateCallback = updateTemplateWrapper()
  }

  const createTemplate = () => {
    createTemplateCallback = createTemplateWrapper()
  }

  const createTemplateWrapper = async (): Promise<string> => {
    const res = await api.createVMTemplate(formData)
    if ('type' in res) {
      return templateErrorToString(res)
    } else {
      return 'ok'
    }
  }

  const updateTemplateWrapper = async (): Promise<string> => {
    const res = await api.patchVMTemplate(selectedTemplate!.id, {
    name: formData.name.length > 0 && formData.name != selectedTemplate!.name ? formData.name : null,
    id: formData.id != selectedTemplate!.id ? formData.id : null,
    comment: formData.comment
  })
  if ('type' in res) {
    return templateErrorToString(res)
  } else {
    // TODO: queryWrapper()
    unselectTemplate()
    return 'ok'
  }
}

let pageNumber: number = 1
let query: string = ''
let formData: VMTemplate = Object.assign({}, defaultVMData)
let selectedTemplate: VMTemplate | null = null
let creatingTemplate: boolean = false

let updateTemplateCallback: Promise<string> | null = null
let createTemplateCallback: Promise<string> | null = null
</script>

<div class="pb-3">
{#if !creatingTemplate}
<div class="button is-success is-fullwidth" on:click={() => creatingTemplate = true}> Создать шаблон </div>
{:else}
<div class="button is-success is-fullwidth" on:click={() => creatingTemplate = false}> Вернуться назад </div>
{/if}
</div>

{#if !creatingTemplate}
{#if selectedTemplate == null}
  <TemplateQueryForm apiUrl={url} bind:query={query} bind:pageNumber={pageNumber} actionCallback={selectTemplate}/>
{:else}
  <div class="box has-text-weight-normal">
    <a class="button mb-5" href="#" on:click={unselectTemplate}> Назад </a>
    <TemplateForm bind:formData={formData}/>
    {#if updateTemplateCallback == null}
      <button class="button is-success is-fullwidth p-3" on:click={updateTemplate}> Обновить данные </button>
    {:else}
      {#await updateTemplateCallback}
        <SuccessMessage title="Обновляем данные..." description="" additionalStyle="is-fullwidth"/>
      {:then res}
        {#if res != 'ok'}
          <DangerMessage title="Ошибка!" description={res} additionalStyle="is-fullwidth"/>
        {/if}
      {:catch}
        <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
        <button class="button is-success is-fullwidth p-3" on:click={updateTemplate}> Обновить данные </button>
      {/await}
    {/if}
    </div>
  {/if}
{:else}
  <div class="box has-text-weight-normal">
    <TemplateForm bind:formData={formData}/>
    {#if createTemplateCallback == null}
      <button class="button is-success is-fullwidth p-3" on:click={createTemplate}> Создать шаблон </button>
    {:else}
      {#await createTemplateCallback}
        <SuccessMessage title="Создаем шаблон..." description="" additionalStyle="is-fullwidth"/>
      {:then res}
        {#if res != 'ok'}
          <DangerMessage title="Ошибка!" description={res} additionalStyle="is-fullwidth"/>
        {/if}
      {:catch}
        <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
        <button class="button is-success is-fullwidth p-3" on:click={createTemplate}> Создать шаблон </button>
      {/await}
    {/if}
  </div>
{/if}
