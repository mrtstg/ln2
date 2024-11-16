<script lang="ts">
  import { ApiClient } from "../../../api/client"
  import SearchField from "../../../components/search/SearchField.svelte"
  import NumberPagination from "../../../components/NumberPagination.svelte"
  import type { TemplateError, VMTemplate } from "../../../api/types/template";
  import type { PageWrapper } from "../../../api/types/pageWrapper";
  import type { ErrorWrapper } from "../../../api/types/errorWrapper";
  import DangerMessage from "../../../components/DangerMessage.svelte"
  import SuccessMessage from "../../../components/SuccessMessage.svelte"

  export let apiUrl: string
  export let pageNumber: number = 1
  export let query: string = ''
  export let actionCallback: (data: VMTemplate) => Promise<void> = async (_) => {}

  let client = new ApiClient(apiUrl)

  const queryWrapper = () => {
    loadCallback = client.queryVMTemplates({ pageSize: 30, query: query, page: pageNumber })
  }

  let loadCallback: Promise<PageWrapper<Array<VMTemplate>> | ErrorWrapper<TemplateError>> | null = null
  queryWrapper()
</script>

<SearchField bind:query={query} maxLength={30} queryWrapper={async () => queryWrapper()}/>

{#if loadCallback != null}
  {#await loadCallback}
    <SuccessMessage title="Ожидайте" description="Загружаем шаблоны..." additionalStyle="is-fullwdith"/>
  {:then queryRes}
    {#if 'type' in queryRes}
      <DangerMessage title="Ошибка" description="Не удалось загрузить базу шаблонов" additionalStyle="is-fullwdith"/>
    {:else}
      {#each queryRes.objects as templateData}
        <div class="box">
          <div class="is-flex is-align-items-center is-flex-direction-row is-justify-content-space-between is-fullwidth">
            <span> { templateData.name } ({ templateData.id }) </span>
            <button class="button" on:click={async () => await actionCallback(templateData)}> Редактировать </button>
          </div>
          <!-- TODO: maybe fails newlines -->
          <span> { templateData.comment } </span>
        </div>
      {/each}
      <NumberPagination bind:pageNumber={pageNumber} pageLoadCallback={async (_) => queryWrapper()} queryRes={queryRes}/>
    {/if}
  {:catch}
    <DangerMessage title="Ошибка" description="Не удалось загрузить базу шаблонов" additionalStyle="is-fullwdith"/>
  {/await}
{/if}
