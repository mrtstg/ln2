<script lang="ts">
  import { ApiClient } from "../api/client"
  import type { UserQuery } from "../api/types"
  import DangerMessage from "./DangerMessage.svelte"
  import SuccessMessage from "./SuccessMessage.svelte"

  export let apiUrl: string
  export let courseId: string
  export let getMembers: boolean = true
  export let getAdmins: boolean = false
  export let pageNumber: number = 1
  export let actionText: string = ''
  export let actionCallback: (login: string) => Promise<void> = async (_) => {}

  let client = new ApiClient(apiUrl)

  let query: string
  $: query = ''

  export const queryWrapper = () => {
    loadCallback = client.queryCourseUsers(courseId, query, getMembers, getAdmins, pageNumber)
  }

  const cleanQuery = () => {
    query = ''
    queryWrapper()
  }

  const goToPage = (page: number) => {
    pageNumber = page
    queryWrapper()
  }

  const nextPage = () => { goToPage(pageNumber + 1) }
  const prevPage = () => { goToPage(pageNumber - 1) }

  let loadCallback: Promise<UserQuery | null> | null = null
  queryWrapper()
</script>

<div class="is-flex is-flex-direction-row is-justify-content-space-between is-fullwidth mb-5">
  <input maxlength="30" class="input is-flex-grow-1" type="text" placeholder="Ваш запрос" bind:value={query}>
  <button class="button ml-3" on:click={queryWrapper}> Искать </button>
  <button class="button ml-3" on:click={cleanQuery}> Очистить </button>
</div>

{#if loadCallback != null}
  {#await loadCallback}
    <SuccessMessage title="Ожидайте" description="Загружаем пользователей..." additionalStyle="is-fullwdith"/>
  {:then queryRes}
    {#if queryRes == null}
      <DangerMessage title="Ошибка" description="Не удалось загрузить базу пользователей" additionalStyle="is-fullwdith"/>
    {:else}
      {#each queryRes.objects as userData}
        <div class="box">
          <div class="is-flex is-align-items-center is-flex-direction-row is-justify-content-space-between is-fullwidth">
            <span> { userData.name } </span>
            {#if actionText.length > 0}
              <button class="button" on:click={async () => await actionCallback(userData.login)}> { actionText } </button>
            {/if}
          </div>
        </div>
      {/each}
      <nav class="pagination is-centered" role="navigation">
        <button class="pagination-previous" on:click={prevPage} disabled={pageNumber == 1}> Назад </button>

        <button class="pagination-next" on:click={nextPage} disabled={pageNumber * queryRes.pageSize >= queryRes.total}> Вперед </button>

        <ul class="pagination-list">
          {#if pageNumber >= 3}
            <li><a class="pagination-link" on:click={() => goToPage(1)}> 1 </a></li>
            <li><span class="pagination-ellipsis">&hellip;</span></li>
          {/if}
          {#if pageNumber >= 2}
            <li><a class="pagination-link" on:click={prevPage}> { pageNumber - 1 } </a></li>
          {/if}
          <li><a class="pagination-link is-current"> { pageNumber } </a></li>
          {#if pageNumber * queryRes.pageSize < queryRes.total}
            <li><a class="pagination-link" on:click={nextPage}> { pageNumber + 1} </a></li>
          {/if}
        </ul>
      </nav>
    {/if}
  {:catch}
    <DangerMessage title="Ошибка" description="Не удалось загрузить базу пользователей" additionalStyle="is-fullwdith"/>
  {/await}
{/if}
