<script lang="ts">
  import { ApiClient } from "../api/client"
  import type { UserDetails } from "../api/types/user"
  import type { PageWrapper } from "../api/types/pageWrapper"
  import DangerMessage from "./DangerMessage.svelte"
  import SuccessMessage from "./SuccessMessage.svelte"
  import SearchField from "./search/SearchField.svelte";
  import NumberPagination from "./NumberPagination.svelte"

  export let apiUrl: string
  export let courseId: string | null = null
  export let taskId: string | null = null
  export let getMembers: boolean = true
  export let getAdmins: boolean = false
  export let pageNumber: number = 1
  export let actionText: string = ''
  export let actionCallback: (data: UserDetails) => Promise<void> = async (_) => {}

  let client = new ApiClient(apiUrl)

  let query: string
  $: query = ''

  export const queryWrapper = () => {
    if (courseId != null) {
      loadCallback = client.queryCourseUsers(courseId, query, getMembers, getAdmins, pageNumber)
    } else if (taskId != null) {
      loadCallback = client.queryCourseUsersByTask(taskId, query, getMembers, getAdmins, pageNumber)
    } else {
      loadCallback = null
    }
  }

  let loadCallback: Promise<PageWrapper<Array<UserDetails>> | null> | null = null
  queryWrapper()
</script>

<SearchField bind:query={query} maxLength={30} queryWrapper={async () => queryWrapper()}/>

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
              <button class="button" on:click={async () => await actionCallback(userData)}> { actionText } </button>
            {/if}
          </div>
        </div>
      {/each}
      <NumberPagination bind:pageNumber={pageNumber} pageLoadCallback={async (_) => queryWrapper()} queryRes={queryRes}/>
    {/if}
  {:catch}
    <DangerMessage title="Ошибка" description="Не удалось загрузить базу пользователей" additionalStyle="is-fullwdith"/>
  {/await}
{/if}
