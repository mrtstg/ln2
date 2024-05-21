<script lang="ts">
  import { ApiClient } from "../../api/client"
  import UserQueryForm from "../../components/UserQueryForm.svelte"

  const url = API_URL;
  const api = new ApiClient(url)

  let courseID: string | null = null
  const parsedURL = new URL(document.URL)
  const courseIDMatch = parsedURL.pathname.match("\/course\/(.*?)\/members")
  if (courseIDMatch != null && courseIDMatch.length > 1) {
    courseID = courseIDMatch[1]
  }

  const updateWindows = () => {
    membersUpdate()
    usersUpdate()
  }

  const userCallback = async (login: string) => {
    await api.assignCourseMember(courseID!, login)
    updateWindows()   
  }

  let usersUpdate: () => void
  let membersUpdate: () => void

  let hideMembers: boolean = false
  let hideUsers: boolean = true
</script>

{#if courseID != null}
  <div class="is-flex is-flex-direction-row is-justify-content-space-between is-align-items-center is-fullwidth my-3">
    <h2 class="subtitle is-4"> Участники курса </h2>
    <button class="button" on:click={() => { hideMembers = !hideMembers }}> { hideMembers ? 'Показать' : 'Скрыть' } </button>
  </div>
  {#if !hideMembers}
    <UserQueryForm bind:queryWrapper={membersUpdate} apiUrl={url} courseId={courseID} actionText="Исключить" actionCallback={userCallback}/>
  {/if}

  <div class="is-flex is-flex-direction-row is-justify-content-space-between is-align-items-center is-fullwidth my-3">
    <h2 class="subtitle is-4"> Добавить участников </h2>
    <button class="button" on:click={() => { hideUsers = !hideUsers }}> { hideUsers ? 'Показать' : 'Скрыть' } </button>
  </div>
  {#if !hideUsers}
    <UserQueryForm bind:queryWrapper={usersUpdate} apiUrl={url} courseId={courseID} getMembers={false} actionText="Добавить" actionCallback={userCallback}/>
  {/if}
{/if}

