<script lang="ts">
  import type { UserDetails } from "../../api/types/user";
  import UserQueryForm from "../../components/UserQueryForm.svelte"

  //@ts-ignore
  const url = API_URL
  let taskID: string | null = null
  const parsedURL = new URL(document.URL)
  const courseIDMatch = parsedURL.pathname.match("\/solves\/tasks\/(.*)")
  if (courseIDMatch != null && courseIDMatch.length > 1) {
    taskID = courseIDMatch[1]
  }

  const userCallback = async (data: UserDetails) => {
    let w = window.open("/solves/task/" + taskID + "/" + data.id, '_blank')
    if (w != null) {
      w.focus()
    }
  }
</script>

{#if taskID != null}
  <UserQueryForm apiUrl={url} courseId={null} taskId={taskID} getMembers={true} actionText="Решения" actionCallback={userCallback}/>
{/if}
