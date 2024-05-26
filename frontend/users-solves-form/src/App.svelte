<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { UserDetails } from "../../api/types";
  import UserQueryForm from "../../components/UserQueryForm.svelte"

  const url = API_URL
  let courseID: string | null = null
  const parsedURL = new URL(document.URL)
  const courseIDMatch = parsedURL.pathname.match("\/solves\/course\/(.*)")
  if (courseIDMatch != null && courseIDMatch.length > 1) {
    courseID = courseIDMatch[1]
  }

  const userCallback = async (data: UserDetails) => {
    let w = window.open("/solves/tasks/" + courseID + "/" + data.id, '_blank')
    if (w != null) {
      w.focus()
    }
  }
</script>

{#if courseID != null}
  <UserQueryForm apiUrl={url} courseId={courseID} getMembers={true} actionText="Решения" actionCallback={userCallback}/>
{/if}
