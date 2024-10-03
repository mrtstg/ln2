<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { CourseTaskDetails } from "../../api/types"
  import DangerMessage from "../../components/DangerMessage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import InputTask from "./lib/InputTask.svelte";
  import { suggestWSProto } from "../../api/utils/vnc"
  import VmTask from "./lib/VMTask.svelte";

  // client
  // @ts-ignore: replaced by vite
  const url = API_URL;
  const api = new ApiClient(url)

  // url for websocket
  //@ts-ignore
  let wsUrl: string = WS_URL
  if (wsUrl.length == 0) {
    wsUrl = window.location.host
  }

  //@ts-ignore
  const wsProto: string = suggestWSProto(window, WS_PROTO)

  let taskID: string | null = null
  let taskDetailsPromise: Promise<CourseTaskDetails | string> | null = null

  const parsedURL = new URL(document.URL)
  const taskIDMatch = parsedURL.pathname.match("\/task\/([0-9].?)")
  if (taskIDMatch != null && taskIDMatch.length > 1) {
    taskID = taskIDMatch[1]
    taskDetailsPromise = api.getCourseTask(parseInt(taskID))
  }
</script>

{#if taskDetailsPromise != null && taskID != null}
  {#await taskDetailsPromise}
    <SuccessMessage title="Ожидайте" description="Получаем данные..." additionalStyle="is-fullwidth"/>
  {:then taskData}
    {#if typeof taskData === 'string'}
      <DangerMessage title="Ошибка!" description={"Ошибка получения данных: " + taskData} additionalStyle="is-fullwidth"/> 
    {:else}
      {#if taskData.type == 'container'}
        <InputTask taskID={taskID} apiUrl={url}/>
      {:else if taskData.type == 'vm'}
        <VmTask taskID={parseInt(taskID)} apiUrl={url} wsProto={wsProto} wsUrl={wsUrl}/>
      {/if}
    {/if}
  {:catch err}
    <DangerMessage title="Ошибка!" description="Не удалось загрузить информацию о задании." additionalStyle="is-fullwidth"/>
  {/await}
{/if}
