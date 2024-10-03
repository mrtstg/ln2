<script lang="ts">
  import { ApiClient } from "../../../api/client"
  import type { TaskDeploymentWrapper } from "../../../api/types/deployment";
  import { onMount } from "svelte";
  import DeploymentView from "../../../components/vnc/DeploymentView.svelte"
  import SuccessMessage from "../../../components/SuccessMessage.svelte";

  export let taskID: number;
  export let apiUrl: string;
  export let wsUrl: string;
  export let wsProto: string;

  let currentState: TaskDeploymentWrapper | string | null
  $: currentState = null
  let intervalId: number | null = null

  const taskDeploymentWrapper = async (): Promise<TaskDeploymentWrapper | string> => {
    const res = await api.getCourseTaskDeployment(taskID)

    if (typeof res === 'string' && currentState != null) {
      return res
    }

    currentState = res
    return res
  }

  onMount(() => {
    taskDeploymentPromise = taskDeploymentWrapper()
    taskDeploymentPromise.then(_ => {
      intervalId = setInterval(() => {
        taskDeploymentPromise = taskDeploymentWrapper()
      }, 2500)
    })

    return () => {
      if (intervalId != null) {
        clearInterval(intervalId)
      }
    }
  })

  const api = new ApiClient(apiUrl)
  let taskDeploymentPromise: Promise<TaskDeploymentWrapper | string> | null = null
</script>

{#if taskDeploymentPromise != null}
  {#if currentState != null && typeof currentState != 'string'}
    {#if currentState.data == null}
      <article class="message">
        <div class="message-body">
          {#if !currentState.pending}
            Для данного задания не развернут стенд. Нажмите на кнопку ниже, что запустить развертывание стенда
            <button class="button"> Развернуть стенд </button>
          {:else}
            У вас уже запущено одно развертывание стенда. Дождитесь его окончания!
          {/if}
        </div>
      </article>
    {:else}
      <DeploymentView showDesktopName={true} wsHost={wsUrl} wsProto={wsProto} deploymentData={currentState.data}/>
    {/if}
  {/if}
{/if}
