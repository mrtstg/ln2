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

  // just took these two from some gist, ngl - idk how it works
  const delayF = (msec: number, value: any) => {
    return new Promise(done => window.setTimeout((() => done(value)), msec));
  }

  const isFinished = (promise: any): Promise<boolean> => {
    return Promise.race([delayF(0, false), promise.then(() => true, () => true)]);
  }

  const taskDeploymentWrapper = async (): Promise<TaskDeploymentWrapper | string> => {
    const res = await api.getCourseTaskDeployment(taskID)

    if (typeof res === 'string' && currentState != null) {
      return res
    }

    currentState = res
    return res
  }

  const sendDeployRequest = async () => {
    await api.deployTaskDeployment(taskID)
    taskDeploymentPromise = taskDeploymentWrapper()
  }

  onMount(() => {
    taskDeploymentPromise = taskDeploymentWrapper()
    intervalId = setInterval(() => {
      isFinished(taskDeploymentPromise).then(finished => {
        if (finished) {
          taskDeploymentPromise = taskDeploymentWrapper()
        }
      })
    }, 2500)

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
            <br>
            <button class="button" on:click={sendDeployRequest}> Развернуть стенд </button>
          {:else}
            У вас уже запущено одно развертывание стенда. Дождитесь его окончания!
          {/if}
        </div>
      </article>
    {:else}
      <DeploymentView showDesktopName={true} wsHost={wsUrl} wsProto={wsProto} deploymentData={currentState.data} apiUrl={apiUrl} showTaskName={false}/>
    {/if}
  {/if}
{/if}
