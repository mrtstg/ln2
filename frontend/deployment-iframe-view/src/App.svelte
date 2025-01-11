<script lang="ts">
  import { suggestWSProto } from "../../api/utils/vnc";
  import { ApiClient } from "../../api/client"
  import type { DeploymentRead, TaskDeploymentWrapper } from "../../api/types/deployment";
  import { onMount } from "svelte";
  import DeploymentView from "../../components/vnc/DeploymentView.svelte"

  //@ts-ignore
  const url = API_URL
  const api = new ApiClient(url)

  // url for websocket
  //@ts-ignore
  let wsUrl: string = WS_URL
  if (wsUrl.length == 0) {
    wsUrl = window.location.host
  }

  //@ts-ignore
  const wsProto: string = suggestWSProto(window, WS_PROTO)

  let deploymentMessage: string = ''
  let deploymentID: string | null = null
  const parsedURL = new URL(document.URL)
  const deploymentIDMatch = parsedURL.pathname.match(".*\/(.+)")
  if (deploymentIDMatch != null && deploymentIDMatch.length > 1) {
    deploymentID = deploymentIDMatch[1]
  }

  let currentState: DeploymentRead | string | null
  $: currentState = null
  let intervalId: number | null = null

  const delayF = (msec: number, value: any) => {
    return new Promise(done => window.setTimeout((() => done(value)), msec));
  }

  const isFinished = (promise: any): Promise<boolean> => {
    return Promise.race([delayF(0, false), promise.then(() => true, () => true)]);
  }

  const deploymentWrapper = async (): Promise<DeploymentRead | string> => {
    const res = await api.getDeployment(deploymentID)

    if (typeof res === 'string' && currentState != null) {
      return res
    }

    currentState = res
    if (typeof res != 'string') {
      deploymentMessage = ''
    } else {
      deploymentMessage = 'Ошибка: ' + res
    }
    return res
  }

  onMount(() => {
    deploymentPromise = deploymentWrapper()
    intervalId = setInterval(() => {
      isFinished(deploymentPromise).then(finished => {
        if (finished) {
          deploymentPromise = deploymentWrapper()
        }
      })
    }, 2500)

    return () => {
      if (intervalId != null) {
        clearInterval(intervalId)
      }
    }
  })

  let deploymentPromise: Promise<DeploymentRead | string> | null = null
</script>

{#if deploymentMessage.length > 0}
  <div class="notification is-warning">
    { deploymentMessage }
  </div>
{/if}
{#if deploymentPromise != null && typeof currentState != 'string' && currentState != null}
  <DeploymentView showDesktopName={true} wsHost={wsUrl} wsProto={wsProto} deploymentData={currentState} apiUrl={url} showTaskName={false}/>
{/if}
