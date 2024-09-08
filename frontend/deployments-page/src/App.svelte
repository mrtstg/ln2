<script lang="ts">
  import type { DeploymentRead } from "../../api/types/deployment"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import WarningMessage from "../../components/WarningMessage.svelte"
  import DeploymentView from "../../components/vnc/DeploymentView.svelte"
  import { ApiClient } from "../../api/client"
  import type { PageWrapper } from "../../api/types/pageWrapper";
  import { suggestWSProto } from "../../api/utils/vnc"

  //@ts-ignore
  const apiUrl = API_URL
  const api = new ApiClient(apiUrl)
  // url for websocket
  //@ts-ignore
  let wsUrl: string = WS_URL
  if (wsUrl.length == 0) {
    wsUrl = window.location.host
  }

  //@ts-ignore
  const wsProto: string = suggestWSProto(window, WS_PROTO)

  let deploymentsPromise: Promise<PageWrapper<Array<DeploymentRead>> | null> | null
  deploymentsPromise = api.getMyDeployments(1)
</script>

{#if deploymentsPromise != null}
  {#await deploymentsPromise}
    <SuccessMessage title="Ожидайте" description="Загружаем данные..." additionalStyle="is-fullwidth"/>
  {:then response}
    {#if response == null}
      <WarningMessage title="Ошибка загрузки!" description="На стороне сервера произошла ошибка. Повторите попытку позже." additionalStyle="is-fullwidth"/>
    {:else}
      {#each response.objects as item} 
        <DeploymentView deploymentData={item} wsHost={wsUrl} wsProto={wsProto} showDesktopName={false}/>
      {/each}
    {/if}
  {:catch err}
    <WarningMessage title="Ошибка загрузки!" description="Не удалось загрузить данные. Ошибка: {err}" additionalStyle="is-fullwidth"/>
  {/await}
{/if}
