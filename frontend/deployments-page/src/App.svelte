<script lang="ts">
  import { onMount } from "svelte";
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

  const deploymentsWrapper = async (pageN: number): Promise<PageWrapper<Array<DeploymentRead>> | null> => {
    let res = await api.getMyDeployments(pageN)
    if (res != null) {
      deployments = res.objects
    }

    return res
  }

  onMount(() => {
    var intervalId = setInterval(() => {
      if (deployments != null) {
        deploymentsPromise = deploymentsWrapper(1)
      }
    }, 2500)

    return () => {
      clearInterval(intervalId)
    }
  })

  let deploymentsPromise: Promise<PageWrapper<Array<DeploymentRead>> | null> | null
  let deployments: Array<DeploymentRead> | null = null
  deploymentsPromise = deploymentsWrapper(1)
</script>

{#if deploymentsPromise != null}
  {#await deploymentsPromise}
    {#if deployments == null}
      <SuccessMessage title="Ожидайте" description="Загружаем данные..." additionalStyle="is-fullwidth"/>
    {/if}
  {:then response}
    {#if response == null}
      <WarningMessage title="Ошибка загрузки!" description="На стороне сервера произошла ошибка." additionalStyle="is-fullwidth"/>
    {/if}
  {:catch err}
    {#if deployments == null}
      <WarningMessage title="Ошибка загрузки!" description="Не удалось загрузить данные. Ошибка: {err}" additionalStyle="is-fullwidth"/>
    {/if}
  {/await}
  {#if deployments != null}
    {#if deployments.length > 0}
      {#each deployments as item (item.id)}
        <DeploymentView deploymentData={item} wsHost={wsUrl} wsProto={wsProto} showDesktopName={false} apiUrl={apiUrl}/>
      {/each}
    {:else}
      <SuccessMessage title="Пусто!" description="Похоже, что у вас нет активных развертываний. Начните прохождения задания с виртуальными машинами и они здесь появятся!" additionalStyle="is-fullwidth"/>
    {/if}
  {/if}
{/if}
