<script lang="ts">
  import NoVnc from "../../components/vnc/NoVNC.svelte";
  import { generateVNCLink, suggestWSProto } from "../../api/utils/vnc"
  import { ApiClient } from "../../api/client"

  //@ts-ignore
  const proto = suggestWSProto(window, WS_PROTO);

  //@ts-ignore
  let url: string = API_URL;
  let api: ApiClient = new ApiClient(url)
  if (url.length == 0) {
    url = window.location.host
  }

  //@ts-ignore
  const isDev = DEV_MODE === "1"

  let vmID: string | null = null
  const parsedURL = new URL(document.URL)
  const vmIDMatch = parsedURL.pathname.match("\/vm\/([0-9]*?)\/console")

  if (vmIDMatch != null && vmIDMatch.length > 1) {
    vmID = vmIDMatch[1]
  }
</script>

{#if vmID != null}
  {#if isDev}
    { JSON.stringify(proto + "://" + url + generateVNCLink(vmID)) }
  {/if}
  <NoVnc 
    url={proto + "://" + url + generateVNCLink(vmID)}
    getPowerCallback={() => { return api.getVMPowerState(vmID)}}
    setPowerCallback={() => { return api.switchVMPowerState(vmID)}}
  />
{/if}
