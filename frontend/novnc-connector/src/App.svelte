<script lang="ts">
  import NoVnc from "../../components/vnc/NoVNC.svelte";

  //@ts-ignore
  const proto = WS_PROTO;

  //@ts-ignore
  let url: string = API_URL;
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
    { JSON.stringify(proto + "://" + url + "/vm/" + vmID + "/vnc") }
  {/if}
  <NoVnc url={proto + "://" + url + "/vm/" + vmID + "/vnc"}/>
{/if}
