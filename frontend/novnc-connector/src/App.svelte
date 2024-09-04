<script lang="ts">
  import NoVnc from "./lib/NoVNC.svelte";

  //@ts-ignore
  const proto = WS_PROTO;

  //@ts-ignore
  const url = API_URL;

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
