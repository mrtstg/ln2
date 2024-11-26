<script lang="ts">
  import { onMount } from "svelte";

  import RFB, { NoVncEvent } from "@novnc/novnc/lib/rfb"
  import { NoVncOptions } from "@novnc/novnc/lib/rfb";

  let desktopName = ""
  let connected: boolean = false
  let quality: number = 7
  let parent: HTMLElement;
  let container: HTMLElement;
  let rfb: RFB | null = null

  export let showDesktopName: boolean = true
  export let url: string;
  export let onConnectCallback: () => Promise<void> = async () => {}
  export let onDisconnectCallback: () => Promise<void> = async () => {}

  const updateDesktop = (e: CustomEvent<{ name: string }>) => {
    desktopName = e.detail.name;
  }

  const updateConnect = (state: boolean) => {
    connected = state
  }

  const changeQuality = (delta: number) => {
    if (quality + delta > 0 && quality + delta < 10) {
      quality = quality + delta
    }
  }

  onMount(() => {
    rfb = new RFB(parent, url)
    rfb.compressionLevel = 3
    rfb.qualityLevel = quality
    rfb.viewOnly = false
    rfb.dragViewport = false
    rfb.clipViewport = false
    rfb.scaleViewport = true

    rfb.addEventListener("desktopname", updateDesktop)
    rfb.addEventListener("connect", (_) => { updateConnect(true); onConnectCallback() })
    rfb.addEventListener("disconnect", (_) => { updateConnect(false); onDisconnectCallback() })

    return () => {
      rfb?.disconnect()
    }
  })
</script>

<div class="vnc-container__wrapper">
  <div class="vnc-container">
    <div class="flex flex-row items-center justify-between dark:text-slate-200 text-slate-900 w-full">
      {#if showDesktopName}
        <h3 class="text-xl font-semibold"> { showDesktopName ? (desktopName.length == 0 ? "Виртуальная машина" : desktopName) : "" } - { connected ? "подключено" : "отключено" } </h3>
      {:else}
        <h3 class="text font-semibold"> { connected ? "Подключено" : "Отключено" } </h3>
      {/if}
    </div>
    <div class="w-max-content h-max-content" bind:this={container}>
      <div class="vnc-screen-container" bind:this={parent}></div>
    </div>
  </div>
</div>
