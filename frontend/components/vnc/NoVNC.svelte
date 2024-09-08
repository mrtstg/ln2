<script lang="ts">
  import { onMount } from "svelte";

  import RFB, { NoVncEvent } from "@novnc/novnc/lib/rfb"
  import { NoVncOptions } from "@novnc/novnc/lib/rfb";

  let desktopName = ""
  let quality: number = 7
  let parent: HTMLElement;
  let container: HTMLElement;
  let rfb: RFB | null = null

  export let showDesktopName: boolean = true
  export let url: string;

  const updateDesktop = (e: CustomEvent<{ name: string }>) => {
    desktopName = e.detail.name;
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
    rfb.showDotCursor = true
    rfb.viewOnly = false
    rfb.focusOnClick = false
    rfb.dragViewport = false
    rfb.clipViewport = false
    rfb.scaleViewport = true

    rfb.addEventListener("desktopname", updateDesktop)

    return () => {
      rfb?.disconnect()
    }
  })
</script>

<div class="vnc-container__wrapper">
  <div class="vnc-container">
    <div class="flex flex-row items-center justify-between dark:text-slate-200 text-slate-900">
      <h3 class="text-xl font-semibold"> { showDesktopName ? desktopName : "" } </h3>
    </div>
    <div class="w-max-content h-max-content" bind:this={container}>
      <div class="vnc-screen-container" bind:this={parent}></div>
    </div>
  </div>
</div>
