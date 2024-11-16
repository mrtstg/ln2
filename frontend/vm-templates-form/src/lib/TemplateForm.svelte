<script lang="ts">
  import type { VMTemplateCreate } from "../../../api/types/template";

  export let formData: VMTemplateCreate
</script>

<div class="field">
  <label class="label">
    Название шаблона
    <input class="input" type="text" maxlength="50" bind:value={formData.name}>
    <p class="help"> Уникальное значение </p>
  </label>
</div>
<div class="field">
  <label class="label">
    ID шаблона
    <input class="input" type="number" min="101" bind:value={formData.id}>
    <p class="help"> Уникальное значение. Берется из Proxmox </p>
  </label>
</div>
<div class="field">
  <label class="label">
    Комментарий к шаблону
    <textarea class="textarea" rows="2" bind:value={formData.comment}></textarea>
  </label>
</div>
