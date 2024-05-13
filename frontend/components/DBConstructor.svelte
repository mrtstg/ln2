<script lang="ts">
  import type { TableData, DatabaseData, ColumnData, ColumnType } from "../api/db"
  import { allTypes } from "../api/db";

  export let changeCallback: (tables: Array<TableData>) => void = () => {};
  export let dbData: DatabaseData = {tables: []}

  let tableData: Array<TableData>
  $: tableData = [...dbData.tables]

  let allFields: Array<String>

  const updateReferences = () => {
    let tableNames = tableData.map(el => el.name)
    for (let i = 0; i < tableData.length; i++) {
      for (let ci = 0; ci < tableData[i].columns.length; ci++) {
        if (tableData[i].columns[ci].references != null) {
          let refTable = tableData[i].columns[ci].references!.split(".")[0]
          if (!tableNames.includes(refTable)) {
            tableData[i].columns[ci].references = null
          }
        }
      }
    }
  }

  const correctTableName = (tableIndex: number) => {
    tableData[tableIndex].name = tableData[tableIndex].name.replace(".", "").replace(" ", "")
  }

  const updateAllFields = () => {
    allFields = tableData.map(el => el.columns.map(col => el.name + "." + col.name)).flat()
  }
  
  const addTable = () => {
    tableData = [...tableData, {name: "Впишите имя таблицы", columns: []}]
    changeCallback(tableData)
  }

  const addCell = (tableIndex: number) => {
    tableData[tableIndex].columns = [
      ...tableData[tableIndex].columns,
      {name: "", type: { type: "varchar" }, primary: false, unique: false, null: false, references: null}
    ]
    changeCallback(tableData)
    updateAllFields()
  }

  const deleteTable = (tableIndex: number) => {
    tableData.splice(tableIndex, 1)
    tableData = [...tableData]
    changeCallback(tableData)
    updateAllFields()
  }

  const removeCell = (tableIndex: number, cellIndex: number) => {
    tableData[tableIndex].columns.splice(cellIndex, 1)
    tableData[tableIndex] = tableData[tableIndex]
    changeCallback(tableData)
    updateAllFields()
  }
</script>

<button class="button is-fullwidth" on:click={addTable}> Добавить таблицу </button>
{#each tableData as table, tableIndex}
<div class="py-5">
  <div class="is-flex is-flex-direction-row is-fullwidth">
    <button class="button is-danger mr-3" on:click={() => deleteTable(tableIndex)}> X </button>
    <input class="input is-fullwidth" bind:value={table.name} on:change={() => { updateReferences(); changeCallback(tableData); updateAllFields()}}/>
  </div>  
  <table class="table is-fullwidth">
    <thead>
      <tr>
        <th> Название колонки </th>
        <th> Тип </th>
        <th> Первичный ключ? </th>
        <th> Уникальное? </th>
        <th> NULL? </th>
        <th> Cсылается на... </th>
      </tr>
    </thead>
    <tbody>
      {#each table.columns as column, colIndex}
        <tr>
          <th class="is-flex is-flex-direction-row">
            <button class="button is-danger mr-3" on:click={() => removeCell(tableIndex, colIndex)}> X </button>
            <input class="input" bind:value={column.name} on:change={() => { correctTableName(colIndex); changeCallback(tableData); updateAllFields()}}/>
          </th>
          <th>
            <div class="select">
              <select bind:value={column.type.type} on:change={() => changeCallback(tableData)}>
                {#each allTypes as opt}
                  <option value={opt}> { opt } </option>
                {/each}
              </select>
            </div>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.primary} on:change={() => changeCallback(tableData)}>
            </label>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.unique} on:change={() => changeCallback(tableData)}>
            </label>
          </th>
          <th>
            <label class="checkbox">
              <input type="checkbox" bind:checked={column.null} on:change={() => changeCallback(tableData)}>
            </label>
          </th>
          <th>
            <div class="select">
              <select bind:value={column.references} on:change={() => changeCallback(tableData)}>
                <option value={null}></option>
                {#each allFields as field}
                  <option value={field}> {field}</option>
                {/each}
              </select>
            </div>
          </th>
        </tr>
      {/each}
    </tbody>
    <tfoot>
      <tr>
        <th colspan="6"><button class="button is-fullwidth" on:click={() => addCell(tableIndex)}> Создать ряд </button></th>
      </tr>
    </tfoot>
  </table>
</div>
{/each}
