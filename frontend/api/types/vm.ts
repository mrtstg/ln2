export type VM = TemplateVM

// TODO: add omitted fields
export type TemplateVM = {
  type: string,
  template: string,
  name: string,
  sockets: number | null,
  cores: number | null,
  memory: number | null,
  networks: Array<VMNetworkInterface>,
  userAvailable: boolean,
  storage: string | null,
  startDelay: number,
  cpuLimit: number,
}

export type VMNetworkInterface = {
  type: NetworkDeviceType,
  bridge: string,
  firewall: boolean
}

export const serviceVMNetworks = ["internet"]

export type VMNetwork = {
  name: string
}

export const allNetworkDevicesTypes = [
  "e1000",
  "e1000-82540em",
  "e1000-82544gc",
  "e1000-82545em",
  "e1000e" ,
  "i82551" ,
  "i82557b",
  "i82559er",
  "ne2k_isa",
  "ne2k_pci",
  "pcnet",
  "rtl8139",
  "virtio" ,
  "vmxnet3"
]

export type NetworkDeviceType = typeof allNetworkDevicesTypes[number]
