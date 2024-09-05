// note: generates relative link by default
export const generateVNCLink = (vmid: string): string => {
  return "/vm/" + vmid + "/vnc"
}
